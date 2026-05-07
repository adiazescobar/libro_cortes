# IV y LATE — Simulación pedagógica
# Curso: Econometría Avanzada (Javeriana)
#
# Parte A: Muestras finitas — OLS sesgado vs IV consistente
# Parte B: LATE paso a paso (Wald = LATE en compliers)

suppressPackageStartupMessages(library(AER))

set.seed(20260506)

# =============================================================
# PARTE A — MUESTRAS FINITAS: OLS SESGADO vs IV CONSISTENTE
# =============================================================
# DGP (instrumento DÉBIL: π=0.2):
#   z, w, eD, u  ~ iid N(0,1)
#   D = 0.2*z + eD + w        (z apenas mueve a D)
#   y = 0.5*D + w + u         (tau verdadero = 0.5; w confounder)
# Sesgo OLS:  cov(D,w)/var(D) ≈ 0.49 → plim(OLS) ≈ 0.99
# IV consistente (plim = 0.5) PERO MUY sesgado en muestra finita
#   cuando F primera etapa es bajo (Bound, Jaeger & Baker 1995).

simulate_one <- function(n) {
  z  <- rnorm(n); w <- rnorm(n); eD <- rnorm(n); u <- rnorm(n)
  D  <- 0.2*z + eD + w
  y  <- 0.5*D + w + u
  b_ols <- coef(lm(y ~ D))[2]
  b_iv  <- cov(z, y) / cov(z, D)
  rho   <- cor(z, D)
  F1    <- (rho^2 / (1 - rho^2)) * (n - 2)
  c(b_ols = unname(b_ols), b_iv = unname(b_iv), F1 = F1)
}

montecarlo <- function(n, reps = 5000) {
  out <- replicate(reps, simulate_one(n))
  data.frame(b_ols = out[1,], b_iv = out[2,], F1 = out[3,])
}

cat(strrep("=", 82), "\n")
cat("PARTE A — Muestras finitas (5000 réplicas, tau verdadero = 0.5)\n")
cat("Instrumento DÉBIL: π=0.2 → en N pequeño el IV se acerca a OLS\n")
cat(strrep("=", 82), "\n")
res_A <- do.call(rbind, lapply(c(30, 100, 300, 1000, 10000), function(n) {
  df <- montecarlo(n)
  data.frame(N = n,
             med_OLS  = median(df$b_ols),
             mean_IV  = mean(df$b_iv),
             med_IV   = median(df$b_iv),
             IV_p25   = quantile(df$b_iv, 0.25, names = FALSE),
             IV_p75   = quantile(df$b_iv, 0.75, names = FALSE),
             F1_mean  = mean(df$F1))
}))
print(res_A, row.names = FALSE, digits = 4)
cat("\nNota: la MEDIA del IV es inestable en N pequeño (colas pesadas con instrumento\n")
cat("      débil). La MEDIANA y el IQR muestran mejor el sesgo de muestra finita.\n")

# =============================================================
# PARTE B — LATE PASO A PASO (réplica de Clase19.pdf)
# =============================================================
# never-takers (d00=1):  5,000  → D=0 siempre,  LATE = -0.5
# always-takers (d11=1): 5,000  → D=1 siempre,  LATE =  0
# compliers (d01=1):    10,000  → D=Z,          LATE = +1
# IV con Z debe recuperar SOLO el LATE de compliers = 1.0

cat("\n", strrep("=", 70), "\n", sep = "")
cat("PARTE B — LATE paso a paso\n")
cat(strrep("=", 70), "\n")

set.seed(54687)
N <- 20000
Z <- as.integer(runif(N) > 0.5)

idx <- seq_len(N)
d00 <- as.integer(idx <= 5000)
d11 <- as.integer(idx > 5000 & idx <= 10000)
d01 <- as.integer(idx > 10000)

late <- ifelse(d00 == 1, -0.5, ifelse(d11 == 1, 0, 1))

y0 <- 0.25 * rnorm(N)
y1 <- y0 + late
D  <- d11 + Z * d01
y  <- D*y1 + (1-D)*y0

cat(sprintf("\nFracciones por tipo: never=%.2f, always=%.2f, compliers=%.2f\n",
            mean(d00), mean(d11), mean(d01)))
cat(sprintf("ATE poblacional (promedio de 'late'): %.4f\n", mean(late)))
cat(sprintf("LATE de compliers (lo que debería recuperar IV): %.4f\n",
            mean(late[d01 == 1])))

b_ols <- coef(lm(y ~ D))[2]
cat(sprintf("\nOLS   coef D : %.4f   (no recupera ATE=0.375 ni LATE=1.0)\n", b_ols))

iv_fit <- ivreg(y ~ D | Z)
b_iv <- coef(iv_fit)[2]
cat(sprintf("IV    coef D : %.4f   (debe ≈ 1.0 = LATE de compliers)\n", b_iv))

wald <- (mean(y[Z==1]) - mean(y[Z==0])) / (mean(D[Z==1]) - mean(D[Z==0]))
cat(sprintf("Wald  manual : %.4f   (= IV, por construcción)\n", wald))
