# =============================================================
# RDD_simulacion.R
# Econometría Avanzada — Javeriana
# Acompañamiento al Cap. Regresión Discontinua (libro_cortes)
#
# Cinco partes (espejo del do-file de Stata):
#   A. RDN con DGP lineal
#   B. RDN con DGP curvo (Gelman & Imbens 2019)
#   C. Sensibilidad al bandwidth
#   D. Tests de validez (rddensity, covariables, placebos)
#   E. RDB (rdrobust fuzzy + 2SLS local)
#   F. Variable de asignación discreta (Kolesár & Rothe 2018)
#
# Requisitos:
#   install.packages(c("rdrobust", "rddensity", "AER"))
#   # Opcional para honest CIs en Parte F:
#   install.packages("RDHonest")
# =============================================================

suppressPackageStartupMessages({
  library(rdrobust)
  library(rddensity)
  library(AER)            # ivreg para 2SLS local
})

SEED <- 20260511

cat_section <- function(title) {
  cat("\n", strrep("=", 60), "\n", title, "\n",
      strrep("=", 60), "\n", sep = "")
}

# =============================================================
# PARTE A — RDN con DGP lineal
# =============================================================
cat_section("PARTE A — RDN con DGP lineal")

set.seed(SEED)
N  <- 2000
Z  <- runif(N, 50, 74)
D  <- as.integer(Z >= 62)
Zt <- Z - 62
y  <- 1 + 0.10 * Zt + 2.0 * D + rnorm(N)

# Visualización primero
png("rdplot_A.png", width = 800, height = 600)
rdplot(y, Z, c = 62, title = "RDN — DGP lineal",
       x.label = "Edad", y.label = "Resultado")
dev.off()

# Estimación I: polinomio global
cat("\n--- Polinomio lineal global ---\n")
print(summary(lm(y ~ D + Zt + D:Zt)))

cat("\n--- Polinomio cuadrático global ---\n")
print(summary(lm(y ~ D + Zt + D:Zt + I(Zt^2) + D:I(Zt^2))))

# Estimación II: local lineal con kernel triangular
cat("\n--- Local lineal con rdrobust (kernel triangular, MSE-óptimo) ---\n")
print(summary(rdrobust(y, Z, c = 62, p = 1, kernel = "triangular")))

# =============================================================
# PARTE B — RDN con DGP curvo
# =============================================================
cat_section("PARTE B — RDN con DGP curvo")

set.seed(SEED)
Z  <- runif(N, 50, 74)
D  <- as.integer(Z >= 62)
Zt <- Z - 62
y  <- 1 + 0.5 * Zt - 0.02 * Zt^2 + 2.0 * D + rnorm(N)

png("rdplot_B.png", width = 800, height = 600)
rdplot(y, Z, c = 62, title = "RDN — DGP curvo")
dev.off()

cat("\n--- [DGP curvo] Polinomio lineal: sesgo esperado ---\n")
print(summary(lm(y ~ D + Zt + D:Zt)))

cat("\n--- [DGP curvo] Polinomio cuadrático: corrige si conocemos el DGP ---\n")
print(summary(lm(y ~ D + Zt + D:Zt + I(Zt^2) + D:I(Zt^2))))

cat("\n--- [DGP curvo] Local lineal: no requiere conocer la curvatura ---\n")
print(summary(rdrobust(y, Z, c = 62, p = 1, kernel = "triangular")))

# =============================================================
# PARTE C — Sensibilidad al bandwidth
# =============================================================
cat_section("PARTE C — Sensibilidad al bandwidth")

set.seed(SEED)
Z  <- runif(N, 50, 74)
D  <- as.integer(Z >= 62)
Zt <- Z - 62
y  <- 1 + 0.10 * Zt + 2.0 * D + rnorm(N)

cat(sprintf("\n%4s  %8s  %8s\n", "h", "coef", "se"))
for (h in c(1, 2, 3, 5, 8, 12)) {
  r <- rdrobust(y, Z, c = 62, h = h, p = 1, kernel = "triangular")
  cat(sprintf("%4d  %8.3f  %8.3f\n", h, r$coef[1], r$se[1]))
}

cat("\n--- Bandwidth MSE-óptimo (CCT, default) ---\n")
print(summary(rdrobust(y, Z, c = 62, bwselect = "mserd")))

cat("\n--- Bandwidth Coverage-error-rate óptimo ---\n")
print(summary(rdrobust(y, Z, c = 62, bwselect = "cerrd")))

# =============================================================
# PARTE D — Tests de validez
# =============================================================
cat_section("PARTE D — Tests de validez")

# D.1 Manipulación
cat("\n--- rddensity (sin manipulación, H0 esperado: no rechazo) ---\n")
print(summary(rddensity(Z, c = 62)))

Z_manip <- Z
mask <- (Z_manip >= 61.5) & (Z_manip < 62) & (runif(N) < 0.5)
Z_manip[mask] <- 62.5
cat("\n--- rddensity (con manipulación artificial, debería rechazar) ---\n")
print(summary(rddensity(Z_manip, c = 62)))

# D.2 Continuidad de covariables predeterminadas
set.seed(SEED)
Z     <- runif(N, 50, 74)
D     <- as.integer(Z >= 62)
sexo  <- as.integer(runif(N) < 0.5)
ingre <- 100 + 5 * D + rnorm(N) * 10
y     <- 1 + 0.10 * (Z - 62) + 2.0 * D + rnorm(N)

cat("\n--- Covariable predeterminada (sexo): no debe saltar ---\n")
print(summary(rdrobust(sexo, Z, c = 62)))

cat("\n--- Covariable contaminada (ingre): sí salta, NO usar como control ---\n")
print(summary(rdrobust(ingre, Z, c = 62)))

# D.3 Placebos en cutoffs falsos
cat("\n--- Placebos en cutoffs falsos: ninguno debe ser significativo ---\n")
for (c_fake in c(58, 60, 64, 66)) {
  r <- rdrobust(y, Z, c = c_fake)
  cat(sprintf("Cutoff falso %3d   coef = %6.3f   robust p = %5.3f\n",
              c_fake, r$coef[1], r$pv[3, 1]))
}

# =============================================================
# PARTE E — RDB
# =============================================================
cat_section("PARTE E — RDB")

set.seed(SEED)
N  <- 5000
Z  <- runif(N, 50, 74)
W  <- as.integer(Z >= 62)
Zt <- Z - 62
p  <- 0.20 + 0.60 * W
D  <- as.integer(runif(N) < p)
y  <- 1 + 0.10 * Zt + 2.0 * D + rnorm(N)

cat("\n--- Forma reducida: salto en y ---\n")
print(summary(rdrobust(y, Z, c = 62)))

cat("\n--- Primera etapa: salto en D ---\n")
print(summary(rdrobust(D, Z, c = 62)))

cat("\n--- RDB (rdrobust con fuzzy = D) ---\n")
print(summary(rdrobust(y, Z, c = 62, fuzzy = D)))

# Equivalencia con 2SLS local
h <- 5
keep <- abs(Zt) < h
df_iv <- data.frame(y = y, D = D, W = W, Zt = Zt)[keep, ]

cat(sprintf("\n--- 2SLS local con bandwidth fijo h = %d (kernel uniforme) ---\n", h))
iv_fit <- ivreg(y ~ D + Zt + W:Zt | W + Zt + W:Zt, data = df_iv)
print(summary(iv_fit, vcov = sandwich::sandwich, diagnostics = FALSE))

# =============================================================
# PARTE F — Variable de asignación discreta (Kolesár & Rothe 2018)
# =============================================================
cat_section("PARTE F — Variable de asignación discreta")

set.seed(SEED)
N  <- 3000
Z  <- sample(0:100, N, replace = TRUE)            # puntaje entero
D  <- as.integer(Z >= 50)
Zt <- Z - 50
y  <- 1 + 0.05 * Zt + 2.0 * D + rnorm(N)

cat("\n--- SE convencionales (subcubren con Z discreta) ---\n")
print(summary(rdrobust(y, Z, c = 50)))

cat("\n--- SE clusterizados en Z (Lee & Card 2008) — más conservadores ---\n")
print(summary(rdrobust(y, Z, c = 50, cluster = Z)))

# Honest CIs (Kolesár & Rothe 2018) — si RDHonest está instalado
if (requireNamespace("RDHonest", quietly = TRUE)) {
  cat("\n--- Honest CIs (RDHonest, Kolesár & Rothe 2018) ---\n")
  print(RDHonest::RDHonest(y ~ Z, cutoff = 50))
} else {
  cat("\n[aviso] RDHonest no instalado — instale con install.packages('RDHonest')\n")
}

cat("\nRDD_simulacion.R — terminado.\n")
