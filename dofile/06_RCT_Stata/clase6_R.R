# ============================================================
# Cap 6: Experimentos Aleatorizados — Script de R
# Data: data.dta (experimento de aula, 4 semestres, N=70)
#   Grupo B = definiciones (tratamiento)
#   Grupo A = texto crítico (control)
#   semestre = estrato (1-4)
#
# Estructura:
#   PASO 3) Balance de covariables
#   PASO 4) Cuatro escenarios de estimación del ATE
#   PASO 5) Efectos heterogéneos
#   PASO 6) Truco de centrar (Wooldridge)
# ============================================================

# ---- 0) Paquetes ----
req <- c("haven", "tidyverse", "estimatr", "modelsummary",
         "broom", "openxlsx", "randomizr")
inst <- req[!req %in% installed.packages()[,"Package"]]
if (length(inst)) install.packages(inst, dependencies = TRUE)

library(haven)
library(tidyverse)
library(estimatr)
library(modelsummary)
library(broom)
library(openxlsx)
library(randomizr)

set.seed(20250813)

# ---- 1) Cargar datos y preparar variables ----
df <- read_dta("data.dta") %>%
  mutate(
    y        = as.numeric(resultado),
    D        = as.integer(grupo == "B"),
    mujer    = as.integer(genero == "Mujer"),
    pregrado = as.integer(programa == "Pregrado"),
    maestria = as.integer(programa == "Maestría"),
    semestre_f = factor(semestre)
  )

X <- c("edad", "mujer", "libros", "pregrado", "maestria")

# Descriptivas rápidas
cat("N =", nrow(df), "\n")
table(df$semestre, df$D) %>% addmargins() %>% print()
df %>% group_by(D) %>% summarise(mean_y = mean(y), .groups = "drop") %>% print()

# ---- 2) (OPCIONAL) Aleatorización en R ----
# A) Bernoulli p=0.5
df$D_bern_50 <- rbinom(nrow(df), 1, 0.5)

# B) Bernoulli p=0.30
df$D_bern_30 <- rbinom(nrow(df), 1, 0.30)

# C) Conteo exacto 40%
N <- nrow(df); Ntr <- round(0.40 * N)
df$D_exact_40 <- 0L
df$D_exact_40[sample(seq_len(N), Ntr, replace = FALSE)] <- 1L

# D) Estratificada 50/50 por semestre (como en nuestro diseño)
df$D_strat_sem <- as.integer(block_ra(blocks = df$semestre_f, prob = 0.5))

# E) Estratificada por mujer x programa
df$block <- interaction(df$mujer, df$programa, drop = TRUE)
df$D_block_50 <- as.integer(block_ra(blocks = df$block, prob = 0.5))

# F) Multi-brazo (T1/T2/C = 0.4/0.4/0.2) con estratos
df$arm <- block_ra(blocks = df$block,
                   conditions = c("T1","T2","C"),
                   prob_each = c(0.4, 0.4, 0.2))

# ---- PASO 3) Balance tipo "difmedias" ----
difmedias <- function(df, vars, byvar = "D") {
  map_dfr(vars, function(v){
    g0 <- df %>% filter(.data[[byvar]] == 0) %>% pull(.data[[v]])
    g1 <- df %>% filter(.data[[byvar]] == 1) %>% pull(.data[[v]])
    tt <- t.test(g1, g0, var.equal = FALSE)
    mean_T <- mean(g1, na.rm = TRUE)
    mean_C <- mean(g0, na.rm = TRUE)
    diff   <- mean_T - mean_C
    tstat  <- unname(tt$statistic)
    pval   <- unname(tt$p.value)
    se     <- if (is.finite(tstat) && abs(tstat) > .Machine$double.eps) diff / tstat else NA_real_
    tibble(
      variable = v,
      mean_T = mean_T, mean_C = mean_C, diff = diff,
      tstat = tstat, pval = pval, se = se,
      sd_T = sd(g1, na.rm = TRUE), sd_C = sd(g0, na.rm = TRUE),
      N_T = sum(!is.na(g1)),       N_C = sum(!is.na(g0))
    )
  })
}

# Balance univariado
bal_X <- difmedias(df, X, byvar = "D")
print(bal_X)
write.xlsx(bal_X, "Table_Balance_raw.xlsx", overwrite = TRUE)

# Prueba conjunta: D ~ X (LPM con F-test)
m_LPM   <- lm_robust(D ~ ., data = df[, c("D", X)], se_type = "HC1")
m_logit <- glm(D ~ ., data = df[, c("D", X)], family = binomial())
m_probit<- glm(D ~ ., data = df[, c("D", X)], family = binomial(link="probit"))

msummary(
  list("LPM (HC1)" = m_LPM, "Logit" = m_logit, "Probit" = m_probit),
  output = "Table_Balance_models.html"
)

# F-test conjunto
cat("\nF-test conjunto (LPM): ")
f_lpm <- summary(lm(D ~ edad + mujer + libros + pregrado + maestria, data = df))$fstatistic
cat("F =", round(f_lpm[1], 3), ", p =", round(pf(f_lpm[1], f_lpm[2], f_lpm[3], lower.tail = FALSE), 3), "\n")

# ---- PASO 4) Cuatro escenarios de estimación del ATE ----
m1 <- lm_robust(y ~ D, data = df, se_type = "HC1")
m2 <- lm_robust(y ~ D + edad + mujer + libros + pregrado + maestria,
                data = df, se_type = "HC1")
m3 <- lm_robust(y ~ D + semestre_f, data = df, se_type = "HC1")
m4 <- lm_robust(y ~ D + semestre_f + edad + mujer + libros + pregrado + maestria,
                data = df, se_type = "HC1")

msummary(
  list("Simple" = m1, "+Controles" = m2, "+Estratos" = m3, "Completo" = m4),
  stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  coef_omit = "semestre|Intercept|pregrado",
  gof_map = c("nobs", "r.squared"),
  title = "Cuatro escenarios de estimación del ATE",
  output = "Tabla_4escenarios.html"
)

# Comparar tau y SE
cat("\n=== Comparación de los 4 escenarios ===\n")
for (nm in c("m1","m2","m3","m4")) {
  mod <- get(nm)
  cat(sprintf("%-12s tau = %.3f  SE = %.3f\n", nm, coef(mod)["D"], mod$std.error["D"]))
}

# ---- PASO 5) Efectos heterogéneos ----
library(ggplot2)

# 5.1 Por mujer (binaria)
mh_mujer <- lm_robust(y ~ D * mujer, data = df, se_type = "HC1")
cat("\n=== HET por mujer ===\n")
print(tidy(mh_mujer))

grid_mujer <- expand_grid(D = 0:1, mujer = 0:1)
grid_mujer$yhat <- predict(lm(y ~ D * mujer, data = df), grid_mujer)
grid_mujer$Grupo <- factor(grid_mujer$D, labels = c("Control", "Tratado"))
grid_mujer$Genero <- factor(grid_mujer$mujer, labels = c("Hombre", "Mujer"))

p_mujer <- ggplot(grid_mujer, aes(x = Genero, y = yhat, fill = Grupo)) +
  geom_col(position = "dodge", alpha = 0.8) +
  labs(title = "Efecto por género", y = "E[Y]", x = "") +
  theme_minimal()
ggsave("het_mujer.png", p_mujer, width = 6, height = 4, dpi = 300)

# 5.2 Por libros (continua)
mh_lib <- lm_robust(y ~ D * libros, data = df, se_type = "HC1")
grid_lib <- expand_grid(D = 0:1, libros = 0:8)
pred_lib <- predict(mh_lib, newdata = grid_lib, se.fit = TRUE)
grid_lib$yhat <- pred_lib$fit
grid_lib$se   <- pred_lib$se.fit
grid_lib$Grupo <- factor(grid_lib$D, labels = c("Control", "Tratado"))

p_lib <- ggplot(grid_lib, aes(x = libros, y = yhat, color = Grupo, fill = Grupo)) +
  geom_ribbon(aes(ymin = yhat - 1.96*se, ymax = yhat + 1.96*se), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1) +
  labs(title = "Efecto marginal según libros leídos", y = "E[Y]", x = "Libros") +
  theme_minimal()
ggsave("het_libros.png", p_lib, width = 7, height = 4.5, dpi = 300)

# 5.3 Por cuartiles de edad
df <- df %>% mutate(q_edad = ntile(edad, 4))
mh_q <- lm_robust(y ~ D * factor(q_edad), data = df, se_type = "HC1")
grid_q <- expand_grid(D = 0:1, q_edad = 1:4)
grid_q$yhat <- predict(lm(y ~ D * factor(q_edad), data = df), grid_q)
grid_q$Grupo <- factor(grid_q$D, labels = c("Control", "Tratado"))
grid_q$Cuartil <- paste0("Q", grid_q$q_edad)

p_q <- ggplot(grid_q, aes(x = Cuartil, y = yhat, fill = Grupo)) +
  geom_col(position = "dodge", alpha = 0.8) +
  labs(title = "Heterogeneidad por cuartiles de edad", y = "E[Y]", x = "Cuartil de edad") +
  theme_minimal()
ggsave("het_qedad.png", p_q, width = 6, height = 4, dpi = 300)

# ---- PASO 6) Truco de centrar (Wooldridge) ----
# Sin centrar: coef(D) = efecto para libros=0
m_nc <- lm_robust(y ~ D * libros, data = df, se_type = "HC1")

# Con centrado: coef(D) = ATE promedio
df$libros_c <- df$libros - mean(df$libros)
m_c <- lm_robust(y ~ D * libros_c, data = df, se_type = "HC1")

msummary(
  list("Sin centrar" = m_nc, "Con centrado" = m_c),
  stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  coef_rename = c("D" = "D (tratamiento)", "libros" = "libros",
                  "libros_c" = "libros (centrado)",
                  "D:libros" = "D x libros", "D:libros_c" = "D x libros_c"),
  gof_map = c("nobs", "r.squared"),
  title = "Truco de centrar: comparación",
  output = "Tabla_centrado.html"
)

cat("\n=== Truco de centrar ===\n")
cat(sprintf("Sin centrar: coef(D) = %.3f (efecto para libros=0)\n", coef(m_nc)["D"]))
cat(sprintf("Con centrado: coef(D) = %.3f (ATE promedio)\n", coef(m_c)["D"]))

# ---- Mensaje final ----
cat("\nListo.\n",
    "- Balance:      Table_Balance_raw.xlsx, Table_Balance_models.html\n",
    "- 4 escenarios: Tabla_4escenarios.html\n",
    "- Heterogeneid: het_mujer.png, het_libros.png, het_qedad.png\n",
    "- Centrado:     Tabla_centrado.html\n")
