# ============================================================
# CLASE: Experimentos controlados — Script de R
# Data real: data.dta  (id, resultado, grupo, edad, genero, programa, libros)
# Objetivos:
#   1) Cargar y preparar datos (y, D, X)
#   2) Aleatorización (varias opciones)  [OPCIONAL]
#   3) Balance tipo "difmedias" (tabla T/C/diff/t/p/se)
#   4) ¿Asignación correcta?  D ~ X  (LPM / Logit)
#   5) Efecto del tratamiento (y~D) con y sin controles
#   6) Efectos heterogéneos (mujer, libros, q_edad)
#   7) Exportar tablas y gráficos
# ============================================================

# ---- 0) Paquetes ----
req <- c(
  "haven", "tidyverse", "estimatr", "modelsummary",
  "broom", "openxlsx", "randomizr" # randomización estratificada/multibrazo
)
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
# Asegúrate de que el archivo "data.dta" esté en el directorio de trabajo
# setwd("...")  # <- ajusta si es necesario
df <- read_dta("data.dta") %>%
  mutate(
    y        = as.numeric(resultado),
    D        = as.integer(grupo == "B"),           # 1=Tratado(B), 0=Control(A)
    mujer    = as.integer(genero == "Mujer"),
    pregrado = as.integer(programa == "Pregrado"),
    maestria = as.integer(programa == "Maestría")
  )

X <- c("edad", "mujer", "libros", "pregrado", "maestria")

# ---- 2) (OPCIONAL) Aleatorización en R ----
# A) Bernoulli p=0.5
df$D_bern_50 <- rbinom(nrow(df), 1, 0.5)

# B) Bernoulli con p=0.30
df$D_bern_30 <- rbinom(nrow(df), 1, 0.30)

# C) Conteo EXACTO de tratados: p.ej., 40% exacto
N <- nrow(df); Ntr <- round(0.40 * N)
df$D_exact_40 <- 0L
df$D_exact_40[sample(seq_len(N), Ntr, replace = FALSE)] <- 1L

# D) Estratificada 50/50 con randomizr (por mujer x programa)
df$block <- interaction(df$mujer, df$programa, drop = TRUE)
df$D_block_50 <- as.integer(block_ra(blocks = df$block, prob = 0.5))

# E) Multi-brazo con proporciones (T1/T2/C = 0.4/0.4/0.2) y estratos
#    Devuelve factores "T1","T2","C"; hacemos dummies si se necesita.
df$arm <- block_ra(blocks = df$block,
                   conditions = c("T1","T2","C"),
                   prob_each = c(0.4, 0.4, 0.2))

# ---- 3) Balance tipo "difmedias" ----
difmedias <- function(df, vars, byvar = "D") {
  stopifnot(byvar %in% names(df))
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

# Balance en X
bal_X <- difmedias(df, X, byvar = "D")
write.xlsx(bal_X, "Table_Balance_raw.xlsx", overwrite = TRUE)

# Balance para outcome
bal_y <- difmedias(df, "y", byvar = "D")
write.xlsx(bal_y, "Diff_y_raw.xlsx", overwrite = TRUE)

# ---- 4) ¿Asignación correcta?  D ~ X ----
# LPM con SE robustas (HC1)
m_LPM   <- lm_robust(D ~ ., data = df[, c("D", X)], se_type = "HC1")
# Logit y Probit (SE por defecto); puedes usar vcovCL si hay clustering
m_logit <- glm(D ~ ., data = df[, c("D", X)], family = binomial())
m_probit<- glm(D ~ ., data = df[, c("D", X)], family = binomial(link="probit"))

msummary(
  list("LPM D~X (HC1)" = m_LPM, "Logit D~X" = m_logit, "Probit D~X" = m_probit),
  output = "table_balance_models.html"
)

# ---- 5) Efecto del tratamiento (y ~ D) con y sin controles ----
m1 <- lm_robust(y ~ D, data = df, se_type = "HC1")
m2 <- lm_robust(y ~ D + edad + mujer + libros + pregrado + maestria,
                data = df, se_type = "HC1")

msummary(
  list("y ~ D" = m1, "y ~ D + X" = m2),
  estimate = "{estimate} ({std.error})",
  gof_map = tribble(
    ~raw, ~clean, ~fmt,
    "nobs", "N", 0,
    "r.squared", "R2", 3
  ),
  fmt = 4,
  output = "table_treatment.html"
)

# ---- 6) Efectos heterogéneos ----
# 6.1 HET por mujer (binaria)
mh_mujer <- lm_robust(y ~ D*mujer + edad + libros + pregrado + maestria,
                      data = df, se_type = "HC1")

# Efecto marginal de D por mujer = 0/1 (predicciones)
new_muj <- expand_grid(
  D = c(0,1),
  mujer = c(0,1),
  edad = mean(df$edad, na.rm = TRUE),
  libros = mean(df$libros, na.rm = TRUE),
  pregrado = 0, maestria = 0
) %>%
  mutate(yhat = predict(lm(y ~ D*mujer + edad + libros + pregrado + maestria, data = df), .))

write.xlsx(new_muj, "margins_mujer.xlsx", overwrite = TRUE)

# 6.2 HET por libros (continua): grid 0..max(libros)
Lmax <- max(df$libros, na.rm = TRUE)
grid_lib <- tibble(libros = 0:Lmax) %>%
  crossing(D = c(0,1),
           edad = mean(df$edad, na.rm = TRUE),
           mujer = 0, pregrado = 0, maestria = 0)

mod_lib <- lm(y ~ D*libros + edad + mujer + pregrado + maestria, data = df)
pred_lib <- grid_lib %>%
  mutate(yhat = predict(mod_lib, newdata = grid_lib))

# Guardar perfiles tratados vs control
write.xlsx(pred_lib, "margins_libros.xlsx", overwrite = TRUE)

# Gráfico: efecto de D según libros (diferencia de líneas)
library(ggplot2)
p_lib <- pred_lib %>%
  select(D, libros, yhat) %>%
  mutate(grupo = ifelse(D == 1, "Tratado", "Control")) %>%
  ggplot(aes(x = libros, y = yhat, linetype = grupo)) +
  geom_line(linewidth = 0.9) +
  labs(x = "Libros", y = "Predicción de y",
       title = "Predicción y vs. libros por grupo (controles al promedio)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")

ggsave("margins_libros.png", p_lib, width = 7, height = 4.5, dpi = 300)

# 6.3 HET por cuartiles de edad
df <- df %>%
  mutate(q_edad = ntile(edad, 4))

mh_qedad <- lm_robust(y ~ D*factor(q_edad) + mujer + libros + pregrado + maestria,
                      data = df, se_type = "HC1")

# Margins “manual” por cuartil: Δ(D=1 − D=0) dentro de cada q_edad (con controles al promedio)
avg_vals <- df %>%
  summarize(
    edad = mean(edad, na.rm = TRUE),
    mujer = mean(mujer, na.rm = TRUE),
    libros = mean(libros, na.rm = TRUE),
    pregrado = mean(pregrado, na.rm = TRUE),
    maestria = mean(maestria, na.rm = TRUE)
  )

grid_q <- expand_grid(
  D = c(0, 1),
  q_edad = sort(unique(df$q_edad))
) %>%
  mutate(
    edad = avg_vals$edad,
    mujer = avg_vals$mujer,
    libros = avg_vals$libros,
    pregrado = avg_vals$pregrado,
    maestria = avg_vals$maestria
  )

mod_q <- lm(y ~ D*factor(q_edad) + mujer + libros + pregrado + maestria, data = df)
pred_q <- grid_q %>%
  mutate(yhat = predict(mod_q, newdata = grid_q)) %>%
  group_by(q_edad) %>%
  summarize(ATE_hat = yhat[D==1] - yhat[D==0], .groups = "drop")

write.xlsx(pred_q, "margins_qedad.xlsx", overwrite = TRUE)

# ---- 7) Export “difmedias” para y y X juntos (como en Stata) ----
bal_yX <- difmedias(df, c("y", X), byvar = "D")
write.xlsx(bal_yX, "means_yX.xlsx", overwrite = TRUE)

# ---- 8) Mensaje final ----
cat("\nListo ✅\n",
    "- Balance (X): Table_Balance_raw.xlsx\n",
    "- Diff(y):     Diff_y_raw.xlsx\n",
    "- Modelos D~X: table_balance_models.html\n",
    "- Tratamiento: table_treatment.html\n",
    "- Margins mujer/libros/q_edad: margins_mujer.xlsx, margins_libros.xlsx, margins_qedad.xlsx\n",
    "- Gráfico:     margins_libros.png\n",
    "- Medias y+X:  means_yX.xlsx\n")
