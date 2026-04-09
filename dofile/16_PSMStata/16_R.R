# =============================================================================
# PSM en R — Econometría Avanzada
# Ana María Díaz Escobar — Pontificia Universidad Javeriana
#
# Equivalente en R del do-file 16_stata.do
# Base: base6.dta | Tratamiento: D | Resultado: y2
#
# Paquetes necesarios:
#   install.packages(c("MatchIt", "cobalt", "WeightIt", "marginaleffects",
#                      "haven", "tidyverse", "lmtest", "sandwich"))
# =============================================================================

library(haven)          # leer .dta de Stata
library(MatchIt)        # emparejamiento PSM
library(cobalt)         # balance de covariables (bal.tab, love.plot)
library(WeightIt)       # IPW
library(marginaleffects)
library(tidyverse)
library(lmtest)
library(sandwich)

# =============================================================================
# 1. CARGAR DATOS
# =============================================================================

# Ajusta la ruta a tu carpeta de trabajo
df <- read_dta("base6.dta")

# Variables
covs <- c("personas", "orden_n", "ocupado_jefe", "educa_jefe",
          "ingresos_hogar_jefe", "hombre")
fmla <- as.formula(paste("D ~", paste(covs, collapse = " + ")))

# =============================================================================
# 2. ESTADÍSTICAS DESCRIPTIVAS
# =============================================================================

df %>%
  group_by(D) %>%
  summarise(across(all_of(c("y1", "y2", covs)), mean, na.rm = TRUE))

# =============================================================================
# 3. MODELO DE SELECCIÓN: LOGIT (equivalente a dprobit en Stata)
# =============================================================================

ps_model <- glm(fmla, data = df, family = binomial("logit"))
summary(ps_model)

# Propensity score
df$ps1 <- predict(ps_model, type = "response")

# Soporte común: distribución del PS por grupo
ggplot(df, aes(x = ps1, fill = factor(D), color = factor(D))) +
  geom_density(alpha = 0.4, linewidth = 1) +
  scale_fill_manual(values = c("0" = "tomato", "1" = "steelblue"),
                    labels = c("Controles", "Tratados")) +
  scale_color_manual(values = c("0" = "tomato", "1" = "steelblue"),
                     labels = c("Controles", "Tratados")) +
  labs(x = "Propensity Score estimado",
       y = "Densidad",
       title = "Distribución del PS: soporte común",
       fill = "Grupo", color = "Grupo") +
  theme_minimal()

# =============================================================================
# 4. MATCHING: VECINO MÁS CERCANO NN(1) SIN REEMPLAZO
# =============================================================================

m_nn1 <- matchit(fmla,
                 data     = df,
                 method   = "nearest",
                 distance = "logit",
                 ratio    = 1,
                 replace  = FALSE)

summary(m_nn1, un = TRUE)   # balance antes y después del matching

# Balance: love.plot equivale al gráfico de pstest
love.plot(m_nn1,
          stats      = "mean.diffs",
          thresholds = c(m = 0.1),
          abs        = TRUE,
          title      = "Balance de covariables: NN(1) sin reemplazo")

# Datos emparejados
df_nn1 <- match.data(m_nn1)

# Estimación del ATT (regresión ponderada en muestra emparejada)
fit_nn1 <- lm(y2 ~ D + personas + orden_n + ocupado_jefe +
                educa_jefe + ingresos_hogar_jefe + hombre,
              data    = df_nn1,
              weights = weights)
coeftest(fit_nn1, vcov = vcovCL(fit_nn1, cluster = ~subclass))

# =============================================================================
# 5A. NN(1) CON REEMPLAZO (con soporte común)
# =============================================================================

m_nn1r <- matchit(fmla,
                  data     = df,
                  method   = "nearest",
                  distance = "logit",
                  ratio    = 1,
                  replace  = TRUE)

summary(m_nn1r, un = FALSE)
love.plot(m_nn1r, stats = "mean.diffs", thresholds = c(m = 0.1), abs = TRUE,
          title = "Balance: NN(1) con reemplazo")

df_nn1r <- match.data(m_nn1r)
fit_nn1r <- lm(y2 ~ D + personas + orden_n + ocupado_jefe +
                 educa_jefe + ingresos_hogar_jefe + hombre,
               data = df_nn1r, weights = weights)
coeftest(fit_nn1r, vcov = vcovCL(fit_nn1r, cluster = ~subclass))

# =============================================================================
# 5B. NN(5) CON REEMPLAZO
# =============================================================================

m_nn5 <- matchit(fmla,
                 data     = df,
                 method   = "nearest",
                 distance = "logit",
                 ratio    = 5,
                 replace  = TRUE)

summary(m_nn5, un = FALSE)

# =============================================================================
# 5C. CALIPER (distancia máxima 0.001 en PS)
# =============================================================================

m_caliper <- matchit(fmla,
                     data     = df,
                     method   = "nearest",
                     distance = "logit",
                     caliper  = 0.001,    # en unidades de PS
                     replace  = FALSE)

summary(m_caliper, un = FALSE)
love.plot(m_caliper, stats = "mean.diffs", thresholds = c(m = 0.1), abs = TRUE,
          title = "Balance: Caliper (0.001)")

# =============================================================================
# 5D. RADIUS (todos los controles dentro del caliper)
# =============================================================================

m_radius <- matchit(fmla,
                    data     = df,
                    method   = "nearest",
                    distance = "logit",
                    caliper  = 0.001,
                    ratio    = Inf,      # todos los controles dentro del caliper
                    replace  = FALSE)

summary(m_radius, un = FALSE)

# =============================================================================
# 5E. KERNEL (equivalente en MatchIt: optimal o subclassification)
# =============================================================================

# MatchIt implementa kernel mediante "full matching" o "subclassification"
# La opción más cercana al kernel de psmatch2 es:

m_full <- matchit(fmla,
                  data     = df,
                  method   = "full",     # full matching = todos los controles ponderados
                  distance = "logit")

summary(m_full, un = FALSE)
love.plot(m_full, stats = "mean.diffs", thresholds = c(m = 0.1), abs = TRUE,
          title = "Balance: Full Matching (equivalente a kernel)")

df_full <- match.data(m_full)
fit_full <- lm(y2 ~ D + personas + orden_n + ocupado_jefe +
                 educa_jefe + ingresos_hogar_jefe + hombre,
               data = df_full, weights = weights)
coeftest(fit_full, vcov = vcovCL(fit_full, cluster = ~subclass))

# =============================================================================
# 6. IPW (Inverse Probability Weighting)
# =============================================================================

# ATT weights con WeightIt
w_att <- weightit(fmla,
                  data   = df,
                  method = "ps",
                  estimand = "ATT")

summary(w_att)
bal.tab(w_att, stats = c("m", "v"), thresholds = c(m = 0.1))

df$w_att <- w_att$weights
fit_ipw <- lm(y2 ~ D, data = df, weights = w_att)
coeftest(fit_ipw, vcov = vcovHC(fit_ipw, "HC3"))

# =============================================================================
# 7. TABLA COMPARATIVA DE ESTIMADORES
# =============================================================================

results <- data.frame(
  Metodo = c("Diferencia cruda", "OLS con covariables",
             "NN(1) sin reemplazo", "NN(1) con reemplazo",
             "NN(5) con reemplazo", "Full matching (kernel)"),
  ATT = c(
    coef(lm(y2 ~ D, data = df))["D"],
    coef(lm(as.formula(paste("y2 ~ D +", paste(covs, collapse = "+"))),
             data = df))["D"],
    coef(fit_nn1)["D"],
    coef(fit_nn1r)["D"],
    NA,   # requiere construir df_nn5
    coef(fit_full)["D"]
  )
)

print(results, digits = 4)
