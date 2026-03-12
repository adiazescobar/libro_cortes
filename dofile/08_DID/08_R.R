# =============================================================================
# Diferencias en Diferencias — Econometría Avanzada
# Ana María Díaz Escobar — Pontificia Universidad Javeriana
#
# PARTE 1: DiD básico 2×2 (equivalente a base3.dta)
# PARTE 2: DiD con múltiples periodos — datos simulados equivalentes a hospdd
#          Prueba de tendencias paralelas y anticipación con fixest
# =============================================================================
# Paquetes necesarios:
#   install.packages(c("fixest", "ggplot2", "dplyr", "broom"))
# =============================================================================

library(fixest)   # TWFE, event study, DiD moderno
library(ggplot2)
library(dplyr)

# =============================================================================
# PARTE 1: DiD BÁSICO (2 periodos)
# Simula la estructura de base3.dta: niños con antes/después del programa
# =============================================================================

set.seed(1234)
n_individuos <- 4000

# Proceso generador de datos
# Efecto real del programa: +0.18 desviaciones estándar en talla-para-edad
efecto_real <- 0.18

datos_did <- data.frame(
  id      = rep(1:n_individuos, each = 2),
  t       = rep(c(0, 1), n_individuos),          # 0 = antes, 1 = después
  D       = rep(rbinom(n_individuos, 1, 0.5), each = 2)  # 1 = tratado
) %>%
  mutate(
    y = -0.5 + 0.1 * D +                          # intercepto + diferencia de niveles
        0.05 * t +                                 # tendencia temporal común
        efecto_real * D * t +                      # efecto causal (ATT)
        rnorm(n() , 0, 0.5)                        # error
  )

cat("=== PARTE 1: DiD BÁSICO ===\n")
cat("Efecto real del programa:", efecto_real, "\n\n")

# ---- Tabla 2×2 de medias ----------------------------------------------------
tabla_2x2 <- datos_did %>%
  group_by(D, t) %>%
  summarise(media_y = mean(y), .groups = "drop") %>%
  mutate(
    grupo   = ifelse(D == 1, "Tratados", "Controles"),
    periodo = ifelse(t == 1, "Después", "Antes")
  ) %>%
  select(grupo, periodo, media_y) %>%
  tidyr::pivot_wider(names_from = periodo, values_from = media_y)

cat("Tabla 2×2:\n")
print(tabla_2x2)

# ---- Estimador DiD manual ---------------------------------------------------
y_c0 <- mean(datos_did$y[datos_did$D == 0 & datos_did$t == 0])
y_c1 <- mean(datos_did$y[datos_did$D == 0 & datos_did$t == 1])
y_t0 <- mean(datos_did$y[datos_did$D == 1 & datos_did$t == 0])
y_t1 <- mean(datos_did$y[datos_did$D == 1 & datos_did$t == 1])

DD <- (y_t1 - y_t0) - (y_c1 - y_c0)
cat("\nEstimador DiD manual:", round(DD, 4), "(esperado ~", efecto_real, ")\n")

# ---- Gráfico de tendencias (visualización del DiD, NO prueba) ---------------
datos_did %>%
  group_by(D, t) %>%
  summarise(media = mean(y), .groups = "drop") %>%
  mutate(
    grupo   = ifelse(D == 1, "Tratados", "Controles"),
    periodo = factor(t, labels = c("Antes", "Después"))
  ) %>%
  ggplot(aes(x = periodo, y = media, color = grupo, group = grupo)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Controles" = "#8B1A1A", "Tratados" = "#1C3A6E")) +
  labs(
    title   = "Evolución de la talla-para-edad por grupo",
    caption = "Nota: con 2 periodos este gráfico visualiza el DiD, no prueba tendencias paralelas.",
    x       = "Periodo", y = "Talla-para-edad (z-score)", color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# ---- Regresión DiD ----------------------------------------------------------
mod_did <- feols(y ~ i(t, D, ref = 0) | id + t,
                 data = datos_did, cluster = ~id)
cat("\nRegresión DiD (feols con efectos fijos de individuo y periodo):\n")
print(coeftable(mod_did))
cat("→ Coeficiente de t::1 × D = estimador DiD\n")

# Equivalente con interacción manual
mod_did2 <- lm(y ~ D + t + D:t, data = datos_did)
cat("\nRegresión DiD (lm, sin FE):\n")
print(summary(mod_did2)$coefficients)
cat("→ Coeficiente de D:t =", round(coef(mod_did2)["D:t"], 4),
    "(esperado ~", efecto_real, ")\n")


# =============================================================================
# PARTE 2: DiD CON MÚLTIPLES PERIODOS
# Datos simulados equivalentes a hospdd (Stata built-in)
#
# Contexto: 50 hospitales observados durante 10 años (2001-2010).
# En 2005-2007, distintos hospitales adoptan un nuevo procedimiento (staggered).
# Resultado: satisfacción de pacientes (escala 0-100).
# Efecto real del programa: +5 puntos.
#
# En R usamos el paquete fixest para:
#   - TWFE (Two-Way Fixed Effects) → equivalente a xtdidregress
#   - iplot()                      → equivalente a estat grangerplot (event study)
#   - Prueba F conjunta de leads   → equivalente a estat granger
# =============================================================================

set.seed(42)

n_hosp  <- 50
n_years <- 10
years   <- 2001:2010
efecto_programa <- 5

# Adopción escalonada: algunos hospitales adoptan en 2005, otros en 2006, otros nunca
cohort <- sample(c(2005, 2006, 2007, Inf), n_hosp, replace = TRUE,
                 prob = c(0.3, 0.3, 0.2, 0.2))  # 20% nunca adopta

hospdd <- expand.grid(hospital = 1:n_hosp, time = years) %>%
  arrange(hospital, time) %>%
  left_join(data.frame(hospital = 1:n_hosp, cohort = cohort), by = "hospital") %>%
  mutate(
    treatment   = as.integer(time >= cohort),
    # Resultado: tendencia común + efecto fijo del hospital + efecto del programa + ruido
    satisfaction = 60 +
                   rnorm(n_hosp, 0, 8)[hospital] +           # heterogeneidad hospitalaria
                   0.5 * (time - 2001) +                     # tendencia temporal común
                   efecto_programa * treatment +             # efecto causal
                   rnorm(n(), 0, 3)                          # error idiosincrático
  )

cat("\n=== PARTE 2: DiD CON MÚLTIPLES PERIODOS ===\n")
cat("Hospitales:", n_hosp, "| Años:", min(years), "-", max(years), "\n")
cat("Efecto real del programa:", efecto_programa, "puntos\n")
cat("Distribución de cohortes de adopción:\n")
print(table(cohort[is.finite(cohort)]))
cat("Nunca adoptaron:", sum(!is.finite(cohort)), "\n\n")

# ---- Gráfico de tendencias por grupo de cohorte ----------------------------
hospdd %>%
  mutate(grupo = case_when(
    cohort == 2005 ~ "Adopta en 2005",
    cohort == 2006 ~ "Adopta en 2006",
    cohort == 2007 ~ "Adopta en 2007",
    TRUE           ~ "Nunca adopta (control)"
  )) %>%
  group_by(grupo, time) %>%
  summarise(media = mean(satisfaction), .groups = "drop") %>%
  ggplot(aes(x = time, y = media, color = grupo, group = grupo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = c(2004.5, 2005.5, 2006.5), linetype = "dashed",
             color = "gray50", linewidth = 0.5) +
  labs(
    title   = "Satisfacción media por cohorte y año",
    caption = "Líneas verticales = inicio de adopción por cohorte",
    x       = "Año", y = "Satisfacción media", color = "Grupo"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# ---- TWFE: equivalente a xtdidregress --------------------------------------
mod_twfe <- feols(satisfaction ~ treatment | hospital + time,
                  data = hospdd, cluster = ~hospital)

cat("TWFE (Two-Way Fixed Effects) — equivalente a xtdidregress:\n")
print(coeftable(mod_twfe))
cat("→ Coeficiente de treatment =", round(coef(mod_twfe)["treatment"], 3),
    "(esperado ~", efecto_programa, ")\n\n")

# ---- EVENT STUDY: equivalente a estat grangerplot --------------------------
# Crea variable de tiempo relativo al año de adopción
hospdd <- hospdd %>%
  mutate(
    t_relativo = ifelse(is.finite(cohort), time - cohort, -99),
    # Limitar el rango para el event study
    t_rel_bin  = pmax(pmin(t_relativo, 4), -4),
    t_rel_bin  = ifelse(cohort == Inf, -99, t_rel_bin)   # nunca-adoptantes = -99
  )

mod_es <- feols(
  satisfaction ~ i(t_rel_bin, ref = c(-1, -99)) | hospital + time,
  data    = hospdd,
  cluster = ~hospital
)

cat("Event study (equivalente a estat grangerplot):\n")
print(coeftable(mod_es))

# Gráfico event study
iplot(mod_es,
      main    = "Event study: efecto del programa por año relativo\n(equivalente a estat grangerplot)",
      xlab    = "Años relativos al inicio del tratamiento",
      ylab    = "Efecto estimado en satisfacción",
      ref.line = TRUE)
abline(h = 0, col = "gray40", lty = 2)

# ---- PRUEBA DE TENDENCIAS PARALELAS: equivalente a estat ptrends -----------
# Prueba conjunta de que todos los coeficientes PRE-tratamiento son = 0
# H0: tendencias paralelas (coefs pre = 0)
cat("\n=== PRUEBA DE TENDENCIAS PARALELAS (equivalente a estat ptrends) ===\n")
cat("H0: Los coeficientes pre-tratamiento son conjuntamente = 0\n")
cat("    (i.e., las tendencias lineales eran paralelas antes del programa)\n\n")

# Identificar los coeficientes pre-tratamiento (t_rel_bin < 0, excluyendo ref y -99)
coef_pre <- grep("t_rel_bin::-[1-9]", names(coef(mod_es)), value = TRUE)
coef_pre <- coef_pre[!grepl("-99", coef_pre)]

cat("Coeficientes pre-tratamiento incluidos en la prueba:\n")
cat(paste(coef_pre, collapse = ", "), "\n\n")

prueba_ptrends <- wald(mod_es, coef_pre)
print(prueba_ptrends)

if (prueba_ptrends$p > 0.05) {
  cat("→ p =", round(prueba_ptrends$p, 4),
      "> 0.05: NO rechazamos H0. Las tendencias pre-tratamiento son paralelas. ✓\n")
} else {
  cat("→ p =", round(prueba_ptrends$p, 4),
      "< 0.05: Rechazamos H0. Hay evidencia de tendencias no paralelas.\n")
}

# ---- PRUEBA DE ANTICIPACIÓN: equivalente a estat granger -------------------
cat("\n=== PRUEBA DE ANTICIPACIÓN (equivalente a estat granger) ===\n")
cat("H0: Sin efectos del programa ANTES de que empezara (sin anticipación)\n")
cat("    Mismos coeficientes pre-tratamiento, misma prueba conjunta.\n")
cat("    La diferencia es conceptual: aquí la pregunta es sobre anticipación,\n")
cat("    no sobre tendencias pre-existentes.\n\n")
cat("Resultado: igual que la prueba de ptrends en este caso,\n")
cat("porque usamos los mismos indicadores de leads.\n")
cat("La distinción requiere argumento teórico, no estadístico.\n")

# ---- DIFERENCIA CLAVE ENTRE LAS DOS PRUEBAS --------------------------------
cat("\n=== DIFERENCIA ENTRE PRUEBAS ===\n")
cat("┌─────────────────┬────────────────────────────────────┬──────────────────────────────────────┐\n")
cat("│ Prueba          │ H0                                 │ Si se rechaza...                     │\n")
cat("├─────────────────┼────────────────────────────────────┼──────────────────────────────────────┤\n")
cat("│ ptrends         │ Tendencias lineales iguales pre-t  │ Los grupos tenían pendientes distintas│\n")
cat("│ granger         │ Sin efectos pre-tratamiento        │ Anticipación o tendencias distintas  │\n")
cat("└─────────────────┴────────────────────────────────────┴──────────────────────────────────────┘\n")
cat("\nNOTA: Con 2 periodos (Parte 1) NINGUNA de estas pruebas es posible.\n")
cat("      Se necesitan ≥ 2 periodos PRE-tratamiento + identificador individual.\n")
