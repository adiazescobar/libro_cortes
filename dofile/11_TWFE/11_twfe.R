# ==============================================================================
# Datos de Panel, DiD, TWFE y Estimadores Modernos — R
# Econometría Avanzada — Ana María Díaz Escobar — Javeriana 2026-I
#
# Paquetes necesarios:
#   install.packages(c("fixest", "bacondecomp", "did", "ggplot2", "dplyr"))
#
# SECCIONES:
#   1. DGP: adopción escalonada (3 unidades, 1980-1989)
#   2. TWFE con fixest::feols
#   3. Descomposición de Goodman-Bacon
#   4. Callaway & Sant'Anna (2021) con did::att_gt
#   5. Event study
# ==============================================================================

library(fixest)
library(bacondecomp)
library(did)
library(ggplot2)
library(dplyr)

# ==============================================================================
# 1. DGP: 3 unidades, años 1980-1989
#    id=1: nunca tratado
#    id=2: trata desde 1985, τ = 2
#    id=3: trata desde 1988, τ = 4
#    Y = 0 si no tratado; Y = τ si tratado  (mismo DGP del libro)
# ==============================================================================

panel <- expand.grid(id = 1:3, t = 1980:1989) |>
  arrange(id, t) |>
  mutate(
    D    = as.integer((id == 2 & t >= 1985) | (id == 3 & t >= 1988)),
    Y    = case_when(id == 2 & D == 1 ~ 2,
                     id == 3 & D == 1 ~ 4,
                     TRUE             ~ 0),
    gvar = case_when(id == 2 ~ 1985,   # primera adopción (0 = nunca)
                     id == 3 ~ 1988,
                     TRUE    ~ 0)
  )

cat("ATT_overall verdadero = (5×2 + 2×4)/7 =", (5*2 + 2*4)/7, "\n")

# ==============================================================================
# 2. TWFE
# ==============================================================================

twfe <- feols(Y ~ D | id + t, data = panel, cluster = ~id)
summary(twfe)
cat("β̂_TWFE =", round(coef(twfe)["D"], 4), "\n")

# ==============================================================================
# 3. Descomposición de Goodman-Bacon
#    Ref: Goodman-Bacon (2021), Journal of Econometrics
# ==============================================================================

bacon_out <- bacon(Y ~ D, data = panel, id_var = "id", time_var = "t")
print(bacon_out)

# Verificación: suma ponderada = β̂_TWFE
cat("Verificación Bacon:",
    round(sum(bacon_out$estimate * bacon_out$weight), 4),
    "≈ β̂_TWFE\n")

# ==============================================================================
# 4. Callaway & Sant'Anna (2021)
#    Ref: Callaway & Sant'Anna (2021), Journal of Econometrics
# ==============================================================================

cs <- att_gt(
  yname          = "Y",
  tname          = "t",
  idname         = "id",
  gname          = "gvar",
  data           = panel,
  control_group  = "nevertreated",
  est_method     = "dr"     # doubly-robust
)

# ATT overall (equivalente a csdid + estat simple en Stata)
att_simple <- aggte(cs, type = "simple")
summary(att_simple)

# ATT por cohorte
att_group  <- aggte(cs, type = "group")
summary(att_group)

# ==============================================================================
# 5. Event study (tiempo relativo al tratamiento)
# ==============================================================================

att_event <- aggte(cs, type = "dynamic", min_e = -4, max_e = 4)
summary(att_event)
ggdid(att_event, title = "Event Study — Callaway & Sant'Anna")
