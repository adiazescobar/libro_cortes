# ==============================================================================
# Datos de Panel, DiD, TWFE y Estimadores Modernos — Python
# Econometría Avanzada — Ana María Díaz Escobar — Javeriana 2026-I
#
# Paquetes necesarios:
#   pip install pyfixest pandas numpy matplotlib
#
# SECCIONES:
#   1. DGP: adopción escalonada (3 unidades, 1980-1989)
#   2. TWFE con pyfixest
#   3. Descomposición de Goodman-Bacon (manual)
#   4. Callaway & Sant'Anna vía pyfixest
#   5. Event study
# ==============================================================================

import pandas as pd
import numpy as np
import pyfixest as pf
import matplotlib.pyplot as plt

# ==============================================================================
# 1. DGP: 3 unidades, años 1980-1989
#    id=1: nunca tratado
#    id=2: trata desde 1985, τ = 2
#    id=3: trata desde 1988, τ = 4
#    Y = 0 si no tratado; Y = τ si tratado  (mismo DGP del libro)
# ==============================================================================

panel = pd.DataFrame(
    [(i, t) for i in [1, 2, 3] for t in range(1980, 1990)],
    columns=["id", "t"]
).sort_values(["id", "t"]).reset_index(drop=True)

panel["D"] = 0
panel.loc[(panel["id"] == 2) & (panel["t"] >= 1985), "D"] = 1
panel.loc[(panel["id"] == 3) & (panel["t"] >= 1988), "D"] = 1

panel["Y"] = 0
panel.loc[(panel["id"] == 2) & (panel["D"] == 1), "Y"] = 2
panel.loc[(panel["id"] == 3) & (panel["D"] == 1), "Y"] = 4

# Primera adopción (0 = nunca tratado)
panel["gvar"] = 0
panel.loc[panel["id"] == 2, "gvar"] = 1985
panel.loc[panel["id"] == 3, "gvar"] = 1988

att_true = (5 * 2 + 2 * 4) / 7
print(f"ATT_overall verdadero = (5×2 + 2×4)/7 = {att_true:.4f}")

# ==============================================================================
# 2. TWFE con pyfixest
# ==============================================================================

fit_twfe = pf.feols("Y ~ D | id + t", data=panel, vcov={"CRV1": "id"})
fit_twfe.summary()
beta_twfe = fit_twfe.coef()["D"]
print(f"\nβ̂_TWFE = {beta_twfe:.4f}")
print(f"Sesgo = β̂_TWFE - ATT = {beta_twfe - att_true:.4f}")

# ==============================================================================
# 3. Descomposición de Goodman-Bacon (manual para el ejemplo de 3 unidades)
#    Los β̂_k para cada comparación 2×2 — ver slides para la fórmula
# ==============================================================================

def did_2x2(df, treated_id, control_id, t_post_start):
    """DiD estándar: (Ȳ_trat,post - Ȳ_trat,pre) - (Ȳ_ctrl,post - Ȳ_ctrl,pre)"""
    sub = df[df["id"].isin([treated_id, control_id]) & (df["t"] < t_post_start + 5)].copy()
    sub["post"] = (sub["t"] >= t_post_start).astype(int)
    y_tt_post = sub.loc[(sub["id"] == treated_id)  & (sub["post"] == 1), "Y"].mean()
    y_tt_pre  = sub.loc[(sub["id"] == treated_id)  & (sub["post"] == 0), "Y"].mean()
    y_ct_post = sub.loc[(sub["id"] == control_id)  & (sub["post"] == 1), "Y"].mean()
    y_ct_pre  = sub.loc[(sub["id"] == control_id)  & (sub["post"] == 0), "Y"].mean()
    return (y_tt_post - y_tt_pre) - (y_ct_post - y_ct_pre)

print("\n── Descomposición de Bacon (manual) ──")
print(f"Never vs Early  (id=2 trat, id=1 ctrl): β = {did_2x2(panel, 2, 1, 1985):.2f}")
print(f"Never vs Late   (id=3 trat, id=1 ctrl): β = {did_2x2(panel, 3, 1, 1988):.2f}")
print(f"Early vs Late ✓ (id=2 trat, id=3 ctrl, ventana 1980-87): "
      f"β = {did_2x2(panel[panel['t'] <= 1987], 2, 3, 1985):.2f}")
print(f"Late vs Early ✗ (id=3 trat, id=2 ctrl, ventana 1985-89): "
      f"β = {did_2x2(panel[panel['t'] >= 1985], 3, 2, 1988):.2f}")
print("(use bacondecomp en Stata para los pesos exactos)")

# ==============================================================================
# 4. Callaway & Sant'Anna vía pyfixest
#    Ref: Callaway & Sant'Anna (2021), Journal of Econometrics
# ==============================================================================

cs = pf.did.att_gt(
    data       = panel,
    yname      = "Y",
    tname      = "t",
    idname     = "id",
    gname      = "gvar",
    control_group = "nevertreated"
)
print("\n── Callaway & Sant'Anna: ATT(g,t) ──")
print(cs.summary())

att_simple = cs.aggregate("simple")
print(f"\nATT overall (CS) = {att_simple.att:.4f}")

# ==============================================================================
# 5. Event study
# ==============================================================================

att_dynamic = cs.aggregate("event")
fig, ax = plt.subplots(figsize=(8, 4))
ax.axhline(0, color="black", linewidth=0.8)
ax.axvline(-0.5, color="gray", linestyle="--", linewidth=0.8)
ax.errorbar(
    att_dynamic.e,
    att_dynamic.att,
    yerr=1.96 * att_dynamic.se,
    fmt="o-", color="#1B3A6B", capsize=4
)
ax.set_xlabel("Tiempo relativo al tratamiento")
ax.set_ylabel("ATT estimado")
ax.set_title("Event Study — Callaway & Sant'Anna")
plt.tight_layout()
plt.savefig("event_study_cs.png", dpi=150)
plt.show()
