"""
IV y LATE — Simulación pedagógica
Curso: Econometría Avanzada (Javeriana)

Parte A: Muestras finitas — OLS sesgado vs IV consistente
Parte B: LATE paso a paso (Wald = LATE en compliers)

Equivalente al do-file IV_LATE_simulacion.do
"""

import numpy as np
import pandas as pd
import statsmodels.api as sm

rng = np.random.default_rng(20260506)

# =============================================================
# PARTE A — MUESTRAS FINITAS: OLS SESGADO vs IV CONSISTENTE
# =============================================================
#
# DGP (instrumento DÉBIL: π=0.2):
#   z, w, eD, u  ~ iid N(0,1)
#   D = 0.2*z + eD + w        (z apenas mueve a D — instrumento débil)
#   y = 0.5*D + w + u         (tau verdadero = 0.5; w es el confounder)
#
# Sesgo OLS:  cov(D, w)/var(D) = 1/(0.04 + 1 + 1) ≈ 0.49
#   → plim(OLS) ≈ 0.5 + 0.49 ≈ 0.99
# IV: consistente (plim = 0.5) PERO MUY sesgado en muestra finita
#     porque el instrumento es débil (Bound, Jaeger & Baker 1995).
#     En N=30: F de la primera etapa ≈ 1.7 → sesgo del IV hacia OLS.

def simulate_one(n, rng):
    z  = rng.standard_normal(n)
    w  = rng.standard_normal(n)
    eD = rng.standard_normal(n)
    u  = rng.standard_normal(n)
    D  = 0.2*z + eD + w
    y  = 0.5*D + w + u

    # OLS
    X_ols = sm.add_constant(D)
    b_ols = np.linalg.lstsq(X_ols, y, rcond=None)[0][1]

    # IV exactamente identificado: cov(z,y)/cov(z,D)
    b_iv = np.cov(z, y, ddof=1)[0,1] / np.cov(z, D, ddof=1)[0,1]

    # F de la primera etapa (ρ² / (1-ρ²)) * (n-2)
    rho = np.corrcoef(z, D)[0, 1]
    F1  = (rho**2 / (1 - rho**2)) * (n - 2)

    return b_ols, b_iv, F1

def montecarlo(n, reps, rng):
    out = np.empty((reps, 3))
    for r in range(reps):
        out[r] = simulate_one(n, rng)
    return pd.DataFrame(out, columns=["b_ols", "b_iv", "F1"])

print("="*82)
print("PARTE A — Muestras finitas (5000 réplicas, tau verdadero = 0.5)")
print("Instrumento DÉBIL: π=0.2 → en N pequeño el IV se acerca a OLS")
print("="*82)
results_A = []
for n in [30, 100, 300, 1000, 10000]:
    df = montecarlo(n, 5000, rng)
    results_A.append({
        "N":        n,
        "med_OLS":  df.b_ols.median(),
        "mean_IV":  df.b_iv.mean(),
        "med_IV":   df.b_iv.median(),
        "IV_p25":   df.b_iv.quantile(0.25),
        "IV_p75":   df.b_iv.quantile(0.75),
        "F1_mean":  df.F1.mean(),
    })
print(pd.DataFrame(results_A).to_string(index=False, float_format=lambda x: f"{x:8.3f}"))
print("\nNota: la MEDIA del IV es inestable en N pequeño (colas pesadas, momentos no")
print("      finitos con instrumento débil). La MEDIANA y el IQR muestran mejor el sesgo.")

# =============================================================
# PARTE B — LATE PASO A PASO (réplica de Clase19.pdf)
# =============================================================
#
# Construcción explícita de los 3 tipos:
#   never-takers (d00=1):     5,000 individuos — D=0 siempre
#   always-takers (d11=1):    5,000 individuos — D=1 siempre
#   compliers (d01=1):       10,000 individuos — D=Z
#
# Efectos heterogéneos:
#   never-takers:  LATE = -0.5
#   always-takers: LATE =  0
#   compliers:     LATE = +1   ← lo que IV debería recuperar
#
# ATE = 0.25*(-0.5) + 0.25*(0) + 0.50*(1) = 0.375
# IV con Z debe identificar SOLO el LATE de compliers = 1.0

print("\n" + "="*70)
print("PARTE B — LATE paso a paso")
print("="*70)

rng_B = np.random.default_rng(54687)
N = 20_000
Z = (rng_B.uniform(size=N) > 0.5).astype(int)

d00 = (np.arange(N) <  5000).astype(int)               # never-takers
d11 = ((np.arange(N) >= 5000) & (np.arange(N) < 10000)).astype(int)  # always-takers
d01 = (np.arange(N) >= 10000).astype(int)              # compliers

late = np.where(d00==1, -0.5, np.where(d11==1, 0.0, 1.0))

y0 = 0.25 * rng_B.standard_normal(N)
y1 = y0 + late

D = d11 + Z * d01
y = D*y1 + (1-D)*y0

print(f"\nFracciones por tipo: never={d00.mean():.2f}, always={d11.mean():.2f}, compliers={d01.mean():.2f}")
print(f"ATE poblacional (promedio de 'late'): {late.mean():.4f}")
print(f"LATE de compliers (lo que debería recuperar IV): {late[d01==1].mean():.4f}")

# OLS (no recupera ni ATE ni LATE — sesgado por selección)
X_ols = sm.add_constant(D)
b_ols = np.linalg.lstsq(X_ols, y, rcond=None)[0][1]
print(f"\nOLS    coef D : {b_ols:.4f}   (no recupera ATE=0.375 ni LATE=1.0)")

# IV (debe recuperar el LATE de compliers ≈ 1.0)
b_iv = np.cov(Z, y, ddof=1)[0,1] / np.cov(Z, D, ddof=1)[0,1]
print(f"IV     coef D : {b_iv:.4f}   (debe ≈ 1.0 = LATE de compliers)")

# Wald manual
EyZ1 = y[Z==1].mean(); EyZ0 = y[Z==0].mean()
EDZ1 = D[Z==1].mean(); EDZ0 = D[Z==0].mean()
wald = (EyZ1 - EyZ0) / (EDZ1 - EDZ0)
print(f"Wald   manual : {wald:.4f}   (= IV, por construcción)")
