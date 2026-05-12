"""
RDD_simulacion.py
Econometría Avanzada — Javeriana
Acompañamiento al Cap. Regresión Discontinua (libro_cortes)

Cinco partes (espejo del do-file de Stata):
  A. RDN con DGP lineal
  B. RDN con DGP curvo (Gelman & Imbens 2019)
  C. Sensibilidad al bandwidth
  D. Tests de validez (rddensity, covariables, placebos)
  E. RDB (rdrobust fuzzy + 2SLS local)
  F. Variable de asignación discreta (Kolesár & Rothe 2018)

Requisitos:
    pip install numpy pandas statsmodels linearmodels matplotlib rdrobust
    # rdrobust en Python: https://rdpackages.github.io/rdrobust/

El paquete rdrobust de Python expone:
    rdrobust, rdbwselect, rdplot   (de rdrobust)
    rddensity                       (de rddensity)  pip install rddensity
"""

import numpy as np
import pandas as pd
import statsmodels.formula.api as smf
import matplotlib.pyplot as plt
from rdrobust import rdrobust, rdbwselect, rdplot

try:
    from rddensity import rddensity
    HAVE_RDDENSITY = True
except ImportError:
    HAVE_RDDENSITY = False
    print("[aviso] rddensity no instalado — Parte D.1 omitida. pip install rddensity")

from linearmodels.iv import IV2SLS

SEED = 20260511

# ===============================================================
# PARTE A — RDN con DGP lineal
# ===============================================================
print("\n" + "=" * 60)
print("PARTE A — RDN con DGP lineal")
print("=" * 60)

rng = np.random.default_rng(SEED)
N = 2000
Z = rng.uniform(50, 74, N)
D = (Z >= 62).astype(int)
Zt = Z - 62
y = 1 + 0.10 * Zt + 2.0 * D + rng.standard_normal(N)
df = pd.DataFrame({"y": y, "Z": Z, "D": D, "Zt": Zt})

# Visualización primero
print("\n--- rdplot ---")
rdplot(y, Z, c=62, title="RDN — DGP lineal",
       x_label="Edad", y_label="Resultado")
plt.savefig("rdplot_A.png", dpi=120, bbox_inches="tight")
plt.close()

# Estimación I: polinomio global
df["DZt"] = df["D"] * df["Zt"]
df["Zt2"] = df["Zt"] ** 2
df["DZt2"] = df["D"] * df["Zt2"]

print("\n--- Polinomio lineal global ---")
m_lin = smf.ols("y ~ D + Zt + DZt", data=df).fit()
print(m_lin.summary().tables[1])

print("\n--- Polinomio cuadrático global ---")
m_cua = smf.ols("y ~ D + Zt + DZt + Zt2 + DZt2", data=df).fit()
print(m_cua.summary().tables[1])

# Estimación II: local lineal con kernel triangular
print("\n--- Local lineal con rdrobust (kernel triangular, MSE-óptimo) ---")
out = rdrobust(y, Z, c=62, p=1, kernel="triangular")
print(out)

# ===============================================================
# PARTE B — RDN con DGP curvo
# ===============================================================
print("\n" + "=" * 60)
print("PARTE B — RDN con DGP curvo")
print("=" * 60)

rng = np.random.default_rng(SEED)
Z = rng.uniform(50, 74, N)
D = (Z >= 62).astype(int)
Zt = Z - 62
y = 1 + 0.5 * Zt - 0.02 * Zt**2 + 2.0 * D + rng.standard_normal(N)
df = pd.DataFrame({"y": y, "Z": Z, "D": D, "Zt": Zt,
                   "DZt": D * Zt, "Zt2": Zt**2, "DZt2": D * Zt**2})

rdplot(y, Z, c=62, title="RDN — DGP curvo", x_label="Z", y_label="y")
plt.savefig("rdplot_B.png", dpi=120, bbox_inches="tight")
plt.close()

print("\n--- [DGP curvo] Polinomio lineal: sesgo esperado ---")
print(smf.ols("y ~ D + Zt + DZt", data=df).fit().summary().tables[1])

print("\n--- [DGP curvo] Polinomio cuadrático: corrige si conocemos el DGP ---")
print(smf.ols("y ~ D + Zt + DZt + Zt2 + DZt2",
              data=df).fit().summary().tables[1])

print("\n--- [DGP curvo] Local lineal: no requiere conocer la curvatura ---")
print(rdrobust(y, Z, c=62, p=1, kernel="triangular"))

# ===============================================================
# PARTE C — Sensibilidad al bandwidth
# ===============================================================
print("\n" + "=" * 60)
print("PARTE C — Sensibilidad al bandwidth")
print("=" * 60)

rng = np.random.default_rng(SEED)
Z = rng.uniform(50, 74, N)
D = (Z >= 62).astype(int)
Zt = Z - 62
y = 1 + 0.10 * Zt + 2.0 * D + rng.standard_normal(N)

print(f"\n{'h':>4}  {'coef':>8}  {'se':>8}")
for h in (1, 2, 3, 5, 8, 12):
    r = rdrobust(y, Z, c=62, h=h, p=1, kernel="triangular")
    coef = float(r.coef.iloc[0])
    se = float(r.se.iloc[0])
    print(f"{h:>4}  {coef:>8.3f}  {se:>8.3f}")

print("\n--- Bandwidth MSE-óptimo (CCT, default) ---")
print(rdrobust(y, Z, c=62, bwselect="mserd"))

print("\n--- Bandwidth Coverage-error-rate óptimo ---")
print(rdrobust(y, Z, c=62, bwselect="cerrd"))

# ===============================================================
# PARTE D — Tests de validez
# ===============================================================
print("\n" + "=" * 60)
print("PARTE D — Tests de validez")
print("=" * 60)

# D.1 Manipulación
if HAVE_RDDENSITY:
    print("\n--- rddensity (sin manipulación, H0 esperado: no rechazo) ---")
    print(rddensity(Z, c=62))

    Z_manip = Z.copy()
    mask = (Z_manip >= 61.5) & (Z_manip < 62) & (rng.uniform(0, 1, N) < 0.5)
    Z_manip[mask] = 62.5
    print("\n--- rddensity (con manipulación artificial, debería rechazar) ---")
    print(rddensity(Z_manip, c=62))

# D.2 Continuidad de covariables predeterminadas
rng = np.random.default_rng(SEED)
Z = rng.uniform(50, 74, N)
D = (Z >= 62).astype(int)
sexo = (rng.uniform(0, 1, N) < 0.5).astype(int)
ingre = 100 + 5 * D + rng.standard_normal(N) * 10
y = 1 + 0.10 * (Z - 62) + 2.0 * D + rng.standard_normal(N)

print("\n--- Covariable predeterminada (sexo): no debe saltar ---")
print(rdrobust(sexo, Z, c=62))

print("\n--- Covariable contaminada (ingre): sí salta, NO usar como control ---")
print(rdrobust(ingre, Z, c=62))

# D.3 Placebos en cutoffs falsos
print("\n--- Placebos en cutoffs falsos: ninguno debe ser significativo ---")
for c_fake in (58, 60, 64, 66):
    r = rdrobust(y, Z, c=c_fake)
    coef = float(r.coef.iloc[0])
    pv = float(r.pv.loc["Robust"])
    print(f"Cutoff falso {c_fake:>3}   coef = {coef:>6.3f}   robust p = {pv:>5.3f}")

# ===============================================================
# PARTE E — RDB
# ===============================================================
print("\n" + "=" * 60)
print("PARTE E — RDB")
print("=" * 60)

rng = np.random.default_rng(SEED)
N = 5000
Z = rng.uniform(50, 74, N)
W = (Z >= 62).astype(int)
Zt = Z - 62
p = 0.20 + 0.60 * W
D = (rng.uniform(0, 1, N) < p).astype(int)
y = 1 + 0.10 * Zt + 2.0 * D + rng.standard_normal(N)

print("\n--- Forma reducida: salto en y ---")
print(rdrobust(y, Z, c=62))

print("\n--- Primera etapa: salto en D ---")
print(rdrobust(D, Z, c=62))

print("\n--- RDB (rdrobust con fuzzy=D) ---")
print(rdrobust(y, Z, c=62, fuzzy=D))

# Equivalencia con 2SLS local
h = 5
mask = np.abs(Zt) < h
df_iv = pd.DataFrame({"y": y, "D": D, "W": W, "Zt": Zt, "WZt": W * Zt})[mask]
df_iv = df_iv.assign(const=1.0)

iv_fit = IV2SLS(df_iv["y"],
                df_iv[["const", "Zt", "WZt"]],
                df_iv[["D"]],
                df_iv[["W"]]).fit(cov_type="robust")
print(f"\n--- 2SLS local con bandwidth fijo h = {h} (kernel uniforme) ---")
print(iv_fit.summary)

# ===============================================================
# PARTE F — Variable de asignación discreta (Kolesár & Rothe 2018)
# ===============================================================
print("\n" + "=" * 60)
print("PARTE F — Variable de asignación discreta")
print("=" * 60)

rng = np.random.default_rng(SEED)
N = 3000
Z = rng.integers(0, 101, N).astype(float)        # puntaje entero 0-100
D = (Z >= 50).astype(int)
Zt = Z - 50
y = 1 + 0.05 * Zt + 2.0 * D + rng.standard_normal(N)

print("\n--- SE convencionales (subcubren con Z discreta) ---")
print(rdrobust(y, Z, c=50))

print("\n--- SE clusterizados en Z (Lee & Card 2008) — más conservadores ---")
print(rdrobust(y, Z, c=50, cluster=Z))

print("\nNota: honest CIs (Kolesár & Rothe 2018) están en el paquete R RDHonest;")
print("no hay implementación oficial en Python a la fecha.")

print("\nRDD_simulacion.py — terminado.")
