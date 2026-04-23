"""
PSM, IPW y Synthetic Control en Python
Clase 16 - EconometriaAV 2026-1

Librerías necesarias:
    pip install numpy pandas scikit-learn matplotlib seaborn statsmodels
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.linear_model import LogisticRegression, LinearRegression
from scipy import stats
import warnings
warnings.filterwarnings('ignore')

# Configurar estilo
sns.set_style("whitegrid")
plt.rcParams['figure.figsize'] = (10, 6)

print("=" * 70)
print("PSM, IPW Y SYNTHETIC CONTROL EN PYTHON")
print("=" * 70)

# ============================================================
# PARTE 1: CREAR DATASET SIMULADO
# ============================================================

np.random.seed(1234)
n = 1000

# Generar covariables
X1 = np.random.normal(0, 1, n)
X2 = np.random.normal(0, 1, n)
X3 = np.random.normal(0, 1, n)

# Modelo de selección (tratamiento depende de X1)
p_tratamiento = 1 / (1 + np.exp(-(0.5 + 0.8*X1 + 0.3*X2)))
D = np.random.binomial(1, p_tratamiento, n)

# Resultado (depende de D y X1)
Y = 2*D + 0.5*X1 + 0.3*X2 + np.random.normal(0, 1, n)

# Crear DataFrame
data = pd.DataFrame({
    'Y': Y,
    'D': D,
    'X1': X1,
    'X2': X2,
    'X3': X3,
    'p_true': p_tratamiento
})

print("\nDataset creado: 1000 observaciones")
print(f"Proporción tratada: {data['D'].mean():.3f}")
print(f"Y promedio (tratados): {data.loc[data['D']==1, 'Y'].mean():.3f}")
print(f"Y promedio (controles): {data.loc[data['D']==0, 'Y'].mean():.3f}")
print(f"Diferencia cruda: {data.loc[data['D']==1, 'Y'].mean() - data.loc[data['D']==0, 'Y'].mean():.3f}")

# ============================================================
# PARTE 2: PROPENSITY SCORE MATCHING (PSM)
# ============================================================

print("\n" + "=" * 70)
print("PROPENSITY SCORE MATCHING (PSM)")
print("=" * 70)

# Estimar el propensity score
X_covs = data[['X1', 'X2', 'X3']].values
ps_model = LogisticRegression()
ps_model.fit(X_covs, data['D'])
ps = ps_model.predict_proba(X_covs)[:, 1]
data['ps'] = ps

print("\nPropensity Score estimado:")
print(f"Rango: [{ps.min():.3f}, {ps.max():.3f}]")
print(f"Media: {ps.mean():.3f}")

# Gráfica de soporte común
fig, ax = plt.subplots(figsize=(10, 6))
ax.hist(ps[data['D']==1], bins=30, alpha=0.6, label='Tratados', color='blue')
ax.hist(ps[data['D']==0], bins=30, alpha=0.6, label='Controles', color='red')
ax.set_xlabel('Propensity Score')
ax.set_ylabel('Frecuencia')
ax.set_title('Distribución del Propensity Score: Verificar Soporte Común')
ax.legend()
plt.tight_layout()
plt.savefig('/Users/adiazescobar/Library/CloudStorage/Dropbox/ClasesR/EconometriaAV/EjerciciosClase/py_pscore_dist.png', dpi=100)
print("\nGráfica de soporte común guardada: py_pscore_dist.png")

# Implementar NN(1) matching
treated = data[data['D'] == 1].copy()
control = data[data['D'] == 0].copy()

matches = []
for idx, row_t in treated.iterrows():
    ps_t = row_t['ps']
    # Encontrar el control más cercano
    dist = np.abs(control['ps'].values - ps_t)
    nn_idx = control.index[np.argmin(dist)]
    matches.append({
        'treated_idx': idx,
        'treated_Y': row_t['Y'],
        'control_idx': nn_idx,
        'control_Y': control.loc[nn_idx, 'Y'],
        'distance': np.min(dist)
    })

matches_df = pd.DataFrame(matches)
att_psm = (matches_df['treated_Y'] - matches_df['control_Y']).mean()

print(f"\nPSM-NN(1) ATT: {att_psm:.4f}")
print(f"Distancia promedio en PS: {matches_df['distance'].mean():.4f}")

# ============================================================
# PARTE 3: INVERSE PROBABILITY WEIGHTING (IPW)
# ============================================================

print("\n" + "=" * 70)
print("INVERSE PROBABILITY WEIGHTING (IPW)")
print("=" * 70)

# Construcción de pesos
data['w_att'] = np.where(data['D'] == 1, 1, data['ps'] / (1 - data['ps']))
data['w_ate'] = np.where(data['D'] == 1, 1 / data['ps'], 1 / (1 - data['ps']))

# Normalizar pesos
data['w_att_norm'] = data.groupby('D')['w_att'].transform(lambda x: x / x.sum())
data['w_ate_norm'] = data['w_ate'] / data['w_ate'].sum()

print(f"\nPesos IPW-ATT:")
print(f"  Tratados (D=1): media={data.loc[data['D']==1, 'w_att'].mean():.3f}")
print(f"  Controles (D=0): media={data.loc[data['D']==0, 'w_att'].mean():.3f}, rango=[{data.loc[data['D']==0, 'w_att'].min():.3f}, {data.loc[data['D']==0, 'w_att'].max():.3f}]")

# Estimación de ATT con IPW
Y_t = data.loc[data['D'] == 1, 'Y'].mean()
Y_c_weighted = (data.loc[data['D'] == 0, 'Y'] * data.loc[data['D'] == 0, 'w_att']).sum() / \
                data.loc[data['D'] == 0, 'w_att'].sum()
att_ipw = Y_t - Y_c_weighted

print(f"\nIPW ATT: {att_ipw:.4f}")

# Regresión ponderada
X_with_const = np.column_stack([np.ones(len(data)), data[['D', 'X1', 'X2', 'X3']].values])

# IPW-ATT
weights = data['w_att'].values
W = np.diag(weights)
X_weighted = np.sqrt(W) @ X_with_const
y_weighted = np.sqrt(W) @ data['Y'].values
beta_ipw = np.linalg.lstsq(X_weighted, y_weighted, rcond=None)[0]

print(f"Coeficiente de D en regresión ponderada: {beta_ipw[1]:.4f}")

# Gráfica de pesos IPW
fig, ax = plt.subplots(figsize=(10, 6))
ax.hist(data.loc[data['D']==0, 'w_att'], bins=30, alpha=0.7, edgecolor='black')
ax.set_xlabel('Peso IPW')
ax.set_ylabel('Frecuencia')
ax.set_title('Distribución de Pesos IPW (Controles)')
ax.axvline(data.loc[data['D']==0, 'w_att'].mean(), color='red', linestyle='--', linewidth=2, label='Media')
ax.legend()
plt.tight_layout()
plt.savefig('/Users/adiazescobar/Library/CloudStorage/Dropbox/ClasesR/EconometriaAV/EjerciciosClase/py_ipw_weights.png', dpi=100)
print("\nGráfica de pesos IPW guardada: py_ipw_weights.png")

# ============================================================
# PARTE 4: SYNTHETIC CONTROL (SIMULACIÓN)
# ============================================================

print("\n" + "=" * 70)
print("SYNTHETIC CONTROL METHOD (Simulado)")
print("=" * 70)

# Crear dataset tipo series de tiempo
np.random.seed(1234)
years = np.arange(1970, 2001)
n_units = 39
n_years = len(years)

# Crear panel de datos
treated_unit = 0  # Unidad 0 es "tratada"
treatment_year = 1988

# Generar outcomes para cada unidad
outcomes = np.zeros((n_units, n_years))
for i in range(n_units):
    # Tendencia base
    trend = np.arange(n_years) * 0.5

    if i == treated_unit:
        # Unidad tratada
        y_base = 120 + trend + np.random.normal(0, 5, n_years)
        # Efecto del tratamiento (post-1988)
        treatment_idx = years >= treatment_year
        y_base[treatment_idx] -= 20  # Reducción de 20 unidades
        outcomes[i, :] = y_base
    else:
        # Unidades de control
        outcomes[i, :] = 100 + trend + np.random.normal(0, 3, n_years)

# Crear DataFrame
synth_data = []
for i in range(n_units):
    for j, year in enumerate(years):
        synth_data.append({
            'unit': i,
            'year': year,
            'Y': outcomes[i, j],
            'treated': (i == treated_unit),
            'post': (year >= treatment_year)
        })

synth_df = pd.DataFrame(synth_data)

# Calcular el control sintético (promedio simple para simplicidad)
pre_period = synth_df['year'] < treatment_year
control_avg = synth_df[pre_period & (synth_df['unit'] != treated_unit)].groupby('year')['Y'].mean()
treated_outcome = synth_df[synth_df['unit'] == treated_unit].set_index('year')['Y']

# Gráfica
fig, ax = plt.subplots(figsize=(12, 6))
ax.plot(years, treated_outcome.values, 'b-', linewidth=2, label='Treated Unit', marker='o')
ax.plot(control_avg.index, control_avg.values, 'r--', linewidth=2, label='Synthetic Control', marker='s')
ax.axvline(treatment_year, color='gray', linestyle=':', linewidth=2, alpha=0.7)
ax.fill_between(years[years >= treatment_year],
                treated_outcome.loc[years >= treatment_year].values,
                control_avg.loc[years >= treatment_year].values,
                alpha=0.2, color='green', label='Estimated Effect')
ax.set_xlabel('Year')
ax.set_ylabel('Outcome')
ax.set_title('Synthetic Control Method: Treated Unit vs. Synthetic Control')
ax.legend()
ax.grid(True, alpha=0.3)
plt.tight_layout()
plt.savefig('/Users/adiazescobar/Library/CloudStorage/Dropbox/ClasesR/EconometriaAV/EjerciciosClase/py_synth_control.png', dpi=100)
print("\nGráfica de control sintético guardada: py_synth_control.png")

# Calcular efecto
post_period = synth_df['year'] >= treatment_year
effect_gap = treated_outcome.loc[years >= treatment_year].values - control_avg.loc[years >= treatment_year].values
effect_avg = effect_gap.mean()

print(f"\nEfecto promedio post-tratamiento: {effect_avg:.3f}")
print(f"Efecto total acumulado: {effect_gap.sum():.1f}")

# ============================================================
# RESUMEN COMPARATIVO
# ============================================================

print("\n" + "=" * 70)
print("RESUMEN COMPARATIVO DE ESTIMADORES")
print("=" * 70)

print(f"\nVerdadero efecto causal (ATT): 2.0")
print(f"Diferencia cruda (sesgo): {data.loc[data['D']==1, 'Y'].mean() - data.loc[data['D']==0, 'Y'].mean():.4f}")
print(f"PSM-NN(1): {att_psm:.4f}")
print(f"IPW-ATT:   {att_ipw:.4f}")
print(f"OLS (naive): {pd.DataFrame(data).corr().loc['Y', 'D']:.4f}")

print("\n" + "=" * 70)
print("CONCLUSIÓN")
print("=" * 70)
print("""
Los tres métodos (PSM, IPW, Synthetic Control) estiman efectos causales
bajo supuestos de selección observable (CIA). Las diferencias entre estimadores
refleja:
  1. Variabilidad muestral
  2. Trade-off sesgo-varianza
  3. Especificación del propensity score

Para mayor robustez, combina múltiples métodos y verifica sensibilidad.
""")

print("\nGráficas guardadas en EjerciciosClase/")
