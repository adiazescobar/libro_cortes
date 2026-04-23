# Sesión Práctica: PSM, IPW y Controles Sintéticos en Stata {#psm-ipw-sinteticos}

::: {.boxinfo}
**Metas de aprendizaje**

- Implementar Propensity Score Matching (PSM) con cinco algoritmos: NN, caliper, radio, kernel y LLR
- Estimar el propensity score y verificar soporte común
- Interpretar la tabla `pstest` y los indicadores de balance (Pseudo R², LR chi², %Bias)
- Calcular errores estándar correctos (bootstrap, Abadie-Imbens)
- Usar Inverse Probability Weighting (IPW) como alternativa al matching
- Implementar Synthetic Control Method para evaluaciones con unidades tratadas únicas
- Realizar análisis de sensibilidad a variables no observadas
:::

---

## Parte I: Propensity Score Matching (PSM) {#psm-sec}

En este capítulo aplicamos todos los métodos de emparejamiento a una base de datos real. La lógica es siempre la misma: estimar el propensity score, elegir un algoritmo de emparejamiento, verificar el balance, y estimar el efecto del tratamiento. Lo que cambia entre métodos es cómo se construye el grupo de control.

La base de datos es `base6.dta`. La variable de tratamiento es `D`, el resultado de interés es `y2`, y el vector de covariables es:

```stata
global X "personas orden_n ocupado_jefe educa_jefe ingresos_hogar_jefe hombre"
```

### Paso 0: Preparación

```stata
clear all
set seed 1298
set more off
use base6.dta
log using "psm_complete.log", replace

global X "personas orden_n ocupado_jefe educa_jefe ingresos_hogar_jefe hombre"
```

---

### Paso 1: Estadísticas descriptivas

Antes de emparejar, conviene conocer la magnitud del problema: ¿cuán diferentes son los grupos?

```stata
tab D
table D, stat(mean y1 y2)
```

La diferencia cruda entre tratados y controles en `y2` combina el efecto causal con el sesgo de selección. El objetivo del PSM es aislar el efecto causal.

---

### Paso 2: El modelo de selección y el propensity score

El propensity score es la probabilidad estimada de recibir el tratamiento dado el vector de covariables. Se estima con un modelo probit o logit:

```stata
logit D $X
predict double pscore1, pr

summ pscore1, detail
```

**Verificación visual del soporte común:**

```stata
twoway (kdensity pscore1 if D==1, lcolor(blue) lwidth(medium)) ///
       (kdensity pscore1 if D==0, lcolor(red) lwidth(medium)), ///
       legend(label(1 "Tratados") label(2 "Controles")) ///
       title("Distribución del Propensity Score") ///
       xtitle("Propensity Score") ytitle("Densidad")
```

El gráfico de densidades superpuestas permite ver visualmente si hay soporte común: necesitamos que las distribuciones del PS para tratados y controles se **solapen** en una región ampla.

---

### Paso 3: Preparar el orden aleatorio

Para el emparejamiento NN sin reemplazo, el resultado depende del orden en que se procesan los tratados. Fijamos un orden aleatorio antes de emparejar:

```stata
set seed 1298
drawnorm orden
sort orden
```

Este paso es **crítico**: sin él, observaciones con el mismo PS pero distinto ID pueden producir resultados distintos en cada corrida.

---

### Paso 4: Opciones de Emparejamiento

#### 4a. NN(1) sin reemplazo, soporte común

El caso más simple: cada tratado se empareja con su control más cercano en PS, sin que ningún control se use dos veces.

```stata
psmatch2 D $X, outcome(y2) n(1) noreplacement common
psgraph
pstest $X, treated(D) both graph
```

**Interpretación:**
- **Unmatched**: diferencia cruda (incluye sesgo de selección)
- **ATT**: efecto del tratamiento sobre los tratados después del emparejamiento
- **S.E.**: error estándar (sin ajuste por estimación del PS)

#### 4b. NN(1) con reemplazo y errores estándar analíticos

```stata
psmatch2 D $X, outcome(y2) n(1) common ai(1)
psgraph
pstest $X, treated(D) both graph
```

La opción `ai(1)` calcula errores estándar analíticos de Abadie & Imbens (2006), que toman en cuenta que el PS es estimado.

#### 4c. NN con múltiples vecinos

```stata
* NN(5): promedio de los 5 vecinos más cercanos
psmatch2 D $X, outcome(y2) n(5) common ai(4)

* NN(10)
psmatch2 D $X, outcome(y2) n(10) common ai(9)
```

Usar más vecinos reduce la varianza a costa de algo de sesgo (los vecinos adicionales son más lejanos).

#### 4d. Caliper

Impone una distancia máxima en PS. Los tratados sin control dentro del caliper se descartan.

```stata
* Caliper estricto
psmatch2 D $X, outcome(y2) caliper(0.001) common ai(1)
psgraph
pstest $X, treated(D) both graph
```

Un caliper más pequeño mejora el balance pero puede dejar sin emparejar a muchos tratados.

#### 4e. Radio

El radio empareja con **todos** los controles dentro del caliper, no solo el más cercano.

```stata
psmatch2 D $X, outcome(y2) radius caliper(0.001) common ai(1)
psgraph
pstest $X, treated(D) both graph
```

#### 4f. Kernel Matching

El kernel usa **todos** los controles, ponderados por su distancia al tratado. Los controles más cercanos en PS reciben mayor peso.

```stata
* Kernel Epanechnikov (por defecto)
psmatch2 D $X, outcome(y2) kernel kerneltype(epan) bwidth(0.06) common
psgraph
pstest $X, treated(D) both graph

* Kernel Gaussiano
psmatch2 D $X, outcome(y2) kernel kerneltype(normal) bwidth(0.06) common
```

Los kernels disponibles: `epan` (Epanechnikov), `normal` (Gaussiano), `biweight`, `uniform`, `tricube`.

#### 4g. Regresión Local Lineal (LLR)

El LLR ajusta una regresión lineal local ponderada por el kernel. Reduce el sesgo en las fronteras del soporte.

```stata
psmatch2 D $X, llr outcome(y2) common
bootstrap r(att), reps(1000) : psmatch2 D $X, llr outcome(y2) common
```

---

### Paso 5: Verificación del Balance

#### Tabla `pstest`

```stata
pstest $X, treated(D) both graph
```

La tabla reporta para cada covariable:

| Columna | Significado |
|---------|-------------|
| **Unmatched** | Medias antes del emparejamiento |
| **Matched** | Medias después del emparejamiento |
| **%Bias** | Diferencia estandarizada ($\times 100$) |
| **t-test** | Significancia de la diferencia de medias |

**Regla práctica:** $|\text{%Bias}| < 5\%$ es excelente; $< 20\%$ es aceptable.

#### Indicadores globales

`pstest` reporta:

| Indicador | ¿Qué esperar después del matching? |
|-----------|-------------------------------------|
| **Pseudo R²** | Debe ser $\approx 0$ |
| **LR chi²** | No debe rechazarse ($p > 0.05$) |
| **MeanBias / MedBias** | Deben ser bajos |

Si el balance es insuficiente, hay que re-especificar el PS (interacciones, cuadráticos) o cambiar el algoritmo.

---

### Paso 6: Errores estándar correctos

#### Bootstrap

Los errores estándar de `psmatch2` (sin opciones adicionales) **no** toman en cuenta que el PS es estimado. La solución más robusta es el bootstrap:

```stata
* Bootstrap con 1000 réplicas
bootstrap r(att), reps(1000) : psmatch2 D $X, outcome(y2) common kernel
```

El bootstrap captura correctamente la variabilidad de toda la cadena: PS + emparejamiento + diferencia de medias.

#### Errores analíticos (Abadie-Imbens)

Para matching NN **con reemplazo**, `psmatch2` ofrece errores estándar analíticos con la opción `ai(#)`:

```stata
psmatch2 D $X, outcome(y2) n(1) common ai(1)
psmatch2 D $X, outcome(y2) n(5) common ai(4)
```

---

### Paso 7: Alternativa nativa: `teffects psmatch`

Stata 13+ incluye `teffects psmatch`, que estima el PS y el efecto del tratamiento en un solo paso con SE correctamente calculados:

```stata
* ATT con NN(1)
teffects psmatch (y2) (D $X, probit), atet

* ATE con NN(4)
teffects psmatch (y2) (D $X, probit), ate nn(4)
```

---

## Parte II: Inverse Probability Weighting (IPW) {#ipw-sec}

IPW es un estimador alternativo al matching que **re-pondera** la muestra en lugar de descartar observaciones. La idea: en lugar de emparejar, usamos pesos que hacen la muestra parecer como si el tratamiento fuera aleatorio.

### ¿Por qué IPW?

- **No descarta observaciones:** usa toda la muestra ponderada
- **Más eficiente que matching:** menos pérdida de varianza
- **Fácil combinar con otros estimadores:** se puede usar en regresión, DiD, etc.

**Descargar do-file IPW:**
[Descargar 02_ipw_stata.do](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/16_PSM_IPW_Sinteticos/02_ipw_stata.do)

---

### Paso 1: Estimar el propensity score

```stata
probit D $X
predict double ps, pr

summ ps
```

---

### Paso 2: Verificar soporte común

Los pesos IPW pueden explotar si hay poco soporte común:

```stata
gen soporte = (ps > 0.1 & ps < 0.9)
summ soporte

twoway (kdensity ps if D==1, lcolor(blue)) ///
       (kdensity ps if D==0, lcolor(red)), ///
       xline(0.1) xline(0.9) ///
       legend(label(1 "Tratados") label(2 "Controles")) ///
       title("Verificar Soporte Común (0.1 < PS < 0.9)")
```

---

### Paso 3: Construir pesos IPW

#### IPW para ATT (Average Treatment Effect on the Treated)

```stata
gen double w_att = cond(D==1, 1, ps/(1-ps))
```

Interpretación: los controles se ponderan inversamente a la probabilidad de ser control ($1/(1-ps)$), lo que hace la distribución de $X$ entre controles ponderados igual a la de los tratados.

#### IPW para ATE (Average Treatment Effect)

```stata
gen double w_ate = cond(D==1, 1/ps, 1/(1-ps))
```

Interpretación: cada observación se pondera inversamente a la probabilidad de estar en su grupo asignado actual.

---

### Paso 4: Usar pesos en regresión

#### Comparación simple de medias ponderadas

```stata
* ATT
mean y2 if D==1
mean y2 if D==0 [pw=w_att]
```

#### Regresión ponderada

```stata
* Especificación simple
reg y2 D [pw=w_att], cluster(id) robust
reg y2 D [pw=w_ate], cluster(id) robust

* Con controles adicionales
reg y2 D $X [pw=w_att], cluster(id) robust
```

#### Comando nativo: `teffects ipw`

Está integrado especialmente para IPW con SEs correctos:

```stata
teffects ipw (y2) (D $X, probit), atet vce(robust)
teffects ipw (y2) (D $X, probit), ate vce(robust)
```

---

### Paso 5: Sensibilidad a pesos extremos

Los pesos IPW pueden ser muy grandes si PS es muy cercano a 0 o 1:

```stata
* Opción 1: Trim observaciones extremas
gen w_att_trim = w_att if soporte==1

* Opción 2: Capping (limitar el peso máximo)
gen w_att_cap = min(w_att, 5)

* Comparar resultados
reg y2 D [pw=w_att], robust
reg y2 D [pw=w_att_trim], robust
reg y2 D [pw=w_att_cap], robust
```

---

### Paso 6: Verificar balance con pesos IPW

Después de IPW, la distribución de $X$ debe ser similar entre tratados y controles **en la muestra ponderada**:

```stata
table D, stat(sum w_att)
```

---

## Parte III: Synthetic Control Method {#synth-sec}

El Synthetic Control Method es un estimador para cuando el tratamiento afecta a **una sola unidad agregada** ($N_T = 1$). Ejemplos:

- ¿Cuánto costó el Pico y Placa a la calidad del aire de Bogotá?
- ¿Cuál fue el efecto de la Prop 99 (impuesto al tabaco) en California en 1988?
- ¿Qué efecto tuvo el Brexit en el PIB del Reino Unido?

**Descargar do-file Controles Sintéticos:**
[Descargar 03_synthetic_controls_stata.do](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/16_PSM_IPW_Sinteticos/03_synthetic_controls_stata.do)

**Descargar datos (caso Prop 99):**
[synth_smoking.dta](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/16_PSM_IPW_Sinteticos/synth_smoking.dta)

---

### La intuición

Si no encuentro un control natural que se parezca exactamente al tratado en el período pre-tratamiento, lo **construyo** como una combinación ponderada de otros controles potenciales (el "donor pool").

El sintético es un control "a la medida": si reproduce bien al tratado en el período pre-tratamiento, podemos asumir razonablemente que también lo habría reproducido en el período post (en ausencia del tratamiento). La brecha post-tratamiento es entonces atribuible al tratamiento.

---

### Paso 1: Setup y exploración

La base de datos es `synth_smoking.dta` (consumo de cigarrillos per capita en EE.UU., 1970-2000).

```stata
clear all
use synth_smoking.dta

xtset state year

* Gráfica básica
twoway (line y year if state==1, lcolor(blue) lwidth(medium)) ///
       (line y year if state!=1, lcolor(gray%30)), ///
       xline(1988, lpattern(dash) lcolor(red)) ///
       legend(label(1 "California") label(2 "Otros Estados")) ///
       title("Consumo de Cigarrillos: California vs. Otros Estados")
```

---

### Paso 2: Setup formal

Disponemos de:
- Una unidad tratada (California, state==1)
- Un "donor pool" de $J$ controles potenciales (otros 38 estados)
- Períodos pre-tratamiento (antes de 1988) y post-tratamiento (1988-2000)
- Outcome: consumo de cigarrillos per capita

**Objetivo:** construir un California **sintético** como combinación ponderada de otros estados:

$$\hat{Y}_{1t}^{N} = \sum_{j=2}^{J+1} w_j Y_{jt}$$

donde $\sum_j w_j = 1$ y $w_j \geq 0$.

---

### Paso 3: Estimación simplificada (manual)

Si el comando `synth` no está disponible, podemos hacer una versión simplificada:

```stata
* Computar medias pre-tratamiento
bys state: egen y_pre = mean(y) if year < 1988

* Comparar California contra promedio de controles
collapse (mean) y, by(year treated)
reshape wide y, i(year) j(treated)

rename y0 y_controls
rename y1 y_calif

gen diff = y_calif - y_controls
gen post = (year >= 1988)
```

---

### Paso 4: Graficar el efecto

```stata
twoway (line diff year, lcolor(blue) lwidth(medium)) ///
       (xline(1988, lpattern(dash))), ///
       title("Efecto Estimado: California vs. Control") ///
       xtitle("Año") ytitle("Brecha")

* Efecto promedio post-tratamiento
summ diff if post==1
```

---

### Paso 5: Análisis de sensibilidad

#### Pre-tendencias

¿Había tendencias diferentes antes de 1988?

```stata
reg diff year if year < 1988
```

Si el coeficiente en `year` es significativo, hay pre-tendencias que violarían el supuesto de "no cambio en trayectoria contrafactual".

#### Placebo

¿Habría detectado el método un efecto ficticio si el "tratamiento" hubiera sido en 1980?

```stata
gen placebo_1980 = (year >= 1980)
reg diff year##placebo_1980
```

Si el efecto placebo es cercano a cero, el método es creíble.

---

### Paso 6: Interpretación

| Elemento | Interpretación |
|----------|----------------|
| **Brecha post-1988** | Efecto estimado de Prop 99 |
| **No pre-tendencias** | Soporte para paralelismo contrafactual |
| **Placebo cercano a cero** | Validez del método |

---

## Descarga los archivos {-}

**Descargar Stata do file (PSM - recomendado para clase):**
[Descargar Stata](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/16_PSM_IPW_Sinteticos/01_psm_stata_CLASSROOM.do)

**Descargar Stata do file (IPW):**
[Descargar IPW Stata](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/16_PSM_IPW_Sinteticos/02_ipw_stata.do)

**Descargar Stata do file (Controles Sintéticos):**
[Descargar Sintéticos Stata](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/16_PSM_IPW_Sinteticos/03_synthetic_controls_stata.do)

**Descargar Python Notebook:**
[Descargar Python](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/16_PSM_IPW_Sinteticos/04_psm_ipw_python.py)

**Descargar base6.dta:**
[Descargar base6.dta](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/16_PSM_IPW_Sinteticos/base6.dta)

**Descargar synth_smoking.dta:**
[Descargar synth_smoking.dta](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/16_PSM_IPW_Sinteticos/synth_smoking.dta)

---

## Resumen comparativo {#resumen-sec}

| Estimador | Ventaja | Desventaja | Cuándo usar |
|-----------|---------|-----------|-------------|
| **PSM-NN** | Intuitivo, transparente | Pequeña muestra de matches | Cuando hay muchos matches buenos |
| **PSM-Kernel** | Usa toda la muestra | Menos transparente | Cuando el soporte es amplio |
| **IPW** | Eficiente, flexible | Pesos extremos posibles | Cuando hay buen soporte común |
| **Synthetic** | Para $N_T=1$ | Requiere muchos pre-períodos | Estudio de caso o política agregada |

---

## Do-files disponibles {-}

Los do-files ejecutables están en `EjerciciosClase/`:

- **01_psm_stata.do** — PSM completo: NN, kernel, caliper, radio, LLR
- **02_ipw_stata.do** — IPW: construcción de pesos, regresión ponderada, sensibilidad
- **03_synthetic_controls_stata.do** — Synthetic Control: simulación, gráficas, pre-tendencias, placebo

**Para ejecutar:**
```stata
do "01_psm_stata.do"
do "02_ipw_stata.do"
do "03_synthetic_controls_stata.do"
```

---

## Lecturas recomendadas {-}

### PSM
- **Caliendo & Kopeinig (2008)** — "Some practical guidance for the implementation of propensity score matching", *Journal of Economic Surveys*
- **Abadie & Imbens (2006)** — "Large sample properties of matching estimators for average treatment effects", *Econometrica*

### IPW
- **Rotnitzky & Robins (1995)** — Fundamentos teóricos
- **Bang & Robins (2005)** — Implementación práctica

### Synthetic Control
- **Abadie & Gardeazabal (2003)** — "The economic costs of conflict: the case of the Basque Country", *Journal of International Economics*
- **Abadie, Diamond & Hainmueller (2010)** — "Synthetic Control Methods for Comparative Case Studies", *Journal of the American Statistical Association*

