# Sesión Práctica: PSM Completo en Stata {#psm-practica}

::: {.boxinfo}
**Metas de aprendizaje**

- Estimar el propensity score con `dprobit` y evaluar la calidad del modelo de selección.
- Verificar visualmente el soporte común antes de emparejar.
- Implementar en Stata los cinco algoritmos principales: vecino más cercano (NN), caliper, radio, kernel y regresión local lineal (LLR).
- Interpretar la tabla `pstest` y los indicadores globales de balance (Pseudo R², LR chi², %Bias).
- Calcular errores estándar correctos con bootstrap y con la fórmula analítica de Abadie-Imbens (`ai()`).
- Realizar el análisis de sensibilidad `sensatt` para evaluar la robustez ante variables no observadas.
:::

---

En este capítulo aplicamos todos los métodos de emparejamiento a una base de datos real. La lógica es siempre la misma: estimar el propensity score, elegir un algoritmo de emparejamiento, verificar el balance, y estimar el efecto del tratamiento. Lo que cambia entre métodos es cómo se construye el grupo de control.

La base de datos es `base6.dta`. La variable de tratamiento es `D`, el resultado de interés es `y2`, y el vector de covariables es:

```stata
global X "personas orden_n ocupado_jefe educa_jefe ingresos_hogar_jefe hombre"
```

---

## Paso 0: Preparación {-}

```stata
clear all
capture log close
use "base6.dta"
log using "log/clase6.txt", replace

global X "personas orden_n ocupado_jefe educa_jefe ingresos_hogar_jefe hombre"
```

---

## Paso 1: Estadísticas descriptivas {-}

Antes de emparejar, conviene conocer la magnitud del problema: ¿cuán diferentes son los grupos?

```stata
tab D
table D, stat(mean y1 y2)
```

La diferencia cruda entre tratados y controles en `y2` combina el efecto causal con el sesgo de selección. El objetivo del PSM es aislar el efecto causal.

---

## Paso 2: El modelo de selección {-}

El propensity score es la probabilidad estimada de recibir el tratamiento dado el vector de covariables. Se estima con un modelo probit o logit:

```stata
dprobit D $X
```

`dprobit` reporta los efectos marginales (en lugar de los coeficientes del índice latente), lo que facilita la interpretación: el coeficiente de cada variable es el cambio en la probabilidad de tratamiento ante un cambio unitario en la variable.

### Selección de variables {-}

¿Cuáles variables incluir en el PS? La regla es incluir todas las que afectan **D** (la selección) y/o **Y** (el resultado). Incluir variables irrelevantes no introduce sesgo pero reduce la eficiencia. Omitir variables que afectan ambas sí introduce sesgo.

Podemos guiarnos con selección progresiva o con LASSO:

```stata
* Selección progresiva hacia adelante (nivel de significancia 10%)
sw, pe(.1): probit D $X

* LASSO (requiere instalar pdslasso)
* ssc install pdslasso
* pdslasso D $X
```

Con base en las pruebas, usamos la especificación completa:

```stata
global X "personas orden_n ocupado_jefe educa_jefe ingresos_hogar_jefe hombre"
dprobit D $X
```

---

## Paso 3: Estimar y guardar el propensity score {-}

```stata
dprobit D $X
predict double ps1
sum ps1
histogram ps1, by(D)
twoway (kdensity ps1 if D==1, lcolor(blue) lwidth(medthick)) ///
       (kdensity ps1 if D==0, lcolor(red)  lwidth(medthick)), ///
       legend(label(1 "Tratados") label(2 "Controles")) ///
       xtitle("Propensity Score") ///
       title("Distribución del PS: soporte común")
```

El gráfico de densidades superpuestas permite ver visualmente si hay soporte común: necesitamos que las distribuciones del PS para tratados y controles se **solapen** en una región amplia.

### Calidad del modelo de PS {-}

Una prueba de calidad del PS consiste en re-estimar el probit controlando por el PS estimado. Si el PS resume bien la información de $X$, los coeficientes de las covariables individuales deben dejar de ser significativos:

```stata
* Sin PS: los coeficientes de X son significativos
dprobit D $X

* Con PS: si el PS es bueno, X pierde significancia
dprobit D ps1 $X
```

Si los coeficientes de $X$ se vuelven no significativos al agregar `ps1`, el PS captura bien la variación relevante.

---

## Paso 4: Preparar el orden aleatorio {-}

Para el emparejamiento NN sin reemplazo, el resultado depende del orden en que se procesan los tratados. Fijamos un orden aleatorio antes de emparejar:

```stata
set seed 50
drawnorm orden
sort orden
```

Este paso es **crítico**: sin él, observaciones con el mismo PS pero distinto ID pueden producir resultados distintos en cada corrida.

---

## Paso 5: Emparejamiento {-}

### 5a. NN(1) sin reemplazo con soporte común {-}

El caso más simple: cada tratado se empareja con su control más cercano en PS, sin que ningún control se use dos veces. La opción `com` descarta los tratados cuyo PS cae fuera del rango del PS de los controles.

```stata
psmatch2 D $X, outcome(y2) n(1) com noreplace
psgraph
pstest $X, both graph
```

**Interpretación de la salida:**

```
Variable   Sample       Treated    Controls   Difference   S.E.    T-stat
y2         Unmatched   ...         ...         ...          ...      ...
           ATT         ...         ...         ...          ...      ...
```

- **Unmatched**: diferencia cruda (sin emparejar) — incluye sesgo de selección
- **ATT**: diferencia después del emparejamiento — estimación del efecto causal sobre los tratados
- **S.E.**: error estándar (no toma en cuenta que el PS es estimado — usar `ai()` para corregir)

### `psgraph`: visualizar el balance en PS {-}

`psgraph` muestra la distribución del PS antes y después del emparejamiento. Después del emparejamiento, la distribución de los controles emparejados debe parecerse a la de los tratados.

### `pstest`: verificar el balance en covariables {-}

```stata
pstest $X, both graph
```

La tabla reporta, para cada covariable:

| Columna | Significado |
|---------|-------------|
| **Mean Treated** | Media de los tratados |
| **Mean Control** | Media de los controles emparejados |
| **%Bias** | Diferencia estandarizada ($\times 100$) |
| **t** | Estadístico t de diferencia de medias |
| **p>t** | Valor-p |

**Regla práctica:** $|\text{%Bias}| < 20\%$ es balance aceptable; $< 5\%$ es excelente.

Al final de la tabla, `pstest` reporta indicadores globales:

| Indicador | ¿Qué mide? | ¿Qué esperar después del matching? |
|-----------|-----------|-------------------------------------|
| **Pseudo R²** | Re-estima el PS en la muestra emparejada | Debe ser $\approx 0$ |
| **LR chi²** | Significancia conjunta de las covariables en el PS | No debe rechazarse ($p > 0.05$) |
| **MeanBias / MedBias** | Sesgo promedio y mediano | Deben ser bajos |

Si el balance es insuficiente después del matching, hay que re-especificar el PS (interacciones, cuadráticos) o cambiar el algoritmo.

---

### 5b. NN(1) usando PS guardado previamente {-}

Si ya estimamos el PS y lo guardamos en `ps1`, podemos pasarlo directamente sin re-estimarlo:

```stata
* Crear variante del PS para el ATT (soporte común del lado de los controles)
gen ps1_sc = ps1
replace ps1_sc = . if ps1 < min(ps1_min_treated, ps1_min_control) | ///
                      ps1 > max(ps1_max_treated, ps1_max_control)
* Nota: psmatch2 define el SC internamente con la opción "com"

psmatch2 D, outcome(y2) n(1) pscore(ps1) com
psgraph
pstest $X
```

Usar `pscore(varname)` en lugar de repetir las covariables asegura que el PS sea idéntico en todas las especificaciones de matching.

---

### 5c. NN(1) con trimming {-}

En lugar de imponer el soporte común desde las colas del PS de los controles, `trim(#)` descarta el `#`% de tratados con el PS más alto (la zona donde hay pocos controles disponibles):

```stata
psmatch2 D $X, outcome(y2) n(1) trim(20)
psgraph
```

`trim(20)` descarta el 20% superior de los tratados ordenados por PS. Esto puede mejorar el balance al precio de excluir tratados extremos.

---

### 5d. ATE en lugar de ATT {-}

Por defecto, `psmatch2` estima el **ATT** (efecto sobre los tratados). Para estimar el **ATE** (efecto sobre toda la población), se agrega la opción `ate`. El soporte común es más estricto: se requiere superposición en ambas colas.

```stata
* ATE con reemplazo
psmatch2 D $X, outcome(y2) n(1) com ate
psgraph
pstest $X

* ATE sin reemplazo
psmatch2 D $X, outcome(y2) n(1) com ate noreplace
psgraph
pstest $X
```

El ATE puede diferir del ATT si los efectos del tratamiento son heterogéneos entre tratados y controles.

---

### 5e. NN con múltiples vecinos {-}

Usar más de un vecino reduce la varianza del estimador a costa de algo de sesgo (los vecinos adicionales son más lejanos):

```stata
* NN(5): promedio de los 5 vecinos más cercanos
psmatch2 D $X, outcome(y2) n(5) com

* NN(10)
psmatch2 D $X, outcome(y2) n(10) com

* Con trimming
psmatch2 D $X, outcome(y2) n(5) trim(20)
psmatch2 D $X, outcome(y2) n(10) trim(20)
```

El trade-off entre sesgo y varianza depende del tamaño de la muestra de controles. Con muchos controles, NN(5) mejora la precisión sin sacrificar balance.

---

### 5f. Caliper {-}

El caliper impone una distancia máxima en PS: los tratados sin control dentro del caliper se descartan. Esto elimina emparejamientos de mala calidad:

$$C(i) = \{j \in D=0 \mid |P_i(X) - P_j(X)| < \kappa\}$$

```stata
* Caliper estricto (0.0001)
psmatch2 D $X, outcome(y2) caliper(0.0001) com
psgraph
pstest $X

* Regla empírica: 0.001 ≈ 20% de la desviación estándar del PS
psmatch2 D $X, outcome(y2) caliper(0.001) com
psgraph
pstest $X
```

Un caliper más pequeño mejora el balance pero puede dejar sin emparejar a muchos tratados (reduciendo la muestra efectiva).

---

### 5g. Radio {-}

El radio empareja con **todos** los controles dentro del caliper, no solo el más cercano. Usa más información y reduce la varianza, pero los controles más lejanos del caliper añaden algo de sesgo:

```stata
* Radio con caliper 0.001 (soporte común)
psmatch2 D $X, outcome(y2) radius caliper(0.001) com
psgraph
pstest $X

* Radio con caliper 0.001 (trimming)
psmatch2 D $X, outcome(y2) radius caliper(0.001) trim(20)
psgraph
pstest $X

* Radio más amplio
psmatch2 D $X, outcome(y2) radius caliper(0.005) trim(20)
psgraph
pstest $X
```

---

### 5h. Kernel {-}

El matching por kernel usa **todos** los controles, ponderados por su distancia al tratado. Los controles más cercanos en PS reciben mayor peso. No hay pérdida de muestra por descarte de controles lejanos — todos contribuyen, con pesos decrecientes:

$$\hat{Y}_i(0) = \sum_{j: D_j=0} \frac{K\!\left(\frac{\hat{p}(X_i) - \hat{p}(X_j)}{h}\right)}{\sum_{k: D_k=0} K\!\left(\frac{\hat{p}(X_i) - \hat{p}(X_k)}{h}\right)} Y_j$$

```stata
* Kernel Epanechnikov (por defecto)
psmatch2 D $X, outcome(y2) com kernel
psgraph
pstest $X

* Kernel Gaussiano
psmatch2 D $X, outcome(y2) com kernel kerneltype(normal) bwidth(0.06)

* Kernel uniforme (equivale a radio con ancho de banda h)
psmatch2 D $X, outcome(y2) com kernel kerneltype(uniform)
```

Los kernels disponibles en `psmatch2`: `epan` (Epanechnikov, por defecto), `normal` (Gaussiano), `biweight`, `uniform`, `tricube`.

---

## Paso 6: Errores estándar correctos {-}

### Bootstrap {-}

Los errores estándar de `psmatch2` (sin opciones adicionales) **no** toman en cuenta que el PS es estimado — son optimistas (demasiado pequeños). La solución más robusta es el bootstrap, que re-estima el PS y el ATT en cada muestra:

```stata
* Bootstrap con 1000 réplicas (kernel Epanechnikov)
bootstrap r(att), reps(1000) : psmatch2 D $X, outcome(y2) com kernel

* Bootstrap con NN(1)
bootstrap r(att), reps(500) : psmatch2 D $X, outcome(y2) n(1) com
```

El bootstrap captura correctamente la variabilidad de toda la cadena de estimación: PS + emparejamiento + diferencia de medias.

### Errores analíticos de Abadie-Imbens (`ai`) {-}

Para matching NN **con reemplazo**, `psmatch2` ofrece los errores estándar analíticos de Abadie & Imbens (2006) con la opción `ai(#)`, donde `#` es el número de vecinos usados para estimar la varianza:

```stata
* NN(1) con reemplazo y ES analíticos
psmatch2 D $X, outcome(y2) n(1) com ai(1)

* NN(5) con reemplazo y ES analíticos (usar ai(4) = n-1 vecinos)
psmatch2 D $X, outcome(y2) n(5) com ai(4)
```

`ai()` es más rápido que bootstrap y tiene fundamento teórico sólido. **Nota:** solo funciona con matching con reemplazo.

---

## Paso 7: Regresión local lineal (LLR) {-}

El LLR es una extensión del kernel matching que ajusta una regresión lineal local ponderada por el kernel, en lugar de usar un simple promedio ponderado. Reduce el sesgo en las fronteras del soporte:

```stata
* LLR con soporte común
psmatch2 D $X, llr outcome(y2) common

* Bootstrap + LLR
bootstrap r(att), reps(1000) : psmatch2 D $X, llr outcome(y2) common

* LLR con trimming
psmatch2 D $X, llr outcome(y2) trim(20)
bootstrap r(att), reps(1000) : psmatch2 D $X, llr outcome(y2) trim(20)
```

---

## Paso 8: Análisis de sensibilidad — `sensatt` {-}

Una limitación del PSM es que no puede controlar el sesgo por **variables no observadas**. El análisis de sensibilidad de Ichino, Mealli & Nannicino (2007) evalúa qué tan grandes deberían ser los efectos de una variable no observada binaria para cambiar las conclusiones.

Se especifican cuatro parámetros que describen la correlación de la variable no observada con el tratamiento y el resultado:

| Parámetro | Descripción |
|-----------|-------------|
| `p11` | $P(\text{no observada}=1 \mid D=1, Y=1)$ |
| `p10` | $P(\text{no observada}=1 \mid D=1, Y=0)$ |
| `p01` | $P(\text{no observada}=1 \mid D=0, Y=1)$ |
| `p00` | $P(\text{no observada}=1 \mid D=0, Y=0)$ |

```stata
* Instalar si no está disponible
* ssc install pscore
* ssc install sensatt

* Análisis de sensibilidad
sensatt des D $X, p11(0.6) p10(0.5) p01(0.5) p00(0.2)
```

Si el ATT estimado cambia poco bajo parámetros "razonables" de la variable no observada, los resultados son robustos a sesgo de selección no observable.

---

## Resumen comparativo de estimadores {-}

La siguiente tabla resume los resultados esperados bajo los distintos algoritmos con `base6.dta`:

| Especificación | Opción | ATT | Tratados | Balance |
|----------------|--------|-----|----------|---------|
| NN(1) sin reemplazo | `n(1) noreplace com` | ... | ... | Verificar |
| NN(1) con reemplazo | `n(1) com ai(1)` | ... | ... | Verificar |
| NN(5) con reemplazo | `n(5) com ai(4)` | ... | ... | Mejor |
| Caliper(0.001) | `caliper(0.001) com` | ... | Menos | Mejor |
| Radio(0.001) | `radius caliper(0.001) com` | ... | Menos | Mejor |
| Kernel Epanechnikov | `kernel com` | ... | Todos | Bueno |
| Kernel Gaussiano | `kernel kerneltype(normal)` | ... | Todos | Bueno |
| LLR | `llr common` | ... | Todos | Bueno |

Si los estimadores difieren mucho entre métodos, conviene investigar: ¿hay poco soporte común? ¿El PS está bien especificado?

---

## Do-file completo de la sesión {-}

```stata
***********************************************************************
* PSM en Stata — Sesión completa
* Base: base6.dta
* Resultado: y2 | Tratamiento: D
***********************************************************************

clear all
capture log close
use "base6.dta"
log using "log/clase6.txt", replace

global X "personas orden_n ocupado_jefe educa_jefe ingresos_hogar_jefe hombre"

* -------------------------------------------------------
* 1. Descriptivos
* -------------------------------------------------------
tab D
table D, stat(mean y1 y2)

* -------------------------------------------------------
* 2. Modelo de selección
* -------------------------------------------------------
dprobit D $X

* -------------------------------------------------------
* 3. Propensity Score
* -------------------------------------------------------
predict double ps1
sum ps1
twoway (kdensity ps1 if D==1, lcolor(blue) lwidth(medthick)) ///
       (kdensity ps1 if D==0, lcolor(red)  lwidth(medthick)), ///
       legend(label(1 "Tratados") label(2 "Controles")) ///
       xtitle("Propensity Score") title("Soporte común")

* Calidad del PS
dprobit D ps1 $X

* -------------------------------------------------------
* 4. Orden aleatorio para NN sin reemplazo
* -------------------------------------------------------
set seed 50
drawnorm orden
sort orden

* -------------------------------------------------------
* 5. Emparejamiento
* -------------------------------------------------------

* 5a. NN(1) sin reemplazo, soporte común
psmatch2 D $X, outcome(y2) n(1) com noreplace
psgraph
pstest $X, both graph

* 5b. NN(1), trimming 20%
psmatch2 D $X, outcome(y2) n(1) trim(20)
psgraph

* 5c. ATE
psmatch2 D $X, outcome(y2) n(1) com ate
psgraph
pstest $X

* 5d. NN(5)
psmatch2 D $X, outcome(y2) n(5) com
psmatch2 D $X, outcome(y2) n(10) com

* 5e. Caliper
psmatch2 D $X, outcome(y2) caliper(0.0001) com
psgraph
pstest $X

* 5f. Radio
psmatch2 D $X, outcome(y2) radius caliper(0.001) com
psgraph
pstest $X

* 5g. Kernel (Epanechnikov)
psmatch2 D $X, outcome(y2) com kernel
psgraph
pstest $X

* Bootstrap + kernel
bootstrap r(att), reps(1000) : psmatch2 D $X, outcome(y2) com kernel

* 5h. LLR
psmatch2 D $X, llr outcome(y2) common
bootstrap r(att), reps(1000) : psmatch2 D $X, llr outcome(y2) common

* -------------------------------------------------------
* 6. Sensibilidad
* -------------------------------------------------------
* ssc install pscore
* ssc install sensatt
sensatt des D $X, p11(0.6) p10(0.5) p01(0.5) p00(0.2)

log close
```

---

## Lecturas recomendadas {-}

- **Caliendo & Kopeinig (2008)** — "Some practical guidance for the implementation of propensity score matching", *Journal of Economic Surveys* — la referencia práctica indispensable
- **Leuven & Sianesi (2003)** — documentación de `psmatch2` (SSC)
- **Ichino, Mealli & Nannicino (2007)** — "From temporary help jobs to permanent employment: what can we learn from matching estimators and their sensitivity?", *Journal of Applied Econometrics* — fundamento de `sensatt`
- **Abadie & Imbens (2006)** — "Large sample properties of matching estimators for average treatment effects", *Econometrica* — errores estándar `ai()`

---

## Descarga los archivos {-}

**Descargar Stata do file:**
[Descargar Stata](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/16_PSMStata/16_stata.do)

**Descargar R script:**
[Descargar R](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/16_PSMStata/16_R.R)

**Descargar Python Notebook:**
[Descargar Python](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/16_PSMStata/16_phyton.ipynb)

[![Abrir en Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/adiazescobar/libro_cortes/blob/main/dofile/16_PSMStata/16_phyton.ipynb)
