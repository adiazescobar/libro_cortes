# PSM en Stata: `psmatch2` {#psm-stata}

::: {.boxinfo}
**Metas de aprendizaje**

- Instalar y usar `psmatch2`, `psgraph` y `pstest`
- Estimar el propensity score y restringir el análisis al soporte común
- Comparar NN, caliper, radio, kernel y LLR en Stata
- Leer los resultados de `pstest` y discutir errores estándar apropiados
:::

---

## Instalación y estructura del comando {-}

El comando estándar para PSM en Stata es `psmatch2`, disponible en SSC:

```stata
ssc install psmatch2
```

La sintaxis completa es:

```stata
psmatch2 depvar [indepvars] [if exp] [in range] [,
    outcome(varlist)
    pscore(varname)   logit odds index
    neighbor(integer) ties
    noreplacement descending
    caliper(real)
    radius
    kernel
    llr
    kerneltype(type) bwidth(real)
    spline nknots(integer)
    mahalanobis(varlist) add pcaliper(real)
    common trim(real)
    ate
    ai(#)]
```

Los comandos complementarios son `psgraph` (gráfica del propensity score) y `pstest` (balance de covariables).

---

## Variables auxiliares que genera `psmatch2` {-}

Después de correr `psmatch2`, el comando crea las siguientes variables en la base de datos:

| Variable | Descripción |
|----------|-------------|
| `_pscore` | Propensity Score estimado |
| `_treated` | Indicador de asignación al tratamiento |
| `_support` | Indicador de soporte común |
| `_weight` | Peso del control emparejado |
| `_y2` | Valor de `outcome` del match |
| `_id` | Identificador (ID) |
| `_n1` | ID del vecino más cercano (vecino 1) |
| `_nn` | Número de controles emparejados |
| `_pdif` | $|\hat{p}(\text{pscore}) - \hat{p}(\text{neighbor})|$ |

Para inspeccionar los resultados del emparejamiento:

```stata
sum pscore1 _pscore
edit D y2 pscore1 _pscore _treated _support _weight _y2 _id _n1 _nn _pdif
```

---

## El propensity score y el soporte común {-}

Antes de emparejar, es útil calcular y guardar el PS con distintas restricciones de soporte común:

| Variable | Descripción |
|----------|-------------|
| `pscore1` | PS sin restricción de soporte común |
| `pscore1_sc` | PS para el ATE (min de tratados y max de controles) |
| `pscore2_sc` | PS para el ATT (min y max del PS de los controles) |

---

## 1a. NN(1) sin reemplazo {-}

El caso más simple: cada tratado se empareja con el control más cercano en PS, sin que ningún control pueda usarse dos veces.

```stata
* Preparar orden aleatorio (importante para NN sin reemplazo)
set seed 1298
drawnorm orden
sort orden

* NN(1) sin reemplazo
psmatch2 D $X, outcome(y2) n(1) pscore(pscore1) noreplacement
psgraph
pstest $X, treated(D) both graph
```

**Resultado típico:**

```text
Variable   Sample       Treated    Controls   Difference   S.E.    T-stat
y2         Unmatched   -.6440      -.9765      .3325        .0338    9.84
           ATT         -.6440      -.9736      .3297        .0342    9.63

Note: S.E. does not take into account that the propensity score is estimated.
```

La diferencia en la muestra sin emparejar (*Unmatched*) es el sesgo bruto; la diferencia *ATT* es el efecto estimado del tratamiento sobre los tratados después del emparejamiento.

---

## 1b. NN(1) con soporte común {-}

La opción `com` (o `common`) restringe el análisis a la región de soporte común, descartando los tratados cuyo PS cae fuera del rango del PS de los controles.

```stata
psmatch2 D $X, outcome(y2) n(1) noreplacement com
psgraph
pstest $X, treated(D) both graph
```

---

## 1c. Soporte común vía trimming {-}

Otra forma de imponer soporte común es `trim(#)`, que descarta el #% de observaciones tratadas con el PS más alto (zona donde hay menos controles disponibles).

```stata
psmatch2 D $X, outcome(y2) n(1) noreplacement trim(20)
psgraph
pstest $X, treated(D) both graph
```

**Comparación de variantes NN(1):**

| Especificación | ATT | Tratados | Controles | Fuera SC | Balanceado |
|---------------|-----|----------|-----------|----------|------------|
| nn(1) | 0.330 | 1,952 | 2,048 | 0 | No |
| nn(1) sc | 0.329 | 1,950 | 2,048 | 2 | No |
| nn(1) trim | 0.337 | 1,562 | 2,048 | 390 | Casi |
| nn(1) con reemplazo | 0.353 | 1,952 | 2,048 | 0 | Sí |

---

## Verificar el balance: `pstest` {-}

### Interpretación de la tabla `pstest` {-}

```stata
pstest $X, treated(D) both graph
```

| Columna | Significado |
|---------|-------------|
| **Unmatched** | Medias antes del emparejamiento |
| **Matched** | Medias después del emparejamiento |
| **%Bias** | Diferencia estandarizada: $B = \frac{\bar{X}_T - \bar{X}_C}{\sqrt{(\hat{V}_T + \hat{V}_C)/2}} \times 100$ |
| **t-test** | Prueba t de diferencia de medias antes y después |

**Regla práctica:** $|B| < 5\%$ es excelente; $|B| < 20\%$ es aceptable.

### Indicadores globales {-}

`pstest` también reporta indicadores de balance global:

| Indicador | Descripción |
|-----------|-------------|
| **Pseudo R²** | Re-estima el PS en la muestra emparejada — debe ser muy pequeño (idealmente $\approx 0$) |
| **LR chi²** | Prueba de significancia conjunta de todos los regresores en el probit del PS. **Antes** del matching: debe rechazarse. **Después** del matching: no debe rechazarse |
| **MeanBias / MedBias** | Sesgo promedio y mediano en las covariables |

Ejemplo de salida:

```
Sample    Pseudo R2   LR chi2   p>chi2   MeanBias   MedBias
Raw       0.011       63.52     0.000    9.5        10.0
Matched   0.005       27.36     0.000    6.2        7.2
```

Si el balance es insuficiente (Pseudo R² todavía alto, LR chi² significativo), hay que re-especificar el modelo del PS o cambiar el algoritmo.

---

## 2. NN con reemplazo y errores estándar analíticos {-}

Con reemplazo, el mismo control puede emparejarse con múltiples tratados. La opción `ai(#)` calcula errores estándar analíticos de Abadie & Imbens (2006), donde `#` es el número de vecinos usados para estimar la varianza:

$$\hat{\sigma}^2(X_i, W_i) = \frac{J}{J+1}\left(Y_i - \frac{1}{J}\sum_{m=1}^{J} Y_{\ell_m(i)}\right)^2$$

```stata
* NN(1) con reemplazo, soporte común y ES analíticos
psmatch2 D $X, outcome(y2) n(1) com ai(1)
psgraph
pstest $X, treated(D) both graph
```

**NN(5) con reemplazo** (reduce varianza a costa de algo de sesgo):

```stata
psmatch2 D $X, outcome(y2) n(5) com ai(4)
```

**Comparación con reemplazo:**

| Especificación | ATT | Tratados | Controles | Fuera SC | Balanceado |
|---------------|-----|----------|-----------|----------|------------|
| nn(1) sc | 0.356 | 1,950 | 2,048 | 2 | Sí |
| nn(5) sc | 0.325 | 1,950 | 2,048 | 2 | Sí |

---

## 3. ATE con NN(1) {-}

Para estimar el ATE (efecto sobre toda la población, no solo los tratados) se agrega la opción `ate`:

```stata
psmatch2 D $X, outcome(y2) n(1) noreplacement com ai(1) ate
psgraph
pstest $X, treated(D) both graph
```

El soporte común para el ATE es más estricto: se requiere superposición en ambas colas de la distribución del PS.

---

## 4. Caliper {-}

Impone una distancia máxima $\kappa$ en el PS. Los tratados sin un control dentro del caliper se descartan:

$$C(i) = \{j \in D=0 \mid |P_i(X) - P_j(X)| < \kappa\}$$

```stata
psmatch2 D $X, outcome(y2) caliper(0.001) com ai(1)
psgraph
pstest $X, treated(D) both graph
```

---

## 5. Radio {-}

En lugar de usar solo el vecino más cercano, el radio empareja con **todos** los controles dentro del caliper:

```stata
psmatch2 D $X, outcome(y2) radius caliper(0.001) com ai(1)
psgraph
pstest $X, treated(D) both graph
```

**Comparación caliper vs. radio:**

| Especificación | ATT | Tratados | Controles | Fuera SC | Balanceado |
|---------------|-----|----------|-----------|----------|------------|
| nn(1) sc | 0.356 | 1,950 | 2,048 | 2 | Sí |
| nn(5) sc | 0.325 | 1,950 | 2,048 | 2 | Sí |
| caliper(0.001) sc | 0.349 | 1,903 | 2,048 | 49 | No |
| radius(0.001) sc | 0.333 | 1,903 | 2,048 | 49 | Sí |

El radio mejora el balance al usar más información de los controles cercanos.

---

## 6. Kernel {-}

El emparejamiento por kernel usa **todos** los controles, ponderados por su distancia al tratado. Los controles más cercanos en PS reciben mayor peso.

```stata
* Kernel Epanechnikov (por defecto) con bandwidth 0.06
psmatch2 D $X, outcome(y2) kernel kerneltype(epan) bwidth(0.06) com
psgraph
pstest $X, treated(D) both graph

* Kernel Gaussiano
psmatch2 D $X, outcome(y2) kernel kerneltype(normal) bwidth(0.06) com
```

Los kernels disponibles en `psmatch2` son: `epan` (Epanechnikov, por defecto), `normal` (Gaussiano), `biweight`, `uniform`, `tricube`.

---

## 7. Alternativa: `teffects psmatch` (Stata nativo) {-}

Stata 13+ incluye `teffects psmatch`, que estima el PS y el efecto del tratamiento en un solo paso con errores estándar correctamente calculados:

```stata
* ATT con NN(1)
teffects psmatch (y2) (D $X, probit), atet

* ATE con NN(4)
teffects psmatch (y2) (D $X, probit), ate nn(4)
```

La ventaja de `teffects` es que los errores estándar toman en cuenta la estimación del PS (a diferencia de `psmatch2` sin `ai()`). La desventaja es que no produce `psgraph` ni `pstest` directamente.

---

## Resumen de la secuencia completa {-}

```stata
* 1. Estimar el PS
logit D $X
predict double pscore1, pr

* 2. Verificar soporte común
twoway (kdensity pscore1 if D==1, lcolor(blue)) ///
       (kdensity pscore1 if D==0, lcolor(red)), ///
       legend(label(1 "Tratados") label(2 "Controles")) ///
       title("Distribución del propensity score")

* 3. Emparejar (especificación principal: NN1 con reemplazo, SC)
set seed 1298
psmatch2 D $X, outcome(y2) n(1) com ai(1)

* 4. Verificar balance
psgraph
pstest $X, treated(D) both graph

* 5. Robustez: kernel Epanechnikov
psmatch2 D $X, outcome(y2) kernel kerneltype(epan) bwidth(0.06) com

* 6. Robustez: NN(5) con reemplazo
psmatch2 D $X, outcome(y2) n(5) com ai(4)
```

---

## Lecturas recomendadas {-}

- **Leuven & Sianesi (2003)** — "PSMATCH2: Stata module to perform full Mahalanobis and propensity score matching, common support graphing, and covariate imbalance testing", SSC
- **Caliendo & Kopeinig (2008)** — "Some practical guidance for the implementation of propensity score matching", *Journal of Economic Surveys* — referencia principal
- **Abadie & Imbens (2006)** — "Large sample properties of matching estimators for average treatment effects", *Econometrica* — fundamento teórico de los ES analíticos
