# RDD en Stata: simulación {#rdd-stata}

::: {.boxinfo}
**Metas de aprendizaje**

- Aplicar la **receta de estimación** en Stata: centrar $\tilde Z$, recortar muestra, modelar $E[Y \mid \tilde Z, D]$
- Verificar empíricamente que en RDN el polinomio global y la regresión local lineal recuperan el mismo salto cuando el modelo está bien especificado
- Ver cómo el polinomio global se equivoca cuando la relación es muy curva
- Visualizar la sensibilidad del estimador al ancho de banda y entender el bandwidth MSE-óptimo (CCT)
- Correr las 4 pruebas de robustez: sensibilidad, balance, placebo, sorting (`rddensity`)
- Estimar RDB y verificar que coincide con 2SLS local
- Reconocer cómo cambian los SE cuando la variable de asignación es discreta
:::

---

## Instalación previa {-}

```stata
ssc install rdrobust
ssc install rddensity
ssc install lpdensity   // dependencia de rddensity
```

**Descargar archivos ejecutables:**

[Descargar do-file Stata (RDD_simulacion.do)](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/19_RDD/RDD_simulacion.do)

[Descargar versión Python (RDD_simulacion.py)](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/19_RDD/RDD_simulacion.py)

[Descargar versión R (RDD_simulacion.R)](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/19_RDD/RDD_simulacion.R)

[Descargar do-file de clase — Sharp + Fuzzy (05_rdd_stata_CLASSROOM.do)](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/19_RDD/05_rdd_stata_CLASSROOM.do)

Los tres primeros archivos contienen las mismas seis partes (RDN lineal, RDN curvo, ancho de banda, pruebas de robustez, RDB, $Z$ discreta) y producen los mismos resultados. El do-file de clase (`05_rdd_stata_CLASSROOM.do`) es la versión pedagógica que se proyecta en clase: simulación paso a paso con DGP conocido (ATE = 4 para Sharp, ATE = 5 para Fuzzy), comparando OLS, regresiones locales, `rdrobust` e `ivreg2`.

---

# Parte A — RDN con DGP lineal {#parte-a-rdd -}

## El proceso generador de datos {-}

Construimos un DGP donde sabemos la respuesta correcta. Tomamos como ejemplo la edad de pensión $c = 62$:

$$Z_i \sim \text{Uniform}(50, 74), \qquad D_i = \mathbb{1}[Z_i \geq 62]$$
$$y_i = 1 + 0.10 (Z_i - 62) + 2.0 \, D_i + \varepsilon_i, \quad \varepsilon_i \sim \mathcal{N}(0, 1)$$

::: {.boxinfo}
**Diagnóstico del DGP**

- **Efecto causal verdadero del salto:** $\tau_{RDN} = 2.0$
- **Pendiente común** $0.10$ a ambos lados (caso 1 de la derivación)
- $Z$ es continua; cumplimiento perfecto (RDN)
:::

Aplicamos la **receta**: paso 0 = centrar $\tilde Z = Z - c$; paso 1 = (todavía no recortamos, primero veamos la muestra completa); paso 2 = $D$ ya está definido; paso 3 = decidir el modelo.

```stata
clear
set seed 20260511
set obs 2000
gen Z    = runiform()*24 + 50           // edad entre 50 y 74
gen D    = (Z >= 62)
gen Zt   = Z - 62                        // Z tilde: Z centrada en el umbral
gen y    = 1 + 0.10*Zt + 2.0*D + rnormal()
```

---

## Visualización primero {-}

Antes de cualquier número, el gráfico:

```stata
rdplot y Z, c(62) graph_options(title("RDN — DGP lineal") ///
    xtitle("Edad (Z)") ytitle("Resultado (y)"))
```

`rdplot` divide $Z$ en bins óptimos (IMSE-optimal por defecto) y ajusta un polinomio a cada lado. Debería verse un salto de aproximadamente 2 unidades en $Z = 62$.

---

## Estimación I — Polinomio global con interacciones {-}

### Caso 1: lineal con misma pendiente {-}

$$E[Y \mid \tilde Z, D] = \alpha + \beta \tilde Z + \tau D$$

```stata
reg y D Zt
```

| Coeficiente | Lectura | Valor esperado |
|---|---|---|
| `_b[_cons]` | $E[Y(D=0) \mid Z = 62]$ | $\approx 1.0$ |
| `_b[D]` | **salto $\tau_{RDN}$** | $\approx 2.0$ |
| `_b[Zt]` | Pendiente común | $\approx 0.10$ |

### Caso 2: lineal con pendientes distintas {-}

$$E[Y \mid \tilde Z, D] = \alpha + \tau D + \beta_0 \tilde Z + \beta_1 (D \cdot \tilde Z)$$

```stata
gen DZt = D*Zt
reg y D Zt DZt
```

| Coeficiente | Valor esperado en este DGP |
|---|---|
| `_b[D]` | $\approx 2.0$ |
| `_b[Zt]` | $\approx 0.10$ (pendiente a la izquierda) |
| `_b[DZt]` | $\approx 0$ (no hay diferencia de pendientes en este DGP) |

### Caso 3: cuadrático con interacciones {-}

$$E[Y \mid \tilde Z, D] = \alpha + \tau D + \beta_0 \tilde Z + \beta_1 (D \cdot \tilde Z) + \beta_2 \tilde Z^2 + \beta_3 (D \cdot \tilde Z^2)$$

```stata
gen Zt2  = Zt^2
gen DZt2 = D*Zt2
reg y D Zt DZt Zt2 DZt2
```

Con el DGP lineal, los términos cuadráticos no agregan información (`_b[Zt2]` y `_b[DZt2]` cercanos a 0) y `_b[D]` sigue siendo $\approx 2.0$. Esto es **buena señal**: el polinomio cuadrático no introduce sesgo cuando el modelo verdadero es lineal — solo gasta grados de libertad.

::: {.boxinfo}
**Por qué hay que centrar $Z$:** si corren `reg y D Z DZ` sin centrar, `_b[D]` da el salto en $Z=0$ (edad 0), que ni siquiera es un punto del soporte. Centrando $Z$ en $c$, `_b[D]` se lee directamente como el salto en el umbral.
:::

---

## Estimación II — Regresión local lineal con kernel {-}

```stata
rdrobust y Z, c(62) p(1) kernel(triangular)
```

| Output | Lectura |
|---|---|
| `Coef.` (Conventional) | Estimador puntual con ancho de banda MSE-óptimo |
| `Coef.` (Bias-Corrected) | Mismo punto pero con corrección de sesgo |
| `Coef.` (Robust) | Igual al bias-corrected |
| `[95% C.I.]` (Robust) | **Este es el CI que se reporta en el paper** |
| `BW est. (h)` | Ancho de banda seleccionado (MSE-óptimo) |
| `Number of obs` | Tamaño efectivo dentro de la ventana |

Con el DGP lineal y $N = 2{,}000$, el coeficiente debe estar muy cerca de 2.0 con CI estrecho.

---

## Comparación rápida {-}

| Especificación | Comando | $\hat\tau_{RDN}$ esperado |
|---|---|---|
| Caso 1 — lineal misma pendiente | `reg y D Zt` | $\approx 2.00$ |
| Caso 2 — lineal pendientes distintas | `reg y D Zt DZt` | $\approx 2.00$ |
| Caso 3 — cuadrático con interacciones | `reg y D Zt DZt Zt2 DZt2` | $\approx 2.00$ |
| Local lineal MSE-óptimo | `rdrobust y Z, c(62)` | $\approx 2.00$ |

Con un DGP lineal y bien comportado, **los cuatro coinciden**. Las diferencias aparecen cuando la relación es curva (Parte B).

---

# Parte B — RDN con DGP curvo {#parte-b-rdd -}

## El problema con polinomios globales {-}

Cambiamos el DGP a algo más realista: una relación cóncava que se aplana lejos del umbral.

$$y_i = 1 + 0.5 \tilde Z_i - 0.02 \tilde Z_i^2 + 2.0 \, D_i + \varepsilon_i$$

donde $\tilde Z = Z - 62$.

```stata
clear
set seed 20260511
set obs 2000
gen Z   = runiform()*24 + 50
gen D   = (Z >= 62)
gen Zt  = Z - 62
gen Zt2 = Zt^2
gen y   = 1 + 0.5*Zt - 0.02*Zt2 + 2.0*D + rnormal()
```

El salto verdadero sigue siendo $\tau_{RDN} = 2$.

---

## El polinomio lineal global se equivoca {-}

```stata
gen DZt = D*Zt
reg y D Zt DZt
```

Con un DGP cuadrático, el polinomio lineal **no puede captar la curvatura** lejos del umbral y devuelve un coeficiente sesgado para $\tau$. En este DGP típicamente da $\hat\tau \approx 2.5$ a $3.0$ (sesgo grande).

---

## El polinomio cuadrático global lo arregla — pero es frágil {-}

```stata
gen DZt2 = D*Zt2
reg y D Zt DZt Zt2 DZt2
```

Con el polinomio cuadrático, $\hat\tau$ vuelve a $\approx 2.0$. **Pero esto requiere conocer el DGP**. En la práctica, no lo conocemos.

::: {.boxwarning}
**Lección Gelman & Imbens (2019):** si subimos a cúbico o cuártico para "estar seguros", el polinomio empieza a ajustarse a ruido y observaciones lejanas — y $\hat\tau$ se vuelve **muy** sensible al orden. Mejor no jugar con polinomios de orden alto.
:::

---

## Local lineal sigue funcionando {-}

```stata
rdrobust y Z, c(62) p(1) kernel(triangular)
```

Aun con el DGP curvo, la regresión local lineal con ancho de banda MSE-óptimo se acerca a 2.0, **sin que tengamos que especificar la curvatura**. Esa es su gran ventaja: solo confiamos en que la relación es **localmente lineal** cerca del umbral, que es un supuesto mucho más débil que "globalmente lineal".

---

# Parte C — Sensibilidad al ancho de banda {#parte-c-rdd -}

## Barrido manual {-}

Para visualizar el trade-off sesgo–varianza, fijamos varios valores de $h$ y comparamos:

```stata
foreach h in 1 2 3 5 8 12 {
    qui rdrobust y Z, c(62) h(`h') p(1) kernel(triangular)
    di "h = `h'   coef = " %5.3f e(tau_cl) "   se = " %5.3f e(se_tau_cl)
}
```

| $h$ | Sesgo esperado | SE esperado |
|---:|:---:|:---:|
| 1 | bajo | alto |
| 2 | bajo | medio |
| 3 | bajo | medio-bajo |
| 5 | medio | bajo |
| 8 | mayor | menor |
| 12 | grande | el más bajo |

A medida que $h$ crece, el SE baja (más datos) pero el sesgo crece. El ancho de banda óptimo equilibra los dos.

---

## Ancho de banda automático CCT {-}

```stata
rdrobust y Z, c(62) bwselect(mserd)     // MSE-optimal
rdrobust y Z, c(62) bwselect(cerrd)     // Coverage-error-rate-optimal
```

`mserd` es el default — minimiza el MSE asintótico del estimador. `cerrd` busca mejor cobertura del CI a costa de algo de sesgo. En este DGP los dos están cerca, pero `cerrd` típicamente elige $h$ ligeramente menor.

---

# Parte D — Pruebas de robustez {#parte-d-rdd -}

Las 4 pruebas obligatorias, en orden:

## 1. Sensibilidad {-}

Ya cubierta en Parte C (barrido manual de $h$). Adicionalmente, comparen el resultado con distintos órdenes de polinomio:

```stata
* Recargamos el DGP base
clear
set seed 20260511
set obs 2000
gen Z  = runiform()*24 + 50
gen D  = (Z >= 62)
gen Zt = Z - 62
gen y  = 1 + 0.10*Zt + 2.0*D + rnormal()

di _newline "*** Sensibilidad al orden del polinomio ***"
rdrobust y Z, c(62) p(1)
rdrobust y Z, c(62) p(2)
```

## 2. Balance de covariables {-}

¿Alguna covariable predeterminada salta en el umbral? Generamos dos covariables — una balanceada y otra contaminada por el tratamiento — para verlo:

```stata
clear
set seed 20260511
set obs 2000
gen Z     = runiform()*24 + 50
gen D     = (Z >= 62)
gen Zt    = Z - 62
gen sexo  = runiform() < 0.5            // covariable balanceada (predeterminada)
gen ingre = 100 + 5*D + rnormal()*10    // contaminada (depende de D)
gen y     = 1 + 0.10*Zt + 2.0*D + rnormal()
```

Probamos cada covariable como "resultado" en el RD:

```stata
rdrobust sexo Z, c(62)         // debe dar coef ≈ 0, no significativo
rdrobust ingre Z, c(62)        // saltará +5: NO es covariable válida
```

`sexo` no debe saltar — bien. `ingre` salta porque depende de $D$ — **mal**: no puede usarse como control.

## 3. Placebo {-}

Estimar el "salto" en umbrales **falsos**:

```stata
foreach c_fake in 58 60 64 66 {
    qui rdrobust y Z, c(`c_fake')
    di "Umbral placebo `c_fake'   coef = " %5.3f e(tau_cl) ///
       "   robust p = " %5.3f e(pv_rb)
}
```

Ninguno de los umbrales falsos debe arrojar un efecto significativo. Si alguno lo hace, sospechar que la forma funcional está mal especificada.

## 4. Sorting (test de McCrary 2008) {-}

¿Hay manipulación de la variable de asignación? Test con `rddensity`:

```stata
rddensity Z, c(62)
```

Con nuestro DGP (sin manipulación), debe **no rechazar** ($p > 0.10$). Para verlo en un caso patológico, manipulemos artificialmente:

```stata
preserve
    replace Z = 62.5 if Z >= 61.5 & Z < 62 & runiform() < 0.5
    rddensity Z, c(62)
restore
```

Ahora la mitad de los individuos en $[61.5, 62)$ "saltan" a $62.5$ — `rddensity` debería rechazar fuerte (p < 0.01).

---

# Parte E — RDB {#parte-e-rdd -}

## DGP con cumplimiento imperfecto {-}

Cruzar el umbral aumenta la probabilidad de tratarse del 20% al 80% — no del 0% al 100%. El **instrumento** es $W_i = \mathbb{1}[Z_i \geq c]$.

$$W_i = \mathbb{1}[Z_i \geq 62]$$
$$P(D_i = 1 \mid Z_i, W_i) = 0.20 + 0.60 \cdot W_i$$
$$y_i = 1 + 0.10 \tilde Z_i + 2.0 \, D_i + \varepsilon_i$$

```stata
clear
set seed 20260511
set obs 5000
gen Z  = runiform()*24 + 50
gen W  = (Z >= 62)
gen Zt = Z - 62
gen p  = 0.20 + 0.60*W
gen D  = runiform() < p
gen y  = 1 + 0.10*Zt + 2.0*D + rnormal()
```

El **efecto causal verdadero del tratamiento** es $\tau = 2$. El **salto en el resultado** en $c$ es $2.0 \times (0.80 - 0.20) = 1.2$ (numerador). El **salto en la toma del tratamiento** es $0.60$ (denominador). El cociente Wald local es $1.2 / 0.60 = 2.0$ — eso es lo que RDB debe recuperar.

---

## Estimación: rdrobust con fuzzy {-}

```stata
rdrobust y Z, c(62) fuzzy(D)
```

Output principal: `Coef.` (Robust) $\approx 2.0$.

---

## Equivalencia con 2SLS local {-}

Aplicando la receta de RDB paso a paso:

```stata
local h = 5
gen WZt = W*Zt
ivregress 2sls y Zt WZt (D = W) if abs(Zt) < `h', vce(robust)
```

El coeficiente de `D` debe estar muy cerca de `rdrobust ... fuzzy(D)`. La diferencia: `rdrobust` usa kernel triangular y ancho de banda óptimo + corrección de sesgo; el 2SLS manual usa kernel uniforme con $h = 5$ fijado.

---

# Parte F — Variable de asignación discreta {#parte-f-rdd -}

## DGP {-}

$Z$ ahora toma solo valores enteros (puntaje de un examen redondeado), con umbral $c = 50$.

```stata
clear
set seed 20260511
set obs 3000
gen Z  = floor(runiform()*101)          // 0 a 100, enteros
gen D  = (Z >= 50)
gen Zt = Z - 50
gen y  = 1 + 0.05*Zt + 2.0*D + rnormal()
tab Z if Z > 45 & Z < 55                // confirmar discreto
```

---

## El problema de los SE convencionales {-}

```stata
rdrobust y Z, c(50)
```

Reporta un SE — pero como advirtieron Kolesár & Rothe (2018), **subcubre**. La cobertura nominal del 95% es en realidad <90% en simulaciones con pocos valores distintos de $Z$.

---

## Solución pragmática: cluster en Z (Lee & Card 2008) {-}

```stata
rdrobust y Z, c(50) cluster(Z)
```

El SE se **infla** — es más conservador, refleja mejor la incertidumbre real cuando $Z$ tiene soporte discreto. Esta es la solución mínima que se debe reportar.

---

## Solución completa: honest CIs {-}

`RDHonest` (R) implementa los CIs de Kolesár & Rothe que cubren uniformemente sobre una clase de funciones con segunda derivada acotada. No hay versión Stata oficial; la práctica es:

- Reportar `rdrobust` con `cluster(Z)` como principal.
- En el apéndice de robustez, reportar `RDHonest` desde R y mostrar que las conclusiones son las mismas (o no).

---

# Síntesis: la receta moderna {#sintesis-rdd -}

::: {.boxinfo}
**Cuando enfrenten un paper o tarea con RD:**

1. **Gráfico primero** (`rdplot`). Si no se ve un salto a ojo, no hay RD.
2. **Receta paso a paso:** centrar $\tilde Z = Z - c$, decidir ancho de banda $h$, elegir modelo para $E[Y \mid \tilde Z, D]$.
3. **Especificación principal:** `rdrobust y Z, c(c)` — local lineal, kernel triangular, ancho de banda MSE-óptimo, CI robust.
4. **Sensibilidad:** mostrar $\hat\tau$ con $h = h_{MSE}/2$, $h_{MSE}$, $2 h_{MSE}$; con polinomios lineal y cuadrático.
5. **Balance:** covariables predeterminadas no deben saltar en $c$.
6. **Placebo:** umbrales falsos no deben dar efectos.
7. **Sorting:** `rddensity` no debe rechazar.
8. **Si $Z$ es discreta:** `cluster(Z)` siempre; honest CIs si hay tiempo.
9. **RDB:** reportar primera etapa (salto en $D$) y forma reducida (salto en $y$) además del ratio.
10. **Interpretación:** efecto **local en el umbral**, LATE de cooperadores si es RDB. No extrapolar a poblaciones lejanas de $c$.
:::

---

## Lecturas complementarias para la práctica {-}

- **Cattaneo, Idrobo & Titiunik (2020)** — *A Practical Introduction to Regression Discontinuity Designs*: el manual moderno con código `rdrobust`/`rddensity` paso a paso.
- **Sitio del software:** [rdpackages.github.io](https://rdpackages.github.io) — papers, replicaciones y documentación oficial de la familia `rdrobust`, `rddensity`, `rdmulti`, `rdpower`, `rdlocrand`.
- **Replicación recomendada:** Lee (2008) en `rdrobust` — el ejemplo canónico con datos electorales de la Cámara de Representantes de EE.UU.
