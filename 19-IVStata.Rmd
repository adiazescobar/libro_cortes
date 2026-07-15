# IV y LATE en Stata: simulación {#iv-stata}

::: {.boxinfo}
**Metas de aprendizaje**

- Verificar empíricamente que IV es **sesgado pero consistente** y que OLS es **inconsistente**
- Visualizar el sesgo de muestra finita del IV bajo **instrumentos débiles** (Bound, Jaeger & Baker, 1995)
- Construir un experimento controlado donde always-takers, never-takers y compliers tienen **efectos heterogéneos** del tratamiento
- Comprobar que el estimador IV recupera el **LATE de los compliers** y no el ATE poblacional
:::

---

## Instalación previa {-}

```stata
ssc install ivreg2
ssc install ranktest
```

**Descargar archivos ejecutables:**

[Descargar do-file Stata (IV_LATE_simulacion.do)](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/18_IV_LATE/IV_LATE_simulacion.do)

[Descargar versión Python (IV_LATE_simulacion.py)](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/18_IV_LATE/IV_LATE_simulacion.py)

[Descargar versión R (IV_LATE_simulacion.R)](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/18_IV_LATE/IV_LATE_simulacion.R)

Los tres archivos contienen el mismo experimento (Parte A: muestras finitas; Parte B: LATE paso a paso) y producen los mismos resultados — usen el lenguaje de su preferencia.

---

# Parte A — Muestras finitas: OLS sesgado vs IV consistente {-}

## El proceso generador de datos {-}

Diseñamos un DGP donde sabemos la respuesta correcta y podemos comparar:

$$z, w, e_D, u \;\sim\; \text{iid }\mathcal{N}(0,1)$$
$$D = 0.2\, z + e_D + w \qquad y = 0.5\, D + w + u$$

::: {.boxinfo}
**Diagnóstico del DGP**

- **Efecto causal verdadero:** $\tau = 0.5$
- **Confounder no observado:** $w$ entra tanto en $D$ como en $y$ → OLS es inconsistente.
- **Sesgo OLS:** $\dfrac{\text{Cov}(D, w)}{\text{Var}(D)} \approx \dfrac{1}{0.04 + 1 + 1} \approx 0.49$, así que $\text{plim}(\hat{\tau}_{OLS}) \approx 0.99$.
- **Instrumento débil:** $z$ explica solo el ~2% de la varianza de $D$ ($\pi = 0.2$). El estadístico $F$ de la primera etapa será chico en muestras pequeñas → IV severamente sesgado en muestra finita.
- **Consistencia del IV:** $\text{plim}(\hat{\tau}_{IV}) = 0.5$ cuando $n \to \infty$.
:::

```stata
clear
set seed 20260506
set obs 1000
gen z  = rnormal()
gen w  = rnormal()
gen eD = rnormal()
gen u  = rnormal()
gen D  = 0.2*z + eD + w
gen y  = 0.5*D + w + u

reg y D                  // OLS — coeficiente sesgado hacia 1
ivreg2 y (D = z), first  // IV — F de primera etapa pequeño
```

---

## Monte Carlo: el programa {-}

Definimos un programa que genera una muestra del DGP, estima OLS y IV, y devuelve los coeficientes y el $F$ de la primera etapa:

```stata
cap program drop monteiv
program monteiv, rclass
    syntax, n(integer)
    drop _all
    set obs `n'
    gen z  = rnormal()
    gen w  = rnormal()
    gen eD = rnormal()
    gen u  = rnormal()
    gen D  = 0.2*z + eD + w
    gen y  = 0.5*D + w + u
    qui reg y D
    return scalar b_ols = _b[D]
    qui ivreg2 y (D = z)
    return scalar b_iv  = _b[D]
    qui reg D z
    return scalar F1    = e(F)
end
```

Para cada tamaño muestral $N \in \{30, 100, 300, 1000, 10000\}$ corremos 2,000 réplicas y resumimos los resultados:

```stata
forvalues k = 1/5 {
    local N : word `k' of 30 100 300 1000 10000
    simulate b_ols=r(b_ols) b_iv=r(b_iv) F1=r(F1), ///
        reps(2000) seed(`k'): monteiv, n(`N')
    di "==== N = `N' ===="
    tabstat b_ols b_iv F1, stat(mean p50 p25 p75) col(stat)
}
```

---

## Resultados esperados {-}

| $N$ | mediana OLS | mediana IV | IV p25 | IV p75 | $\bar F$ primera etapa |
|----:|:-----------:|:----------:|:------:|:------:|:----------------------:|
| 30      | **0.99** | **0.80** | 0.06 | 1.46 | **1.7** |
| 100     | 0.99 | 0.61 | 0.07 | 1.02 | 3.1 |
| 300     | 0.99 | 0.50 | 0.17 | 0.75 | 7.0 |
| 1,000   | 0.99 | 0.50 | 0.34 | 0.64 | 21.1 |
| 10,000  | **0.99** | **0.50** | 0.45 | 0.55 | **201** |

**Lo que se ve:**

1. **OLS está clavado en 0.99** sin importar $N$ — es **inconsistente**. Más datos no arreglan el sesgo de selección.
2. En $N = 30$ el IV tiene **mediana 0.80** (sesgo de +0.30 hacia OLS) y un IQR enorme `[0.06, 1.46]`. Algunas réplicas dan estimaciones absurdas — incluso negativas o $> 2$. El $F$ de la primera etapa es 1.7, muy por debajo del umbral de Stock-Yogo: **régimen de instrumento débil**.
3. A medida que $N$ crece, el $F$ aumenta y el IV converge a 0.5: **consistencia visible**.
4. La **media** del IV en muestras pequeñas es inestable y a veces engañosa porque la distribución del IV con instrumento débil tiene **colas pesadas** (no tiene momentos finitos en el límite). Por eso reportamos **mediana e IQR** en vez de media y desviación estándar.

::: {.boxinfo}
**Mensaje pedagógico:** "IV es consistente" es una propiedad asintótica. En muestras pequeñas con instrumento débil, IV puede estar **igual de sesgado que OLS, o peor**. La cura puede ser peor que la enfermedad.
:::

---

## Visualización {-}

Un histograma comparativo en $N = 10000$ deja la consistencia evidente:

```stata
twoway (hist b_ols, color(red%40) width(0.005)) ///
       (hist b_iv,  color(blue%40) width(0.005)), ///
       xline(0.5,  lcolor(black) lpattern(dash)) ///
       xline(0.99, lcolor(red)   lpattern(dash)) ///
       legend(order(1 "OLS" 2 "IV") position(2)) ///
       title("Distribución de los estimadores, N=10,000") ///
       xtitle("Estimador")
```

El histograma azul (IV) está concentrado alrededor de 0.5 (línea negra punteada, valor verdadero); el histograma rojo (OLS) está concentrado alrededor de 0.99 (línea roja punteada, plim del OLS). Para $N = 30$, el histograma azul es ancho y se solapa con el rojo: en muestra pequeña, es difícil distinguir el IV del OLS.

---

# Parte B — LATE paso a paso {-}

## La idea {-}

Vamos a construir una población artificial con tres tipos de individuos y **efectos heterogéneos** del tratamiento, para verificar que el IV identifica el LATE de los compliers — no el ATE poblacional.

| Tipo | $D_i(0)$ | $D_i(1)$ | LATE individual | Fracción |
|------|----------|----------|-----------------|----------|
| Never-takers   | 0 | 0 | $-0.5$ | 25% |
| Always-takers  | 1 | 1 | $0$    | 25% |
| Compliers      | 0 | 1 | $+1.0$ | 50% |
| Defiers        | — | — | (excluidos por monotonicidad) | 0% |

**ATE poblacional:** $0.25 \cdot (-0.5) + 0.25 \cdot 0 + 0.50 \cdot 1.0 = 0.375$

**LATE de compliers:** $1.0$ ← **es lo que IV debe recuperar**

---

## Construcción del experimento {-}

```stata
clear
set seed 54687
set obs 20000

* Instrumento aleatorio: 50% recibe Z=1, 50% recibe Z=0
gen Z = uniform() > 0.5
tab Z

* Tipos: 5,000 never, 5,000 always, 10,000 compliers
gen d00 = (_n <= 5000)                       // never-takers
gen d11 = (_n >  5000 & _n <= 10000)         // always-takers
gen d01 = (_n > 10000)                       // compliers
```

Inspeccionamos las construcciones:

```stata
tab d00
tab d11
tab d01
```

---

## Asignación de los efectos heterogéneos {-}

```stata
gen late = -0.5 if d00 == 1
replace late = 0  if d11 == 1
replace late = 1  if d01 == 1
tab late
```

| LATE individual | Frecuencia | % |
|-----:|-----:|---:|
| -0.5 | 5,000 | 25.00 |
|  0   | 5,000 | 25.00 |
|  1   | 10,000 | 50.00 |

---

## Resultados potenciales y resultado observado {-}

```stata
* Resultado potencial sin tratamiento: pequeño ruido base
gen y0 = 0.25 * invnormal(uniform())
gen y1 = y0 + late                          // Y(D=1) = Y(D=0) + efecto individual

* Tratamiento observado (regla de monotonicidad):
*   never-takers:  D = 0 siempre
*   always-takers: D = 1 siempre
*   compliers:     D = Z
gen D = d11 + Z*d01
tab D

* Resultado observado
gen y = D*y1 + (1-D)*y0
```

---

## Estimación: lo que cada método recupera {-}

### OLS — no recupera nada {-}

```stata
reg y D
```

OLS no recupera ni el ATE (0.375) ni el LATE (1.0). Está sesgado por la selección no aleatoria en el tratamiento (los always-takers tienen $D = 1$ por razones distintas a las del instrumento).

### IV — recupera el LATE de compliers {-}

```stata
ivreg2 y (D = Z)
```

El coeficiente del IV debe estar muy cerca de **1.0** — el LATE de los compliers, **no** el ATE poblacional (0.375). Los always-takers y never-takers no contribuyen al estimador porque el instrumento no los mueve.

### Wald manual {-}

```stata
sum y if Z==1
local EyZ1 = r(mean)
sum y if Z==0
local EyZ0 = r(mean)
sum D if Z==1
local EDZ1 = r(mean)
sum D if Z==0
local EDZ0 = r(mean)
di "Wald = " (`EyZ1' - `EyZ0') / (`EDZ1' - `EDZ0')
```

El estimador de Wald y el IV coinciden por construcción cuando $Z$ y $D$ son binarios.

---

## ¿Y si el efecto fuera homogéneo? {-}

Si todos los individuos tuvieran el mismo efecto $\tau$, entonces:

$$\text{LATE} = \text{ATE} = \tau$$

y la distinción se vuelve irrelevante. La importancia del LATE aparece solo cuando hay **heterogeneidad** en los efectos. En la práctica casi siempre la hay, así que la lectura correcta de un coeficiente de IV es: *"el efecto promedio en el subgrupo de individuos cuya decisión cambia con el instrumento"*.

---

## LATE con varios instrumentos {-}

Si tuviéramos dos instrumentos $Z_1$ y $Z_2$ que mueven a poblaciones distintas (por ejemplo, $Z_1 = 1$ si nació entre mayo y agosto; $Z_2 = 1$ si nació entre septiembre y abril), cada uno identifica un LATE distinto:

$$\rho_j = \frac{\text{Cov}(Y, Z_j)}{\text{Cov}(D, Z_j)}, \qquad j = 1, 2$$

MCO2E con ambos instrumentos en simultáneo recupera un **promedio ponderado**:

$$\hat{\rho}_{MCO2E} = \psi \rho_1 + (1 - \psi) \rho_2$$

donde el peso $\psi$ depende de la fuerza relativa de cada instrumento en la primera etapa. Es decir, MCO2E con varios instrumentos no estima un objeto único — estima una mezcla de LATEs cuya interpretación depende de qué instrumentos haya en el modelo.

---

## Resumen {-}

| Resultado | OLS | IV | Verdad |
|-----------|-----|----|----|
| Parte A (efecto homogéneo τ=0.5) | 0.99 (inconsistente) | 0.50 si $N$ grande | 0.50 |
| Parte B (efectos heterogéneos) | 0.50 (no es ATE ni LATE) | 1.00 | LATE = 1.00; ATE = 0.375 |

**Dos lecciones clave:**

1. **OLS no se arregla con más datos.** Si hay endogeneidad, OLS es inconsistente para cualquier tamaño muestral.
2. **IV no es magia.** Es consistente solo si el instrumento es válido y suficientemente fuerte. Y en presencia de heterogeneidad, identifica un parámetro distinto al ATE: el LATE de los compliers, una subpoblación que depende del instrumento elegido.

---

## Materiales para la clase {-}

Los archivos ejecutables están en `dofile/18_IV_LATE/`:

- **IV_LATE_simulacion.do** — Stata: programa Monte Carlo + simulación LATE
- **IV_LATE_simulacion.py** — Python (numpy/pandas/statsmodels)
- **IV_LATE_simulacion.R** — R (paquete `AER` para `ivreg`)

Ejecución directa en Stata:

```stata
do "IV_LATE_simulacion.do"
```

**Descargas directas:**

[Descargar Stata](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/18_IV_LATE/IV_LATE_simulacion.do)

[Descargar Python](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/18_IV_LATE/IV_LATE_simulacion.py)

[Descargar R](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/18_IV_LATE/IV_LATE_simulacion.R)

---

## Lecturas recomendadas {-}

- **Bound, Jaeger & Baker (1995)** — "Problems with instrumental variables estimation when the correlation between the instruments and the endogenous explanatory variable is weak", *JASA* — el paper canónico sobre instrumentos débiles.
- **Imbens & Angrist (1994)** — la demostración original Wald = LATE.
- **Stock & Yogo (2005)** — valores críticos para el test de instrumentos débiles.
- **Angrist & Pischke (2009)** — *Mostly Harmless Econometrics*, capítulo 4.
