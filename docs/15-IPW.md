# Ponderación por Probabilidad Inversa (IPW) {#ipw}

## La idea: combinar MCO y PSM {-}

Hirano & Imbens proponen un estimador que usa una versión flexible del propensity score como ponderador en una regresión MCO. La intuición es simple: en lugar de descartar observaciones (como en el matching), **re-ponderamos** cada observación para que la muestra ponderada se comporte como si el tratamiento fuera aleatorio.

El objetivo es estimar:

$$\tau = E[Y_i(1) - Y_i(0)]$$

Bajo la **CIA** ($\{Y_i(1), Y_i(0)\} \perp D_i \mid X_i$) y el **soporte común** ($0 < P(X) < 1$), podemos identificar este efecto usando los ponderadores del propensity score.

---

## El estimador IPW {-}

### Identificación {-}

Bajo CIA y soporte común se puede demostrar que:

$$E\left[\frac{Y \cdot D}{P(X)}\right] = E[Y(1)]$$

$$E\left[\frac{Y \cdot (1-D)}{1-P(X)}\right] = E[Y(0)]$$

**Demostración (para el primer resultado):**

$$E\left[\frac{Y \cdot D}{P(X)} \,\bigg|\, X=x\right] = E\left[\frac{Y(1)}{P(X)} \cdot \mathbf{1}(D=1) \,\bigg|\, D=1,\, X=x\right] \cdot P(X)$$

$$= \frac{E[Y(1) \mid X=x] \cdot P(X)}{P(X)} = E[Y(1) \mid X=x]$$

Integrando sobre $X$: $E\left[\frac{Y \cdot D}{P(X)}\right] = E[Y(1)]$.

El mismo argumento da $E\left[\frac{Y(1-D)}{1-P(X)}\right] = E[Y(0)]$.

Por tanto:

$$ATE = E\left[\frac{Y \cdot D}{P(X)}\right] - E\left[\frac{Y \cdot (1-D)}{1-P(X)}\right]$$

### El estimador de Horvitz-Thompson {-}

El estimador de muestra finita es:

$$\hat{\tau} = \frac{\sum_{i=1}^{N} \frac{D_i Y_i}{\hat{p}(X_i)}}{\sum_{i=1}^{N} \frac{D_i}{\hat{p}(X_i)}} - \frac{\sum_{i=1}^{N} \frac{(1-D_i) Y_i}{1-\hat{p}(X_i)}}{\sum_{i=1}^{N} \frac{(1-D_i)}{1-\hat{p}(X_i)}}$$

Los denominadores normalizan los pesos para que sumen 1 dentro de cada grupo, lo que mejora el comportamiento en muestras finitas.

---

## Los ponderadores IPW {-}

### Ponderador para el ATE {-}

Para estimar el ATE, el ponderador de cada observación es:

$$W(D_i, X_i) = \frac{D_i}{\hat{p}(X_i)} + \frac{1-D_i}{1-\hat{p}(X_i)}$$

- Para los **tratados** ($D_i=1$): peso $= 1/\hat{p}(X_i)$ — las observaciones tratadas con PS bajo (poco probable que fueran tratadas) reciben mayor peso
- Para los **controles** ($D_i=0$): peso $= 1/(1-\hat{p}(X_i))$ — los controles con PS alto reciben mayor peso

Intuitivamente: el ponderador "corrige" la sobre/sub-representación de cada observación en la muestra.

### Implementación en la práctica {-}

Como $P(X)$ no se conoce, se usa el PS estimado:

$$\hat{W}(D_i, X_i) = \frac{D_i}{\hat{p}(X_i)} + \frac{1-D_i}{1-\hat{p}(X_i)}$$

La regresión MCO ponderada para estimar el ATE es:

$$Y_i = \alpha_0 + \tau D_i + \alpha_1 X_i + \alpha_2 (X_i - \bar{X}) \cdot D_i + \varepsilon_i$$

estimada con pesos $\hat{W}(D_i, X_i)$.

---

## Implementación en Stata {-}

### Opción 1: `teffects ipw` (recomendado) {-}

Stata 13+ incluye el comando `teffects ipw` que estima el PS y el ATE/ATT en un solo paso:

```stata
* ATE con IPW (probit para el PS)
teffects ipw (y2) (D $X, probit)

* ATT con IPW
teffects ipw (y2) (D $X, probit), atet

* Verificar balance de covariables
tebalance summarize
```

Los errores estándar de `teffects ipw` toman en cuenta la estimación del PS (son correctamente calculados, a diferencia de los errores "ingenuos" del paso a paso manual).

### Opción 2: IPW manual paso a paso {-}

Si se quiere más control sobre la especificación:

```stata
* Paso 1: Estimar el PS
logit D $X
predict double pscore, pr

* Paso 2: Construir los ponderadores
gen double w_ate = D/pscore + (1-D)/(1-pscore)

* Paso 3: Regresión ponderada
gen double xcent = X - r(mean)     // centrar X
reg y2 D X c.xcent#i.D [pw=w_ate]

* El coeficiente en D es el ATE estimado
```

---

## IPW vs. PSM: ¿cuál usar? {-}

| Característica | PSM | IPW |
|---------------|-----|-----|
| Usa todas las observaciones | No (descarta sin match) | Sí |
| Sensible a PS extremos | Moderado | Alto (pesos muy grandes) |
| Estimador por defecto | ATT | ATE |
| Inferencia | Bootstrap o `ai()` | Analítica (`teffects`) |
| Interpretación | Diferencia de medias emparejadas | Diferencia ponderada |

**Ventaja del IPW:** usa toda la información disponible — no hay pérdida de muestra.

**Desventaja del IPW:** con observaciones con $\hat{p}(X) \approx 0$ o $\hat{p}(X) \approx 1$, los ponderadores se vuelven muy grandes y dominan el estimador (*peso extremo*). Soluciones comunes:
- Winsorización de los pesos
- Recorte del soporte común antes de ponderar
- Usar IPW normalizado (Horvitz-Thompson normalizado)

---

## Doble robustez (*Doubly Robust*) {-}

El estimador de doble robustez (también llamado AIPW — *Augmented IPW*) combina el modelo de resultados con el modelo del PS. Es **consistente si cualquiera de los dos modelos** está correctamente especificado:

$$\hat{\tau}_{DR} = \frac{1}{N}\sum_{i=1}^{N}\left[\frac{D_i Y_i}{\hat{p}(X_i)} - \frac{D_i - \hat{p}(X_i)}{\hat{p}(X_i)}\hat{m}_1(X_i)\right] - \frac{1}{N}\sum_{i=1}^{N}\left[\frac{(1-D_i) Y_i}{1-\hat{p}(X_i)} + \frac{D_i - \hat{p}(X_i)}{1-\hat{p}(X_i)}\hat{m}_0(X_i)\right]$$

donde $\hat{m}_d(X_i) = E[Y \mid X_i, D=d]$ es el modelo de resultados estimado.

En Stata:

```stata
* AIPW (Augmented IPW) — doble robusto
teffects aipw (y2 $X) (D $X, probit)
teffects aipw (y2 $X) (D $X, probit), atet
```

---

## IPWRA: IPW con ajuste de regresión {-}

El estimador IPWRA (*Inverse Probability Weighted Regression Adjustment*) es una implementación práctica de la doble robustez. Es el estimador usado por defecto en muchos paquetes de evaluación de impacto (incluyendo el paper PruebaClaude5).

```stata
* IPWRA — ATE
teffects ipwra (y2 $X) (D $X, probit)

* IPWRA — ATT
teffects ipwra (y2 $X) (D $X, probit), atet

* Verificar balance
tebalance summarize
```

**¿Por qué IPWRA?** Combina lo mejor de la regresión (ajuste de covariables) y del IPW (ponderación por PS). Si el modelo de PS está bien especificado, los pesos aseguran que la comparación es válida. Si el modelo de resultados está bien especificado, el ajuste elimina el sesgo residual.

---

## Comparación de estimadores bajo CIA {-}

| Estimador | Comando Stata | Supuesto adicional | Doble robusto |
|-----------|--------------|-------------------|---------------|
| Matching exacto | `nnmatch` | Soporte común + suficientes controles | No |
| PSM (NN) | `psmatch2` | CIA + soporte común | No |
| IPW | `teffects ipw` | CIA + soporte común + PS correcto | No |
| AIPW | `teffects aipw` | CIA + soporte común | **Sí** |
| IPWRA | `teffects ipwra` | CIA + soporte común | **Sí** |

En la práctica, se recomienda:

1. **Estimación principal:** IPWRA o AIPW (doble robusto)
2. **Robustez:** PSM con distintos algoritmos
3. **Si hay PS extremos:** recortar soporte común y comparar

---

## Lecturas recomendadas {-}

- **Hirano, Imbens & Ridder (2003)** — "Efficient estimation of average treatment effects using the estimated propensity score", *Econometrica* — fundamento teórico del IPW
- **Robins, Rotnitzky & Zhao (1994)** — Estimadores doblemente robustos
- **Wooldridge (2010)** — *Econometric Analysis of Cross Section and Panel Data*, Cap. 21 — tratamiento accesible de IPW y doble robustez
- **StataCorp** — `help teffects` — documentación completa de los comandos `teffects ipw`, `teffects aipw`, `teffects ipwra`
