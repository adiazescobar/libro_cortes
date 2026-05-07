# Propensity Score Matching {#psm}

::: {.boxinfo}
**Metas de aprendizaje**

- Definir el propensity score y su relación con la CIA
- Verificar soporte común de forma visual y conceptual
- Comparar algoritmos de emparejamiento no paramétricos
- Interpretar balance, ATT/ATE y sensibilidad después del matching
:::

---

## Del emparejamiento exacto al propensity score {-}

El capítulo anterior mostró que el emparejamiento exacto se vuelve impracticable cuando el vector de controles $X$ tiene muchas dimensiones. Rosenbaum y Rubin (1983) demostraron un resultado fundamental que resuelve este problema:

> Si $(Y(1), Y(0)) \perp D \mid X$ (CIA), entonces también se cumple $(Y(1), Y(0)) \perp D \mid P(X)$, donde $P(X) = P(D=1 \mid X)$.

Es decir: **condicionar en el propensity score es suficiente para eliminar el sesgo de selección**, aunque hayamos colapsado todo el vector $X$ en un único escalar. En lugar de buscar un clon exacto en todas las dimensiones de $X$, buscamos individuos con la misma probabilidad estimada de haber recibido el tratamiento.

---

## El propensity score: definición y estimación {-}

El **propensity score** es la probabilidad condicional de recibir el tratamiento dado el vector de covariables observadas:

$$p(X_i) \equiv P(D_i = 1 \mid X_i)$$

### Estimación {-}

En la práctica, $p(X)$ no se conoce y debe estimarse. Los modelos estándar son:

```stata
* Logit (más común)
logit D $X
predict double pscore, pr

* Probit (alternativa)
probit D $X
predict double pscore, pr
```

La elección entre logit y probit generalmente no cambia los resultados. Lo que importa más es la **especificación**: incluir todas las variables que determinan tanto la selección ($D$) como el resultado ($Y$), posiblemente con términos cuadráticos e interacciones si la relación no es lineal en el índice.

---

## Los dos supuestos de identificación {-}

### Supuesto 1: CIA (Conditional Independence Assumption) {-}

$$\{Y_i(1), Y_i(0)\} \perp D_i \mid X_i$$

Una vez controlamos por $X$, el tratamiento es como si fuera aleatorio. Este es el supuesto más fuerte: requiere que no haya variables no observadas que afecten simultáneamente la selección y el resultado.

### Supuesto 2: Soporte común (Overlap) {-}

$$0 < P(D_i = 1 \mid X_i = x) < 1 \quad \forall x \text{ en el soporte de } X$$

Debe haber individuos tratados y no tratados para cada valor de $X$. En términos del propensity score, esto significa que la distribución de $\hat{p}(X)$ para los tratados y la distribución para los controles deben solaparse.

```stata
* Verificar soporte común visualmente
twoway (kdensity pscore if D==1, lcolor(blue)) ///
       (kdensity pscore if D==0, lcolor(red)),  ///
       legend(label(1 "Tratados") label(2 "Controles")) ///
       title("Distribución del propensity score")
```

Cuando hay poca superposición (tratados con $\hat{p} > 0.9$ sin controles equivalentes), se restringe el análisis a la región de soporte común: se descartan los tratados con propensity score fuera del rango $[\min(\hat{p}_{D=0}), \max(\hat{p}_{D=0})]$.

---

## La receta del PSM en 7 pasos {-}

Caliendo & Kopeinig (2008) proponen esta secuencia:

| Paso | Acción | En Stata |
|------|--------|----------|
| 1 | Estimar el propensity score (logit/probit) | `logit D $X` + `predict pscore, pr` |
| 2 | Verificar soporte común | `twoway kdensity` por grupo |
| 3 | Elegir el algoritmo de matching | (ver sección siguiente) |
| 4 | Emparejar y calcular ATT/ATE | `psmatch2` |
| 5 | Verificar el balance post-matching | `pstest` |
| 6 | Estimar el efecto del tratamiento | resultado del paso 4 |
| 7 | Pruebas de sensibilidad (Rosenbaum bounds) | `rbounds` |

---

## Métodos de emparejamiento no paramétricos {-}

Una vez estimado $\hat{p}(X_i)$, el siguiente paso es asignar controles a cada tratado. Hay varios algoritmos:

### Vecino más cercano (Nearest Neighbour, NN) {-}

Para cada tratado $i$, se elige el control $j^*$ que minimiza la distancia en el propensity score:

$$j^*(i) = \arg\min_{j \in \{D=0\}} |\hat{p}(X_i) - \hat{p}(X_j)|$$

**Con reemplazo**: el mismo control puede ser usado como clon de múltiples tratados. Reduce el sesgo pero puede inflar la varianza si el control es usado muchas veces.

**Sin reemplazo**: cada control se usa como máximo una vez. El resultado depende del orden en que se procesen los tratados.

**NN múltiple** ($m$-NN): se usan los $m$ controles más cercanos (promedio). Reduce la varianza a costa de mayor sesgo.

### Caliper {-}

Modifica el NN imponiendo una distancia máxima: si el control más cercano está a más de $\kappa$ unidades de distancia en el PS, el tratado se descarta (fuera del soporte común):

$$j^*(i) = \arg\min_{j: |\hat{p}(X_i) - \hat{p}(X_j)| < \kappa} |\hat{p}(X_i) - \hat{p}(X_j)|$$

La regla empírica de Rosenbaum y Rubin: $\kappa = 0.2 \times \hat{\sigma}_{p(X)}$ (20% de la desviación estándar del PS).

### Radio (*Radius*) {-}

Empareja con **todos** los controles dentro del caliper $\kappa$, no solo el más cercano. Reduce la varianza (más información) pero puede incluir controles más lejanos.

### Estratificación {-}

Divide el soporte de $\hat{p}(X)$ en $K$ intervalos y estima el ATT dentro de cada estrato. El ATT global es el promedio ponderado por la proporción de tratados en cada estrato.

### Kernel {-}

Usa una función kernel para ponderar a todos los controles en función de su distancia al tratado — los más cercanos reciben mayor peso. La estimación del contrafactual para el tratado $i$ es:

$$\hat{Y}_i(0) = \sum_{j: D_j=0} \frac{K\!\left(\frac{\hat{p}(X_i) - \hat{p}(X_j)}{h}\right)}{\sum_{k: D_k=0} K\!\left(\frac{\hat{p}(X_i) - \hat{p}(X_k)}{h}\right)} Y_j$$

donde $h$ es el ancho de banda (*bandwidth*) y $K(\cdot)$ es la función kernel. Los kernels más usados:

| Kernel | Fórmula $K(u)$ | Característica |
|--------|----------------|----------------|
| **Uniforme** | $\frac{1}{2} \cdot \mathbf{1}(|u|<1)$ | Igual peso dentro del caliper |
| **Triangular** | $(1-|u|) \cdot \mathbf{1}(|u|<1)$ | Peso lineal decreciente |
| **Epanechnikov** | $\frac{3}{4}(1-u^2) \cdot \mathbf{1}(|u|<1)$ | Óptimo asintótico |
| **Gaussiano** | $\frac{1}{\sqrt{2\pi}} e^{-u^2/2}$ | Soporte ilimitado, colas pesadas |

### ¿Cuál método elegir? {-}

No hay una respuesta única. La práctica estándar es:

1. Usar NN(1) con caliper como estimación principal
2. Reportar robustez con kernel Epanechnikov y NN(3)
3. Si los resultados son sensibles al método, investigar la causa (¿poco soporte común?)

---

## ATT vs. ATE {-}

El método de matching y la especificación del PS cambian según qué parámetro se quiere estimar:

| Parámetro | Definición | Cuándo es relevante |
|-----------|-----------|---------------------|
| **ATT** | $E[Y(1) - Y(0) \mid D=1]$ | Efecto sobre quienes *sí* participan — evaluación de programa |
| **ATE** | $E[Y(1) - Y(0)]$ | Efecto si se asignara el tratamiento *a todos* — decisión de política |

Para el **ATT**, el soporte común se define desde la perspectiva de los tratados: $[\min(\hat{p}_{D=1}), \max(\hat{p}_{D=0})]$.

Para el **ATE**, el soporte se restringe a la región donde ambas distribuciones se superponen: $[\max(\min\hat{p}_D, \min\hat{p}_C), \min(\max\hat{p}_D, \max\hat{p}_C)]$.

---

## Verificación del balance {-}

Después del matching, hay que verificar que los grupos tratado y control están **balanceados** en las covariables. El indicador estándar es la diferencia estandarizada (*standardized bias*):

$$SB_k = \frac{\bar{X}_{kT} - \bar{X}_{kC}}{\sqrt{(\hat{V}_{kT} + \hat{V}_{kC})/2}} \times 100$$

La regla de Rosenbaum & Rubin: un $|SB_k| < 20\%$ indica balance aceptable; $|SB_k| < 5\%$ es excelente.

```stata
* pstest verifica el balance antes y después del matching
pstest $X, treated(D) mweight(_weight) both
* before: diferencia antes del matching
* after:  diferencia después del matching
* %bias:  diferencia estandarizada
```

Si el balance es insuficiente, hay que re-especificar el modelo del PS (agregar interacciones, términos cuadráticos) o cambiar el algoritmo de matching.

---

## Inferencia estadística {-}

La inferencia con PSM es **más compleja** que en OLS porque el PS es estimado, no conocido. Ignorar la variabilidad en la estimación del PS produce errores estándar demasiado pequeños.

Opciones prácticas:

1. **Bootstrap** (recomendado): re-estima el PS y el efecto del tratamiento en cada muestra bootstrap
2. **Errores analíticos de `psmatch2`**: asumen PS conocido — son conservadores (sobreestiman la varianza)
3. **Inferencia basada en aleatorización** (si $N$ es pequeño)

La implementación en Stata es directa: `psmatch2` con la opción `bwidth()` para bootstrap.

---

## Lecturas recomendadas {-}

- **Rosenbaum & Rubin (1983)** — "The central role of the propensity score in observational studies for causal effects", *Biometrika* — el paper fundacional
- **Caliendo & Kopeinig (2008)** — "Some practical guidance for the implementation of propensity score matching", *Journal of Economic Surveys* — guía práctica indispensable
- **Dehejia & Wahba (1999)** — "Causal effects in nonexperimental studies: Reevaluating the evaluation of training programs", *JASA*
- **Heckman, Ichimura & Todd (1997)** — "Matching as an econometric evaluation estimator", *Review of Economic Studies*
