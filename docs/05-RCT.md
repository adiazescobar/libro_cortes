# Experimentos aleatorizados — Clase teórica

::: {.boxinfo}
**Objetivo del capítulo**

- Entender por qué la aleatorización elimina el sesgo de selección
- Formalizar la notación de resultados potenciales (ATE, CATE)
- Traducir resultados potenciales a regresiones lineales en cuatro escenarios de diseño
- Comprender cuándo y por qué incluir controles y estratos

### Lecturas{-}

- **Paper Alert:** [When Should You Adjust Standard Errors for Clustering? (NBER)](https://www.nber.org/papers/w24003)
- **Teoría:** [Lectura 4. Capítulo 4 Bernal y Peña (PDF)](https://www.dropbox.com/s/vxpgxt22pvphwx3/Capitulo%204%20Bernal%20y%20Pe%C3%B1a.pdf?dl=0)

:::

## Pregunta causal {-}

¿Cuándo la diferencia observada entre tratados y controles identifica el efecto causal promedio, y cómo debe cambiar la estimación cuando el diseño incorpora estratos o controles pretratamiento?

## Intuición y motivación {-}

Comparar resultados entre dos grupos no basta para aprender un efecto causal: quienes reciben un programa pueden ser distintos de quienes no lo reciben. La aleatorización resuelve este problema al construir un grupo de control que representa lo que habría ocurrido con los tratados en ausencia del tratamiento. El punto central del capítulo es entender formalmente por qué funciona esa comparación y cómo llevar el diseño a la regresión.

## Notación, parámetros y estimandos {-}

Antes de avanzar, fijemos la notación que usaremos en todo el módulo de experimentos:

- \( Y_i \): resultado (outcome).
- \( D_i \in \{0,1\} \): asignación aleatoria al tratamiento (1 = tratado, 0 = control).
- \( X_i \): controles **pre-tratamiento** (baseline).
- \( S_i \): estrato/bloque (vector de dummies de estratificación).

**Resultados potenciales:**

- \( Y_i(D=1) \): resultado si \( i \) recibe tratamiento.
- \( Y_i(D=0) \): resultado si \( i \) está en control.

El **problema fundamental** de la inferencia causal es que para cada individuo solo observamos uno de los dos:

\[
Y_i = D_i \cdot Y_i(D=1) + (1-D_i) \cdot Y_i(D=0)
\]

::: {.boxnote}
**Intuición — el contrafactual promedio:** aunque nunca observamos simultáneamente los dos resultados potenciales de una misma persona, no necesitamos reconstruir cada contrafactual individual para estimar un efecto promedio. El grupo de control aporta el promedio que habría observado el grupo tratado sin tratamiento, siempre que la asignación haga comparables ambos grupos en expectativa.
:::

**Efectos:**

- **ATE (Average Treatment Effect):** efecto promedio en **toda** la población.
\[
ATE = \mathbb{E}[Y_i(D=1)-Y_i(D=0)]
\]

- **ATT (Average Treatment Effect on the Treated):** efecto promedio entre quienes **reciben** tratamiento.
\[
ATT = \mathbb{E}[Y_i(D=1)-Y_i(D=0) \mid D_i=1]
\]

- **ATU (Average Treatment Effect on the Untreated):** efecto promedio entre quienes **no reciben** tratamiento.
\[
ATU = \mathbb{E}[Y_i(D=1)-Y_i(D=0) \mid D_i=0]
\]

- **CATE (efecto promedio condicional):**
\[
CATE(x) = \mathbb{E}[Y_i(D=1)-Y_i(D=0) \mid X_i=x]
\]

Nota: en general \( ATE \neq ATT \neq ATU \). Por ejemplo, si un programa de capacitación laboral beneficia más a quienes lo eligen voluntariamente (porque están más motivados), entonces \( ATT > ATU \). Sin embargo, en un **RCT con asignación aleatoria**, la independencia \( D_i \perp (Y_i(D=1), Y_i(D=0)) \) garantiza que:

\[
ATE = ATT = ATU
\]

porque los grupos de tratamiento y control son, en promedio, idénticos en todo (observable y no observable).

::: {.boxnote}
**Resultado clave — independencia en expectativa:** si \(D_i \perp (Y_i(D=1),Y_i(D=0))\), entonces
\[
\begin{aligned}
\mathbb{E}[Y_i(D=1)\mid D_i=1]&=\mathbb{E}[Y_i(D=1)\mid D_i=0]=\mathbb{E}[Y_i(D=1)],\\
\mathbb{E}[Y_i(D=0)\mid D_i=1]&=\mathbb{E}[Y_i(D=0)\mid D_i=0]=\mathbb{E}[Y_i(D=0)].
\end{aligned}
\]
La igualdad es una propiedad del mecanismo de asignación repetido, no una promesa de igualdad exacta en una realización particular del experimento.
:::

## Supuestos de identificación {-}

La igualdad entre la diferencia observada y el efecto causal requiere declarar con precisión las condiciones del diseño:

1. **Asignación aleatoria:** (D_i \perp (Y_i(D=1),Y_i(D=0))). La probabilidad de tratamiento es conocida y no depende de los resultados potenciales.
2. **SUTVA:** el tratamiento de una unidad no modifica los resultados potenciales de otra y no existen versiones ocultas del tratamiento.
3. **Medición y seguimiento comparables:** el resultado se mide de la misma forma en ambos brazos y la observación de (Y_i) no depende diferencialmente del tratamiento de una manera que vuelva selectiva la muestra analítica.
4. **Respeto del diseño:** si la asignación se realiza dentro de estratos o bloques, el análisis debe conservar esa estructura para representar correctamente las probabilidades de asignación y aprovechar la precisión del diseño.

Los dos primeros supuestos sostienen directamente la interpretación causal. Los dos últimos protegen la correspondencia entre el experimento diseñado, la muestra observada y el estimador que finalmente reportamos.

## Desarrollo teórico y demostraciones {-}

---

### De la motivación a los resultados potenciales {-}

Veamos un ejemplo concreto. Supongamos que el tratamiento es un **programa de capacitación** y el outcome es el **salario**:

- \( Y_i(D=1) \): salario de \( i \) si recibe capacitación.
- \( Y_i(D=0) \): salario de \( i \) si no recibe capacitación.

Lo que observamos es la **diferencia de medias** entre tratados y controles:

\[
\underbrace{\mathbb{E}[Y_i \mid D_i=1] - \mathbb{E}[Y_i \mid D_i=0]}_{\text{diferencia observada}}
\]

Podemos descomponer esta diferencia sumando y restando \( \mathbb{E}[Y_i(D=0) \mid D_i=1] \):

\[
\mathbb{E}[Y_i(D=1) \mid D_i=1] - \mathbb{E}[Y_i(D=0) \mid D_i=0]
\]
\[
= \underbrace{\mathbb{E}[Y_i(D=1) \mid D_i=1] - \mathbb{E}[Y_i(D=0) \mid D_i=1]}_{ATT} + \underbrace{\mathbb{E}[Y_i(D=0) \mid D_i=1] - \mathbb{E}[Y_i(D=0) \mid D_i=0]}_{\text{sesgo de selección}}
\]

**Interpretación del sesgo de selección:**

El término \( \mathbb{E}[Y_i(D=0) \mid D_i=1] - \mathbb{E}[Y_i(D=0) \mid D_i=0] \) compara el salario **sin capacitación** entre quienes eligen capacitarse y quienes no. En nuestro ejemplo:

- Las personas más motivadas eligen capacitarse (\( D_i=1 \)).
- Pero la motivación **también** aumenta el salario, incluso sin capacitación.
- Por lo tanto \( \mathbb{E}[Y_i(D=0) \mid D_i=1] > \mathbb{E}[Y_i(D=0) \mid D_i=0] \): los tratados habrían ganado más **de todas formas**.
- Esto genera un sesgo **positivo**: atribuimos a la capacitación algo que en realidad es motivación.

::: {.boxnote}
**Demostración — sesgo cero en expectativa:** considere una población finita de \(N\) unidades y una aleatorización completa con \(n_1\) tratados y \(n_0=N-n_1\) controles. La diferencia de medias es
\[
\widehat{\tau}=\frac{1}{n_1}\sum_{i=1}^{N}D_iY_i(D=1)-\frac{1}{n_0}\sum_{i=1}^{N}(1-D_i)Y_i(D=0).
\]
Como \(\mathbb{E}_{D}[D_i]=n_1/N\) y \(\mathbb{E}_{D}[1-D_i]=n_0/N\), la esperanza sobre posibles asignaciones de cada media de brazo satisface
\[
\mathbb{E}_{D}\!\left[\frac{1}{n_1}\sum_iD_iY_i(D=1)\right]=\frac{1}{N}\sum_iY_i(D=1),\qquad
\mathbb{E}_{D}\!\left[\frac{1}{n_0}\sum_i(1-D_i)Y_i(D=0)\right]=\frac{1}{N}\sum_iY_i(D=0).
\]
Por linealidad, \(\mathbb{E}_{D}[\widehat{\tau}]=N^{-1}\sum_i[Y_i(D=1)-Y_i(D=0)]=SATE\). Así, el sesgo de aleatorización de \(\widehat{\tau}\) es cero, aunque una asignación particular no produzca balance exacto.
:::

**¿Qué hace la aleatorización?**

Al asignar \( D_i \) al azar, garantizamos que los grupos sean comparables **antes** del tratamiento:

\[
D_i \perp (Y_i(D=1), Y_i(D=0)) \implies \mathbb{E}[Y_i(D=0) \mid D_i=1] = \mathbb{E}[Y_i(D=0) \mid D_i=0]
\]

El sesgo de selección se hace **cero**, y la diferencia observada identifica directamente el efecto causal:

\[
\mathbb{E}[Y_i \mid D_i=1] - \mathbb{E}[Y_i \mid D_i=0] = ATT = ATE
\]

<img src="05-RCT_files/figure-html/unnamed-chunk-1-1.png" width="672" />

## ¿En regresión lineal cómo se ve esto? {-}

Lo que acabamos de ver con resultados potenciales (la descomposición ATT + sesgo de selección) tiene un equivalente directo en el lenguaje de regresión: el **sesgo por variable omitida**. Veamos cómo se conectan.

Tenemos el siguiente modelo de regresión lineal:

\[
Y = \alpha + \tau D + \varepsilon
\]

Donde:

- \( Y \) es el resultado (por ejemplo, salario),
- \( D \) es una variable binaria de tratamiento,
- \( \varepsilon \) incluye **motivación**, que no observamos.


Supongamos que la verdadera relación es:

\[
Y = \alpha + \tau D + \gamma M + u
\]

Donde:

- \( M \) es motivación (no observable),
- \( u \) es un nuevo error sin correlación con \( D \),
- Pero **no incluimos** \( M \) en la estimación → queda absorbido en \( \varepsilon = \gamma M + u \)

El modelo estimado es:

\[
Y = \alpha + \tau D + \varepsilon \quad \text{con} \quad \varepsilon = \gamma M + u
\]


Recordemos que el estimador de mínimos cuadrados ordinarios es:

\[
\hat{\beta} = (X'X)^{-1} X'Y
\]

Con \( X = [\mathbf{1}, D] \), tenemos:

\[
\hat{\beta} =
\begin{bmatrix}
\hat{\alpha} \\
\hat{\tau}
\end{bmatrix}
=
\left( \begin{bmatrix}
1 & D_1 \\
\vdots & \vdots \\
1 & D_n \\
\end{bmatrix}'
\begin{bmatrix}
1 & D_1 \\
\vdots & \vdots \\
1 & D_n \\
\end{bmatrix} \right)^{-1}
\begin{bmatrix}
1 & D_1 \\
\vdots & \vdots \\
1 & D_n \\
\end{bmatrix}' Y
\]

Queremos analizar el **sesgo** en \(\hat{\tau}\). Sustituyendo \( Y = \alpha + \tau D + \gamma M + u \)

Entonces:

\[
\hat{\tau} = \tau + \gamma \cdot \frac{\text{Cov}(D, M)}{\text{Var}(D)}
\]

Interpretación

- Si \( \text{Cov}(D, M) \neq 0 \), es decir, **si el tratamiento está correlacionado con la motivación**, el estimador de \(\tau\) estará **sesgado**.
- El sesgo es proporcional a:
  - El efecto de la motivación sobre \( Y \): \( \gamma \)
  - La correlación entre \( D \) y \( M \): \( \text{Cov}(D, M) \)


**Resumen del sesgo**

::: {.table .table-bordered .table-striped}
| Correlación entre \( D \) y \( M \) | Efecto de \( M \) sobre \( Y \) (\( \gamma \)) | ¿Hay sesgo en \( \hat{\tau} \)? | Dirección esperada del sesgo      |
|:----------------------------------:|:---------------------------------------------:|:-------------------------------:|:---------------------------------:|
| Cero                               | Cualquiera                                     | No                           | –                                 |
| Positiva                           | Positiva                                       | Sí                          | \( \hat{\tau} > \tau \) (sesgo hacia arriba) |
| Positiva                           | Negativa                                       | Sí                          | \( \hat{\tau} < \tau \) (sesgo hacia abajo) |
| Negativa                           | Positiva                                       | Sí                          | \( \hat{\tau} < \tau \) (sesgo hacia abajo) |
| Negativa                           | Negativa                                       | Sí                          | \( \hat{\tau} > \tau \) (sesgo hacia arriba) |
:::

**Lectura de la tabla**:

- Si el tratamiento está **correlacionado positivamente** con la motivación y la motivación **aumenta** el resultado, el estimador de \( \tau \) estará **sesgado hacia arriba**.
- Si la motivación está **omitida** y además está **correlacionada con el tratamiento**, siempre hay **sesgo**.
- Solo si la **motivación no está correlacionada con el tratamiento**, aunque no la observemos, **no hay sesgo**.


### ¿Qué hace la aleatorización?{-}

La **aleatorización** garantiza \( \text{Cov}(D, M) = 0 \), eliminando el sesgo de selección sin necesidad de observar \( M \).

 *Visualización: motivación, selección y aleatorización*

<img src="05-RCT_files/figure-html/unnamed-chunk-2-1.png" width="672" />

---

### Densidad de la motivación por grupo {-}

<img src="05-RCT_files/figure-html/unnamed-chunk-3-1.png" width="672" />

Por lo tanto ya no es necesario observar la motivación, ya que la aleatorización garantiza que los grupos sean comparables. Esto es exactamente lo mismo que vimos con resultados potenciales: el sesgo de selección \( \mathbb{E}[Y_i(D=0) \mid D_i=1] - \mathbb{E}[Y_i(D=0) \mid D_i=0] \) se hace cero, y en regresión \( \text{Cov}(D,M) = 0 \). Son dos caras de la misma moneda.

::: {.box-cuidado}
**Advertencia — balance muestral:** la aleatorización garantiza balance en expectativa, no igualdad exacta de medias en cada muestra. Una pequeña diferencia basal fortuita es compatible con un experimento correctamente aleatorizado; debe interpretarse junto con el mecanismo de asignación y no como una prueba automática de sesgo.
:::

Ahora veamos cómo esto se traduce a regresión en cuatro escenarios de diseño experimental, de lo más simple a lo más completo.

---

## RCT simple, sin estratos, sin controles {-}

**Objetivo:** identificación "pura" por aleatorización.

**Estimador (diferencia de medias):**

\[
\widehat{ATE} = \bar{Y}_1 - \bar{Y}_0
\]

donde \( \bar{Y}_1 \) es el promedio de \( Y \) en tratados y \( \bar{Y}_0 \) el promedio en controles.

**Regresión equivalente:**

\[
Y_i = \alpha + \tau D_i + u_i
\]

Aquí \( \hat{\tau} = \bar{Y}_1 - \bar{Y}_0 \). Es decir, el coeficiente de OLS sobre la dummy de tratamiento reproduce exactamente la diferencia de medias.

::: {.boxnote}
**Mensaje clave:** en un RCT, no "necesitas controles" para que el estimador sea causal; la aleatorización garantiza comparabilidad entre grupos. La regresión simple es suficiente para identificar el ATE.
:::

<img src="05-RCT_files/figure-html/unnamed-chunk-4-1.png" width="672" />

---

## RCT simple, sin estratos, con controles {-}

**Objetivo:** misma identificación causal, mayor **precisión**.

\[
Y_i = \alpha + \tau D_i + \beta' X_i + u_i
\]

**Puntos clave:**

- \( \hat{\tau} \) **sigue siendo causal** (la asignación es aleatoria, incluir \( X_i \) no cambia eso).
- \( X_i \) se incluye para **reducir la varianza residual**: si \( X_i \) predice \( Y_i \), absorbe parte de la variación no explicada y los errores estándar de \( \hat{\tau} \) se reducen.
- Los controles ideales son **baseline** (pre-tratamiento). Hay que evitar "malos controles": variables que fueron **afectadas** por el tratamiento.

::: {.boxnote}
**Mensaje clave:** controles en un RCT son principalmente para **eficiencia** (intervalos de confianza más estrechos), no para "corregir sesgo". El sesgo ya fue eliminado por la aleatorización.
:::

<img src="05-RCT_files/figure-html/unnamed-chunk-5-1.png" width="672" />

---

## RCT estratificado (bloques), sin controles adicionales {-}

**Objetivo:** respetar el diseño experimental y ganar precisión.

Si la asignación fue aleatoria **dentro de estratos** \( S \), la especificación estándar es:

\[
Y_i = \alpha + \tau D_i + \delta' S_i + u_i
\]

**Puntos clave:**

- Incluir dummies de estrato suele **mejorar la precisión** porque captura diferencias sistemáticas entre bloques.
- Alinea el análisis con el diseño: si la aleatorización fue **dentro** de estratos, el análisis debe **condicionar** en ellos.
- Omitir los estratos no sesga \( \hat{\tau} \), pero puede hacerlo **menos preciso** y los errores estándar pueden ser incorrectos.

::: {.boxnote}
**Mensaje clave:** si estratificaste la asignación, **controla por estratos** en el análisis. Es coherencia entre diseño y estimación.
:::

::: {.boxnote}
**Resultado clave — estratos y controles pretratamiento:** condicionar en los estratos usados para asignar preserva la comparación dentro de los bloques del diseño. Agregar covariables medidas antes del tratamiento que predicen el resultado puede reducir la varianza residual; ninguna de las dos decisiones reemplaza la aleatorización como fuente de identificación.
:::

<img src="05-RCT_files/figure-html/unnamed-chunk-6-1.png" width="672" />

---

## RCT estratificado + controles adicionales {-}

**Objetivo:** máxima precisión manteniendo interpretación causal.

\[
Y_i = \alpha + \tau D_i + \delta' S_i + \beta' X_i + u_i
\]

Aquí combinamos:

- **Dummies de estrato** \( S_i \): porque el diseño lo requiere (la aleatorización fue dentro de estratos).
- **Controles baseline** \( X_i \): porque mejoran la precisión (absorben varianza residual).

::: {.boxnote}
**Mensaje clave:** efectos fijos de estrato "por diseño" + controles baseline "por eficiencia". Es la especificación más completa y la que típicamente se reporta en papers experimentales.
:::

::: {.box-cuidado}
**Advertencia — selección de controles y especificación:** no deben incluirse variables postratamiento, pues pueden ser mecanismos del efecto o abrir selección. La especificación principal debe justificarse antes de observar resultados, respetar los estratos del diseño y usar solo controles pretratamiento; comparar muchas variantes hasta encontrar significancia invalida la interpretación confirmatoria.
:::

**Resumen de los cuatro escenarios:**

::: {.table .table-bordered .table-striped}
| Escenario | Regresión | ¿Por qué? |
|:---:|:---|:---|
| 1 | \( Y_i = \alpha + \tau D_i + u_i \) | Identificación pura por aleatorización |
| 2 | \( Y_i = \alpha + \tau D_i + \beta' X_i + u_i \) | + precisión con controles baseline |
| 3 | \( Y_i = \alpha + \tau D_i + \delta' S_i + u_i \) | Respetar diseño estratificado |
| 4 | \( Y_i = \alpha + \tau D_i + \delta' S_i + \beta' X_i + u_i \) | Diseño + eficiencia (especificación completa) |
:::

En los cuatro casos, \( \hat{\tau} \) estima el **ATE** de forma consistente. Lo que cambia es la **precisión**.

::: {.boxnote}
**Comparación — cuatro especificaciones:** el modelo simple muestra la identificación directa; añadir controles baseline busca precisión; añadir efectos fijos de estrato alinea la estimación con una asignación bloqueada; combinar ambos reúne diseño y eficiencia. El coeficiente puede variar fortuitamente entre columnas, pero el estimando causal permanece siendo el ATE cuando no hay interacciones y se respeta el diseño.
:::

---

## Puente a heterogeneidad: efectos diferenciados (CATE) {-}

Hasta ahora hemos estimado un efecto **promedio** \( \tau \). Pero en muchos contextos queremos saber si el tratamiento tiene **efectos diferentes** según alguna característica observable \( Z_i \) (por ejemplo, género, edad, nivel educativo).

Para esto usamos una **interacción**:

\[
Y_i = \alpha + \tau D_i + \theta Z_i + \gamma (D_i \cdot Z_i) + u_i
\]

(Si aplica, se agregan \( \delta' S_i + \beta' X_i \) como antes.)

**Interpretación:**

- Efecto para \( Z = 0 \): \( \tau \)
- Efecto para \( Z = 1 \): \( \tau + \gamma \)
- \( \gamma \) mide la **diferencia** en el efecto del tratamiento entre los dos grupos.

**ATE (promedio):** \( \tau + \gamma \cdot \mathbb{E}[Z] \)

::: {.boxnote}
**Intuición — interacción, efecto base y CATE:** la interacción permite que el efecto cambie con \(Z\). Sin centrar, \(\tau\) es el efecto base evaluado en \(Z=0\), mientras que \(\tau+\gamma z\) es el CATE en \(Z=z\); promediar esos efectos sobre la distribución de \(Z\) produce el ATE.
:::

### El truco de centrar (Wooldridge) {-}

Si definimos \( Z_c = Z - \bar{Z} \) y estimamos la regresión con \( D_i \cdot Z_c \) en lugar de \( D_i \cdot Z_i \):

\[
Y_i = \alpha + \tau D_i + \theta Z_{c,i} + \gamma (D_i \cdot Z_{c,i}) + u_i
\]

entonces el coeficiente \( \tau \) **es directamente el ATE**, porque \( \mathbb{E}[Z_c] = 0 \).

::: {.boxnote}
**Mensaje clave:** si interactúas el tratamiento con una covariable, **centra la covariable** para que el coeficiente de \( D \) siga siendo directamente interpretable como el ATE promedio. Sin centrar, \( \tau \) solo es el efecto para el grupo con \( Z = 0 \).
:::

<img src="05-RCT_files/figure-html/unnamed-chunk-7-1.png" width="672" />

---

## Interpretación del estimador {-}

En una regresión sin interacciones, el coeficiente de (D_i) representa la diferencia promedio entre los resultados de tratamiento y control. En un RCT correctamente implementado, esa comparación identifica el ATE. Los controles pretratamiento y los efectos fijos de estrato pueden cambiar la precisión y, en muestras finitas, mover ligeramente el punto estimado, pero no son la fuente de identificación.

Cuando aparece una interacción, el coeficiente de (D_i) deja de ser automáticamente el efecto promedio: corresponde al efecto en el valor de referencia del moderador. Centrar una covariable continua permite recuperar en ese coeficiente el efecto evaluado en su media.

## Supuestos, propiedades y condiciones de validez {-}

- **Identificación:** proviene de la asignación aleatoria y de SUTVA.
- **Insesgamiento o consistencia:** depende de que la comparación estimada corresponda al mecanismo de asignación y no esté seleccionada por atrición o medición diferencial.
- **Precisión:** puede mejorar al incluir estratos y covariables pretratamiento que predicen el resultado.
- **Inferencia:** los errores estándar deben reflejar el nivel real de asignación; agrupar por una dimensión con muy pocos clusters exige métodos y cautelas adicionales.

## Amenazas, limitaciones y errores comunes {-}

::: {.box-cuidado}
- Interpretar un desequilibrio aislado de covariables como prueba de que la aleatorización falló.
- Incluir controles postratamiento y convertirlos en “malos controles”.
- Olvidar los efectos fijos de estrato cuando la asignación se hizo dentro de bloques.
- Leer el coeficiente de (D) como ATE cuando aparece interactuado con una covariable sin centrar.
- Usar errores estándar agrupados con muy pocos clusters sin discutir la fragilidad de la inferencia.
:::

## Resumen {-}

::: {.box-resumen}
La aleatorización elimina el sesgo de selección porque hace independiente la asignación de los resultados potenciales. La diferencia de medias —o, equivalentemente, el coeficiente de (D) en una regresión simple— identifica el ATE. Los controles pretratamiento y los efectos fijos de estrato no crean la identificación: pueden mejorar la precisión y alinear el análisis con el diseño. Las interacciones permiten estudiar heterogeneidad, pero su interpretación exige definir cuidadosamente el grupo o valor de referencia.
:::

## Práctica tipo parcial {-}

::: {.box-ejercicios}
**RCT-T1 — Resultados potenciales, insesgadez y balance**

En un RCT individual, las 200 personas constituyen la población finita de interés y tienen resultados potenciales \(Y_i(D=1)\) y \(Y_i(D=0)\). La mitad es asignada al azar a \(D_i=1\). El estimando es el efecto promedio del tratamiento en estas 200 personas (SATE). Antes del tratamiento, el promedio de ingreso es 0,2 desviaciones estándar mayor entre tratados que entre controles por una diferencia fortuita de esta asignación.

1. Identifique el estimando de la diferencia de medias del resultado final. 2. Demuestre su insesgadez tomando expectativa sobre posibles asignaciones. 3. Explique por qué el balance basal exacto no es requisito para la identificación y cómo interpretaría la diferencia observada.

**Puntaje sugerido:** 10 puntos.

**Producto esperado:** estimando escrito con resultados potenciales, derivación algebraica breve y explicación conceptual de cuatro a seis líneas.
:::

::: {.box-ejercicios}
**RCT-T2 — Lectura comparada de cuatro regresiones**

Un experimento con 200 observaciones reporta cuatro columnas estimadas sobre la misma muestra. La asignación se realizó dentro de cuatro bloques, por lo que hay tres dummies de bloque no redundantes; además, el plan de análisis definió dos controles baseline. Todas las columnas incluyen intercepto y tratamiento.

| Columna | Coeficiente de tratamiento | Error estándar | Grados de libertad residuales |
|:--:|:--:|:--:|:--:|
| A | 2,06 | 0,52 | 198 |
| B | 2,01 | 0,39 | 196 |
| C | 2,08 | 0,45 | 195 |
| D | 2,03 | 0,31 | 193 |

Las especificaciones disponibles son: RCT simple; RCT simple con controles baseline; RCT estratificado con efectos fijos de bloque; y RCT estratificado con efectos fijos de bloque y controles baseline. Relacione cada columna con una especificación usando la estructura de cada modelo y los grados de libertad, no el orden de los errores estándar. Identifique el parámetro estimado y explique por qué los coeficientes son próximos mientras cambia la precisión.

**Puntaje sugerido:** 10 puntos.

**Producto esperado:** correspondencia única y justificada de las cuatro columnas mediante conteo de parámetros, identificación del parámetro causal y comparación concisa de identificación y precisión.
:::

::: {.box-ejercicios}
**RCT-T3 — Interacción, CATE y centrado**

Considere \(Y_i=\alpha+\tau D_i+\theta Z_i+\gamma(D_i\cdot Z_i)+u_i\), donde \(Z_i\) no está centrada, \(\mathbb{E}[Z_i]=4\), \(\tau=1,5\) y \(\gamma=0,4\).

1. Interprete el efecto base. 2. Calcule el CATE para \(Z=2\) y \(Z=6\). 3. Obtenga el ATE y muestre cómo se interpreta el coeficiente de tratamiento al usar \(Z_{c,i}=Z_i-4\). 4. Formule una advertencia sobre interpretar \(\tau\) como ATE cuando la covariable no está centrada.

**Puntaje sugerido:** 10 puntos.

**Producto esperado:** cuatro expresiones o cálculos claramente rotulados y una advertencia interpretativa de dos o tres líneas.
:::

## Preguntas para clase {-}

::: {.box-ejercicios}
1. ¿Por qué una diferencia de medias identifica el ATE en un RCT, pero no necesariamente en datos observacionales?
2. ¿Qué cambia —y qué no cambia— al agregar controles pretratamiento a la regresión?
3. ¿Por qué conviene incluir efectos fijos de estrato si la aleatorización se hizo dentro de bloques?
4. ¿Cómo cambia la interpretación del coeficiente de tratamiento cuando interactuamos (D) con una covariable?
5. ¿Qué riesgos aparecen al agrupar errores estándar con pocos clusters?
:::

::: {.boxvideo .green title="Videos recomendados:"}


- [Video 1](https://www.youtube.com/embed/eGRd8jBdNYg)
- [Video 2](https://www.youtube.com/embed/crpuBZv6XtA)
- [Video 3](https://www.youtube.com/embed/xlX3VtuIfQ0)

Y todos los que encuentres en Google buscando: **RCT Esther Duflo**
:::


::: {.boxnote }

**PROMPT DE CHATGPT PARA REFLEXIÓN PROFUNDA**

**Instrucciones**: Copia este mensaje en ChatGPT o la IA de tu preferencia. Tu objetivo no es obtener respuestas, sino **reflexionar guiado por preguntas**.

---

Hola. Actúa como mi tutor metodológico. No quiero que me des respuestas. Quiero que me ayudes a pensar como si estuviéramos en una tutoría.

Estoy estudiando diseños experimentales. Entiendo que si el tratamiento se asigna aleatoriamente, entonces \( \text{Cov}(D, X) = 0 \), incluso para variables no observables.

Pero sigo viendo que en muchos papers experimentales incluyen **controles en la regresión**. Ayúdame a pensar **paso a paso** si eso es necesario o no.

Por favor, hazme preguntas como:

- ¿Qué gana o pierde la estimación si incluyo controles?
- ¿Qué pasa si hay desequilibrios por azar?
- ¿Qué efecto tiene sobre la precisión del estimador?
- ¿Los controles ayudan a mejorar algo aunque \( \hat{\tau} \) ya sea insesgado?
- ¿Hay casos en que incluir controles puede ser problemático?

No me des respuestas. Solo nuevas preguntas que me ayuden a entender mejor este punto.

:::


---

## Puente a la clase práctica {-}

En la clase práctica aplicaremos estas cuatro especificaciones a un experimento de aula con 70 observaciones y asignación dentro de semestre. Verificaremos balance, compararemos la precisión de los modelos y evaluaremos heterogeneidad sin cambiar el estimando causal.

## Referencias {-}

- Bernal, R. y Peña, X. *Guía práctica para la evaluación de impacto*, capítulo 4.
- Abadie, A., Athey, S., Imbens, G. W. y Wooldridge, J. M. “When Should You Adjust Standard Errors for Clustering?” NBER Working Paper 24003.
