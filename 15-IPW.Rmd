# Ponderación por probabilidad inversa — Clase teórica {#ipw}

::: {.boxinfo}
**Metas de aprendizaje**

- Formular el ATE y el ATT antes de elegir un ponderador.
- Derivar las identidades de reponderación a partir del propensity score.
- Distinguir Horvitz–Thompson, Hájek y estimadores doblemente robustos.
- Diagnosticar positividad práctica, balance y concentración de los pesos.
:::

::: {.boxinfo}
**Lecturas centrales**

- [Bernal y Peña — capítulo 6 (PDF)](lecturas/bernal-pena/capitulo-06.pdf)
- [Cunningham — capítulo 5: Matching and Subclassification](https://mixtape.scunning.com/05-matching_and_subclassification)
:::

---

## Pregunta causal, estimandos y datos observados {-}

Sea $D_i\in\{0,1\}$ el tratamiento, $X_i$ un vector de covariables medidas antes del tratamiento y $Y_i$ el resultado observado. Cada unidad tiene dos resultados potenciales: $Y_i(D=1)$ si recibe tratamiento y $Y_i(D=0)$ si no lo recibe. Por consistencia,

$$
Y_i=D_iY_i(D=1)+(1-D_i)Y_i(D=0).
$$

La pregunta no es si los tratados tienen un resultado medio distinto de los controles, sino qué contraste de resultados potenciales se quiere aprender. Dos estimandos frecuentes son

$$
ATE=E[Y(D=1)-Y(D=0)]
$$

y

$$
ATT=E[Y(D=1)-Y(D=0)\mid D=1].
$$

El ATE describe el efecto medio en la población de interés; el ATT describe el efecto medio para quienes fueron tratados. La elección entre ambos precede a la elección de pesos: producen poblaciones objetivo diferentes y, por ello, exigen diagnósticos distintos.

## Supuestos de identificación {-}

IPW reemplaza la aleatorización incondicional por una comparación aleatoria **condicional** a $X$. Para interpretar sus estimadores causalmente se requieren, como mínimo, los siguientes supuestos.

1. **Consistencia y no interferencia (SUTVA).** El tratamiento observado corresponde al resultado potencial pertinente y el tratamiento de una unidad no altera el resultado potencial de otra.
2. **Independencia condicional (CIA).**

   $$
   \{Y(D=1),Y(D=0)\}\perp D\mid X.
   $$

   Tras condicionar en $X$, no quedan determinantes conjuntos no observados del tratamiento y los resultados potenciales.
3. **Positividad.** Para los valores de $X$ de la población objetivo,

   $$
   0<e(X)=P(D=1\mid X)<1.
   $$

   Para ATT basta la versión relevante para los tratados: cada perfil $X$ observado entre tratados debe tener controles comparables.

La CIA es una afirmación sustantiva sobre qué covariables se midieron y cuándo se midieron; no se verifica con una regresión ni con un test de balance. La positividad también tiene una dimensión empírica: probabilidades muy cercanas a cero o uno pueden satisfacer la desigualdad formal y, aun así, producir inferencia poco informativa.

## La identidad de reponderación {-}

Bajo los supuestos anteriores,

$$
E\left[\frac{D Y}{e(X)}\right]=E[Y(D=1)]
\qquad\text{y}\qquad
E\left[\frac{(1-D)Y}{1-e(X)}\right]=E[Y(D=0)].
$$

Por ejemplo, condicionando en $X=x$ y usando consistencia y CIA,

$$
\begin{aligned}
E\left[\frac{DY}{e(X)}\mid X=x\right]
&=E\left[\frac{D\,Y(D=1)}{e(x)}\mid X=x\right]\\
&=E[Y(D=1)\mid X=x].
\end{aligned}
$$

Al integrar sobre $X$ se obtiene la primera identidad. Así, ponderar a cada tratado por el inverso de su probabilidad de tratamiento crea una pseudopoblación en la que los perfiles $X$ de los tratados representan a la población objetivo; el término para controles hace lo mismo para $Y(D=0)$.

### Pesos para ATE y ATT {-}

Con un propensity score estimado $\hat e_i=\hat e(X_i)$, los pesos ATE no estabilizados son

$$
w_i^{ATE}=\frac{D_i}{\hat e_i}+\frac{1-D_i}{1-\hat e_i}.
$$

El ATE repondera tanto tratados como controles hacia la distribución covariable de toda la población. Para ATT, los tratados son la población de referencia y los pesos pueden escribirse como

$$
w_i^{ATT}=D_i+(1-D_i)\frac{\hat e_i}{1-\hat e_i}.
$$

Por tanto, el control con alta probabilidad de tratamiento recibe más peso porque es más parecido, en términos de $X$, a la población tratada. Estos pesos no justifican incluir covariables postratamiento: hacerlo puede bloquear parte del efecto causal o introducir sesgo de selección.

::: {.boxexam}
**IPW-T1.** Una política se implementó solo en municipios urbanos. Defina con precisión una población objetivo y el estimando apropiado si el interés es el efecto para los municipios efectivamente intervenidos. Indique qué versión de positividad debe evaluarse y por qué incluir una medida de empleo posterior a la política en $X$ sería problemático.
:::

## Horvitz–Thompson y Hájek {-}

Los dos estimadores usan la misma idea de reponderación, pero sus normalizaciones son distintas. Esta diferencia importa en muestras finitas y no debe esconderse bajo una misma etiqueta.

### Horvitz–Thompson: no normalizado {-}

El estimador Horvitz–Thompson (HT) del ATE es **no normalizado**:

$$
\widehat{ATE}_{HT}=
\frac{1}{n}\sum_{i=1}^{n}\frac{D_iY_i}{\hat e_i}
-\frac{1}{n}\sum_{i=1}^{n}\frac{(1-D_i)Y_i}{1-\hat e_i}.
$$

Cada media ponderada mantiene el denominador $n$, no la suma realizada de pesos del grupo. Para ATT, una forma HT útil es

$$
\widehat{ATT}_{HT}=
\frac{1}{n_1}\left[
\sum_{i=1}^{n}D_iY_i-
\sum_{i=1}^{n}(1-D_i)\frac{\hat e_i}{1-\hat e_i}Y_i
\right],
$$

donde $n_1=\sum_iD_i$. Estas expresiones son directamente análogas a estimadores de encuestas: las sumas de pesos estiman tamaños o masas de población, y no tienen por qué coincidir exactamente con sus valores esperados en una muestra concreta.

### Hájek: normalizado {-}

El estimador Hájek sustituye esas masas estimadas por las sumas de pesos observadas. Es, por construcción, **normalizado**:

$$
\widehat{ATE}_{H}=
\frac{\sum_i D_iY_i/\hat e_i}{\sum_i D_i/\hat e_i}
-\frac{\sum_i(1-D_i)Y_i/(1-\hat e_i)}{\sum_i(1-D_i)/(1-\hat e_i)}.
$$

Para ATT, la media tratada se compara con una media de controles normalizada por su peso ATT:

$$
\widehat{ATT}_{H}=
\frac{\sum_iD_iY_i}{\sum_iD_i}
-\frac{\sum_i(1-D_i)\hat e_iY_i/(1-\hat e_i)}
{\sum_i(1-D_i)\hat e_i/(1-\hat e_i)}.
$$

Hájek suele ser más estable en muestras finitas porque evita que una suma accidentalmente grande o pequeña de pesos cambie la escala de una media. No obstante, esa estabilidad no elimina el problema que produce pesos extremos; por ello deben reportarse ambas la elección del estimando y la forma de normalización.

### Pesos estabilizados {-}

Los **pesos estabilizados** conservan la reponderación relativa dentro de cada brazo y cambian su escala. Para ATE se usan, por ejemplo,

$$
sw_i^{ATE}=D_i\frac{P(D=1)}{\hat e_i}+
(1-D_i)\frac{P(D=0)}{1-\hat e_i}.
$$

En medias Hájek calculadas por brazo, los factores marginales $P(D=d)$ se cancelan entre numerador y denominador. En cambio, no deben insertarse mecánicamente en la fórmula HT anterior: sin la normalización correspondiente, esa sustitución ya no estima el ATE. La estabilización facilita interpretar las sumas de pesos, pero un reescalamiento constante dentro de cada brazo no mejora el $ESS$ ni reduce la concentración relativa. No crea soporte donde no lo hay, no vuelve correcta una especificación equivocada de $e(X)$ y no convierte automáticamente un estimador HT en Hájek.

::: {.boxexam}
**IPW-T2.** Escriba el estimador Hájek del ATE y señale sus dos denominadores. Después explique cómo difiere del HT no normalizado si, en una muestra, la suma de pesos de los controles no es igual a $n$.
:::

## Diagnósticos: balance, positividad y precisión {-}

El propensity score es un instrumento para balancear covariables, no una meta por sí mismo. Después de ponderar se comparan medias, diferencias estandarizadas y, cuando corresponde, distribuciones completas de cada covariable pretratamiento. Un balance cercano entre grupos ponderados respalda que el modelo de pesos está logrando el objetivo observable; **el balance no demuestra** CIA, pues la confusión no observada puede persistir incluso con balance perfecto en $X$.

Un resumen complementario es el tamaño efectivo de muestra,

$$
ESS=\frac{(\sum_i w_i)^2}{\sum_i w_i^2}.
$$

Si pocos pesos dominan, $ESS$ puede ser mucho menor que el tamaño nominal. Conviene reportar, por brazo y por estimando, cuantiles de los pesos, su máximo, sus sumas y $ESS$, junto con gráficas de $\hat e(X)$ por estado de tratamiento. Los errores estándar de un análisis aplicado deben además reconocer que el propensity score fue estimado; una regresión ponderada manual ilustra la mecánica, pero no reemplaza automáticamente la inferencia de procedimientos como `teffects`.

### Pesos extremos y cambios de diseño {-}

Trimming, truncamiento y winsorización no son arreglos automáticos. El *trimming* elimina observaciones con $\hat e(X)$ fuera de un intervalo y **cambia la población** a la región de soporte retenida; ya no identifica sin más el ATE o ATT original. Truncar o winsorizar conserva las observaciones, pero modifica los pesos y, con ellos, la pseudopoblación y el estimando implícito. En ambos casos se intercambia varianza por posible sesgo y por dependencia adicional de decisiones del analista. Se debe declarar el umbral, cuántas observaciones o pesos se afectaron y volver a evaluar qué efecto se está estimando.

La respuesta apropiada a positividad práctica débil puede incluir revisar covariables, redefinir la población objetivo, usar métodos que enfaticen la zona de solapamiento o presentar análisis de sensibilidad. No basta con aplicar winsorización y continuar interpretando el resultado como si nada hubiera cambiado.

## AIPW e IPWRA: doble robustez con límites {-}

IPW puro depende de un modelo de tratamiento adecuado. Los estimadores aumentados incorporan también modelos de resultado $m_d(X)=E[Y\mid D=d,X]$. Para el ATE, AIPW puede escribirse como

$$
\widehat{ATE}_{AIPW}=\frac{1}{n}\sum_{i=1}^{n}\left[
\hat m_1(X_i)-\hat m_0(X_i)
+\frac{D_i}{\hat e_i}\{Y_i-\hat m_1(X_i)\}
-\frac{1-D_i}{1-\hat e_i}\{Y_i-\hat m_0(X_i)\}
\right].
$$

IPWRA ajusta modelos de resultado por brazo con ponderaciones inversas y luego promedia predicciones en la población objetivo. En implementaciones de Stata, `teffects aipw` y `teffects ipwra` estiman estas variantes, mientras `teffects ipw` corresponde al estimador basado solo en el modelo de tratamiento.

“Doblemente robusto” significa que AIPW o IPWRA pueden ser consistentes si el modelo del tratamiento está correctamente especificado o si los modelos de resultado necesarios están correctamente especificados —para ATE, tanto $m_1(X)$ como $m_0(X)$—, bajo los demás supuestos causales y condiciones regulares. No significa inmunidad ante confusión no observada, falta de positividad, covariables postratamiento, medición deficiente de $X$ o mala especificación simultánea de ambos componentes. Tampoco es una garantía de buena precisión cuando los pesos son muy concentrados.

::: {.boxexam}
**IPW-T3.** Un análisis AIPW obtiene excelente balance ponderado, pero varios tratados tienen $\hat e(X)$ cercano a uno y el modelo de resultado omite una interacción importante. Evalúe qué parte de la afirmación de doble robustez podría seguir siendo útil, qué supuestos siguen sin verificarse y qué diagnósticos reportaría antes de interpretar el efecto.
:::

## IPW frente a matching {-}

Matching construye comparaciones locales entre unidades similares y, según el algoritmo, puede descartar observaciones sin contraparte. IPW construye una pseudopoblación mediante pesos. No es correcto afirmar sin matices que IPW “usa toda la muestra”: con positividad deficiente, algunas observaciones pueden recibir peso extremo, ser recortadas de manera justificada o aportar muy poca información efectiva.

| Aspecto | Matching | IPW |
|---|---|---|
| Mecanismo | Selecciona o asigna contrapartes | Repondera observaciones según $\hat e(X)$ |
| Población objetivo | Depende del algoritmo y los matches | ATE, ATT u otra población definida por los pesos |
| Riesgo principal | Malos matches y dependencia del algoritmo | Pesos extremos y positividad práctica débil |
| Diagnóstico central | Calidad del emparejamiento y balance | Balance, soporte, distribución de pesos y $ESS$ |

Ambos métodos dependen de la calidad de las covariables pretratamiento y de la superposición. La elección debe responder a la pregunta causal, la población objetivo y la geometría del soporte, no a una regla mecánica.

## Lecturas recomendadas {-}

- **Hirano, Imbens y Ridder (2003)**, “Efficient Estimation of Average Treatment Effects Using the Estimated Propensity Score”, *Econometrica*.
- **Robins, Rotnitzky y Zhao (1994)**, “Estimation of Regression Coefficients When Some Regressors Are Not Always Observed”, *Journal of the American Statistical Association*.
- **Wooldridge (2010)**, *Econometric Analysis of Cross Section and Panel Data*, capítulo 21.
- **StataCorp**, documentación de `teffects ipw`, `teffects aipw` y `teffects ipwra`.
