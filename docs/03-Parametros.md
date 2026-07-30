# Parámetros causales — Clase teórica {#parametros-causales-teoria}

## Objetivos y lecturas {-}

Al finalizar este capítulo, podrá definir resultados potenciales, distinguir ATE, ATT, ATU y CATE, y explicar por qué una diferencia observada no necesariamente tiene una interpretación causal. También podrá reconocer los supuestos que convierten comparaciones observadas en parámetros causales.

::: {.boxinfo}
**Lecturas centrales**

- [Bernal y Peña — capítulo 2 (PDF)](https://www.dropbox.com/s/zsqa2gcbbgdi5i3/Capitulo%202%20Bernal%20y%20Pe%C3%B1a.pdf?dl=1)
- [Bernal y Peña — capítulo 3 (PDF)](https://www.dropbox.com/s/837u3ea36r7t5me/Capitulo%203%20Bernal%20y%20Pe%C3%B1a.pdf?dl=1)
- [Cunningham — capítulo 4: Potential Outcomes](https://mixtape.scunning.com/04-potential_outcomes)
:::

Lectura complementaria: Angrist, J. D. y Pischke, J.-S. (2010), *The Credibility Revolution in Empirical Economics*.

## Pregunta causal y población de interés {-}

Usaremos como hilo conductor la pregunta: **¿Cuál es el efecto de participar en un programa de capacitación sobre el salario de la población elegible?**

La **población de interés** está formada por todas las personas elegibles para el programa. Para cada persona \(i\), \(D_i=1\) indica participación y \(D_i=0\) no participación. El resultado \(Y_i\) es su salario en el periodo de seguimiento. Estas definiciones deben fijarse antes de introducir la notación causal: el efecto depende de qué población, qué intervención y qué resultado se estudian.

## Resultados potenciales {-}

Para cada unidad \(i\), \(Y_i(D=1)\) es el salario que tendría bajo tratamiento y \(Y_i(D=0)\) el salario que tendría sin tratamiento. El resultado observado satisface

\[
Y_i=D_iY_i(D=1)+(1-D_i)Y_i(D=0).
\]

Cuando \(D_i=1\), observamos \(Y_i(D=1)\); cuando \(D_i=0\), observamos \(Y_i(D=0)\). Nunca observamos ambos para la misma persona en el mismo momento. El resultado potencial faltante es su **contrafactual individual**.

El efecto causal individual es

\[
\tau_i=Y_i(D=1)-Y_i(D=0).
\]

::: {.boxinfo title="💡 Intuición: una unidad, dos resultados potenciales"}

Cada persona tiene dos resultados potenciales definidos por la intervención, aunque los datos revelan solo uno. El efecto causal compara esos dos estados para la misma unidad; comparar personas distintas requiere, además, un argumento de identificación.

:::

## El problema fundamental {-}

El problema fundamental de la inferencia causal es que \(\tau_i\) no puede calcularse directamente: solo uno de sus dos componentes es observable. La inferencia causal reemplaza ese contrafactual individual faltante por una comparación válida a nivel de grupos, apoyada en un diseño y en supuestos explícitos.

El siguiente ejemplo conserva los ocho perfiles y hace visible el problema. `yd0` y `yd1` representan, respectivamente, \(Y_i(D=0)\) y \(Y_i(D=1)\). El atributo pretratamiento \(X\) vale 0 para las primeras cuatro personas y 1 para las últimas cuatro. La columna \(Y_i\) aplica la regla de observación y \(\tau_i\) muestra el efecto individual, disponible aquí solo porque el ejemplo revela ambos resultados potenciales.

| i | yd0 | yd1 | D | X | resultado observado \(Y_i\) | efecto individual \(\tau_i\) |
|---:|---:|---:|---:|---:|---:|---:|
| 1 | 10 | 12 | 1 | 0 | 12 | 2 |
| 2 | 4  | 5  | 0 | 0 | 4  | 1 |
| 3 | 9  | 10 | 1 | 0 | 10 | 1 |
| 4 | 10 | 11 | 1 | 0 | 11 | 1 |
| 5 | 5  | 6  | 0 | 1 | 5  | 1 |
| 6 | 3  | 2  | 0 | 1 | 3  | -1 |
| 7 | 12 | 11 | 1 | 1 | 11 | -1 |
| 8 | 5  | 7  | 0 | 1 | 5  | 2 |

Con la tabla, calcule ATE, ATT, ATU, \(CATE(0)\), \(CATE(1)\), la diferencia naïve entre salarios observados y su sesgo respecto del ATT. Luego explique qué contrafactual no estaría disponible en datos reales.

::: {.boxnote title="🔎 Ejemplo guiado: ocho personas, cuatro parámetros"}

La tabla permite separar cuatro poblaciones objetivo: las ocho personas para el ATE, las tratadas para el ATT, las no tratadas para el ATU y cada estrato de \(X\) para el CATE. Antes de calcular, identifique el subconjunto sobre el cual promedia cada estimando.

:::

## ATE, ATT, ATU y CATE {-}

Los cuatro parámetros responden preguntas distintas:

\[
ATE=\mathbb{E}[Y_i(D=1)-Y_i(D=0)]
\]

es el efecto promedio en la población de interés;

\[
ATT=\mathbb{E}[Y_i(D=1)-Y_i(D=0)\mid D_i=1]
\]

es el efecto promedio entre quienes participaron;

\[
ATU=\mathbb{E}[Y_i(D=1)-Y_i(D=0)\mid D_i=0]
\]

es el efecto promedio entre quienes no participaron; y

\[
CATE(x)=\mathbb{E}[Y_i(D=1)-Y_i(D=0)\mid X_i=x]
\]

es el efecto promedio condicional para el subgrupo con \(X_i=x\). Para interpretar CATE como heterogeneidad entre subgrupos definidos antes de la intervención, \(X_i\) debe ser una característica **pretratamiento**. Condicionar en una variable afectada por el programa cambiaría la pregunta y puede introducir sesgo.

Por la ley de expectativas iteradas, el ATE agrega los efectos según el estado de participación:

\[
ATE=P(D_i=1)ATT+P(D_i=0)ATU.
\]

::: {.boxnote title="✅ Resultado clave: del ATT y ATU al ATE"}

El ATE es un promedio ponderado del ATT y el ATU, con pesos dados por la proporción de personas tratadas y no tratadas en la población de interés.

:::

También agrega los efectos condicionales sobre la distribución de las características:

\[
ATE=\mathbb{E}[CATE(X_i)].
\]

::: {.boxnote title="✅ Resultado clave: del CATE al ATE"}

Los CATE describen heterogeneidad por características pretratamiento. Promediarlos usando la distribución de \(X_i\) en la población de interés recupera el ATE.

:::

Estas relaciones muestran por qué ATT, ATU y ATE coinciden cuando no existe heterogeneidad relevante, pero pueden diferir cuando la ganancia del programa varía entre personas o subgrupos.

## Diferencia observada y sesgo de selección {-}

La comparación directa de medias observadas es

\[
\mathbb{E}[Y_i\mid D_i=1]-\mathbb{E}[Y_i\mid D_i=0].
\]

Para relacionarla con el ATT, sustituimos los resultados observados por los resultados potenciales correspondientes y sumamos y restamos el mismo término, \(\mathbb{E}[Y_i(D=0)\mid D_i=1]\):

\[
\begin{aligned}
&\mathbb{E}[Y_i(D=1)\mid D_i=1]-\mathbb{E}[Y_i(D=0)\mid D_i=0]\\
={}&\mathbb{E}[Y_i(D=1)\mid D_i=1]-\mathbb{E}[Y_i(D=0)\mid D_i=1]\\
&+\mathbb{E}[Y_i(D=0)\mid D_i=1]-\mathbb{E}[Y_i(D=0)\mid D_i=0].
\end{aligned}
\]

Por tanto,

\[
\mathbb{E}[Y_i\mid D_i=1]-\mathbb{E}[Y_i\mid D_i=0]
=ATT+\mathbb{E}[Y_i(D=0)\mid D_i=1]
-\mathbb{E}[Y_i(D=0)\mid D_i=0].
\]

El último par de términos es el **sesgo de selección**. Si es positivo, la diferencia observada excede el ATT; si es negativo, queda por debajo del ATT; si es cero, la diferencia observada coincide con el ATT. Por ello, la selección no siempre produce una sobreestimación: su dirección depende de cómo habrían diferido ambos grupos sin tratamiento.

::: {.boxwarning title="⚠️ Advertencia: más datos no corrigen selección"}

Aumentar el tamaño de la muestra reduce la incertidumbre de la diferencia observada, pero no elimina la diferencia contrafactual entre quienes eligen participar y quienes no. Una comparación sesgada puede estimarse con mucha precisión.

:::

## Supuestos de identificación {-}

La **independencia incondicional** exige

\[
(Y_i(D=1),Y_i(D=0))\perp D_i.
\]

Una asignación aleatoria bien implementada hace plausible este supuesto: el estado de tratamiento no contiene información sobre los resultados potenciales. En ese caso, la diferencia de medias identifica el ATE.

En estudios observacionales suele plantearse **independencia condicional**, acompañada de positividad:

\[
(Y_i(D=1),Y_i(D=0))\perp D_i\mid X_i,
\qquad 0<P(D_i=1\mid X_i=x)<1.
\]

La primera condición afirma que, dentro de grupos con el mismo \(X_i\) pretratamiento, la participación no aporta información adicional sobre los resultados potenciales. La segunda exige que para cada valor relevante de \(x\) haya probabilidad positiva tanto de participar como de no participar; sin soporte común no existe una comparación empírica para ese subgrupo. Ninguna de las dos condiciones puede garantizarse solo incluyendo muchos controles: deben justificarse con conocimiento institucional y del mecanismo de asignación.

::: {.boxwarning title="⚠️ Advertencia: ignorabilidad sin positividad no basta"}

Aunque la independencia condicional fuera plausible, no puede identificarse el efecto para un perfil \(X_i=x\) si todas las personas con ese perfil reciben el mismo estado de tratamiento. Sin observaciones comparables en ambos estados, falta soporte para construir el contrafactual.

:::

No todos los diseños identifican efectos imponiendo independencia. Las variables instrumentales se apoyan, entre otros requisitos, en **relevancia** y **exclusión**; la regresión discontinua, en **continuidad** alrededor del umbral; y diferencias en diferencias, en **tendencias paralelas**. En general, IV, RDD y DiD no “cumplen independencia”: identifican parámetros causales bajo estructuras distintas y, a veces, para poblaciones locales específicas.

## Comparación antes-después {-}

Suponga dos periodos: \(t=0\), antes del programa, y \(t=1\), después. La notación \(Y_{it}(D=1)\) y \(Y_{it}(D=0)\) separa claramente el tiempo del estado de tratamiento. Para una persona tratada observamos \(Y_{i0}(D=0)\) antes y \(Y_{i1}(D=1)\) después. El contrafactual faltante para evaluar el efecto en el periodo posterior es

\[
Y_{i1}(D=0),
\]

no el resultado del periodo anterior. La comparación antes-después es

\[
Y_{i1}(D=1)-Y_{i0}(D=0),
\]

mientras que el efecto causal posterior es

\[
Y_{i1}(D=1)-Y_{i1}(D=0).
\]

La diferencia entre ambas cantidades es el cambio que habría ocurrido aun sin tratamiento, \(Y_{i1}(D=0)-Y_{i0}(D=0)\). Por ejemplo, si el salario pasa de 6 a 9 pero habría subido a 8 sin capacitación, el cambio observado es 3, el efecto causal es 1 y el componente temporal es 2. Una comparación antes-después solo identifica el efecto si se justifica que ese cambio contrafactual ausente es cero; tener un periodo previo no resuelve por sí mismo el problema causal.

## SUTVA {-}

SUTVA, el supuesto de valor estable del tratamiento unitario, reúne dos condiciones:

1. **Ausencia de interferencia:** el resultado potencial de una persona depende de su propio estado de tratamiento, no del tratamiento asignado a otras personas.
2. **Tratamiento bien definido:** cada estado de tratamiento representa una intervención suficientemente precisa, sin versiones relevantes ocultas.

La vacunación puede violar ausencia de interferencia porque vacunar a otras personas cambia el riesgo de una unidad no vacunada. Las redes también generan derrames entre contactos. Un programa administrado con dosis o intensidades distintas puede violar la condición de tratamiento bien definido. Antes de usar \(Y_i(D=1)\) y \(Y_i(D=0)\), el investigador debe decidir si esos dos estados describen adecuadamente la intervención y el entorno.

::: {.boxvideo .green title="🎥 Videos recomendados:"}

<iframe width="100%" height="315" src="https://www.youtube.com/embed/ln5LBKiF8hE" title="Introducción a resultados potenciales" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

<iframe width="100%" height="315" src="https://www.youtube.com/embed/iPBV3BlV7jk" title="Mastering Metrics e inferencia causal" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

:::

### Actividad breve {-}

Para una evaluación antes-después de un programa de formación, responda en cuatro puntos:

1. ¿Qué **estimando** causal quiere recuperar?
2. ¿Cuál es el **contrafactual faltante**?
3. ¿Qué **supuesto** permitiría interpretar causalmente la comparación?
4. Mencione **dos amenazas** concretas a ese supuesto.

## Síntesis {-}

Un efecto causal compara dos resultados potenciales para la misma unidad, aunque solo uno se observa. ATE, ATT, ATU y CATE delimitan poblaciones objetivo diferentes y se relacionan mediante reglas de agregación. La diferencia observada mezcla el ATT con selección; la comparación antes-después mezcla tratamiento con cambios temporales. La identificación exige explicitar tanto el supuesto que reconstruye el contrafactual como SUTVA, que hace coherentes los estados de tratamiento.

## Ejercicios {-}

1. Use la tabla de ocho personas para calcular ATE, ATT, ATU, \(CATE(0)\) y \(CATE(1)\). Verifique las dos relaciones de agregación del ATE.
2. Calcule la diferencia naïve de la tabla y descompóngala en ATT y sesgo de selección. Interprete el signo del sesgo.
3. Proponga un ejemplo en el cual una comparación antes-después falle y otro en el cual SUTVA falle. En cada caso, identifique el resultado potencial o la condición que genera el problema.

::: {.boxejercicio title="Elección del parámetro causal"}

**T-P1.** Un programa de capacitación se dirige exclusivamente a personas desempleadas de larga duración, pero solo una parte de las elegibles participa. El gobierno quiere decidir si mantiene el programa para quienes efectivamente se inscriben. Escoja entre ATE, ATT, ATU y CATE; justifique cuál es la población relevante y escriba el estimando usando \(Y_i(D=1)\) y \(Y_i(D=0)\).

**Puntaje sugerido:** 4 puntos.

:::

::: {.boxejercicio title="Diferencia observada y selección"}

**T-P2.** Considere los datos del enunciado \(\mathbb{E}[Y_i\mid D_i=1]=12\), \(\mathbb{E}[Y_i\mid D_i=0]=7\) y \(ATT=2\). Derive el sesgo de selección a partir de la descomposición completa de la diferencia observada y explique qué significa su signo en términos de los resultados que las personas tratadas y no tratadas habrían obtenido sin tratamiento.

**Puntaje sugerido:** 5 puntos.

:::

::: {.boxejercicio title="Diagnóstico de los supuestos"}

**T-P3.** Una intervención escolar es adoptada voluntariamente por las escuelas. Para cierto perfil institucional existe una escuela tratada, pero ninguna escuela no tratada comparable. Además, el programa puede generar derrames entre compañeros. Diagnostique por separado la independencia, la positividad y SUTVA, y proponga una modificación del diseño que atienda estas amenazas.

**Puntaje sugerido:** 6 puntos.

:::

## Puente a la práctica {-}

El capítulo siguiente, `04-ParametrosStata.Rmd`, lleva estas definiciones a datos: construye el resultado observado de la muestra de ocho perfiles, calcula ATE, ATT, ATU y CATE, y reproduce en Stata la diferencia naïve y su descomposición. Al interpretar cada salida, conviene regresar a tres preguntas: cuál es el estimando, cuál es el contrafactual faltante y qué supuesto permite identificarlo.

## Referencias {-}

- Angrist, J. D. y Pischke, J.-S. (2010). The credibility revolution in empirical economics: How better research design is taking the con out of econometrics. *Journal of Economic Perspectives*, 24(2), 3–30.
- Bernal, R. y Peña, X. (2011). *Guía práctica para la evaluación de impacto*. Universidad de los Andes.
