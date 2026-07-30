# Malos controles — Clase teórica {#bad-controls-teoria}

::: {.boxinfo}
**Metas de aprendizaje**

- Definir el estimando antes de escoger controles.
- Distinguir causas comunes, mediadores y colisionadores.
- Explicar por qué el momento de medición no basta para clasificar una variable.
- Aplicar estos criterios a experimentos y diferencias en diferencias.
:::

::: {.boxinfo}
**Lecturas centrales**

- [Bernal y Peña — capítulo 3 (PDF)](https://www.dropbox.com/s/837u3ea36r7t5me/Capitulo%203%20Bernal%20y%20Pe%C3%B1a.pdf?dl=1)
- [Cunningham — capítulo 3: Directed Acyclic Graphs](https://mixtape.scunning.com/03-directed_acyclical_graphs)
:::

## Primero el estimando {-}

Sea \(D\) un tratamiento y \(Y\) el resultado. Con la notación del curso,

\[
Y_i(D=1),\qquad Y_i(D=0),
\]

el efecto promedio es

\[
ATE=E[Y_i(D=1)-Y_i(D=0)].
\]

En el diseño DID básico, el contraste identifica normalmente el **ATT**:

\[
ATT=E[Y_{i1}(D=1)-Y_{i1}(D=0)\mid D_i=1],
\]

bajo tendencias paralelas, consistencia, ausencia de anticipación, composición estable y ausencia de interferencia. Los controles se evalúan respecto de este estimando y de estos supuestos; no existe una lista universal de variables que siempre deban incluirse.

::: {.boxcerebro}
**Regla central:** antes de preguntar “¿incluyo \(X\)?”, pregunte “¿qué efecto quiero identificar y qué papel causal cumple \(X\)?”.
:::

## Tres estructuras causales básicas {-}

### Causa común o *fork* {-}

\[
D \leftarrow X \rightarrow Y
\]

El camino por \(X\) es una fuente de confusión. Si \(X\) está bien medida y no crea otros caminos, ajustarla puede cerrar ese *backdoor*.

### Mediador o *chain* {-}

\[
D \rightarrow M \rightarrow Y
\]

Si interesa el efecto total, ajustar por \(M\) bloquea parte del efecto que se quiere medir. Un coeficiente condicionado en \(M\) solo puede interpretarse como efecto directo bajo supuestos adicionales de mediación; no basta con agregar \(M\) a una regresión.

### Colisionador {-}

\[
D \rightarrow C \leftarrow U \rightarrow Y
\]

El camino está cerrado sin ajustar. Condicionar en \(C\), o seleccionar la muestra con base en \(C\), crea asociación entre \(D\) y \(U\) y abre un camino no causal hacia \(Y\).

::: {.boxadvertencia}
“Más controles” no significa “menos sesgo”. Un control puede cerrar un camino de confusión, bloquear el efecto de interés o abrir una asociación espuria.
:::

## Buenos, malos y neutrales {-}

Un **buen control** es una variable cuyo ajuste bloquea caminos no causales relevantes sin bloquear el efecto de interés ni abrir caminos nuevos. Una causa común pretratamiento suele ser un buen candidato.

Un **mal control** es una variable cuyo ajuste impide identificar el estimando: por ejemplo, un mediador cuando interesa el efecto total, un colisionador o una consecuencia del tratamiento que induce selección.

Un **control neutral** no es necesario para identificación. Puede aumentar precisión si predice \(Y\), o reducirla si predice principalmente \(D\). Por eso también debe justificarse.

### El tiempo no resuelve por sí solo la decisión {-}

La regla “controle todo lo medido antes del tratamiento” es insuficiente. Una variable pretratamiento puede ser:

- un colisionador y producir **M-bias**;
- un predictor fuerte de \(D\), pero débil de \(Y\), y causar **amplificación de sesgo** ante confusión no observada;
- neutral para identificación y útil únicamente para precisión.

Asimismo, una variable posterior al tratamiento no siempre genera sesgo, pero su ajuste requiere una justificación causal especialmente fuerte. La temporalidad ayuda a ordenar las flechas; el DAG determina qué caminos se abren o se cierran.

## La demostración de Angrist y Pischke {-}

Suponga que \(D\) afecta el resultado \(Y\) y también una variable de agrupación \(F\):

\[
D\rightarrow Y,\qquad D\rightarrow F.
\]

Como \(D\) es aleatorio,

\[
\{Y(D=1),Y(D=0),F(D=1),F(D=0)\}\perp D.
\]

Sin ajustar por \(F\),

\[
E[Y\mid D=1]-E[Y\mid D=0]
=E[Y(D=1)]-E[Y(D=0)]=ATE.
\]

Ahora compare observaciones con \(F=1\). Entre tratadas, \(F=1\) significa \(F(D=1)=1\); entre controles significa \(F(D=0)=1\). Por tanto,

\[
\begin{aligned}
&E[Y\mid D=1,F=1]-E[Y\mid D=0,F=1]\\
&=E[Y(D=1)\mid F(D=1)=1]-E[Y(D=0)\mid F(D=0)=1]\\
&=\underbrace{E[Y(D=1)-Y(D=0)\mid F(D=1)=1]}_{\text{efecto en el estrato }F(D=1)=1}\\
&\quad+\underbrace{E[Y(D=0)\mid F(D=1)=1]
-E[Y(D=0)\mid F(D=0)=1]}_{\text{sesgo de agrupación}}.
\end{aligned}
\]

::: {.boxcerebro}
La comparación condicionada mezcla un efecto causal en un estrato principal con una diferencia de composición. No recupera automáticamente el ATE.
:::

En el ejemplo de educación, ocupación y salarios, la educación modifica quién entra a una ocupación. Comparar personas dentro de esa ocupación enfrenta grupos seleccionados de manera distinta. En una aplicación particular el sesgo puede atenuar el efecto; en general, **puede ser positivo o negativo**.

## Aplicación a diferencias en diferencias {-}

El estimador DID de dos periodos es

\[
\widehat{DID}=
\{E[Y_1\mid D=1]-E[Y_0\mid D=1]\}
-\{E[Y_1\mid D=0]-E[Y_0\mid D=0]\}.
\]

Los controles pretratamiento pueden servir para formular tendencias paralelas condicionales o mejorar precisión. Pero no reparan automáticamente tendencias diferentes y deben tener soporte común. Variables como empleo, ingreso, migración o composición familiar medidas después del programa pueden ser mediadores o variables de selección.

::: {.boxadvertencia}
En DID, controlar una variable afectada por el programa puede cambiar simultáneamente el estimando, la composición comparada y la plausibilidad de tendencias paralelas.
:::

## Checklist para decidir {-}

1. ¿Cuál es el estimando: ATE, ATT, efecto total o efecto directo?
2. ¿Qué flechas llegan y salen de la variable?
3. ¿El ajuste cierra un *backdoor*?
4. ¿Bloquea un canal del tratamiento?
5. ¿Abre un colisionador o selecciona la muestra?
6. ¿La variable fue medida antes o después de \(D\)?
7. ¿Se incluye para identificación o solo para precisión?

## Preguntas tipo examen {-}

::: {.boxejercicio}
**Código:** BC-T1
**Tipo:** Análisis causal
**Fuente:** Elaboración propia
**Enunciado:** Un programa de capacitación afecta el empleo formal y este afecta el salario. Dibuje el DAG y explique qué estimando recuperaría una comparación de salarios que condiciona en empleo formal. Indique los supuestos adicionales necesarios para darle una interpretación causal directa.
**Puntaje sugerido:** 4 puntos
**Producto esperado:** DAG, estimando y argumento de identificación en máximo 180 palabras.
:::

::: {.boxejercicio}
**Código:** BC-T2
**Tipo:** Demostración
**Fuente:** Angrist y Pischke
**Enunciado:** Partiendo de resultados potenciales para \(Y\) y \(F\), derive la descomposición de la diferencia condicionada en \(F=1\). Explique por qué los dos grupos condicionados no pertenecen necesariamente al mismo estrato.
**Puntaje sugerido:** 5 puntos
**Producto esperado:** Derivación algebraica y explicación económica del término de composición.
:::

::: {.boxejercicio}
**Código:** BC-T3
**Tipo:** Diseño de investigación
**Fuente:** Cinelli, Forney y Pearl
**Enunciado:** Evalúe la afirmación “toda covariable pretratamiento debe incluirse”. Construya un contraejemplo con un colisionador pretratamiento o con amplificación de sesgo y explique el camino causal relevante.
**Puntaje sugerido:** 4 puntos
**Producto esperado:** DAG y justificación en máximo 160 palabras.
:::

## Lecturas {-}

- Angrist, J. D. y Pischke, J.-S. (2009), *Mostly Harmless Econometrics*, sección sobre malos controles.
- Cinelli, C., Forney, A. y Pearl, J. (2022), “A Crash Course in Good and Bad Controls”.
