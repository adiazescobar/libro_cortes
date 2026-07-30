# Controles sintéticos — Clase teórica {#controles-sinteticos}

::: {.boxinfo}
**Lecturas centrales**

- [Bernal y Peña — capítulo 6 (PDF)](lecturas/bernal-pena/capitulo-06.pdf)
- [Cunningham — capítulo 10: Synthetic Control](https://mixtape.scunning.com/10-synthetic_control)
- [Abadie, Diamond y Hainmueller (2010) — artículo original (PDF)](https://economics.mit.edu/sites/default/files/publications/Synthetic%20Control%20Methods.pdf)
:::

::: {.boxinfo}
**Metas de aprendizaje**

- Explicar por qué una combinación convexa puede construir un contrafactual más creíble que un único control.
- Distinguir ajuste pretratamiento, identificación e inferencia placebo.
- Diagnosticar soporte, contaminación del *donor pool* y sensibilidad a donantes influyentes.
:::

---

## Una intervención, una unidad tratada y ningún control perfecto {-}

Algunas políticas afectan a una unidad agregada en un momento claramente definido: una ciudad adopta una restricción de circulación, una región cambia su legislación laboral o un país recibe una reforma institucional. La unidad es especial, el número de unidades tratadas puede ser uno y el resultado se observa durante varios periodos antes y después de la intervención. En ese escenario, comparar la unidad tratada con el promedio de todas las unidades no tratadas suele ser poco convincente. Una ciudad industrial no adquiere un contrafactual adecuado por promediar, con el mismo peso, ciudades turísticas, agrícolas y financieras.

Sea la unidad $i=1$ la tratada y sean $i=2,\ldots,J+1$ las unidades potencialmente donantes. La intervención comienza en $T_0+1$. Para la unidad tratada existen dos trayectorias potenciales:

$$
Y_{1t}(D=1) \quad\text{y}\quad Y_{1t}(D=0).
$$

Después de la intervención observamos $Y_{1t}(D=1)$, pero no la trayectoria que habría seguido la misma unidad sin tratamiento, $Y_{1t}(D=0)$. El efecto causal dinámico de interés es

$$
\tau_{1t}=Y_{1t}(D=1)-Y_{1t}(D=0),
\qquad t>T_0.
$$

No se trata necesariamente de un ATE sobre una población de individuos. Es una secuencia de efectos para una unidad tratada concreta. También puede resumirse sobre un horizonte posterior $\mathcal T_1$:

$$
\bar\tau_1=\frac{1}{|\mathcal T_1|}
\sum_{t\in\mathcal T_1}\tau_{1t}.
$$

El problema fundamental sigue siendo el mismo del marco de resultados potenciales: falta $Y_{1t}(D=0)$ después de $T_0$. Lo distintivo del método es cómo intenta reconstruirlo.

::: {.boxinfo}
**Intuición: construir, no encontrar, el control.** Si ninguna unidad no tratada reproduce por sí sola a la tratada, una combinación de varias puede hacerlo. Una donante aporta el nivel inicial, otra la tendencia y otra características predictoras del resultado. El control sintético es esa combinación explícita, no el promedio automático de todas las unidades disponibles.
:::

## La solución: una combinación convexa de donantes {-}

El control sintético asigna a cada donante $j$ un peso $w_j$ y estima la trayectoria no tratada mediante

$$
\widehat Y_{1t}(D=0)=\sum_{j=2}^{J+1}w_jY_{jt},
\qquad w_j\geq 0,
\qquad \sum_{j=2}^{J+1}w_j=1.
$$

La no negatividad y la suma unitaria hacen que el sintético sea una **combinación convexa**. Cada valor sintético es un promedio ponderado de resultados observados en el conjunto donante. Los pesos no se eligen para maximizar el efecto posterior: se estiman usando únicamente información anterior al tratamiento.

### Un ejemplo numérico pequeño {-}

Supongamos que una reforma comienza en una provincia en 2024. Tres provincias no tratadas pueden contribuir al contrafactual con pesos

$$
W=(0.50,\;0.30,\;0.20)',
$$

que son no negativos y suman uno. Si en 2025 sus resultados son 70, 90 y 85, respectivamente, entonces

$$
\widehat Y_{1,2025}(D=0)
=0.50(70)+0.30(90)+0.20(85)=79.
$$

Si el resultado observado de la provincia tratada es $Y_{1,2025}(D=1)=73$, el efecto estimado es

$$
\widehat\tau_{1,2025}=73-79=-6.
$$

El signo negativo indica que el resultado observado quedó seis unidades por debajo de la trayectoria sintética. No significa que la provincia donante con mayor peso “causó” la mitad del efecto: los pesos construyen el contrafactual y no descomponen causalmente el efecto entre donantes.

## Cómo se eligen los pesos: distinguir (W) de (V) {-}

Sea $X_1$ un vector de $K$ predictores pretratamiento de la unidad tratada. Puede contener características anteriores a la intervención y valores rezagados del resultado. Sea $X_0$ la matriz $K\times J$ que contiene los mismos predictores para las unidades donantes. Los pesos se agrupan en

$$
W=(w_2,\ldots,w_{J+1})',
$$

y pertenecen al simplex

$$
\mathcal W=\left\{W:w_j\geq 0,\ \sum_{j=2}^{J+1}w_j=1\right\}.
$$

Para una matriz diagonal semidefinida positiva $V$, los pesos se eligen resolviendo

$$
W^*(V)=\arg\min_{W\in\mathcal W}
(X_1-X_0W)'V(X_1-X_0W).
$$

Esta expresión contiene dos clases de pesos diferentes:

| Objeto | Dimensión | Qué pondera | Pregunta que responde |
|---|---:|---|---|
| $W$ | $J\times 1$ | Unidades donantes | ¿Cuánto contribuye cada unidad al control sintético? |
| $V$ | $K\times K$ diagonal | Predictores | ¿Qué discrepancias predictoras son más importantes para reproducir la trayectoria anterior? |

La elección de $V$ orienta la búsqueda de $W$. En la implementación `synth` de Stata conviene separar tres objetos que operan simultáneamente:

- `xperiod()` especifica los años sobre los cuales se promedian los predictores que no llevan una ventana propia;
- `mspeperiod()` especifica los años pretratamiento del resultado usados para minimizar el MSPE y seleccionar $V$;
- `nested` es el algoritmo de optimización usado para buscar $V$, no una ventana temporal ni una partición de validación.

Así, si `mspeperiod()` incluye toda la trayectoria anterior, reproducir esa trayectoria es ajuste *in-sample*. Es un diagnóstico necesario, pero no una prueba reservada u *out-of-sample*. Un predictor con gran peso en $V$ puede ser decisivo para construir $W$, pero no es una unidad donante; y una unidad con gran peso en $W$ no es necesariamente la más parecida en cada predictor por separado.

La selección de predictores y ventanas temporales debe decidirse por conocimiento sustantivo y capacidad predictiva previa, no probando especificaciones hasta obtener la brecha posterior deseada. Usar resultados posteriores para elegir $V$, $W$ o el conjunto de predictores contaminaría el diseño.

### Del contrafactual al estimador {-}

Una vez estimado $W^*$ con datos pretratamiento,

$$
\widehat Y_{1t}(D=0)=\sum_{j=2}^{J+1}w_j^*Y_{jt}
$$

y, para cada periodo posterior,

$$
\widehat\tau_{1t}
=Y_{1t}(D=1)-\widehat Y_{1t}(D=0).
$$

Si se desea un promedio posterior,

$$
\widehat{\bar\tau}_1
=\frac{1}{|\mathcal T_1|}
\sum_{t\in\mathcal T_1}\widehat\tau_{1t}.
$$

::: {.boxkey}
**Conexión entre gráfica y estimando.** En la gráfica de trayectorias, la distancia vertical “tratada menos sintética” en el periodo $t$ es $\widehat\tau_{1t}$. En la gráfica de brechas, esa misma distancia aparece directamente como la altura respecto de cero. La gráfica muestra un efecto dinámico estimado; convertir el área visual o una brecha puntual en una afirmación causal requiere todavía defender el diseño.
:::

::: {.boxexam}
**SC-T1.** Una región tratada tiene tres donantes potenciales. Con pesos $W=(0.40,0.35,0.25)'$, sus resultados posteriores son 82, 70 y 90, mientras el resultado observado de la región tratada es 68. Calcule $\widehat Y(D=0)$ y $\widehat\tau=Y(D=1)-\widehat Y(D=0)$. Interprete el signo y explique por qué los pesos no son “porcentajes del efecto” atribuibles a cada donante.
:::

## Soporte: la geometría de la envolvente convexa {-}

La restricción $W\in\mathcal W$ tiene una interpretación geométrica. El conjunto

$$
\left\{X_0W:W\in\mathcal W\right\}
$$

es la **envolvente convexa** de los vectores de predictores de los donantes. Si $X_1$ está dentro de esa envolvente, existe al menos una combinación convexa capaz de reproducirlo exactamente en los predictores considerados. Si está cerca, el método puede aproximarlo. Si queda lejos, ninguna redistribución de pesos no negativos que sumen uno cerrará la brecha.

Esta restricción favorece la **interpolación**: el contrafactual se construye dentro del soporte aportado por unidades observadas. Evita, por ejemplo, asignar un peso 1.4 a una unidad y $-0.4$ a otra para fabricar un valor fuera del rango donante. Esa disciplina hace transparentes los problemas de soporte, pero no los resuelve.

Un mal ajuste pretratamiento puede ser evidencia de que:

- los donantes no abarcan las características o trayectorias relevantes de la unidad tratada;
- los predictores son insuficientes o están medidos en ventanas poco informativas;
- hubo quiebres estructurales previos que impiden una relación estable;
- la unidad tratada es demasiado singular para una combinación convexa del *donor pool*.

Agregar donantes no siempre mejora el soporte. Una nueva unidad ayuda solo si aporta información comparable y permanece libre del tratamiento o de sus efectos indirectos. Un conjunto grande de unidades irrelevantes no compensa la ausencia de donantes sustantivamente adecuados.

::: {.boxwarning}
**No extrapolar una conclusión que el diseño no sostiene.** Cuando el tratado queda fuera de la envolvente convexa, el algoritmo puede devolver la mejor aproximación disponible, pero “la mejor” no implica “buena”. Una brecha posterior calculable no convierte un contrafactual mal ajustado en uno creíble.
:::

## ¿Qué hace creíble al contrafactual sintético? {-}

El control sintético es una estrategia de diseño comparativo, no una consecuencia mecánica de minimizar una función. Su interpretación causal exige argumentos sobre el tratamiento, el conjunto donante y la estabilidad temporal.

### Ajuste pretratamiento informativo {-}

La trayectoria sintética debe reproducir razonablemente la trayectoria observada de la unidad tratada antes de $T_0+1$. También conviene examinar el balance de predictores. Un periodo pretratamiento largo permite observar si la combinación acompaña niveles, cambios y giros relevantes, no solo una media.

El buen ajuste anterior es importante porque muestra que el método pudo reconstruir hechos observados que no necesitaba explicar con tratamiento. Sin embargo, es una condición de credibilidad, no una demostración de que el mismo vínculo habría continuado después.

### No anticipación y tratamiento bien fechado {-}

Antes de $T_0+1$, la trayectoria usada para estimar los pesos debe corresponder a $Y(D=0)$. Si hogares, firmas o gobiernos reaccionan cuando se anuncia la política, el efecto puede comenzar antes de la implementación formal. Incluir esos periodos anticipatorios como si fueran no tratados fuerza al sintético a imitar parte del efecto y puede atenuar o distorsionar la brecha posterior. La fecha debe fijarse con evidencia institucional y, si anuncio e implementación difieren, el análisis debe justificar qué evento inicia el tratamiento.

### Ausencia de interferencia y contaminación de donantes {-}

El resultado de una unidad donante no debería cambiar por el tratamiento aplicado a la unidad 1. Esta condición puede fallar por *spillovers*, migración, comercio, desplazamiento de actividad, aprendizaje de políticas o exposición a la misma intervención. Un donante contaminado ya no representa limpiamente $Y(D=0)$: incorpora directa o indirectamente parte del tratamiento.

La contaminación no se diagnostica solo con el peso. Una unidad con peso cero puede ser irrelevante para el estimador principal, pero sí afectar placebos; una unidad con peso positivo y exposición indirecta amenaza directamente la trayectoria sintética. La elegibilidad del *donor pool* debe decidirse con información institucional anterior a observar los efectos estimados.

### Estabilidad de la relación contrafactual {-}

El argumento requiere que las relaciones que permitieron a $X_0W^*$ reproducir a $X_1$ antes del tratamiento sean suficientemente estables después. No exige que todas las unidades tengan el mismo nivel o que sus resultados sean estacionarios. Sí exige que, en ausencia de tratamiento, no aparezca una ruptura específica de la unidad tratada que el sintético no habría compartido.

Un shock concurrente exclusivo de la unidad tratada —un desastre natural, otra reforma, una huelga o un cambio de medición— puede generar una brecha aun si la política de interés no tuvo efecto. Un shock concentrado en donantes con peso positivo también puede mover artificialmente el contrafactual. La cronología y la búsqueda de intervenciones simultáneas son parte del análisis causal, no notas contextuales opcionales.

::: {.boxinfo}
**Supuesto y credibilidad.** La lectura causal combina: tratamiento consistente y bien fechado; ausencia de anticipación; donantes no tratados y sin interferencia relevante; soporte pretratamiento; y estabilidad de la relación contrafactual frente a shocks posteriores. Ningún diagnóstico numérico verifica por sí solo este conjunto de condiciones.
:::

::: {.boxwarning}
**Buen preajuste no equivale a identificación.** Una gráfica pretratamiento cercana, un RMSPE pequeño o una regresión de las prebrechas con pendiente no significativa describen los datos observados. Una regresión de prebrechas **no constituye una prueba de validez**: puede tener poca potencia, no observa $Y(D=0)$ después del tratamiento y no descarta anticipación, interferencia, contaminación ni shocks concurrentes.
:::

::: {.boxexam}
**SC-T2.** Un control sintético reproduce casi exactamente a la ciudad tratada durante diez años anteriores. Después de la política aparece una brecha grande. Sin embargo, el principal donante comercia intensamente con la ciudad y, el mismo mes, solo la ciudad tratada cambia su sistema de medición. Evalúe por separado ajuste, interferencia/contaminación, shock concurrente e identificación. ¿Qué información adicional pediría antes de interpretar causalmente la brecha?
:::

## Medir el ajuste con RMSPE {-}

Sea la brecha estimada

$$
\widehat\alpha_{1t}=Y_{1t}-\sum_{j=2}^{J+1}w_j^*Y_{jt}.
$$

Antes del tratamiento, $Y_{1t}=Y_{1t}(D=0)$ bajo no anticipación. El **RMSPE pretratamiento** resume el tamaño de las discrepancias anteriores:

$$
RMSPE_{1}^{pre}=
\sqrt{\frac{1}{T_0}\sum_{t=1}^{T_0}
\widehat\alpha_{1t}^{2}}.
$$

Para $T_1$ periodos posteriores,

$$
RMSPE_{1}^{post}=
\sqrt{\frac{1}{T_1}\sum_{t=T_0+1}^{T_0+T_1}
\widehat\alpha_{1t}^{2}}.
$$

La razón

$$
R_1=\frac{RMSPE_{1}^{post}}{RMSPE_{1}^{pre}}
$$

mide cuánto se deteriora el ajuste después del tratamiento respecto de su error previo. Una razón alta es más informativa cuando el ajuste previo es sustantivamente bueno y cuando el RMSPE posterior refleja una brecha persistente. Deben reportarse el numerador, el denominador, la razón y la gráfica: una razón puede crecer por un RMSPE pre muy pequeño aunque la brecha posterior tenga magnitud modesta.

El RMSPE tampoco decide identificación. Resume discrepancias en las unidades del resultado, penaliza más los errores grandes y facilita comparaciones bajo una especificación común. No descarta que la brecha posterior se deba a un shock distinto del tratamiento.

## Placebos espaciales: ¿es excepcional la brecha tratada? {-}

Un placebo *in space* reasigna ficticiamente el tratamiento, una por una, a unidades no tratadas. Para cada unidad $i$ se repite el procedimiento con:

- la misma fecha de intervención;
- la misma especificación de predictores y ventanas;
- un conjunto donante que excluye a la unidad placebo;
- los mismos cálculos de $RMSPE_i^{pre}$, $RMSPE_i^{post}$ y $R_i$.

La pregunta es comparativa: ¿la ruptura de la unidad verdaderamente tratada es inusual frente a rupturas obtenidas donde no ocurrió la intervención? Mostrar las brechas placebo ayuda a reconocer si el método produce separaciones grandes de manera generalizada.

### Comparabilidad y la regla $5\times$ {-}

Una unidad placebo con ajuste anterior extremadamente malo no constituye una comparación informativa para una tratada bien ajustada. Por eso puede presentarse, además del conjunto completo, una comparación restringida a unidades que satisfacen

$$
RMSPE_i^{pre}\leq 5\,RMSPE_1^{pre}.
$$

En esta clase usaremos la regla $5\times$, **declarada antes de observar las razones placebo**. Primero se mostrarán todos los placebos y después el conjunto comparable. El umbral es una regla transparente de elegibilidad, no una constante universal ni una garantía de validez. Cambiarlo después de ver los resultados puede fabricar una apariencia de rareza; por eso conviene reportar sensibilidad a criterios razonables.

Entre $M$ asignaciones elegibles, incluida la unidad tratada, una proporción descriptiva puede calcularse como

$$
\widehat p_{pl}=
\frac{1+\sum_{i\in\mathcal E,\,i\neq 1}
\mathbf 1(R_i\geq R_1)}{M},
$$

donde $\mathcal E$ es el conjunto elegible. El “1” cuenta a la unidad tratada, cuya razón es al menos tan grande como ella misma. Con pocos donantes, esta proporción tiene resolución discreta: su mínimo es $1/M$.

::: {.boxwarning}
**La proporción placebo no es un p-valor asintótico convencional.** Es una medida de rareza dentro de un conjunto de reasignaciones y depende de la comparabilidad de las unidades, la especificación común y la regla de elegibilidad. Solo bajo un diseño de asignación e intercambiabilidad defendible admite una interpretación de randomización más fuerte. No proviene automáticamente de una distribución normal, no corrige un mal diseño y no debe reportarse como si fuera una prueba t usual.
:::

## Placebo temporal y sensibilidad al conjunto donante {-}

### Placebo temporal {-}

El placebo *in time* fija una fecha ficticia anterior a la intervención real y reestima el diseño usando únicamente información disponible antes de esa fecha ficticia. Si se pretende evaluar 2010 como placebo, los predictores, la selección de $V$ y la estimación de $W$ no pueden usar datos de 2010 en adelante. De lo contrario, el ejercicio contiene fuga de información.

Una brecha importante inmediatamente después de la fecha falsa debilita la interpretación de que la ruptura observada es exclusiva de la intervención real. Una ausencia de brecha es tranquilizadora, pero no prueba no anticipación ni elimina la posibilidad de un shock concurrente en la fecha verdadera. El placebo temporal complementa; no sustituye a los placebos espaciales.

### Leave-one-out {-}

El análisis *leave-one-out* retira, uno por uno, los donantes con peso positivo y vuelve a estimar el sintético. Su objetivo es revelar si la trayectoria y la brecha dependen casi totalmente de una unidad influyente. Si al excluir un donante la conclusión cambia de signo, magnitud o momento, esa dependencia debe aparecer en la interpretación.

No basta con recalcular la trayectoria usando los pesos originales después de borrar una unidad: los pesos restantes deben reestimarse bajo las restricciones convexas. Tampoco suele ser informativo retirar solo donantes con peso cero del ajuste principal. Un donante con peso alto merece atención, pero no es automáticamente problemático; el problema aparece cuando la evidencia es inestable o cuando ese donante es sustantivamente inadecuado o está contaminado.

Además del *leave-one-out*, pueden justificarse análisis que excluyan grupos expuestos a spillovers, regiones con mediciones incompatibles o unidades afectadas por shocks conocidos. Estas exclusiones deben motivarse por el diseño y no por el signo del efecto que producen.

::: {.boxinfo}
**Lectura avanzada.** Abadie, Diamond y Hainmueller (2010, 2015) desarrollan la construcción, los placebos y la transparencia del método para estudios comparativos. Abadie (2021) enfatiza diseño, selección del *donor pool* e interpretación. Extensiones como controles sintéticos aumentados permiten cierta extrapolación o corrección de sesgo, pero cambian el estimador y sus supuestos; no convierten automáticamente un mal soporte en evidencia causal.
:::

::: {.boxexam}
**SC-T3.** La unidad tratada tiene $RMSPE^{pre}=2$ y $RMSPE^{post}=18$. De 40 asignaciones espaciales, incluida la tratada, 32 cumplen la regla predeclarada $RMSPE_i^{pre}\leq 5RMSPE_1^{pre}$; dos de esas 32 tienen una razón post/pre al menos tan grande como la tratada. Calcule la razón de la tratada y la proporción placebo. Explique por qué esta proporción no es un p-valor asintótico convencional y proponga cómo un placebo temporal y un *leave-one-out* responden preguntas distintas.
:::

## Protocolo para interpretar una aplicación {-}

Una conclusión responsable debe avanzar en capas, sin sustituir una por otra:

1. **Pregunta y estimando.** Definir la unidad, el tratamiento, la fecha y si interesa $\widehat\tau_{1t}$ o un promedio posterior.
2. **Soporte y construcción.** Justificar el *donor pool*, los predictores y las ventanas; reportar $V$, $W$ y si la tratada puede aproximarse dentro de la envolvente convexa.
3. **Ajuste.** Mostrar trayectorias y predictores pretratamiento, junto con $RMSPE^{pre}$; reconocer discrepancias importantes.
4. **Magnitud.** Presentar $Y(D=1)$ frente a $\widehat Y(D=0)$, la brecha por periodo y un resumen posterior interpretable en las unidades originales.
5. **Rareza comparativa.** Mostrar placebos espaciales completos, declarar la regla $5\times$ antes del filtro y describir la proporción de razones al menos tan extremas.
6. **Sensibilidad.** Examinar placebo temporal, *leave-one-out* y exclusiones sustantivas por contaminación o shocks.
7. **Amenazas restantes.** Discutir anticipación, interferencia, cambios de medición y políticas concurrentes aunque los diagnósticos sean favorables.

La evidencia es más persuasiva cuando la unidad tratada tiene soporte y buen ajuste anterior, la brecha posterior es grande y persistente, pocos placebos comparables exhiben rupturas semejantes, el resultado sobrevive a retirar donantes influyentes y no existe una explicación concurrente convincente. Ninguna de estas piezas, aislada, reemplaza a las demás.

## Lecturas recomendadas {-}

- Abadie, A. y Gardeazabal, J. (2003). “The Economic Costs of Conflict: A Case Study of the Basque Country”, *American Economic Review*.
- Abadie, A., Diamond, A. y Hainmueller, J. (2010). “Synthetic Control Methods for Comparative Case Studies”, *Journal of the American Statistical Association*.
- Abadie, A., Diamond, A. y Hainmueller, J. (2015). “Comparative Politics and the Synthetic Control Method”, *American Journal of Political Science*.
- Abadie, A. (2021). “Using Synthetic Controls: Feasibility, Data Requirements, and Methodological Aspects”, *Journal of Economic Literature*.
