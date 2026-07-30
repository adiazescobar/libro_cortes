# Emparejamiento exacto — Introducción {#emparejamiento-exacto}

::: {.boxinfo}
**Metas de aprendizaje**

- Explicar qué sesgo intenta reducir el emparejamiento.
- Definir las celdas de comparación usando covariables pretratamiento.
- Reconocer los supuestos de no confusión condicional, soporte común y SUTVA.
- Distinguir el ATT original del efecto para la población emparejada.
- Entender por qué la dimensionalidad conduce al propensity score.
:::

::: {.boxinfo}
**Lecturas centrales**

- [Bernal y Peña — capítulo 6 (PDF)](lecturas/bernal-pena/capitulo-06.pdf)
- [Cunningham — capítulo 5: Matching and Subclassification](https://mixtape.scunning.com/05-matching_and_subclassification)
:::

## De la aleatorización a la selección en observables {-}

En un experimento aleatorizado, la asignación genera grupos comparables en promedio. En un estudio observacional, las personas eligen o reciben el tratamiento por razones que también pueden afectar sus resultados. La diferencia de medias observada mezcla entonces el efecto causal con selección:

\[
\begin{aligned}
&E[Y_i\mid D_i=1]-E[Y_i\mid D_i=0] \\
&=\underbrace{E[Y_i(D=1)-Y_i(D=0)\mid D_i=1]}_{ATT}
+\underbrace{E[Y_i(D=0)\mid D_i=1]-E[Y_i(D=0)\mid D_i=0]}_{\text{sesgo de selección}}.
\end{aligned}
\]

El problema es contrafactual: para las unidades tratadas observamos \(Y_i(D=1)\), pero no \(Y_i(D=0)\). El emparejamiento busca controles con covariables pretratamiento comparables para aproximar ese resultado faltante.

::: {.boxadvertencia}
**El método no crea un experimento**

Encontrar dos unidades con el mismo \(X\) no demuestra que sean iguales en motivación, habilidad u otras características no observadas. La interpretación causal depende de supuestos que deben defenderse con conocimiento institucional.
:::

## La idea: comparar dentro de celdas {-}

Suponga que \(X_i\) contiene educación y género, medidos antes del tratamiento. Cada combinación define una celda:

```text
Celda A: universitarias
Celda B: hombres con secundaria
Celda C: universitarios
Celda D: mujeres con secundaria
```

El emparejamiento exacto conserva una celda solo cuando contiene al menos una unidad tratada y una de control. Dentro de esa celda, el resultado promedio de los controles aproxima \(E[Y_i(D=0)\mid D_i=1,X_i=x]\), pero únicamente bajo los supuestos de identificación.

## Supuestos de identificación {-}

### 1. No confusión condicional {-}

La **no confusión condicional** —también llamada independencia condicional— exige que, dado el vector de covariables pretratamiento \(X_i\), la asignación no dependa de los resultados potenciales:

\[
\{Y_i(D=1),Y_i(D=0)\}\perp D_i\mid X_i.
\]

Para identificar el ATT basta una versión referida al resultado sin tratamiento, pero escribir ambos resultados potenciales ayuda a distinguir este supuesto de una afirmación sobre balance observado. Ningún procedimiento de matching puede comprobar que se midieron todos los factores de confusión.

### 2. Soporte común {-}

Para estimar el ATT, cada perfil \(x\) presente entre los tratados debe tener probabilidad positiva de permanecer sin tratamiento:

\[
P(D_i=0\mid X_i=x)>0
\quad\text{para los valores de }x\text{ observados entre tratados}.
\]

La condición de overlap más fuerte, \(0<P(D_i=1\mid X_i=x)<1\), permite considerar efectos para una población más amplia. En una muestra finita, el requisito práctico es concreto: debe existir al menos un control en cada celda tratada que se quiera conservar.

### 3. SUTVA y temporalidad {-}

SUTVA requiere tratamientos bien definidos y ausencia de interferencia relevante entre unidades. Además, \(X_i\) debe medirse antes del tratamiento. Emparejar por una variable causada por \(D_i\) puede bloquear parte del efecto o introducir sesgo, como vimos en el capítulo de malos controles.

::: {.boxcerebro}
**Tres preguntas antes de emparejar**

1. ¿Las covariables capturan causas comunes del tratamiento y del resultado?
2. ¿Fueron medidas antes del tratamiento?
3. ¿Hay controles comparables para los perfiles tratados relevantes?
:::

## Ejemplo manual: quién entra al estimando {-}

Considere seis personas. Educación y género son pretratamiento.

| ID | \(D\) | Educación | Género | Celda | \(Y\) | Estado del match |
|---|---:|---|---|---|---:|---|
| T1 | 1 | Universitaria | Mujer | A | 18 | Match con C1 |
| C1 | 0 | Universitaria | Mujer | A | 12 | Control de T1 |
| T2 | 1 | Secundaria | Hombre | B | 11 | Match con C2 |
| C2 | 0 | Secundaria | Hombre | B | 8 | Control de T2 |
| T3 | 1 | Universitaria | Hombre | C | 20 | **Sin match** |
| C3 | 0 | Secundaria | Mujer | D | 7 | Celda sin tratados |

Las celdas A y B pertenecen al soporte común observado. La celda C contiene un tratado sin match y la D no contribuye al ATT porque no contiene tratados.

Para la población emparejada \(\mathcal S=\{A,B\}\), el estimador es

\[
\widehat{ATT}_{\mathcal S}
=\frac{1}{N_{T,\mathcal S}}
\sum_{i:D_i=1,\,X_i\in\mathcal S}
\left(Y_i-\overline Y_{0,X_i}\right)
=\frac{(18-12)+(11-8)}{2}=4.5.
\]

Este 4.5 no estima automáticamente el ATT de los tres tratados originales. Estima el efecto promedio para los tratados de la **población emparejada**, es decir, aquellos cuyos perfiles están en el soporte común. Recuperar el efecto para T3 exigiría extrapolación o supuestos adicionales.

::: {.boxadvertencia}
**Descartar observaciones cambia la pregunta**

La pérdida de una unidad no es solo un problema de precisión. Si los tratados sin match son distintos, el estimando pasa del ATT original a un efecto para la subpoblación con soporte.
:::

## La maldición de la dimensionalidad {-}

Con variables discretas, el número de celdas posibles crece rápidamente. Si cada covariable tiene dos categorías:

| Covariables binarias | Celdas posibles |
|---:|---:|
| 1 | \(2^1=2\) |
| 2 | \(2^2=4\) |
| 5 | \(2^5=32\) |
| 10 | \(2^{10}=1{,}024\) |
| 20 | \(2^{20}=1{,}048{,}576\) |

Con covariables continuas, dos unidades rara vez coinciden exactamente. Aumentar \(X\) puede mejorar la plausibilidad de no confusión, pero también fragmenta el soporte y deja más tratados sin match. No existe una razón universal de controles por tratado que resuelva esta tensión: importa su distribución conjunta dentro de las celdas relevantes.

## Lo que el emparejamiento no resuelve {-}

::: {.boxadvertencia}
**Límites del diseño**

- **Confusión no observada:** balancear \(X\) no balancea automáticamente variables omitidas.
- **Malos controles:** incluir mediadores o variables postratamiento puede cambiar el estimando o crear sesgo.
- **Falta de soporte:** ninguna técnica fabrica contrafactuales donde no existen controles comparables.
- **Inferencia:** reutilizar controles, elegir entre empates y estimar distancias afecta la incertidumbre; matching no convierte los errores estándar convencionales en válidos por defecto.
:::

## Puente hacia el propensity score {-}

Rosenbaum y Rubin (1983) definieron el propensity score

\[
e(X_i)=P(D_i=1\mid X_i)
\]

como un **puntaje de balance**: bajo los supuestos apropiados, permite organizar comparaciones usando un escalar en lugar de todas las dimensiones de \(X_i\). Esta reducción motiva el capítulo de PSM, pero no elimina los problemas de diseño. Estimar \(e(X_i)\) no garantiza balance en la muestra y no garantiza identificación causal; ambos aspectos deben evaluarse y justificarse.

La clase empírica posterior comparará el matching exacto o restringido mediante `teffects nnmatch` y `ematch()` con el emparejamiento por propensity score. Aquí basta retener la lógica: **definir primero el estimando y el soporte; escoger después el algoritmo**.

## Preguntas tipo examen {-}

::: {.boxpregunta}
**Código:** EXACT-T1
**Tipo:** Soporte y estimando

Una base contiene cuatro celdas de covariables pretratamiento. Dos tienen tratados y controles, una contiene solo tratados y otra solo controles. Identifique qué observaciones contribuyen al estimador de matching exacto y explique si el resultado corresponde al ATT original o al efecto para una subpoblación.
:::

::: {.boxpregunta}
**Código:** EXACT-T2
**Tipo:** Selección de covariables

Para estimar el efecto de capacitación sobre salarios, una investigadora propone emparejar por edad, educación previa, salario previo y asistencia efectiva al curso. Clasifique las covariables según su temporalidad, identifique el posible mal control y explique qué supuesto seguiría siendo necesario aun si el balance observado fuera perfecto.
:::

## Para llevar a PSM {-}

- Matching exacto compara unidades dentro de celdas de covariables pretratamiento.
- La interpretación causal requiere no confusión condicional, soporte común y SUTVA.
- Excluir tratados sin match cambia la población objetivo.
- La dimensionalidad hace difícil formar celdas exactas y motiva un puntaje de balance.
- El capítulo \@ref(psm) estudia cómo construir, diagnosticar y usar ese puntaje sin confundir algoritmo con identificación.

## Lecturas recomendadas {-}

- **Rosenbaum y Rubin (1983)** — “The Central Role of the Propensity Score in Observational Studies for Causal Effects”, *Biometrika*, 70(1), 41–55.
- **Imbens y Rubin (2015)** — *Causal Inference for Statistics, Social, and Biomedical Sciences*, capítulo 12.
- **Angrist y Pischke (2009)** — *Mostly Harmless Econometrics*, capítulo 3.
