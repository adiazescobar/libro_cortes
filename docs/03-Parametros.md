# Parámetros Causales (Teoría) {#parametros-causales-teoria}

::: {.boxinfo}
## Objetivos de aprendizaje {-}

- Entender qué es un **resultado potencial**  
- Diferenciar entre **ATE**, **ATT**, **ATU** y el **estimador naïve**  
- Comprender la lógica del **sesgo de selección** y su relación con los **contrafactuales**
:::

::: {.boxnote}
## Lecturas recomendadas {-}

- **Lectura 1:** *The Credibility Revolution* - Angrist y Pischke (2010)  
  [Enlace al artículo](https://www.aeaweb.org/articles?id=10.1257/jep.24.2.3)  
- **Lectura 2:** Bernal y Peña – Capítulo 2  
- **Lectura 3:** Bernal y Peña – Capítulo 3  
:::

## Resultados potenciales {-}

Para cada unidad \( i \), existen dos posibles resultados:

- \( Y_i(D=1) \): resultado si **recibe tratamiento**  
- \( Y_i(D=0) \): resultado si **no recibe tratamiento**

Pero solo observamos uno de ellos:

\[
Y_i = D_i \cdot Y_i(D=1) + (1 - D_i) \cdot Y_i(D=0)
\]

Donde:  
- \( D_i = 1 \) si la unidad fue tratada  
- \( D_i = 0 \) si no fue tratada

---

## Parámetros de interés {-}

### Efecto Promedio del Tratamiento (ATE) {-}

\[
ATE = \mathbb{E}[Y_i(D=1) - Y_i(D=0)]
\]

### Efecto Promedio del Tratamiento sobre los Tratados (ATT) {-}

\[
ATT = \mathbb{E}[Y_i(D=1) - Y_i(D=0) \mid D_i = 1]
\]

> **Nota:** No confundir ATT con **ITT (Intention-to-Treat)**^[El ITT (Intention-to-Treat) es el efecto promedio del **tratamiento asignado**, sin importar si la unidad efectivamente recibió el tratamiento. Se calcula como: \(ITT = \mathbb{E}[Y_i \mid Z_i = 1] - \mathbb{E}[Y_i \mid Z_i = 0]\), donde \(Z_i\) es la asignación al tratamiento. Es especialmente útil en experimentos con incumplimiento (non-compliance), donde algunos asignados al tratamiento no lo reciben, o viceversa.]

### Efecto Promedio del Tratamiento sobre los No Tratados (ATU) {-}

\[
ATU = \mathbb{E}[Y_i(D=1) - Y_i(D=0) \mid D_i = 0]
\]

### Estimador naïve (comparación directa de medias) {-}

\[
\mathbb{E}[Y_i \mid D_i = 1] - \mathbb{E}[Y_i \mid D_i = 0]
\]

---

## ¿Por qué no es suficiente el estimador naïve? {-}

El estimador naïve asume implícitamente que:

\[
\mathbb{E}[Y_i(D=0) \mid D_i = 1] = \mathbb{E}[Y_i(D=0) \mid D_i = 0]
\]

Es decir, que los grupos tratados y no tratados son **comparables** en su resultado contrafactual.  
Este supuesto es **poco realista** si la asignación al tratamiento está relacionada con factores que afectan el resultado, como motivación, ingresos o necesidad.

---

## Sesgo de selección {-}

El estimador naïve se puede descomponer como:

\[
\underbrace{\mathbb{E}[Y_i \mid D_i = 1] - \mathbb{E}[Y_i \mid D_i = 0]}_{\text{Naïve}} = \underbrace{\mathbb{E}[Y_i(D=1) - Y_i(D=0) \mid D_i = 1]}_{ATT} + \underbrace{\mathbb{E}[Y_i(D=0) \mid D_i = 1] - \mathbb{E}[Y_i(D=0) \mid D_i = 0]}_{\text{Sesgo de selección}}
\]

El **sesgo de selección** mide si los tratados y controles eran diferentes **antes** del tratamiento:

\[
\text{Sesgo} = \mathbb{E}[Y_i(D=0) \mid D_i = 1] - \mathbb{E}[Y_i(D=0) \mid D_i = 0]
\]

> Si los tratados habrían tenido mejores resultados **incluso sin tratamiento** (por ejemplo, por mayor motivación), entonces el sesgo es **positivo** y el estimador naïve **sobreestima** el verdadero efecto del tratamiento.

---

## El supuesto de independencia (o ignorabilidad) {-}

Para que el estimador naïve identifique correctamente el efecto causal, necesitamos que se cumpla el **supuesto de independencia**:

\[
(Y_i(D=1), Y_i(D=0)) \perp D_i
\]

Este supuesto establece que los **resultados potenciales son independientes de la asignación al tratamiento**. En otras palabras:

- Recibir o no el tratamiento **no está relacionado** con lo que habría pasado en cualquiera de los dos escenarios
- Los grupos tratados y no tratados son **comparables** en todos los aspectos relevantes
- No hay **selección** en quién recibe el tratamiento basada en características que también afectan el resultado

### Independencia condicional {-}

En la práctica, rara vez tenemos independencia incondicional. Más comúnmente trabajamos con **independencia condicional** (o ignorabilidad condicional):

\[
(Y_i(D=1), Y_i(D=0)) \perp D_i \mid X_i
\]

Esto significa que, **condicionando en ciertas variables observables** \(X_i\), la asignación al tratamiento es independiente de los resultados potenciales.

### ¿Cuándo se cumple independencia? {-}

El supuesto de independencia se cumple automáticamente cuando:

1. **Asignación aleatoria del tratamiento** (experimentos controlados aleatorizados)
   - La aleatorización garantiza que \(D_i\) es independiente de todas las características, observables y no observables

2. **Diseños cuasi-experimentales bien implementados**
   - Variables instrumentales
   - Regresión discontinua
   - Diferencias-en-diferencias (con supuestos adicionales)

3. **"Selección en observables"** (con supuestos fuertes)
   - Si controlamos por **todas** las variables que afectan tanto \(D_i\) como \(Y_i\)
   - Requiere tener datos muy completos y conocer el proceso de selección

### ¿Cuándo se viola? {-}

El supuesto de independencia se viola cuando hay **selección** en el tratamiento:

- **Auto-selección**: Los individuos eligen participar basándose en ganancias esperadas
  - Ejemplo: Solo los más motivados se inscriben en un programa de capacitación

- **Selección administrativa**: Alguien asigna el tratamiento basándose en características
  - Ejemplo: Un programa social focalizado en los más pobres

- **Variables omitidas**: Hay factores no observados que afectan tanto \(D_i\) como \(Y_i\)
  - Ejemplo: Habilidad innata afecta tanto la probabilidad de ir a la universidad como los ingresos futuros

> **Recordatorio:** Cuando se viola independencia, el estimador naïve produce estimaciones sesgadas. Por eso la econometría moderna se enfoca en diseños que garanticen (o aproximen) este supuesto.

---

::: {.boxejercicio .green title="🧠 Pausa activa: ¿Dónde está el contrafactual?"}

## Ejercicio en clase: resultados potenciales y sesgo de selección {-}

Supongamos que tenemos una muestra de 8 individuos. Algunos recibieron tratamiento (\(D = 1\)) y otros no (\(D = 0\)). Cada persona tiene dos resultados potenciales:

- \(Y(1)\): lo que obtendría si recibe el tratamiento  
- \(Y(0)\): lo que obtendría si no lo recibe

Pero solo observamos **uno** de esos dos valores:  
\[
Y_i = D_i \cdot Y_i(1) + (1 - D_i) \cdot Y_i(0)
\]

Datos

| i  | yd0 | yd1 | D |
|----|-----|-----|---|
| 1  | 10  | 12  | 1 |
| 2  | 4   | 5   | 0 |
| 3  | 9   | 10  | 1 |
| 4  | 10  | 11  | 1 |
| 5  | 5   | 6   | 0 |
| 6  | 3   | 2   | 0 |
| 7  | 12  | 11  | 1 |
| 8  | 5   | 7   | 0 |
---

*🎯 Preguntas para discutir en grupo*

1. ¿Cuál es el **contrafactual** que NO podemos observar para cada individuo?
2. Calcula el **efecto promedio del tratamiento sobre los tratados (ATT)**.
3. Calcula el **estimador naïve**:  
   \[
   \mathbb{E}[Y \mid D = 1] - \mathbb{E}[Y \mid D = 0]
   \]
4. ¿Cuál es el **sesgo de selección** entre ambos estimadores?
5. Reflexiona: ¿por qué hay sesgo en este ejemplo? ¿Qué supuesto implícito está fallando?

📌 Pista:

Los individuos tratados tienen mejores valores de \(Y(0)\) (lo que habrían obtenido sin tratamiento) que los no tratados.  
¿Es válido entonces comparar directamente los promedios observados entre grupos?

:::


## Sesgo en comparaciones antes-después (sin grupo de control) {-}

Una estrategia común es comparar el **resultado promedio antes y después del tratamiento** en el mismo grupo de individuos tratados:

\[
\text{Estimador Antes-Después} = \mathbb{E}[Y_{t = 1} \mid D = 1] - \mathbb{E}[Y_{t = 0} \mid D = 1]
\]

Este estimador es observable, pero **no necesariamente causal**, porque no tenemos el contrafactual de lo que habría pasado en \( t = 1 \) sin tratamiento.

¿Qué observamos?

- En \( t = 1 \), observamos \( Y(D=1) \): el resultado **con tratamiento**  
- En \( t = 0 \), observamos \( Y(D=0) \): el resultado **sin tratamiento**

Para identificar el efecto del tratamiento, lo que quisiéramos conocer es:

\[
Y(D=0) \text{ en } t = 1
\]

Es decir, **¿qué habría pasado en el periodo \( t = 1 \) si no hubiéramos tratado a nadie?** El efecto causal real para una unidad sería:

\[
Y(D=1) - Y(D=0) \text{ en el mismo periodo } t = 1
\]

Pero en el diseño antes-después usamos \( Y(D=0) \) del periodo anterior como sustituto de ese contrafactual. Entonces, el sesgo es:

\[
\text{Sesgo} = \underbrace{\mathbb{E}[Y(D=0) \text{ en } t = 1]}_{\text{contrafactual deseado}} - \underbrace{\mathbb{E}[Y(D=0) \text{ en } t = 0]}_{\text{valor observado como "antes"}}
\]

Este sesgo aparece si el resultado habría cambiado con el tiempo incluso sin el tratamiento.

🧪 Ejemplo ilustrativo

| Año  | Resultado observado | Tratamiento |
|------|---------------------|-------------|
|2019  | 6                   | 0 (antes)   |
|2020  | 9                   | 1 (después) |

- Estimador antes-después:  
  \[
  9 - 6 = 3
  \]
- Pero supongamos que, sin tratamiento, el resultado en 2020 habría sido 8  
  \[
  \Rightarrow Y(D=0) \text{ en } 2020 = 8
  \]
- Entonces el efecto causal verdadero es:  
  \[
  9 - 8 = 1
  \]
- Y el sesgo de selección por tiempo es:
  \[
  3 - 1 = 2
  \]



El diseño antes-después asume que no habría cambio en el tiempo sin tratamiento. Este supuesto es **muy fuerte** y raramente cierto. Por eso, necesitamos un grupo de control que nos ayude a estimar \( \mathbb{E}[Y(D=0) \text{ en } t = 1] \).

> En otras palabras, sin grupo de control **no podemos saber si el cambio fue por el tratamiento o por el tiempo**.

---

## SUTVA: Supuesto de Valor Estable del Tratamiento Unitario {-}

El **SUTVA (Stable Unit Treatment Value Assumption)** es un supuesto fundamental en inferencia causal que establece dos condiciones:

1. **No interferencia**: El resultado potencial de una unidad no se ve afectado por el estado de tratamiento de otras unidades
   - Es decir: \(Y_i(D_1, D_2, ..., D_n) = Y_i(D_i)\)
   - El tratamiento del vecino no afecta mi resultado

2. **Consistencia del tratamiento**: No hay versiones diferentes del tratamiento
   - Es decir: Si \(D_i = 1\), entonces \(Y_i = Y_i(1)\)
   - Un curso online de 8 semanas debe ser el mismo para todos los tratados

### ¿Por qué es importante SUTVA? {-}

SUTVA nos permite:
- Definir claramente los resultados potenciales \(Y_i(1)\) y \(Y_i(0)\)
- Comparar grupos de forma válida
- Estimar efectos causales sin ambigüedad

### ¿Cuándo se viola SUTVA? {-}

**Ejemplos de violación por interferencia:**
- Vacunación: si mis vecinos están vacunados, mi riesgo de contagio disminuye
- Redes sociales: si mis amigos usan una app, mi experiencia cambia
- Efectos de equilibrio general: un programa de empleo puede afectar salarios de no participantes

**Ejemplos de violación por inconsistencia del tratamiento:**
- Un programa educativo implementado de forma diferente en distintas escuelas
- Medicamentos con diferentes dosis o formas de administración

<div class="figure" style="text-align: center">
<img src="img/sutva_meme.png" alt="Cuando asumes SUTVA pero hay efectos de derrame (spillovers)" width="60%" />
<p class="caption">(\#fig:sutva-meme)Cuando asumes SUTVA pero hay efectos de derrame (spillovers)</p>
</div>

> **Reflexión:** ¿Se te ocurre algún ejemplo de tu vida cotidiana donde SUTVA claramente **no se cumpla**?

---

::: {.boxvideo .green title="🎥 Videos recomendados:"}


Estos videos ayudan a reforzar visualmente los conceptos de **resultados potenciales**, **contrafactuales** y **sesgo en el diseño antes-después**.


<iframe width="100%" height="315" src="https://www.youtube.com/embed/ln5LBKiF8hE" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>


<iframe width="100%" height="315" src="https://www.youtube.com/embed/iPBV3BlV7jk" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

:::




---

::: {.boxnote}

🛠️ 💬 **PROMPT DE CHATGPT PARA REFLEXIÓN PROFUNDA**

Estás escribiendo el apartado metodológico de un artículo donde se implementa un programa de formación técnica para jóvenes. Tu grupo de tratamiento incluye individuos que aplicaron y fueron aceptados. No tienes grupo de control explícito, pero tienes datos de resultados antes y después.

📌 Instrucciones:

Escribe a ChatGPT usando el siguiente mensaje:

Actúa como mi tutor metodológico. No quiero que simplemente expliques los conceptos, sino que me hagas preguntas, desafíes mis supuestos, y me ayudes a razonar paso a paso como si estuviéramos en una asesoría.

🧪 Contexto: Quiero evaluar el efecto de un programa de formación técnica para jóvenes. Tengo datos de ingreso mensual antes y después del programa, pero solo para quienes participaron. Estoy pensando en calcular:

    Ȳ_despues - Ȳ_antes

para reportar el efecto del programa.

Quiero que me ayudes a pensar críticamente si esta estrategia identifica un efecto causal. Por favor:

1. Guíame para identificar cuál es el verdadero contrafactual que estoy ignorando.
2. Pregúntame qué estoy asumiendo implícitamente.
3. Explórame en qué condiciones este estimador funcionaría bien.
4. Hazme reflexionar sobre qué sesgos podrían surgir si los ingresos hubieran aumentado igual sin el programa.
5. Llévame a conectar este ejemplo con los conceptos de Y(D=1), Y(D=0), ATT, ATE y el estimador naïve.

⚠️ Importante: no me lo des todo resuelto. Quiero que me vayas preguntando cosas, como haría un buen profe. Quiero pensar, no solo escuchar. Hazlo interactivo.
:::



---
