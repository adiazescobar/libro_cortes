# Prueba de Entrada {-}



<style type="text/css">
/* Estilos personalizados para la prueba de entrada */

.quiz-section {
  background-color: #f8f9fa;
  border-left: 4px solid #1F77B4;
  padding: 20px;
  margin: 20px 0;
  border-radius: 5px;
}

.quiz-section h2 {
  color: #1F77B4;
  margin-top: 0;
}

.question-box {
  background-color: white;
  padding: 15px;
  margin: 15px 0;
  border-radius: 8px;
  box-shadow: 0 2px 4px rgba(0,0,0,0.1);
}

.question-number {
  font-weight: bold;
  color: #1F77B4;
  font-size: 1.1em;
}

.intro-box {
  background: linear-gradient(135deg, #1F77B4 0%, #2E86AB 100%);
  color: white;
  padding: 25px;
  border-radius: 10px;
  margin-bottom: 30px;
}

.intro-box h3 {
  margin-top: 0;
  color: white;
}

.scoring-guide {
  background-color: #e8f4f8;
  border: 2px solid #1F77B4;
  padding: 20px;
  border-radius: 10px;
  margin-top: 30px;
}

.resources-box {
  background-color: #fff3cd;
  border-left: 4px solid #ffc107;
  padding: 20px;
  margin: 20px 0;
  border-radius: 5px;
}

.webex-check {
  margin-left: 10px;
}

/* Estilo para respuestas correctas e incorrectas */
.webex-correct {
  color: #28a745;
  font-weight: bold;
}

.webex-incorrect {
  color: #dc3545;
}

.tip-box {
  background-color: #d4edda;
  border-left: 4px solid #28a745;
  padding: 15px;
  margin: 15px 0;
  border-radius: 5px;
}

.warning-box {
  background-color: #f8d7da;
  border-left: 4px solid #dc3545;
  padding: 15px;
  margin: 15px 0;
  border-radius: 5px;
}
</style>

<div class="intro-box">
<h3>Bienvenido a la Prueba de Entrada</h3>

Esta prueba diagnostica tiene como objetivo evaluar tus conocimientos previos en estadistica, regresion lineal, causalidad y manejo basico de Stata.

**Instrucciones:**

- La prueba contiene **18 preguntas** divididas en 4 secciones
- Responde cada pregunta seleccionando la opcion correcta o escribiendo tu respuesta
- Al seleccionar una respuesta, recibiras retroalimentacion inmediata
- No hay limite de tiempo, pero intenta responder sin consultar materiales
- Al final encontraras una guia de puntuacion y recursos para repasar

**Tiempo estimado:** 15-20 minutos

</div>

---

## Seccion 1: Estadistica Basica {-}

<div class="quiz-section">

Esta seccion evalua conceptos fundamentales de estadistica descriptiva e inferencial que son esenciales para el analisis econometrico.

<div class="question-box">
<span class="question-number">Pregunta 1.</span> Si una variable X tiene media 50 y desviacion estandar 10, y sigue una distribucion normal, que porcentaje de las observaciones se encuentra entre 30 y 70?

<select class='webex-select'><option value='blank'></option><option value=''>68%</option><option value='answer'>95%</option><option value=''>99%</option><option value=''>50%</option></select>


<div class='webex-solution'><button>Ver explicacion</button>

En una distribucion normal:

- El 68% de los datos esta dentro de 1 desviacion estandar de la media
- El **95%** de los datos esta dentro de 2 desviaciones estandar de la media
- El 99.7% esta dentro de 3 desviaciones estandar

Como 30 = 50 - 2(10) y 70 = 50 + 2(10), estamos hablando de 2 desviaciones estandar, por lo tanto es el **95%**.

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 2.</span> El p-value (valor p) representa:

<select class='webex-select'><option value='blank'></option><option value=''>La probabilidad de que la hipotesis nula sea verdadera</option><option value='answer'>La probabilidad de observar datos tan extremos o mas, dado que H0 es verdadera</option><option value=''>La probabilidad de que la hipotesis alternativa sea falsa</option><option value=''>El nivel de significancia del test</option></select>


<div class='webex-solution'><button>Ver explicacion</button>

El **p-value** es la probabilidad de obtener un resultado tan extremo o mas extremo que el observado, **asumiendo que la hipotesis nula es verdadera**.

Es importante no confundirlo con:

- La probabilidad de que H0 sea verdadera (error comun)
- El nivel de significancia (alpha), que es un umbral que nosotros establecemos

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 3.</span> Rechazar la hipotesis nula cuando en realidad es verdadera se conoce como:

<select class='webex-select'><option value='blank'></option><option value='answer'>Error Tipo I</option><option value=''>Error Tipo II</option><option value=''>Sesgo de seleccion</option><option value=''>Error estandar</option></select>


<div class='webex-solution'><button>Ver explicacion</button>

- **Error Tipo I (Falso Positivo):** Rechazar H0 cuando es verdadera. La probabilidad de cometer este error es alpha (nivel de significancia).
- **Error Tipo II (Falso Negativo):** No rechazar H0 cuando es falsa. La probabilidad de cometer este error es beta.

Recuerda: El poder estadistico = 1 - beta

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 4.</span> Un intervalo de confianza del 95% significa que:

<select class='webex-select'><option value='blank'></option><option value=''>Hay 95% de probabilidad de que el parametro verdadero este en este intervalo especifico</option><option value='answer'>Si construyeramos 100 intervalos de esta manera, 95 de ellos contendrian el parametro verdadero</option><option value=''>El 95% de los datos esta dentro del intervalo</option><option value=''>Estamos 95% seguros de nuestra estimacion</option></select>


<div class='webex-solution'><button>Ver explicacion</button>

La interpretacion correcta es **frecuentista**: si repitieramos el muestreo muchas veces y calcularamos un IC del 95% cada vez, aproximadamente el 95% de esos intervalos contendrian el verdadero valor del parametro.

**No significa** que hay 95% de probabilidad de que el parametro este en un intervalo particular ya calculado.

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 5.</span> La varianza mide:

<select class='webex-select'><option value='blank'></option><option value=''>La tendencia central de los datos</option><option value='answer'>La dispersion de los datos alrededor de la media</option><option value=''>La relacion entre dos variables</option><option value=''>El valor mas frecuente</option></select>


<div class='webex-solution'><button>Ver explicacion</button>

La **varianza** mide que tan dispersos estan los datos respecto a su media. Se calcula como el promedio de las desviaciones al cuadrado:

$$Var(X) = E[(X - \mu)^2]$$

La desviacion estandar es simplemente la raiz cuadrada de la varianza, y tiene la ventaja de estar en las mismas unidades que la variable original.

</div>

</div>

</div>

---

## Seccion 2: Regresion Lineal {-}

<div class="quiz-section">

Esta seccion evalua tu comprension del modelo de regresion lineal, sus supuestos e interpretacion.

<div class="question-box">
<span class="question-number">Pregunta 6.</span> En el modelo $Y = \beta_0 + \beta_1 X + \varepsilon$, el coeficiente $\beta_1$ representa:

<select class='webex-select'><option value='blank'></option><option value=''>El valor de Y cuando X = 0</option><option value='answer'>El cambio esperado en Y por cada unidad adicional de X</option><option value=''>La correlacion entre X e Y</option><option value=''>La varianza de Y explicada por X</option></select>


<div class='webex-solution'><button>Ver explicacion</button>

En una regresion lineal simple:

- $\beta_0$ (intercepto): El valor esperado de Y cuando X = 0
- $\beta_1$ (pendiente): El **cambio esperado en Y** por cada unidad adicional en X, manteniendo todo lo demas constante

Matematicamente: $\beta_1 = \frac{\partial E[Y|X]}{\partial X}$

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 7.</span> Si $R^2 = 0.75$, esto significa que:

<select class='webex-select'><option value='blank'></option><option value=''>El coeficiente de correlacion es 0.75</option><option value=''>El modelo tiene 75% de probabilidad de ser correcto</option><option value='answer'>El 75% de la variacion en Y es explicada por las variables independientes</option><option value=''>El 75% de las observaciones estan correctamente predichas</option></select>


<div class='webex-solution'><button>Ver explicacion</button>

El **coeficiente de determinacion** $R^2$ indica la proporcion de la varianza de la variable dependiente que es explicada por el modelo.

$$R^2 = 1 - \frac{SCR}{SCT} = \frac{SCE}{SCT}$$

Donde:

- SCR = Suma de Cuadrados de los Residuos
- SCT = Suma de Cuadrados Total
- SCE = Suma de Cuadrados Explicada

Un $R^2 = 0.75$ indica que el modelo explica el 75% de la variabilidad observada en Y.

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 8.</span> Cual de las siguientes NO es un supuesto del modelo clasico de regresion lineal (OLS)?

<select class='webex-select'><option value='blank'></option><option value=''>Los errores tienen media cero</option><option value=''>Los errores son homocedasticos</option><option value=''>No hay multicolinealidad perfecta</option><option value='answer'>Los errores deben seguir una distribucion uniforme</option></select>


<div class='webex-solution'><button>Ver explicacion</button>

Los supuestos clasicos de OLS incluyen:

1. Linealidad en parametros
2. Muestreo aleatorio
3. No hay multicolinealidad perfecta
4. Media condicional cero de los errores: $E[\varepsilon|X] = 0$
5. Homocedasticidad: $Var(\varepsilon|X) = \sigma^2$
6. (Para inferencia) Normalidad de los errores

**No se requiere** que los errores sigan una distribucion uniforme. De hecho, para inferencia en muestras pequenas se asume normalidad.

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 9.</span> Si el p-value asociado a un coeficiente es 0.03, esto significa que:

<select class='webex-select'><option value='blank'></option><option value=''>El coeficiente es significativo al 1%</option><option value='answer'>El coeficiente es significativo al 5%</option><option value=''>El coeficiente no es significativo</option><option value=''>Hay 3% de probabilidad de que el coeficiente sea correcto</option></select>


<div class='webex-solution'><button>Ver explicacion</button>

Un p-value de 0.03 significa:

- **Es significativo al 5%** (porque 0.03 < 0.05)
- **Es significativo al 10%** (porque 0.03 < 0.10)
- **NO es significativo al 1%** (porque 0.03 > 0.01)

Rechazamos H0 cuando el p-value es menor que nuestro nivel de significancia elegido.

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 10.</span> En un modelo $\log(Y) = \beta_0 + \beta_1 X + \varepsilon$, el coeficiente $\beta_1$ se interpreta aproximadamente como:

<select class='webex-select'><option value='blank'></option><option value=''>El cambio absoluto en Y por unidad de X</option><option value=''>El cambio en log(Y) en terminos absolutos</option><option value='answer'>El cambio porcentual en Y por cada unidad adicional de X</option><option value=''>La elasticidad de Y respecto a X</option></select>


<div class='webex-solution'><button>Ver explicacion</button>

Cuando la variable dependiente esta en logaritmos (modelo log-lineal o semi-log):

$$\log(Y) = \beta_0 + \beta_1 X$$

El coeficiente $\beta_1$ se interpreta como: **un aumento de una unidad en X esta asociado con un cambio de aproximadamente $\beta_1 \times 100$% en Y**.

Para cambios pequenos: $\frac{\Delta Y}{Y} \approx \beta_1 \cdot \Delta X$

Nota: Para la elasticidad necesitariamos $\log(Y) = \beta_0 + \beta_1 \log(X)$.

</div>

</div>

</div>

---

## Seccion 3: Causalidad {-}

<div class="quiz-section">

Esta seccion es fundamental para el curso. Evalua tu comprension de la diferencia entre correlacion y causalidad, y los conceptos basicos de inferencia causal.

<div class="question-box">
<span class="question-number">Pregunta 11.</span> La observacion de que "los paises con mayor consumo de chocolate tienen mas premios Nobel per capita" es un ejemplo de:

<select class='webex-select'><option value='blank'></option><option value=''>Causalidad directa</option><option value=''>Causalidad inversa</option><option value='answer'>Correlacion espuria (correlacion sin causalidad)</option><option value=''>Efecto placebo</option></select>


<div class='webex-solution'><button>Ver explicacion</button>

Este es un ejemplo clasico de **correlacion espuria**. Aunque existe una correlacion estadistica, no hay razon para creer que comer chocolate cause que las personas ganen premios Nobel.

Posibles explicaciones:

- **Variable confusora**: El nivel de desarrollo economico de un pais podria afectar tanto el consumo de chocolate como el numero de premios Nobel
- **Coincidencia estadistica**: Con suficientes variables, algunas correlaran por azar

Recuerda: **Correlacion no implica causalidad!**

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 12.</span> El sesgo de seleccion ocurre cuando:

<select class='webex-select'><option value='blank'></option><option value=''>La muestra es muy pequena</option><option value=''>Los datos tienen errores de medicion</option><option value='answer'>Los individuos tratados son sistematicamente diferentes de los no tratados</option><option value=''>El modelo tiene muchas variables</option></select>


<div class='webex-solution'><button>Ver explicacion</button>

El **sesgo de seleccion** surge cuando los individuos que reciben el tratamiento difieren sistematicamente de los que no lo reciben, en caracteristicas que tambien afectan el resultado.

Ejemplo: Si comparamos ingresos de universitarios vs. no universitarios, las diferencias observadas no solo reflejan el efecto de la educacion, sino tambien diferencias previas en habilidad, motivacion, contexto familiar, etc.

$$E[Y_0|D=1] \neq E[Y_0|D=0]$$

Donde $Y_0$ es el resultado potencial sin tratamiento.

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 13.</span> La asignacion aleatoria del tratamiento es importante porque:

<select class='webex-select'><option value='blank'></option><option value=''>Aumenta el tamano de la muestra</option><option value=''>Reduce los costos del estudio</option><option value='answer'>Hace que los grupos de tratamiento y control sean comparables en expectativa</option><option value=''>Elimina los errores de medicion</option></select>


<div class='webex-solution'><button>Ver explicacion</button>

La **aleatorizacion** es el estandar de oro en inferencia causal porque:

1. **Elimina el sesgo de seleccion**: En expectativa, los grupos tratado y control son identicos en todas las caracteristicas (observables y no observables)

2. **Balancea confusores**: Tanto los factores conocidos como los desconocidos se distribuyen equitativamente

3. **Permite identificar efectos causales**: La diferencia simple de medias estima el efecto causal promedio:
$$E[Y|D=1] - E[Y|D=0] = ATE$$

Por eso los experimentos aleatorizados (RCT) son tan valorados en la evaluacion de impacto.

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 14.</span> El contrafactual se refiere a:

<select class='webex-select'><option value='blank'></option><option value=''>Los datos que se perdieron en el estudio</option><option value=''>El grupo de control</option><option value='answer'>Lo que habria ocurrido en ausencia del tratamiento</option><option value=''>Los efectos secundarios del tratamiento</option></select>


<div class='webex-solution'><button>Ver explicacion</button>

El **contrafactual** es el concepto central de la inferencia causal. Se refiere a lo que **habria pasado** a una unidad si no hubiera recibido el tratamiento.

El problema fundamental de la inferencia causal es que **nunca observamos el contrafactual** para un individuo especifico (no podemos ver a la misma persona con y sin tratamiento al mismo tiempo).

Para el individuo $i$:

- Observamos: $Y_i = D_i \cdot Y_{1i} + (1-D_i) \cdot Y_{0i}$
- El contrafactual para los tratados es $Y_{0i}$
- El contrafactual para los no tratados es $Y_{1i}$

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 15.</span> El principal problema al comparar simplemente los resultados de participantes de un programa vs. no participantes es:

<select class='webex-select'><option value='blank'></option><option value=''>La diferencia en tamano de los grupos</option><option value=''>Los errores de medicion en los datos</option><option value='answer'>La autoseleccion (los que participan pueden ser diferentes de los que no)</option><option value=''>La falta de datos</option></select>


<div class='webex-solution'><button>Ver explicacion</button>

La **autoseleccion** es un problema critico porque las personas que eligen participar en un programa probablemente difieren de las que no participan en:

- Motivacion
- Informacion disponible
- Habilidades
- Circunstancias personales

Ejemplo: Las personas que se inscriben en un programa de capacitacion laboral probablemente son mas motivadas o tienen mas informacion sobre oportunidades, lo que afectaria sus resultados independientemente del programa.

Esta es la razon por la que necesitamos metodos como:

- Experimentos aleatorios
- Diferencias en diferencias
- Variables instrumentales
- Regression discontinuity

</div>

</div>

</div>

---

## Seccion 4: Stata {-}

<div class="quiz-section">

Esta seccion evalua tu familiaridad basica con el software Stata, que usaremos durante todo el curso.

<div class="question-box">
<span class="question-number">Pregunta 16.</span> En Stata, cual comando usarias para ver las primeras observaciones de tu base de datos?

<select class='webex-select'><option value='blank'></option><option value=''>view</option><option value='answer'>browse o list</option><option value=''>show</option><option value=''>display</option></select>


<div class='webex-solution'><button>Ver explicacion</button>

En Stata hay varias formas de ver los datos:

- **`browse`**: Abre el visor de datos (Data Editor en modo lectura)
- **`list`**: Muestra las observaciones en la ventana de resultados
- **`list in 1/10`**: Muestra solo las primeras 10 observaciones

Ejemplo:
```
browse
list in 1/5
list var1 var2 in 1/10
```

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 17.</span> En una regresion en Stata, si el coeficiente de la variable `x` es 2.35, esto significa que:

<select class='webex-select'><option value='blank'></option><option value=''>La variable x tiene un p-value de 2.35</option><option value='answer'>Por cada unidad adicional de x, Y aumenta en 2.35 unidades (en promedio)</option><option value=''>El R-cuadrado del modelo es 2.35</option><option value=''>Hay 2.35 observaciones con esa variable</option></select>


<div class='webex-solution'><button>Ver explicacion</button>

El coeficiente en una regresion lineal indica el **cambio en la variable dependiente** asociado con un cambio de una unidad en la variable independiente, manteniendo las demas variables constantes.

Si $\hat{\beta}_x = 2.35$, interpretamos:

> "Un aumento de una unidad en X esta asociado con un aumento promedio de 2.35 unidades en Y, ceteris paribus."

En Stata, el output de regresion muestra:

- `Coef.`: El valor del coeficiente
- `Std. Err.`: El error estandar
- `t`: El estadistico t
- `P>|t|`: El p-value

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 18.</span> Cual es el comando correcto en Stata para realizar una regresion de la variable dependiente Y sobre las variables independientes X1 y X2?

<input class='webex-solveme nospaces ignorecase' size='25' data-answer='["reg Y X1 X2","regress Y X1 X2"]'/>


<div class='webex-solution'><button>Ver explicacion</button>

En Stata, el comando para regresion lineal es:

```
regress varY varX1 varX2 varX3
```

O de forma abreviada:

```
reg varY varX1 varX2 varX3
```

La sintaxis general es:
```
reg variable_dependiente variables_independientes [if] [in] [, opciones]
```

Ejemplo completo:
```
reg salario educacion experiencia edad, robust
```

</div>

</div>

</div>

---

## Guia de Puntuacion {-}

<div class="scoring-guide">

### Como evaluar tu desempeno {-}

Cuenta el numero de respuestas correctas en cada seccion:

| Seccion | Preguntas | Tu puntaje |
|---------|-----------|------------|
| Estadistica Basica | 5 | __ / 5 |
| Regresion Lineal | 5 | __ / 5 |
| Causalidad | 5 | __ / 5 |
| Stata | 3 | __ / 3 |
| **Total** | **18** | **__ / 18** |

### Interpretacion {-}

- **16-18 correctas (90-100%):** Excelente! Tienes una base solida para el curso.
- **13-15 correctas (70-89%):** Buen nivel. Repasa los temas donde tuviste errores.
- **10-12 correctas (55-69%):** Nivel aceptable. Te recomendamos repasar los conceptos basicos.
- **Menos de 10 correctas (<55%):** Necesitas reforzar los prerequisitos. Consulta los recursos abajo.

</div>

---

## Recursos para Repasar {-}

<div class="resources-box">

### Si necesitas repasar Estadistica Basica {-}

- **Khan Academy - Estadistica y Probabilidad:** [https://es.khanacademy.org/math/statistics-probability](https://es.khanacademy.org/math/statistics-probability)
- **OpenIntro Statistics:** Libro gratuito disponible en [https://www.openintro.org/book/os/](https://www.openintro.org/book/os/)
- **Capitulos 1-3** de cualquier libro introductorio de estadistica

### Si necesitas repasar Regresion Lineal {-}

- **Wooldridge, J.M. "Introductory Econometrics"** - Capitulos 1-4
- **Khan Academy - Regresion:** [https://es.khanacademy.org/math/statistics-probability/describing-relationships-quantitative-data](https://es.khanacademy.org/math/statistics-probability/describing-relationships-quantitative-data)
- **Clase de Econometria Basica de Ben Lambert:** [https://www.youtube.com/playlist?list=PLwJRxp3blEvZyQBTTOMFRP_TDaSdly3gU](https://www.youtube.com/playlist?list=PLwJRxp3blEvZyQBTTOMFRP_TDaSdly3gU)

### Si necesitas repasar Causalidad {-}

- **Cunningham, Scott. "Causal Inference: The Mixtape"** - Capitulos 1-3: [https://mixtape.scunning.com/](https://mixtape.scunning.com/)
- **Angrist & Pischke. "Mastering Metrics"** - Introduccion
- **Videos de Nick Huntington-Klein:** [https://www.youtube.com/playlist?list=PLcTBLulJV_AIuXCxr__V8XAzWZosMQIfW](https://www.youtube.com/playlist?list=PLcTBLulJV_AIuXCxr__V8XAzWZosMQIfW)

### Si necesitas aprender/repasar Stata {-}

- **UCLA IDRE Stata Learning Modules:** [https://stats.oarc.ucla.edu/stata/modules/](https://stats.oarc.ucla.edu/stata/modules/)
- **Stata Video Tutorials:** [https://www.stata.com/links/video-tutorials/](https://www.stata.com/links/video-tutorials/)
- **Cameron & Trivedi. "Microeconometrics Using Stata"** - Capitulo 1

</div>

<div class="tip-box">

### Consejos para el Curso {-}

1. **Practica con datos reales:** La mejor forma de aprender econometria es aplicandola.

2. **No memorices, entiende:** Los conceptos de causalidad son mas importantes que las formulas.

3. **Haz los ejercicios:** Los talleres de clase son oportunidades para practicar antes de los examenes.

4. **Pregunta:** Si algo no esta claro, pregunta en clase o en horas de oficina.

5. **Forma grupos de estudio:** Discutir los conceptos con companeros ayuda a consolidar el aprendizaje.

</div>

---

<center>
**Buena suerte en el curso!**

Si tienes dudas sobre algun tema de esta prueba, no dudes en consultarlas durante la primera semana de clase.
</center>
