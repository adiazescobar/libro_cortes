
# Stata para Principiantes 

::: {.boxinfo}
Puedes consultar el **Stata Cheat Sheet** completo aquí: [https://geocenter.github.io/StataTraining/pdf/AllCheatSheets.pdf](https://geocenter.github.io/StataTraining/pdf/AllCheatSheets.pdf)
:::

## Macros en Stata {-}

Las **macros** en Stata son herramientas para almacenar texto que luego puede reutilizarse en comandos posteriores. No son variables, no almacenan datos numéricos como tal, sino texto que puede ser evaluado o invocado más adelante. Se usan con frecuencia para simplificar código, automatizar tareas repetitivas, o construir loops.

Stata tiene dos tipos principales de macros:

- `local`: válidas solo dentro del entorno donde se definieron (por ejemplo, dentro de un programa o loop).
- `global`: válidas en todo el entorno de trabajo mientras dure la sesión (desaconsejadas para la mayoría de tareas por riesgo de sobreescritura accidental).

### Macro local: definición y expansión {-}

```stata
local uno 1
display `uno'
```

Esto imprimirá `1` en la consola. La macro `uno` se expande y sustituye por su contenido (`1`) antes de ejecutar el comando.



### Evaluar expresiones dentro de macros {-}

Si queremos que Stata **evalue una expresión**, usamos el signo igual `=` luego del nombre del macro.

```stata
local suma = 2 + 2
display `suma'
```

Stata calcula `2 + 2` y guarda el resultado `4` como texto dentro de `suma`. Al hacer `display`, se imprime el número 4.



### Macro con texto {-}

```stata
local saludo "¡Hola, mundo!"
display "`saludo'"
```

Esto mostrará:

```
¡Hola, mundo!
```

Ojo: cuando una macro contiene texto, siempre encierra su invocación entre comillas para evitar errores de interpretación.



### Macro global: uso y precaución {-}

```stata
global pi 3.1416
display $pi
```

Al usar `global`, la invocación se hace con **signo dólar** (`$`). El contenido se mantiene accesible en toda la sesión.

> ⚠️ Se recomienda evitar nombres obvios en `global`  ya que puede generar conflictos si se reutilizan nombres con comandos establecidos. 



### Scalar vs. Macro {-}

Un **scalar** almacena valores numéricos (reales), no texto. Se usa para cálculos matemáticos, estadísticas o comparaciones numéricas.

```stata
scalar x = 2 + 3
display x
```

Esto imprime `5`.

```stata
scalar area = 3.1416 * (2^2)
display area
```

Resultado:

```
12.5664
```

Diferencia clave:

- `macro`: almacena texto (puede ser número, pero como cadena).
- `scalar`: almacena un número real que puede usarse en operaciones matemáticas.


### Buenas prácticas {-}

- Usa `local` por defecto.
- Usa nombres descriptivos para evitar confusión.
- Cierra comillas cuando el contenido tiene espacios o texto.
- Borra scalars con `scalar drop nombre` si ya no los necesitas.


::: {.boxejercicio}
### 🧩 Ejercicio recomendado  {-}

Crea una macro con tu nombre y otra con tu año de nacimiento. Luego muestra una frase concatenando ambas:

```stata
local nombre "Ana"
local nacimiento 1980
display "Hola, mi nombre es `nombre' y nací en `nacimiento'"
```

:::


::: {.boxnote}
Las macros son esenciales para automatizar análisis en Stata. Úsalas para loops, programación y construcción flexible de comandos.
:::

## Loops en Stata {-}

Los **loops** en Stata permiten automatizar tareas repetitivas, iterando sobre listas de elementos o rangos numéricos. Son muy útiles cuando necesitas aplicar un mismo comando a varias variables, realizar simulaciones o crear múltiples gráficos/tablas de forma eficiente.

### Loop con `foreach` {-}

`foreach` itera sobre una lista de elementos, que pueden ser:

- nombres de variables
- palabras clave
- números, si se combinan con `of numlist`

#### a) Iterar sobre variables específicas  {-}

```stata 
foreach var in mpg price displacement {
    regress `var' weight
}
```

#### b) Iterar sobre nombres arbitrarios  {-}

```stata 
foreach color in rojo azul verde {
    display "El color es `color'"
}
```

#### c) Iterar sobre variables en la base usando `of varlist`  {-}

```stata 
sysuse auto, clear
foreach v of varlist price weight length {
    summarize `v'
}
```

#### d) Iterar sobre subconjuntos: condición y resultado  {-}

```stata 
foreach s of varlist price weight {
    quietly summarize `s' if foreign == 1
    display "Promedio de `s' para foreign = 1: " r(mean)
}
```



### Loop con `forvalues` {-}

`forvalues` itera sobre una **secuencia numérica** definida por un rango o paso.

#### a) Secuencia simple  {-}

```stata
forvalues i = 1/5 {
    display "Iteración `i'"
}
```

#### b) Incrementos diferentes  {-}

```stata
forvalues j = 10(2)20 {
    display "`j'"
}
```

#### c) Anidar loops con `forvalues`  {-}

```stata
forvalues i = 1/3 {
    forvalues j = 1/3 {
        display "Fila: `i', Columna: `j'"
    }
}
```

### Loop con `while` {-}

`while` permite ejecutar código mientras una condición sea verdadera. Es útil para estructuras de control más manuales o condicionales más complejas.

```stata
local i = 1
while `i' <= 5 {
    display "`i'"
    local ++i
}
```


::: {.boxejercicio}
### 🧠 Ejercicios recomendados  {-}

1. Usa `foreach` para crear un gráfico `histogram` para cada una de las siguientes variables: `mpg`, `price`, y `weight`.
2. Usa `forvalues` para crear 10 variables llamadas `x1`, `x2`, ..., `x10` con valores aleatorios entre 0 y 100.
3. Crea un loop anidado que calcule y muestre el producto de cada par `(i, j)` para `i` en 1 a 3 y `j` en 1 a 4.
4. Usa `while` para contar hacia atrás desde 10 hasta 1.
5. En un loop, calcula la media de cada variable numérica del conjunto de datos, pero **solo si su desviación estándar es mayor que 5**.

:::

## Programas definidos por el usuario {-}

En Stata puedes definir tus propios programas usando el comando `program define`. Esto es especialmente útil para empaquetar comandos que usas con frecuencia o para crear rutinas más limpias dentro de proyectos complejos.

### Estructura básica {-}

```stata
program define saludo
    display "Hola, FELIZ día"
end

saludo
```

Este programa se llama `saludo` y simplemente imprime un mensaje. Para ejecutarlo, basta con escribir su nombre.



### Programas con argumentos {-}

Puedes pasar información a un programa con `args` o con `syntax`.

#### a) Con `args`  {-}

```stata
capture program drop cuadrado

program define cuadrado
    args x
    display "El cuadrado de `x' es: " = `x'^2
end

cuadrado 4
```

> Esto imprimirá: `El cuadrado de 4 es: 16`


#### b) Con `syntax` (más power)  {-}

```stata
capture program drop promedio

program define promedio
    syntax varlist(min=1 max=1)
    summarize `varlist'
end

promedio mpg
```

> `syntax` verifica que se cumpla una estructura: aquí, exactamente una variable.



### Tu programa original mejorado: `mysum` {-}

```stata
capture program drop mysum

program define mysum
    syntax varlist(min=1 max=1)
    quietly summarize `varlist'
    display "Variable: `varlist'"
    display "Promedio: " %6.2f r(mean)
    display "Desviación estándar: " %6.2f r(sd)
end

mysum weight
```


### Notas importantes {-}

- Siempre usar `capture program drop nombre` antes de definir uno nuevo.
- Usa `syntax` cuando quieras controlar argumentos y prevenir errores.
- Dentro del programa, puedes acceder a estadísticas almacenadas con `r()`, `e()`, etc.
- Usa `quietly` para ejecutar comandos sin mostrar su salida completa.


::: {.boxejercicio}
### 🔧 Ejercicios recomendados  {-}

1. Crea un programa llamado `saluda` que reciba un nombre y diga "Hola, [nombre]".
2. Crea un programa `promedio_si` que calcule el promedio de una variable solo para observaciones que cumplen una condición (por ejemplo, `foreign == 1`). Usa `args` para pasar la variable.
3. Define un programa llamado `comparar` que reciba dos variables y calcule la diferencia de medias entre ambas (no un `ttest`, solo `mean(var1) - mean(var2)`).
4. Intenta crear un programa con `syntax` que valide que el usuario haya pasado exactamente una variable, y que esta sea numérica. Si no, que dé un mensaje de error.

:::

## Almacenamiento de resultados con `postfile` y loops {-}

Cuando queremos guardar resultados generados dentro de un loop para analizarlos después (por ejemplo, coeficientes, medias, errores estándar), Stata nos ofrece una herramienta poderosa: `postfile`.

### ¿Qué hace `postfile`? {-}

Permite crear una tabla temporal (como una mini base de datos) en la que puedes ir guardando los resultados de cada iteración del loop. Al final, puedes abrirla como si fuera cualquier base.


### Paso a paso: guardar medias con `summarize` {-}

Supongamos que queremos guardar la media y la desviación estándar de varias variables numéricas de forma automatizada.

```stata
sysuse auto, clear

tempname resultados
tempfile archivo

postfile `resultados' str15 variable media sd using `archivo'

foreach var in price weight length {
    quietly summarize `var'
    post `resultados' ("`var'") (r(mean)) (r(sd))
}

postclose `resultados'

use `archivo', clear
list
```


### ¿Qué hicimos aquí? {-}

1. `tempname` crea un alias para el objeto de `postfile`.
2. `tempfile` genera una ruta temporal para almacenar los resultados.
3. `postfile` define las variables a guardar (aquí: nombre, media y desviación estándar).
4. Dentro del loop, usamos `post` para guardar cada fila.
5. Cerramos con `postclose`.
6. Cargamos el archivo resultante con `use` y lo exploramos.


### Buenas prácticas {-}

- Usa `tempfile` para evitar escribir archivos por accidente.
- Usa tipos de datos adecuados: `str` para nombres, `numeric` para estadísticas.
- Siempre cierra el objeto con `postclose` antes de usar el archivo.


::: {.boxejercicio}
### 📦 Ejercicios con `postfile`  {-}

1. Modifica el ejemplo para guardar también el número de observaciones (`r(N)`).
2. Aplica un `regress` en un loop sobre varias variables dependientes y guarda los coeficientes de `weight` en cada una.
3. Crea una base de resultados que incluya, por cada variable, un indicador lógico que diga si su media es mayor a 500.
4. Exporta la base final a Excel usando `export excel`.

:::

## Resumen y buenas prácticas {-}

A lo largo de este capítulo exploramos los componentes fundamentales para comenzar a trabajar eficientemente en Stata. Aprendimos a:

- Usar **macros locales y globales** para automatizar tareas y hacer código más flexible.
- Escribir **loops (`foreach`, `forvalues`, `while`)** para aplicar comandos de forma repetida sin redundancia.
- Definir **programas personalizados** usando `program define`, con argumentos simples (`args`) o controlados (`syntax`).
- Implementar un programa divertido con frases de reguetón, mostrando que también se puede aprender con humor.
- Utilizar **`postfile`** para almacenar resultados generados dentro de loops y analizarlos de manera estructurada.

---

### Buenas prácticas al programar en Stata {-}

✅ Usa `local` en lugar de `global` siempre que sea posible.  
✅ Nombra tus macros y archivos temporales de forma clara y consistente.  
✅ Usa `capture` para evitar errores si un programa ya existe.  
✅ Cierra siempre `postfile` con `postclose` antes de usar el archivo.  
✅ Usa `syntax` dentro de tus programas para validar entradas del usuario.  
✅ No olvides comentar tu código. Facilita su mantenimiento y revisión.  
✅ Cuando sea posible, **grafica** tus resultados. Visualizar patrones es clave.  
✅ Prefiere loops bien documentados a copiar y pegar comandos.

---

::: {.boxejercicio}
### 🧠 Ejercicio de repaso  {-}

Imagina que estás analizando una base de datos de estudiantes. Quieres:

1. Crear un loop que recorra varias variables (como `math_score`, `reading_score`, `attendance`).
2. Para cada variable:
   - Calcular la media y desviación estándar
   - Guardar esos valores con `postfile`
3. Escribir un programa llamado `informe_var` que reciba una variable y muestre un mini informe con:
   - Nombre de la variable
   - Media
   - Desviación estándar
   - Mensaje personalizado si la media es mayor a un umbral dado
4. Ejecutar `informe_var` dentro del loop usando `syntax`

:::

## ¿Y si quiero hacerlo en R o Python? {-}

Aquí les dejo cómo realizar tareas comunes de análisis de datos en **Stata**, **R** y **Python**. Puede servir como guía rápida para quienes están aprendiendo varios lenguajes a la vez o quieren migrar entre ellos.


## 1. Asignar valores y mostrar texto {-}

| Tarea                    | Stata                       | R                                | Python                            |
|-------------------------|-----------------------------|----------------------------------|-----------------------------------|
| Asignar un número       | `scalar x = 5`              | `x <- 5`                         | `x = 5`                           |
| Asignar texto           | `local nombre "Ana"`       | `nombre <- "Ana"`               | `nombre = "Ana"`                 |
| Mostrar texto + var     | `display "Hola `nombre'"   | `cat(paste("Hola", nombre))`     | `print(f"Hola {nombre}")`        |

---

## 2. Operaciones básicas {-}

| Operación            | Stata           | R             | Python         |
|---------------------|------------------|---------------|----------------|
| Suma                | `display 2+2`    | `2 + 2`       | `2 + 2`        |
| Raíz cuadrada       | `display sqrt(4)`| `sqrt(4)`     | `math.sqrt(4)` |
| Valor absoluto      | `abs(-2)`        | `abs(-2)`     | `abs(-2)`      |

> En R necesitas `library()` si usas `sqrt`. En Python, debes importar `import math`.



## 3. Loops básicos {-}

### Iterar sobre lista de variables {-}


**Stata:**
```stata
foreach var in var1 var2 var3 {
    summarize `var'
}
```

**R:**
```r
for (var in c("var1", "var2", "var3")) {
    summary(df[[var]])
}
```

**Python:**
```python
for var in ["var1", "var2", "var3"]:
    print(df[var].describe())
```

---

## 4. Crear funciones/programas {-}

**Stata:**
```stata
program define cuadrado
    args x
    display "Resultado: " = `x'^2
end
cuadrado 4
```

**R:**
```r
cuadrado <- function(x) {
  return(x^2)
}
cuadrado(4)
```

**Python:**
```python
def cuadrado(x):
    return x**2
cuadrado(4)
```

---

## 5. Guardar resultados dentro de un loop {-}

**Stata (con postfile):**
```stata
tempname resultados
tempfile archivo
postfile `resultados' str10 var media using `archivo'
foreach v of varlist x1 x2 x3 {
    quietly summarize `v'
    post `resultados' ("`v'") (r(mean))
}
postclose `resultados'
use `archivo', clear
```

**R:**
```r
resultados <- data.frame(var = character(), media = numeric())
for (v in c("x1", "x2", "x3")) {
  media <- mean(df[[v]], na.rm = TRUE)
  resultados <- rbind(resultados, data.frame(var = v, media = media))
}
```

**Python:**
```python
resultados = []
for v in ["x1", "x2", "x3"]:
    media = df[v].mean()
    resultados.append({"var": v, "media": media})
resultados_df = pd.DataFrame(resultados)
```

## DESCARGA LOS DOCUMENTOS {-}

**Descargar Stata do file**:
[Descargar Stata](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/Clase0_StataBasics/clase0_stata.do)

**Descargar R script**:
[Descargar R](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/Clase0_StataBasics/clase0_R.R)

**Descargar Phyton Notebook**:
[Descargar Phyton](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/Clase0_StataBasics/clase0_phyton.ipynb)

[![Abrir en Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/adiazescobar/libro_cortes/blob/main/dofile/Clase0_StataBasics/clase0_phyton.ipynb)

**Descarga los Datos**
[Descargar Datos](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/Clase0_StataBasics/hh_98.dta)




