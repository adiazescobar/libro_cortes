# Diseño: clase empírica de parámetros causales

**Fecha:** 2026-08-10

## Problema

La clase empírica actual no sigue con claridad el ejercicio manual del capítulo
teórico. El do-file introduce una regla nueva de selección mediante `invlogit()`,
expande la base hasta 80.000 observaciones y dedica demasiado espacio a una
infraestructura Monte Carlo que oculta el objetivo pedagógico. Además, el
capítulo no presenta el box de descargas usado en las demás clases empíricas.

## Objetivo pedagógico

La clase debe mostrar, usando siempre los mismos ocho perfiles y sus mismos
resultados potenciales, cuatro resultados en secuencia:

1. Stata, R y Python reproducen exactamente los estimandos calculados a mano:
   ATE, ATT, ATU, CATE(0), CATE(1), diferencia naïve y sesgo respecto del ATT.
2. Aumentar el tamaño nominal de la muestra hasta 10.000 observaciones sin
   modificar la regla de asignación no elimina el sesgo ni produce consistencia.
3. Reemplazar solamente `D` por una asignación aleatoria rompe la selección;
   los resultados potenciales y la construcción de `Y` permanecen iguales.
4. Al volver a generar un `D` aleatorio en cada repetición, el promedio de la
   diferencia de medias se centra en el ATE. Una realización individual no tiene
   que coincidir exactamente con el ATE.

## Secuencia de la clase

### 1. Replicación del ejercicio manual

Se carga la base de ocho observaciones, se construyen `X`, `tau` y el resultado
observado `y`, y se listan las filas. Los estimandos se calculan mediante
comandos transparentes (`summarize`, condiciones `if` y diferencias de medias),
sin programas auxiliares innecesarios. La regresión `regress y D` se usa para
verificar su equivalencia algebraica con la diferencia de medias, no como un
método de identificación adicional.

### 2. Más observaciones con la misma selección

Los ocho perfiles se replican proporcionalmente hasta obtener exactamente
10.000 observaciones. Se mantiene el `D` original de cada perfil y se vuelven a
calcular el ATT, la diferencia naïve y su sesgo. Los valores poblacionales no
cambian: solo aumenta el tamaño nominal. El texto distinguirá explícitamente
sesgo, precisión e información independiente, y no interpretará la replicación
de filas como nueva evidencia muestral.

### 3. Una asignación aleatoria

Sobre los mismos 10.000 perfiles se elimina el `D` observacional y se genera
`D = runiform() < 0.5` con una semilla reproducible. Después se reconstruye `y`
usando los mismos `yd0` y `yd1`. Este bloque mostrará una realización del diseño
aleatorio y advertirá que su estimación puede diferir del ATE por azar.

### 4. Monte Carlo mínimo

Cada repetición conserva los mismos resultados potenciales y genera únicamente
un nuevo `D` aleatorio. Se guarda la diferencia de medias, se resume su
distribución y se compara su media con el ATE. No habrá una regla simulada de
selección, `invlogit()`, ni un segundo proceso generador de datos. El Monte Carlo
demuestra insesgadez en repetición y, si se comparan tamaños muestrales, la
concentración del estimador cuando aumenta N.

## Materiales descargables

El comienzo de `04-ParametrosStata.Rmd` usará el mismo contenedor
`.class-materials` de las demás clases e incluirá el encabezado “Descarga antes
de comenzar”. Contendrá enlaces directos `raw.githubusercontent.com` para:

- do-file de Stata;
- base de ocho observaciones;
- script de R;
- notebook de Python;
- enlace para abrir el notebook en Colab.

Los resultados derivados y las figuras pueden enlazarse después del box, pero
no reemplazan los cuatro materiales necesarios para seguir la clase.

## Alineación entre lenguajes

`04_stata.do`, `04_R.R` y `04_phyton.ipynb` implementarán la misma secuencia,
las mismas semillas conceptuales y los mismos estimandos. Los tres producirán
salidas verificables con nombres comunes para:

- parámetros del ejercicio manual;
- comparación N=8 versus N=10.000 bajo selección;
- asignación aleatoria única;
- resumen Monte Carlo bajo asignación aleatoria.

Las diferencias inevitables entre generadores pseudoaleatorios de los tres
lenguajes no se interpretarán como errores. La verificación exigirá igualdad en
los resultados deterministas y tolerancias previamente definidas para los
resultados de simulación.

## Verificación

La implementación se considerará completa cuando:

1. los tres scripts ejecuten sin errores desde la carpeta de materiales;
2. los tres reproduzcan exactamente los estimandos manuales;
3. N=10.000 conserve el mismo sesgo de la asignación observacional;
4. el promedio Monte Carlo bajo aleatorización quede dentro de una tolerancia
   estadística explícita alrededor del ATE;
5. el capítulo compile y muestre correctamente el box de descargas;
6. todos los fragmentos de código presentados en el capítulo correspondan al
   código ejecutado por los materiales descargables.

## Fuera de alcance

No se rediseñará el capítulo teórico, no se crearán procesos alternativos de
selección y no se añadirá contenido de RCT que corresponda al capítulo
siguiente. Tampoco se modificarán archivos ajenos a esta pareja de capítulos y
sus materiales.
