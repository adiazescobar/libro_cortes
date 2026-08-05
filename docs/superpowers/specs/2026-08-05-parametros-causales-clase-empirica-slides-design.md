# Parámetros causales — Clase empírica: diseño de presentación

## Objetivo

Convertir el capítulo existente `04-ParametrosStata.Rmd` en una presentación Xaringan autocontenida para una sesión de 1 hora y 45 minutos. La profesora debe poder dictar toda la clase sin abrir Stata. La presentación conservará el orden, los ejemplos, las variables, los comandos, los resultados y las conclusiones del libro.

La copia local de Canvas aporta dos actividades asociadas a esta unidad: `Pausa 1` y la tarea `ATE`. Se integrarán como práctica guiada sin introducir ejemplos econométricos nuevos.

## Alcance

El trabajo producirá:

- una presentación Xaringan en HTML;
- una versión PDF de respaldo;
- el archivo fuente `.Rmd` de la presentación;
- un notebook `.ipynb` ejecutable de principio a fin en Google Colab;
- los archivos intermedios estrictamente necesarios para incorporar outputs verificados.

Los entregables vivirán en una carpeta nueva dentro de `Slides/2026_02/`. No se modificarán las presentaciones existentes ni los materiales de Regresión Discontinua.

## Fuentes canónicas

La presentación seguirá exclusivamente:

- `04-ParametrosStata.Rmd`;
- `dofile/04_ParametrosStata/04_stata.do`;
- `dofile/04_ParametrosStata/04_data.dta`;
- `dofile/04_ParametrosStata/04_stata.log`;
- `dofile/04_ParametrosStata/results/parameters_results.csv`;
- `dofile/04_ParametrosStata/results/monte_carlo_summary.csv`;
- las tres figuras generadas por el do-file;
- las explicaciones e interpretaciones ya incluidas en el capítulo;
- `teams_migration_ready/06_quizzes_and_assessments/008_Pausa_1.md`;
- `teams_migration_ready/07_assignments/001_ATE/instructions.md`, corrigiendo el error algebraico identificado en la exportación.

No se agregarán estimadores, aplicaciones ni extensiones conceptuales externas.

## Formato visual

La presentación usará Xaringan y tomará como referencia visual `Slides/2026_02/Clase1_v3`:

- misma identidad cromática y tipográfica;
- jerarquía equivalente de títulos, secciones y pies;
- bloques de código con estilo uniforme;
- HTML como formato principal y PDF como respaldo;
- texto y outputs legibles desde un salón;
- ninguna tabla, salida o bloque de código fuera del lienzo.

No se creará un archivo PowerPoint.

## Arquitectura pedagógica

La presentación tendrá aproximadamente 48–55 diapositivas. La secuencia seguirá el capítulo:

1. objetivos y mapa de la clase;
2. datos y resultados potenciales;
3. construcción de `y` y `tau`;
4. descripción por tratamiento y grupo pretratamiento;
5. diferencia de medias y regresión simple;
6. ATE, ATT, ATU y CATE;
7. diferencia naïve y sesgo de selección;
8. duplicación de observaciones;
9. una asignación aleatoria;
10. Monte Carlo con selección y aleatorización;
11. síntesis causal;
12. taller guiado recuperado de Canvas;
13. implementación en Google Colab.

Cada procedimiento seguirá, cuando la densidad lo requiera, esta secuencia:

1. pregunta o propósito;
2. comando exacto de Stata;
3. output real y limpio;
4. interpretación estadística, económica y causal;
5. conclusión permitida y conclusión no permitida.

Se incluirán preguntas breves para estudiantes. La respuesta aparecerá en la diapositiva siguiente, nunca en la misma.

## Actividad recuperada de Canvas

La `Pausa 1` se convertirá en un taller guiado de 10–12 minutos dentro de la presentación. Se conservará la tabla original de ocho individuos y se preguntará por:

1. ATE;
2. ATT;
3. ATU;
4. estimador naïve;
5. sesgo de selección.

Primero se mostrará la tabla y las preguntas sin respuestas. Después se revelarán los cálculos paso a paso y los resultados verificados: ATE = 0.75, ATT = 0.75, ATU = 0.75, naïve = 6.75 y sesgo de selección = 6.00.

La tarea `ATE` se incorporará como ejercicio algebraico final. La exportación de Canvas contiene `(1+\pi)(ATT-ATU)`, que es incorrecto. La presentación utilizará y demostrará la identidad correcta:

$$
\text{Naïve} = ATE + \text{sesgo de selección} + (1-\pi)(ATT-ATU).
$$

El PDF etiquetado externamente como `Taller 1` no se incorporará porque internamente corresponde al Taller 2 y desarrolla RCT, balance, heterogeneidad y poder estadístico.

## Tratamiento de los outputs de Stata

Se usará un formato híbrido:

- el comando se mostrará literalmente;
- el output provendrá de una ejecución fresca del do-file;
- salidas cortas aparecerán como fragmentos monoespaciados limpios;
- resultados extensos se reformatearán en tablas construidas desde los CSV producidos por Stata;
- se preservarán observaciones, coeficientes, errores estándar, valores p, intervalos de confianza, medias y categorías de referencia cuando sean necesarios;
- los números destacados coincidirán con la ejecución verificada y con el libro.

No se usarán capturas de la interfaz completa de Stata.

## Interpretaciones

Después de cada resultado importante se explicará:

- la comparación y los grupos involucrados;
- las unidades del resultado;
- la población a la que corresponde;
- si es una diferencia descriptiva, asociación o efecto causal;
- el supuesto necesario para una interpretación causal;
- qué puede concluirse y qué no.

La redacción se derivará del capítulo existente y no ampliará su alcance.

## Google Colab

El notebook:

- cargará `04_data.dta` desde el repositorio de GitHub;
- utilizará principalmente `pandas`, `numpy` y `statsmodels`;
- reproducirá los procedimientos principales en el mismo orden;
- mostrará resultados debajo de cada celda;
- se ejecutará de principio a fin sin rutas locales;
- comparará resultados con Stata mediante aserciones con tolerancias explícitas;
- documentará cualquier diferencia causada por convenciones de software.

La sección final de las diapositivas mostrará, para cada procedimiento principal, el comando de Stata, el código Python, el resultado y la comparación interpretada.

## Verificación

Antes de entregar:

1. ejecutar el do-file completo en Stata 19;
2. comprobar que cada output corresponde al comando mostrado;
3. contrastar los resultados con el capítulo;
4. ejecutar todas las celdas del notebook;
5. verificar las comparaciones Stata–Python;
6. renderizar HTML y PDF;
7. revisar visualmente todas las diapositivas;
8. corregir desbordamientos, texto pequeño, rutas rotas y figuras faltantes;
9. confirmar que la presentación puede dictarse sin abrir Stata.

## Criterios de aceptación

- Duración prevista: 1 hora y 45 minutos.
- Extensión prevista: 48–55 diapositivas.
- Orden y contenido coincidentes con el libro.
- Todos los comandos y resultados relevantes de Stata incorporados.
- Outputs legibles y numéricamente verificados.
- Notebook reproducible desde Google Colab.
- Taller guiado de Canvas incluido con respuestas reveladas en diapositivas posteriores.
- Demostración ATE incluida con el signo algebraico corregido.
- HTML y PDF sin errores de renderizado ni desbordamientos.
- Ninguna modificación a presentaciones existentes o materiales de RDD.
