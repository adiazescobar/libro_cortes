# Diseño de ampliación pedagógica de Parámetros causales

**Fecha:** 2026-07-15  
**Alcance:** ampliación de `03-Parametros.Rmd` y `04-ParametrosStata.Rmd`; creación de una clave privada para profesora y monitor.  
**Estado:** aprobado por Ana María.

## Motivación

La primera versión estandarizada logró claridad académica, reproducibilidad y buenas visualizaciones, pero la página práctica quedó demasiado sintética y ambos capítulos necesitan más apoyos visuales y preguntas tipo examen. La ampliación conservará los resultados y gráficos aprobados, recuperará desarrollo paso a paso y añadirá evaluación formativa sin revelar respuestas en la página estudiantil.

## Regla transversal para el resto del libro

Los capítulos siguientes deberán mantener una estructura uniforme, pero “uniforme” no significará “breve”. Cada capítulo debe incluir suficiente desarrollo para que un estudiante pueda reconstruir el razonamiento, interpretar resultados y prepararse para el examen.

- Los capítulos teóricos usarán bloques de color para intuición, resultado clave, advertencia, ejemplo y pregunta tipo examen.
- Los capítulos prácticos usarán bloques de color para comando, salida, interpretación, error frecuente, resultado clave y pregunta tipo examen.
- Las preguntas tipo examen aparecerán sin respuesta, pista ni mecanismo desplegable en la página estudiantil.
- Las soluciones vivirán en una clave separada, no enlazada desde el libro.
- Las futuras diapositivas prácticas seguirán incluyendo todos los resultados y pasos necesarios para dictar la clase.

## Capítulo teórico

### Contenido que permanece

Se conserva la revisión académica ya aprobada: resultados potenciales con notación `Y_i(D=1)` y `Y_i(D=0)`, ATE, ATT, ATU, CATE, identidades de agregación, descomposición del sesgo, independencia, positividad, antes-después, SUTVA, videos, ejemplo de ocho personas y puente a Stata.

### Bloques de color

El capítulo incorporará como mínimo:

1. **Intuición:** diferencia entre efecto individual y parámetro promedio.
2. **Resultado clave:** relación entre ATE, ATT y ATU.
3. **Resultado clave:** agregación del CATE para recuperar el ATE.
4. **Advertencia:** una muestra grande no elimina sesgo de selección.
5. **Ejemplo:** lectura guiada de la muestra de ocho personas.
6. **Advertencia:** independencia condicional requiere positividad y covariables pretratamiento relevantes.

Los bloques deben usar las clases visuales ya disponibles en el libro (`boxinfo`, `boxnote`, `boxwarning`, `boxejercicio`) o extensiones compatibles con el mismo sistema de color. No se crearán estilos aislados que hagan ver este capítulo distinto del resto.

### Preguntas tipo examen

Se añadirán tres preguntas sin respuesta visible:

1. **Selección múltiple con justificación:** identificar el parámetro adecuado entre ATE, ATT, ATU y CATE para una población y subgrupo descritos.
2. **Demostración corta:** descomponer una diferencia observada en ATT y sesgo de selección, mostrando cada paso algebraico.
3. **Diagnóstico de identificación:** evaluar independencia, positividad y SUTVA en un caso aplicado, indicando cuál supuesto falla y por qué.

Cada pregunta tendrá enunciado autosuficiente, puntaje sugerido y espacio o instrucciones claras sobre lo que debe entregar el estudiante. No incluirá solución, pista ni retroalimentación automática.

## Capítulo práctico de Stata

### Principio de ampliación

La página mantendrá descargas al inicio y resultados canónicos provenientes de Stata, pero recuperará el razonamiento operativo entre código y resultado. No se volverán a pegar consolas completas; se ampliarán los comandos, explicaciones, resultados centrales y conexiones entre etapas.

### Desarrollo paso a paso

La práctica debe desarrollar con mayor detalle:

1. **Preparación de datos:** carga de `04_data.dta`, inspección de variables y construcción de `X`, `y` y `tau`.
2. **Descripción de grupos:** `tabulate`, `summarize` y `bysort`, explicando qué se aprende de cada comando.
3. **Diferencia de medias:** relación entre `ttest` y la comparación naïve, incluyendo el orden de la resta que reporta Stata.
4. **Regresión simple:** equivalencia entre el coeficiente de `D` y la diferencia de medias; interpretación de constante, coeficiente, error estándar e intervalo.
5. **Programa `estimadores`:** explicar `program define`, `syntax` o `args`, resultados `r()`, escalares y flujo del programa; mostrar una versión ejecutable completa.
6. **ATE, ATT, ATU y CATE:** cálculo separado, condiciones `if` y comparación en una tabla canónica.
7. **Descomposición del sesgo:** cálculo paso a paso desde medias observadas y ATT.
8. **Duplicación de observaciones:** mostrar el código y distinguir tamaño nominal de información independiente.
9. **Asignación aleatoria:** semilla, generación de `D`, variación muestral y diferencia entre insesgadez en expectativa e igualdad exacta.
10. **Monte Carlo con selección:** explicar población, regla de selección, repetición, almacenamiento y distribución del sesgo.
11. **Monte Carlo con aleatorización:** identificar qué cambia respecto al escenario anterior.
12. **Comparación gráfica:** conservar los tres gráficos aprobados y ampliar su lectura guiada.

### Bloques de color

La práctica incorporará bloques recurrentes y visualmente consistentes:

- **Comando clave:** sintaxis que el estudiante debe poder reproducir.
- **Salida central:** tabla o cifra proveniente de los CSV canónicos.
- **Interpretación:** lectura económica y estadística.
- **Error frecuente:** dirección de la resta en `ttest`, confusión entre más observaciones duplicadas y más información, o igualdad exacta bajo aleatorización.
- **Resultado clave:** lección que conecta código e identificación.
- **Pregunta tipo examen:** aplicación sin respuesta visible.

Los bloques no reemplazan los encabezados ni fragmentan excesivamente la página; deben guiar una lectura continua.

### Preguntas tipo examen

Se añadirán cuatro ejercicios sin respuesta visible:

1. **Lectura de output:** interpretar coeficiente, constante, intervalo y relación con diferencia de medias.
2. **Cálculo:** recuperar ATE, ATT, ATU, dos CATE y sesgo a partir de una tabla pequeña de resultados potenciales.
3. **Depuración de código:** identificar y corregir errores en un programa de Stata que calcula estimandos.
4. **Diseño de simulación:** modificar la regla de selección o asignación y anticipar cómo cambiarán centro y dispersión del histograma de sesgo.

Cada ejercicio especificará puntaje, comandos permitidos y productos esperados. No tendrá solución, pista ni respuesta desplegable.

## Clave privada para profesora y monitor

Se creará en una ubicación privada externa comunicada fuera del repositorio. No se usará `docs/instructor/`: `docs` es el directorio de publicación de GitHub Pages y cualquier archivo ubicado allí podría quedar accesible aunque no tenga enlaces.

La clave:

- no se incluirá en Git ni en `_bookdown.yml`;
- no tendrá enlaces desde ningún Rmd estudiantil;
- no se copiará ni publicará en los HTML de `docs`;
- identificará cada pregunta mediante un código estable: `T-P1` a `T-P3` y `S-P1` a `S-P4`;
- contendrá respuesta correcta, procedimiento, criterios de calificación, errores frecuentes y puntaje sugerido;
- permitirá que profesora y monitor califiquen de manera consistente.

El archivo permanecerá en la carpeta docente privada de Dropbox para continuidad entre profesora y monitor. El repositorio solo contendrá las preguntas sin solución y pruebas que confirmen que ninguna respuesta se filtra al material estudiantil.

## Reproducibilidad y cifras

- Se conservarán el pipeline de Stata y los artefactos canónicos actuales.
- Las cifras adicionales visibles deberán interpolarse desde CSV exportados por Stata.
- Si la ampliación necesita un resultado que todavía no está exportado, primero se ampliará el do-file y se regenerarán los artefactos.
- No se transcribirán manualmente resultados numéricos en la prosa.
- Los tres gráficos actuales permanecerán sin cambios sustantivos salvo ajustes de accesibilidad o tamaño.

## Pruebas y validación

La ampliación estará completa cuando:

1. el capítulo teórico contenga al menos seis bloques de color y exactamente tres preguntas codificadas `T-P1` a `T-P3`;
2. el capítulo práctico contenga desarrollo completo de las doce etapas, bloques de los seis tipos definidos y exactamente cuatro preguntas `S-P1` a `S-P4`;
3. ninguna pregunta estudiantil incluya respuesta, pista o elemento desplegable;
4. la clave privada externa contenga los siete códigos y sus cinco componentes de calificación;
5. ninguna página estudiantil enlace o mencione la ruta de la clave;
6. el render temporal no contenga identificadores de la clave privada ni las soluciones;
7. todas las cifras visibles continúen proviniendo de artefactos canónicos;
8. la suite automatizada completa pase;
9. ambos capítulos se revisen en escritorio y móvil, con especial atención a longitud, ritmo visual, código y tablas;
10. `docs/` permanezca sin publicar hasta la aprobación de Ana María.

## Límites

- No se crearán todavía las diapositivas.
- No se modificarán los estimandos ni la estrategia Monte Carlo ya aprobada.
- No se añadirán respuestas desplegables ni autoevaluación interactiva.
- No se expandirán otros capítulos en esta ronda; la estructura se documenta como patrón para aplicarla posteriormente.
