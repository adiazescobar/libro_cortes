# Diseño: recursos y resultados en clases prácticas

**Fecha:** 2026-07-14  
**Proyecto:** Libro de Econometría Avanzada  
**Piloto:** Capítulo 6, Experimentos aleatorizados — Clase empírica

## Objetivo

Facilitar que los estudiantes encuentren los materiales de trabajo sin recorrer toda la página y conectar cada comando de Stata con la evidencia que produce.

## Alcance

El patrón se probará primero en `06-RCT2.Rmd`. Después de la revisión de Ana María podrá replicarse en las demás clases prácticas. No se añadirán bloques de descarga a las clases exclusivamente teóricas.

Las diapositivas de las clases prácticas constituyen una fase posterior. A diferencia del libro, deberán incluir todos los resultados producidos en la clase.

## Bloque inicial de recursos

Después de los objetivos y antes de la pregunta empírica aparecerá un bloque visible llamado **Materiales para la clase**. Incluirá enlaces directos, según disponibilidad, a:

- do-file de Stata;
- base de datos;
- notebook de Colab o Python;
- archivos de resultados completos en Excel o CSV.

Los enlaces deben apuntar a archivos que formen parte del libro publicado o a ubicaciones externas estables. El bloque no se repetirá al final de la página.

## Resultados dentro de la página

La clase práctica seguirá la secuencia pedagógica:

> pregunta o estimando → comando Stata → tabla principal → interpretación

Se mostrarán dentro de la página solamente los resultados centrales:

1. estadísticas descriptivas y balance;
2. estimaciones principales;
3. heterogeneidad o robustez que sea indispensable para el objetivo de la clase.

Cada tabla debe:

- derivarse de los mismos datos y especificaciones que usa el do-file;
- tener título, nombres de variables legibles, tamaño de muestra y nota cuando corresponda;
- ubicarse inmediatamente después del bloque de código o de la explicación que la genera;
- incluir una interpretación breve que enfatice magnitud, precisión e identificación, no solo significancia estadística.

Los resultados auxiliares permanecerán disponibles mediante los archivos descargables para evitar que el capítulo se vuelva innecesariamente largo.

## Fuente y reproducibilidad

Las tablas del libro podrán renderizarse con R a partir de `data.dta` o de archivos canónicos exportados por Stata. Cuando el resultado dependa de una rutina específica de Stata, la fuente será el archivo exportado por el do-file. No se transcribirán números manualmente.

El capítulo debe detener el render con un mensaje claro si falta un archivo requerido o una variable indispensable. Las tablas publicadas y los archivos de descarga deben corresponder a la misma ejecución y muestra analítica.

## Presentación visual

El bloque de materiales reutilizará el lenguaje visual ya incorporado al piloto, con botones o enlaces suficientemente visibles y distinguibles. Las tablas usarán el ancho disponible, encabezados compactos, alternancia suave de filas y desplazamiento horizontal solamente si no existe una versión legible más compacta.

En pantallas pequeñas los enlaces se apilarán y las tablas conservarán encabezados y cifras sin superposición.

## Verificación del piloto

Antes de aprobar el patrón se comprobará que:

- los enlaces de descarga aparecen antes de la pregunta empírica y funcionan;
- el do-file, la base y el notebook descargados son los archivos correctos;
- las tablas centrales coinciden con los resultados canónicos de Stata;
- no hay resultados principales mencionados en el texto que estén ausentes de la página;
- el capítulo renderiza sin errores;
- la página es legible en vista de escritorio y en una ventana angosta;
- el capítulo teórico `05-RCT.Rmd` permanece sin un bloque de descargas.

## Criterio de aceptación

Un estudiante debe poder abrir la clase práctica, descargar inmediatamente los materiales necesarios y entender los resultados principales de Stata sin abandonar la página. Quien necesite el detalle completo podrá descargar los archivos de resultados.
