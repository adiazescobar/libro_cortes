# Diseño: bloque fundacional del libro

**Fecha:** 2026-07-14  
**Proyecto:** Libro de Econometría Avanzada  
**Capítulos:** Prueba de entrada, Introducción y Stata para principiantes

## Objetivo

Convertir los tres capítulos iniciales en una entrada coherente al curso: diagnosticar prerrequisitos, motivar la inferencia causal y garantizar un piso operativo común en Stata antes de comenzar los módulos econométricos.

## Orden de implementación

El bloque se trabajará y aprobará en este orden:

1. `00-PruebaEntrada.Rmd`;
2. `01-intro.Rmd`;
3. `02-StataBasics.Rmd`.

Cada capítulo tendrá pruebas, render temporal y revisión visual antes de avanzar al siguiente.

## Prueba de entrada

### Propósito académico

La prueba será diagnóstica y evaluará exclusivamente prerrequisitos que el estudiante debería traer al curso. No se penalizará el desconocimiento de métodos que serán enseñados en Econometría Avanzada.

### Cobertura

La prueba conservará cuatro áreas:

1. **Estadística básica:** esperanza, varianza, distribución muestral, error estándar, intervalos de confianza y lectura elemental de pruebas de hipótesis.
2. **Regresión lineal:** interpretación de coeficientes, residuos, supuestos básicos de MCO, sesgo por variable omitida a nivel introductorio y lectura de una salida de regresión.
3. **Causalidad básica:** correlación frente a causalidad, contrafactual, grupo de comparación, selección y diferencia entre variables pretratamiento y postratamiento.
4. **Stata:** estructura de comandos, carga y descripción de datos, generación de variables, condiciones, regresión y lectura de resultados almacenados básicos.

Los conceptos propios del curso —por ejemplo, estimadores modernos de DiD, LATE, IPW, RDD o descomposiciones TWFE— no formarán parte del puntaje diagnóstico.

### Revisión académica

Cada pregunta se auditará para comprobar:

- correspondencia con un prerrequisito explícito;
- una única respuesta inequívocamente correcta;
- distractores plausibles que reflejen errores conceptuales reales;
- notación, unidades y condiciones suficientes para responder;
- ausencia de pistas gramaticales o diferencias artificiales de longitud;
- dificultad apropiada para estudiantes que comienzan Econometría Avanzada;
- retroalimentación que explique el concepto sin limitarse a revelar la opción correcta.

La revisión producirá una matriz interna con sección, competencia, nivel de dificultad, clave y justificación. La página para estudiantes no mostrará la clave antes de finalizar.

### Experiencia del estudiante

Al comienzo aparecerán:

- propósito no punitivo;
- duración estimada;
- número de preguntas y áreas;
- explicación del botón de calificación;
- recomendación de responder sin consultar materiales.

La página mantendrá puntaje total y por sección. Después de calificar, mostrará recomendaciones de repaso específicas según el desempeño de cada área. Las tildes y etiquetas se normalizarán en español.

### Robustez técnica

El render no instalará paquetes. Si `webexercises` no está disponible, se detendrá con un mensaje que indique cómo instalarlo antes de compilar. La lógica de retroalimentación permanecerá oculta hasta que el estudiante solicite calificar.

Se probarán el conteo de preguntas, las claves, el cálculo del puntaje, la calificación repetida, los campos sin responder y el funcionamiento en una pantalla angosta.

## Introducción

### Función

El capítulo debe motivar el curso y construir el vocabulario mínimo que necesitan los módulos siguientes, sin comportarse todavía como una clase teórica especializada.

### Estructura

1. Objetivos y mapa del capítulo.
2. Por qué importa la inferencia causal para economía y política pública.
3. Cómo formular una pregunta causal.
4. Contrafactual y resultados potenciales.
5. Problema fundamental de la inferencia causal.
6. Diferencia observada, efecto causal y sesgo de selección.
7. Comparaciones prohibidas: grupos distintos y antes–después sin control adecuado.
8. Familias de estrategias del curso.
9. Mapa del libro y puente a Stata.

Se conservarán los ejemplos sustantivos, gráficos, videos y derivaciones útiles. Se eliminarán encabezados duplicados, se unificará la notación y se evitará anticipar demostraciones que pertenecen a capítulos posteriores.

El capítulo no tendrá bloque de descargas.

## Stata para principiantes

### Función

El capítulo garantizará que todos los estudiantes puedan ejecutar y entender las rutinas de programación que usarán en las clases prácticas posteriores.

### Estructura

1. Materiales para la clase inmediatamente después del título.
2. Objetivos y preparación del entorno.
3. Macros locales, globales y escalares.
4. Loops con `foreach`, `forvalues` y `while`.
5. Programas con `args` y `syntax`.
6. Almacenamiento reproducible con `postfile`.
7. Buenas prácticas y errores frecuentes.
8. Ejercicios acumulativos.
9. Equivalencias breves con R y Python.
10. Resumen y checklist de preparación.

Cada concepto seguirá la secuencia:

> objetivo → comando Stata → salida visible → interpretación → ejercicio breve

Las descargas incluirán el do-file, los datos, el script de R y el notebook de Python/Colab. Se revisará el nombre mal escrito `clase0_phyton.ipynb`; cualquier corrección conservará compatibilidad mediante un enlace o copia con el nombre anterior durante la transición.

Los resultados principales generados por Stata se mostrarán dentro de la página. Las salidas auxiliares permanecerán en los archivos descargables.

## Consistencia visual

Los tres capítulos reutilizarán tipografía, colores, cajas y tablas aprobados en el piloto RCT. La prueba diagnóstica tendrá un lenguaje visual propio para distinguir instrucciones, preguntas, puntaje y recomendaciones, sin parecer un examen sancionatorio.

En móvil no habrá texto superpuesto, controles fuera de pantalla ni necesidad de desplazamiento horizontal para responder preguntas.

## Verificación del bloque

El bloque se considerará aprobado cuando:

- la prueba evalúe solo prerrequisitos y todas sus claves hayan sido justificadas;
- el puntaje y las recomendaciones funcionen en escritorio y móvil;
- la introducción presente una progresión sin duplicaciones;
- Stata para principiantes tenga descargas al comienzo y resultados visibles;
- todos los archivos descargables existan y correspondan a los enlaces;
- los tres capítulos rendericen sin instalar dependencias ni modificar `docs/`;
- los cambios sean revisables mediante commits pequeños;
- los artefactos locales preexistentes permanezcan fuera de los commits.

## Fuera de alcance

Este bloque no modifica todavía Parámetros causales ni los módulos posteriores. Tampoco crea las diapositivas; cuando se produzcan, las diapositivas prácticas deberán incluir todos los resultados correspondientes.
