# Malos controles — diseño académico y pedagógico

## Objetivo

Revisar la pareja de capítulos “Malos controles” para que conserve la secuencia y profundidad de la clase, use criterios causales correctos y replique la arquitectura aprobada para las demás parejas teórica–empírica.

## Decisiones aprobadas

- Mantener tres aplicaciones: mediador, colisionador y proxy postratamiento contaminado.
- Organizar la decisión de control alrededor del estimando y del DAG, no de la regla mecánica “variable previa = buen control”.
- Conservar y corregir la demostración de Angrist y Pischke sobre agrupación por una variable postratamiento.
- Incorporar de forma selectiva las ideas de Cinelli, Forney y Pearl: backdoors, mediadores, colisionadores, amplificación de sesgo y controles neutrales.
- Mantener la notación del curso \(Y(D=1)\) y \(Y(D=0)\).
- Usar “ATT” para el estimando DID básico cuando corresponda.
- Poner las descargas al comienzo de la clase empírica.
- Mostrar en la página tablas con resultados producidos por Stata.
- Incluir exactamente tres preguntas teóricas y cuatro empíricas, sin respuestas desplegables ni soluciones visibles.
- Crear una clave separada, fuera del libro publicado, para la profesora y el monitor.
- Delegar toda numeración de títulos y subtítulos a Bookdown.

## Correcciones académicas obligatorias

1. Una variable predeterminada no es automáticamente un buen control; puede ser colisionador o amplificador de sesgo.
2. Controlar un mediador impide interpretar el coeficiente como efecto total. Solo bajo supuestos adicionales puede interpretarse como efecto directo.
3. El camino del colisionador es \(D\rightarrow C\leftarrow U\rightarrow Y\); condicionar en \(C\) lo abre.
4. El signo del sesgo de agrupación de Angrist–Pischke no es universal.
5. El ejemplo del proxy contaminado debe tener un DGP internamente coherente y distinguir:
   - tratamiento aleatorio con proxy postratamiento;
   - tratamiento confudido y proxy pretratamiento imperfecto.
6. En DID, los controles deben ser compatibles con el estimando, la identificación condicional y la composición de la muestra.

## Arquitectura

### Clase teórica

1. Pregunta causal y estimando.
2. Regla de caminos causales: forks, chains y colliders.
3. Buenos controles, malos controles y controles neutrales.
4. Mediadores y efecto total frente a efecto directo.
5. Colisionadores y selección.
6. Variables pretratamiento que pueden ser malas.
7. Demostración Angrist–Pischke.
8. Aplicación a DID.
9. Checklist y tres preguntas tipo examen.

### Clase empírica

1. Materiales para la clase.
2. Mapa de decisiones antes de correr una regresión.
3. Caso mediador.
4. Caso colisionador.
5. Caso proxy contaminado.
6. Comparación conjunta de estimadores.
7. Aplicación y diagnóstico en DID.
8. Cuatro preguntas tipo examen.
9. Replicación en R y Python.

Cada caso debe incluir estimando, DAG, DGP, predicción previa, código Stata, tabla canónica, interpretación y una advertencia.

## Productos

- `09-BadControls.Rmd`
- `10-BadControlsStata.Rmd`
- `dofile/10_BadControls/10_stata.do`
- resultados canónicos en `dofile/10_BadControls/results/`
- clave privada fuera de los archivos incluidos en `_bookdown.yml`
- pruebas contractuales y render completo.
