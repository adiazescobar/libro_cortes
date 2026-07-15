# POWER — clases teórica y práctica

**Fecha:** 2026-07-15  
**Estado:** diseño aprobado, pendiente de implementación  
**Repositorio:** `libro_cortes`

## Objetivo

Transformar el capítulo actual de poder estadístico en dos clases consecutivas:

1. una clase teórica que reproduzca la secuencia, los ejemplos y el lenguaje usados en el aula;
2. una clase práctica en Stata que conserve y amplíe todos los comandos, ejercicios y materiales existentes.

El resultado debe seguir el estándar pedagógico aprobado para Parámetros y RCT: teoría completa, práctica guiada, bloques de color, preguntas tipo examen sin respuestas visibles, resultados reproducibles y clave docente privada.

## Fuentes y jerarquía

Las fuentes se usarán en este orden:

1. `POWER.pptx`: fuente principal para el orden de exposición, ejemplos, énfasis y lenguaje de aula.
2. `Power_Calculation_3ie.pdf`: fuente para comprobar fórmulas, parámetros y supuestos.
3. `07-POWER.Rmd`: fuente para preservar contenido, comandos, amenazas, ética y ejercicios actuales.
4. `dofile/07_Power/07_stata.do`, `07_R.R`, `07_phyton.ipynb` y `BM_parcial.do`: fuentes reproducibles de la práctica.

No se eliminará contenido actual. Cuando haya duplicación, se redistribuirá entre teoría y práctica según su función pedagógica.

## Arquitectura del libro

- Crear `07-POWER-Teoria.Rmd` inmediatamente antes del capítulo práctico.
- Mantener `07-POWER.Rmd` como clase práctica para preservar la URL actual `poder-estadistico-stata.html`.
- Agregar la teoría a `_bookdown.yml` antes de `07-POWER.Rmd`.
- No renombrar ni renumerar los capítulos posteriores.
- Bookdown será la única fuente de numeración; los encabezados no llevarán números manuales, `PASO` ni `Etapa`.

## Clase teórica

### Secuencia de aula

La teoría seguirá la progresión de las 26 diapositivas de `POWER.pptx`:

1. pregunta de diseño: ¿cuánta muestra necesitamos?;
2. hipótesis nula y alternativa;
3. error tipo I, error tipo II, significancia y potencia;
4. tabla de decisiones y estados de la naturaleza;
5. media poblacional, media muestral y distribución de medias;
6. reducción del error estándar al aumentar la muestra;
7. superposición de distribuciones bajo nula y alternativa;
8. definición e interpretación de potencia;
9. efecto mínimo detectable;
10. relación entre MDE, tamaño muestral, varianza, alfa y beta;
11. taller de desempleo juvenil, incluyendo atrición;
12. parámetros necesarios para un cálculo de potencia;
13. asignación individual con resultado continuo;
14. ganancia de precisión mediante controles y `R^2`;
15. resultado binario;
16. tasas y persona-tiempo;
17. asignación por clústeres, ICC y efecto de diseño;
18. amenazas, derrames, equilibrio general, comportamiento y ética;
19. síntesis y puente hacia Stata.

### Desarrollo conceptual

El capítulo explicará y derivará, con notación consistente:

- `alpha`, `beta` y `1-beta`;
- pruebas de una y dos colas;
- relación entre varianza del resultado, error estándar y muestra;
- MDE para resultados continuos y binarios;
- asignación desigual entre brazos;
- reducción de varianza por covariables basales;
- efecto del cumplimiento parcial sobre el efecto reducido y el tamaño requerido;
- inflación por atrición;
- efecto de diseño por clustering e ICC;
- diferencia entre número de clústeres y número de unidades por clúster;
- límites de los cálculos mecánicos cuando los insumos son inciertos.

Las fórmulas se contrastarán con la guía de 3ie. Cualquier diferencia de convención —por ejemplo, definición de proporción tratada o prueba unilateral— se explicará explícitamente.

### Casos de aula

Se preservarán y desarrollarán:

- taller del programa de capacitación y desempleo juvenil;
- programa de aprendizaje e ingresos en Bogotá;
- subsidio de transporte y mamografías;
- incentivos y circuncisión masculina;
- vacuna contra malaria y tasas de mortalidad;
- escuelas de campo y degradación de tierras con aleatorización por aldeas.

Cada caso deberá declarar población, resultado, diseño, alfa, potencia, asignación, insumos y cantidad buscada.

### Bloques y evaluación

- Al menos ocho bloques `.box*` con funciones distintas: intuición, demostración, resultado clave, advertencia, comparación, error frecuente y conexión con diseño.
- Exactamente tres preguntas: `POWER-T1`, `POWER-T2` y `POWER-T3`.
- Cobertura mínima:
  - T1: errores tipo I/II y lectura de potencia;
  - T2: derivación o comparación de MDE y tamaño muestral;
  - T3: clustering, ICC y decisiones de diseño.
- Cada pregunta incluirá puntaje sugerido y producto esperado.
- No habrá respuestas, pistas, desplegables ni retroalimentación automática.

## Clase práctica en Stata

### Materiales

El bloque de descargas aparecerá inmediatamente después del título. Incluirá, como mínimo:

- do-file de Stata;
- script de R;
- notebook de Python/Colab;
- lectura o guía de cálculo;
- archivos adicionales necesarios para los ejercicios.

Los enlaces completos de descarga no se repetirán al final.

### Flujo guiado

La práctica tendrá entre 14 y 18 etapas semánticas, sin numeración manual:

1. definir estimando y diseño;
2. fijar alfa, potencia y prueba unilateral/bilateral;
3. documentar media, varianza o proporción de control;
4. tamaño de muestra para un efecto continuo;
5. MDE dado un tamaño muestral;
6. asignación desigual entre brazos;
7. resultado binario;
8. covariables y `R^2`;
9. cumplimiento parcial;
10. atrición;
11. tasas y exposición;
12. asignación por clústeres e ICC;
13. número de clústeres frente a tamaño del clúster;
14. análisis de sensibilidad;
15. taller de desempleo juvenil;
16. ejemplos aplicados;
17. replicación en R/Python cuando aporte valor;
18. checklist para preregistro y presupuesto.

### Resultados reproducibles

- El código Stata será ejecutable en el orden mostrado.
- Las salidas centrales se exportarán a artefactos canónicos, preferiblemente CSV.
- La página mostrará tablas con insumos, N o MDE, supuestos y comparación entre escenarios.
- Ninguna cifra empírica visible se transcribirá manualmente si puede derivarse de una fuente canónica.
- Stata, R y Python usarán la misma parametrización antes de comparar resultados.
- Las diferencias por redondeo, cuantiles o convenciones de comandos se documentarán.

### Bloques y evaluación

- Al menos doce bloques `.box*` distribuidos en el flujo: comando clave, salida central, interpretación, error frecuente, decisión de diseño y resultado clave.
- Exactamente cuatro preguntas: `POWER-S1`–`POWER-S4`.
- Cobertura mínima:
  - S1: tamaño muestral/MDE continuo;
  - S2: resultado binario y controles;
  - S3: take-up, atrición o sensibilidad;
  - S4: clústeres, ICC y comparación reproducible.
- Cada pregunta será autocontenida e incluirá puntaje sugerido, comandos permitidos y producto esperado.
- No habrá respuestas o pistas visibles.

## Clave docente privada

Se creará una clave externa al repositorio, en la carpeta docente privada usada para los capítulos anteriores.

- Contendrá siete secciones, una por cada código `POWER-T1`–`POWER-S4`.
- Cada sección incluirá respuesta correcta, procedimiento, criterios de calificación, errores frecuentes y puntaje.
- La correspondencia pregunta–clave será uno a uno.
- Tendrá permisos `0600`.
- Su nombre, ruta, identificadores y contenido no aparecerán en Rmd, YAML, `docs`, HTML, pruebas rastreadas ni documentación del repositorio.

## Preservación

Se protegerán mediante pruebas contractuales:

- todas las fórmulas y escenarios actuales;
- tamaño muestral y MDE sin controles;
- controles y `R^2`;
- cumplimiento parcial;
- resultados binarios;
- tasas;
- diseños por clústeres;
- amenazas de RCT y Belmont;
- ejercicio de Bertrand y Mullainathan;
- código Stata, R y Python;
- descargas existentes.

## Validación

La entrega se considerará completa únicamente cuando:

1. las pruebas nuevas y la suite completa estén verdes;
2. Stata produzca los resultados canónicos requeridos;
3. cualquier comparación Stata–R/Python sea reproducible;
4. los siete ejercicios sean académicamente resolubles y correspondan a la clave;
5. la auditoría de privacidad confirme cero filtraciones;
6. el libro completo renderice con exit 0 en una carpeta temporal;
7. ambos capítulos pasen QA visual en escritorio y móvil;
8. no exista doble numeración, overflow global o respuestas visibles;
9. todos los materiales descargables respondan HTTP 200;
10. `docs` permanezca sin publicar hasta aprobación explícita.

## Entregables

- `07-POWER-Teoria.Rmd`.
- `07-POWER.Rmd` ampliado como práctica.
- `_bookdown.yml` actualizado.
- resultados canónicos y ajustes mínimos al pipeline, si son necesarios.
- contratos automatizados.
- clave docente externa privada.
- vistas previas locales de ambos capítulos.

## Fuera de alcance

- crear las diapositivas nuevas del curso;
- modificar o reemplazar `POWER.pptx`;
- publicar los HTML en `docs`;
- rediseñar capítulos posteriores;
- cambiar la notación global ya aprobada en el resto del libro.
