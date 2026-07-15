# Diseño de los capítulos de Parámetros causales

**Fecha:** 2026-07-15  
**Alcance:** `03-Parametros.Rmd`, `04-ParametrosStata.Rmd` y materiales reproducibles asociados.  
**Estado:** aprobado por Ana María.

## Objetivo

Estandarizar los capítulos teórico y práctico de Parámetros causales con las plantillas pedagógicas ya aprobadas para el libro, corregir imprecisiones académicas y asegurar que todos los resultados empíricos publicados provengan de una ejecución reproducible en Stata.

## Reglas transversales del libro

1. Todos los capítulos teóricos deben compartir estructura, jerarquía visual y nivel de profundidad.
2. Todos los capítulos prácticos deben compartir la secuencia descarga–objetivos–código–resultado–interpretación–ejercicio.
3. Los videos académicamente útiles permanecen en los capítulos teóricos.
4. Los prompts extensos para ChatGPT se convierten en actividades breves, concretas y evaluables.
5. Las páginas prácticas muestran los resultados esenciales para aprender e interpretar; el código y las salidas completas quedan en archivos descargables.
6. Las diapositivas prácticas que se construirán posteriormente incluirán todos los pasos y todos los resultados necesarios para dictar la clase.
7. Las clases teóricas no incluyen un bloque de descargas.
8. Las descargas de las clases prácticas aparecen inmediatamente después del título.
9. La notación de resultados potenciales será `Y_i(D=1)` y `Y_i(D=0)` en todos los capítulos. Cuando el índice individual no sea necesario, se usará `Y(D=1)` y `Y(D=0)`. No se alternará con `Y_i(1)`, `Y_i(0)`, `Y(1)` o `Y(0)`.

## Capítulo 3: Parámetros causales (teoría)

### Estructura pedagógica

El capítulo seguirá este orden:

1. objetivos de aprendizaje y lecturas;
2. pregunta causal y población de interés;
3. resultados potenciales, tratamiento observado y resultado observado;
4. problema fundamental de la inferencia causal;
5. efectos individuales y parámetros promedio ATE, ATT, ATU y CATE;
6. relación entre ATE, ATT, ATU y CATE;
7. comparación naïve y demostración completa del sesgo de selección;
8. independencia incondicional e independencia condicional;
9. soporte común o positividad cuando se condicione en covariables;
10. comparación antes-después y su contrafactual faltante;
11. SUTVA: no interferencia y tratamientos bien definidos;
12. síntesis, ejercicios, puente a la práctica y referencias.

### Criterios académicos

- Usar exclusivamente la notación de clase `Y_i(D=1)` y `Y_i(D=0)`, o sus versiones sin índice cuando corresponda.
- Distinguir parámetros causales de estimadores muestrales.
- Explicar que el ATT, ATU y ATE son parámetros distintos cuando existe heterogeneidad de efectos y selección.
- Definir el efecto promedio condicional como

  \[
  CATE(x)=\mathbb{E}[Y_i(D=1)-Y_i(D=0)\mid X_i=x],
  \]

  y explicar que describe heterogeneidad causal entre subgrupos definidos por covariables pretratamiento.
- Mostrar que el ATE agrega los CATE sobre la distribución de $X$:

  \[
  ATE=\mathbb{E}[CATE(X_i)].
  \]
- Mostrar la identidad:

  \[
  ATE = P(D=1)ATT + P(D=0)ATU.
  \]

- Derivar paso a paso que la diferencia observada entre tratados y controles es el ATT más el sesgo de selección.
- No afirmar que la independencia de resultados potenciales se cumple en general para IV, RDD o DiD. Cada uno identifica un parámetro bajo supuestos propios; se presentarán como estrategias alternativas de identificación.
- Presentar independencia condicional junto con positividad y dejar claro que exige observar los factores de confusión relevantes.
- Formular el antes-después con índices de tratamiento y tiempo para no confundir `Y(D=0)` con “resultado antes”.
- Presentar SUTVA mediante sus dos componentes: ausencia de interferencia y tratamiento bien definido. No describir el segundo componente simplemente como “el mismo tratamiento para todos”.
- Mantener los dos videos existentes.
- Sustituir el prompt largo de ChatGPT por una actividad corta que pida identificar el estimando, el contrafactual faltante, el supuesto requerido y dos amenazas a la identificación.

### Ejemplo numérico

Se conservará la muestra de ocho individuos porque conecta directamente con la práctica. El capítulo mostrará los resultados potenciales, el tratamiento y el resultado observado, y pedirá calcular ATE, ATT, ATU, un CATE por una covariable pretratamiento, diferencia naïve y sesgo de selección. Las soluciones o pistas deben ser suficientes para verificar los cálculos sin anticipar toda la actividad.

## Capítulo 4: Parámetros causales en Stata

### Descargas al inicio

Inmediatamente después del título se ofrecerán:

- do-file de Stata;
- base `04_data.dta`;
- script de R;
- notebook de Python y enlace a Colab;
- archivo con resultados completos de Stata;
- tablas o gráficos generados por la ejecución, cuando corresponda.

Se conservará el nombre histórico `04_phyton.ipynb` para no romper enlaces existentes, aunque la etiqueta visible dirá “Python”.

### Estructura pedagógica

1. descargas;
2. objetivos y conocimientos previos;
3. carga de datos y construcción de `y` y `tau`;
4. descripción de tratados y controles;
5. diferencia de medias y regresión simple;
6. cálculo de ATE, ATT, ATU y CATE por subgrupo pretratamiento;
7. comparación naïve y descomposición del sesgo;
8. experimento de aumento artificial del tamaño muestral;
9. experimento de asignación aleatoria;
10. Monte Carlo con selección;
11. Monte Carlo con aleatorización;
12. comparación de escenarios;
13. ejercicios, síntesis y puente al capítulo siguiente.

Cada sección empírica seguirá la secuencia **pregunta → comando → resultado central → interpretación → ejercicio breve**.

### Resultados visibles y descargables

La página mostrará únicamente:

- frecuencias y medias esenciales por grupo;
- coeficiente de la diferencia de medias/regresión y su interpretación;
- ATE, ATT, ATU, CATE por subgrupo, estimador naïve y sesgo para los ocho individuos;
- resumen del experimento de tamaño muestral;
- resumen del experimento aleatorio;
- media, desviación estándar y cuantiles relevantes del sesgo en cada Monte Carlo;
- dos histogramas y una comparación compacta de escenarios.

Los logs y resultados completos permanecerán descargables. No se pegarán bloques extensos de consola en el cuerpo del capítulo.

### Reproducibilidad y correcciones

- El do-file será la fuente canónica de los resultados de Stata.
- Toda cifra de la página provendrá de archivos exportados por el do-file, no de transcripción manual.
- El código debe ejecutar de principio a fin sin `...` ni instrucciones que dependan de pasos omitidos.
- Se usará un único nombre de variable para el sesgo en todos los escenarios y gráficos.
- Las bases de Monte Carlo se guardarán explícitamente antes de combinarlas.
- Se evitará usar `preserve`/`restore` de una forma que pierda escalares o resultados necesarios para almacenar cada repetición.
- Cada Monte Carlo usará 1.000 repeticiones y 80.000 observaciones por repetición, construidas al replicar los ocho perfiles de clase 10.000 veces. Las semillas quedarán fijadas y documentadas en el do-file.
- El texto distinguirá aumentar una base mediante duplicación de observaciones de obtener una nueva muestra independiente. La duplicación demostrará que replicar exactamente los mismos datos no resuelve selección, pero no se presentará como mayor información estadística.
- La aleatorización se describirá como insesgada en repetición bajo el diseño, no como garantía de igualdad exacta entre la estimación de una muestra y el ATE.

## Materiales y límites

- Se editarán los dos Rmd existentes y los archivos dentro de `dofile/04_ParametrosStata/` necesarios para reproducibilidad.
- Se crearán archivos de resultados tabulares si son necesarios para que el Rmd consuma salidas verificadas.
- No se crearán todavía las diapositivas; solo se documenta el requisito de que posteriormente contengan todos los resultados.
- No se publicará en `docs` hasta que Ana María apruebe las vistas previas locales.
- No se modificarán capítulos posteriores salvo ajustes mínimos de navegación que resulten indispensables.

## Validación

La entrega se considerará completa cuando:

1. existan pruebas contractuales para estructura, uso exclusivo de `Y_i(D=1)` y `Y_i(D=0)`, descargas y ausencia de resultados transcritos manualmente;
2. el do-file ejecute completo en Stata 19 y produzca los archivos esperados;
3. las cifras visibles coincidan con las salidas exportadas;
4. el libro renderice sin errores;
5. ambos capítulos hayan sido inspeccionados en escritorio y móvil;
6. enlaces y descargas funcionen desde la vista previa local;
7. no aparezca sintaxis propia de diapositivas ni código incompleto en el HTML;
8. una revisión académica final no encuentre errores importantes pendientes.

## Estrategia de cambios

El trabajo continuará directamente en `main`, siguiendo la decisión ya aprobada para este libro, mediante commits pequeños y temáticos. Los artefactos locales ajenos a estos capítulos no se modificarán ni se incluirán en los commits.
