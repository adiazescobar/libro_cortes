# Piloto RCT: clase teórica, clase empírica y verificación Stata–Python

**Fecha:** 2026-07-14  
**Estado:** aprobado conceptualmente; pendiente revisión de esta especificación  
**Alcance inicial:** `05-RCT.Rmd`, `06-RCT2.Rmd` y sus archivos auxiliares

## Objetivo

Convertir el módulo de experimentos aleatorizados en el patrón canónico del libro de Econometría Avanzada: una clase teórica seguida por una clase empírica que parte de Stata, replica la misma estimación en Python/Google Colab y verifica automáticamente que ambos lenguajes produzcan los mismos resultados dentro de tolerancias explícitas.

El piloto debe demostrar que la estructura pedagógica, el diseño visual, los enlaces de descarga y la verificación numérica funcionan antes de propagarlos a los demás temas.

## Restricciones globales

- Stata es siempre el punto de partida y Python/Colab la réplica.
- Stata y Python deben leer la misma base congelada `data.dta`.
- No se escribirán resultados numéricos manualmente en los capítulos.
- Los resultados visibles deben provenir de archivos generados.
- No se modificarán ni eliminarán borradores o cambios locales ajenos al piloto.
- No se enviarán correos.
- Los nuevos correos serán preview-only por defecto.
- No se usará un iframe vivo de Google Colab.
- El render continuará usando bookdown/gitbook y no debe romper PDF o EPUB deliberadamente, aunque el piloto visual se valide primero en HTML.

## Arquitectura del módulo

### Clase teórica

`05-RCT.Rmd` será el capítulo “Experimentos aleatorizados — Clase teórica”. Su estructura canónica será:

1. Objetivos de aprendizaje.
2. Motivación aplicada.
3. Modelo y resultados potenciales.
4. Supuestos de identificación.
5. Derivación e intuición formal.
6. Interpretación económica.
7. Regresión e inferencia en un RCT.
8. Errores comunes.
9. Resumen.
10. Preguntas para clase.
11. Lecturas recomendadas.

Las simulaciones R existentes se conservarán cuando aporten intuición visual. No constituirán la réplica empírica Stata–Python.

### Clase empírica

`06-RCT2.Rmd` será el capítulo “Experimentos aleatorizados — Clase empírica”. Su estructura canónica será:

1. Objetivos de la práctica.
2. Pregunta empírica.
3. Datos.
4. Preparación de la base en Stata.
5. Estimación en Stata.
6. Interpretación de resultados en Stata.
7. Replicación en Python/Google Colab.
8. Verificación Stata vs. Python.
9. Errores frecuentes.
10. Ejercicios empíricos.
11. Descarga los archivos.

Las secciones pedagógicas no numeradas usarán `{-}`. Las partes sustantivas del desarrollo permanecerán numeradas. No habrá encabezados Markdown de nivel `#` dentro de un capítulo.

## Datos y especificaciones

La única base del piloto será:

`dofile/06_RCT_Stata/data.dta`

Antes de estimar, ambos lenguajes construirán de forma equivalente:

- `D = 1(grupo == "B")`;
- `y = resultado`;
- `mujer = 1(genero == "Mujer")`;
- indicadores de nivel académico;
- indicadores de semestre con la misma categoría base;
- las mismas muestras completas para cada modelo.

Los cuatro modelos principales serán:

1. diferencia de medias: `y ~ D`;
2. RCT simple con controles predeterminados;
3. RCT estratificado mediante efectos fijos de semestre;
4. RCT estratificado con controles predeterminados.

Los modelos usarán errores estándar robustos HC1, equivalentes a `vce(robust)` en Stata. Las extensiones de heterogeneidad se mantendrán en la práctica, pero se verificarán después de que los cuatro modelos principales pasen el contrato básico.

## Flujo reproducible

1. `clase6_stata.do` lee `data.dta`, estima los modelos y exporta un CSV normalizado.
2. `clase6_python.ipynb` lee el mismo `data.dta`, replica las transformaciones y exporta un CSV con el mismo esquema.
3. Un comparador independiente lee ambos CSV, combina por `modelo` y `termino`, calcula diferencias absolutas y asigna `PASS`, `WARN` o `FAIL`.
4. El comparador genera la tabla que consume `06-RCT2.Rmd`.
5. El capítulo nunca contiene cifras copiadas manualmente.

## Contrato de resultados

Los resultados Stata y Python compartirán estas columnas:

```text
modelo,termino,coeficiente,error_estandar,N,R2,prueba,estadistico,p_value
```

La tabla de verificación añadirá:

```text
coef_abs_diff,se_abs_diff,N_igual,R2_abs_diff,estado
```

Tolerancias:

- coeficientes: diferencia absoluta menor que `1e-3`;
- errores estándar: diferencia absoluta menor que `1e-3`;
- N: igualdad exacta;
- R²: diferencia absoluta menor que `1e-2`.

`PASS` requiere satisfacer todas las condiciones aplicables. `WARN` se reserva para diferencias esperadas y documentadas que no afectan coeficientes ni N. Cualquier incumplimiento no explicado será `FAIL` y bloqueará la publicación del piloto.

## Estructura de archivos del piloto

Se conservarán las rutas históricas para no romper enlaces. Dentro de `dofile/06_RCT_Stata/` se normalizará el flujo:

```text
data.dta
clase6_stata.do
clase6_python.ipynb
clase6_R.R
results/resultados_stata.csv
results/resultados_python.csv
results/verificacion_stata_python.csv
```

El comparador se ubicará inicialmente junto al módulo RCT. Solo se promoverá a una herramienta común después de validar que su interfaz sirve para otros temas.

## Integración con Colab

El capítulo empírico usará el patrón B + D:

- botón visible “Abrir en Colab”;
- enlace estable al notebook versionado en GitHub;
- fragmentos clave de Python visibles en el capítulo;
- tabla automática de verificación Stata–Python.

No se incluirá preview HTML ni iframe en el piloto. Esta decisión podrá revisarse después de probar peso de página, navegación móvil y duplicación de contenido.

## Diseño visual

El piloto adoptará el sistema tipográfico del libro de Econometría II:

- Fraunces para títulos;
- Hanken Grotesk para cuerpo;
- JetBrains Mono para código y navegación;
- fondo `#FCFCFB`, tinta `#17181A` y acento `#C0562F`;
- ancho de lectura máximo de 840 px.

Se añadirán cajas específicas, compatibles con las cajas canónicas existentes:

- `box-stata`: preparación y estimación en Stata;
- `box-colab`: apertura y réplica en Colab;
- `box-verificacion`: resultados del comparador;
- `box-cuidado`: diferencias de implementación y errores frecuentes;
- `box-ejercicios`: ejercicios;
- `box-resumen`: síntesis.

Los cambios de CSS se limitarán a reglas reutilizables y no reemplazarán estilos de manera indiscriminada.

## Manejo de errores

- Si falta `data.dta`, Stata y Python deben detenerse; Python no generará datos de ejemplo.
- Si faltan columnas requeridas, ambos flujos deben informar sus nombres y detenerse.
- Si los CSV no comparten claves `modelo`–`termino`, el comparador debe producir `FAIL`.
- Si una especificación usa una muestra distinta, la diferencia de N debe bloquear la verificación.
- Si el render no encuentra la tabla generada, debe mostrar un error, no una tabla desactualizada.
- Los resultados generados deberán llevar metadatos mínimos de fecha, software y versión del flujo.

## Validación

El piloto se considerará listo para revisión cuando:

1. Stata ejecute sin errores y genere su CSV.
2. Python ejecute sin errores y genere su CSV.
3. Los cuatro modelos principales obtengan `PASS`.
4. Bookdown renderice el libro sin romper capítulos vecinos.
5. Los enlaces a `data.dta`, `.do` y `.ipynb` funcionen.
6. La página se revise visualmente en escritorio y ancho móvil.
7. No aparezca ninguna dirección de estudiantes ni credencial en los nuevos archivos.

## Fuera de alcance del piloto

- Migración de DiD, TWFE, PSM, IPW, IV o RDD.
- Separación definitiva de controles sintéticos.
- Reescritura completa de correos.
- Incorporación de estudiantes 2026-2.
- Envío de correos.
- Eliminación de archivos históricos o borradores.
- Rotación externa de credenciales Gmail.

## Secuencia posterior al piloto

Una vez aprobado el RCT renderizado, se propagará el patrón en este orden:

1. Parámetros causales.
2. Malos controles.
3. DiD.
4. TWFE.
5. PSM e IPW.
6. Controles sintéticos como módulo separado.
7. IV/LATE.
8. RDD.
9. Correos modulares por tema.

