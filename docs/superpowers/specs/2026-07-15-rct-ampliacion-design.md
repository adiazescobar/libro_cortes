# Diseño de ampliación pedagógica de Experimentos aleatorizados

**Fecha:** 2026-07-15  
**Alcance:** `05-RCT.Rmd`, `06-RCT2.Rmd` y una clave docente privada externa.  
**Estado:** aprobado por Ana María.

## Objetivo

Aplicar a los capítulos de experimentos aleatorizados el estándar pedagógico aprobado en Parámetros causales, conservando íntegramente las demostraciones, simulaciones, ejemplos, resultados reproducibles y la verificación Stata–Python del piloto existente.

## Reglas transversales

- La uniformidad del libro no implica brevedad: se conservará el desarrollo suficiente para reconstruir argumentos y código.
- La teoría no tendrá descargas; la práctica mantendrá materiales inmediatamente después del título.
- Se usarán bloques visuales consistentes con el sistema existente.
- Las preguntas tipo examen no mostrarán respuestas, pistas, retroalimentación automática ni desplegables.
- Las soluciones se guardarán en una ubicación docente privada externa, comunicada fuera del repositorio.
- Ningún archivo rastreado, página HTML, `_bookdown.yml` o contenido bajo `docs` incluirá identificadores o rutas de la clave privada.
- La notación seguirá usando estados explícitos `Y_i(D=1)` y `Y_i(D=0)`.
- Bookdown será la única fuente de numeración: ningún encabezado incluirá prefijos manuales como `1.`, `PASO 1` o `Etapa 1`.
- Las cifras empíricas visibles provendrán de archivos canónicos exportados por Stata o de la verificación Stata–Python.
- Las futuras diapositivas incluirán todos los resultados, pero quedan fuera de esta ampliación.

## Capítulo teórico RCT

### Contenido que debe permanecer

Se preservarán:

1. pregunta causal, intuición y motivación;
2. resultados potenciales y estimandos;
3. independencia inducida por aleatorización;
4. descomposición de diferencia observada y sesgo de selección;
5. traducción al modelo de regresión;
6. simulaciones de autoselección frente a aleatorización;
7. RCT simple sin controles;
8. RCT simple con controles;
9. RCT estratificado sin controles adicionales;
10. RCT estratificado con controles adicionales;
11. heterogeneidad, interacciones y CATE;
12. centrado de covariables según Wooldridge;
13. interpretación, validez, amenazas, resumen, videos y referencias.

No se resumirán ni trasladarán fuera del cuerpo las demostraciones completas.

### Bloques de color

El capítulo incluirá como mínimo ocho bloques:

- **Intuición:** por qué el azar construye el contrafactual promedio.
- **Resultado clave:** independencia en expectativa, no balance exacto en cada muestra.
- **Demostración:** diferencia observada bajo aleatorización.
- **Advertencia:** significancia de balance no es prueba de aleatorización perfecta.
- **Comparación:** cuándo los cuatro modelos estiman el mismo parámetro y por qué difiere su precisión.
- **Resultado clave:** papel de estratos y controles pretratamiento.
- **Advertencia:** controles postratamiento y búsqueda oportunista de especificaciones.
- **Intuición:** interacción, efecto base y CATE.

Los bloques usarán clases existentes (`boxinfo`, `boxnote`, `boxwarning`, `boxejercicio`, `box-cuidado`, `box-resumen`) o variantes ya definidas en `style.css`.

### Preguntas tipo examen

Se añadirán exactamente tres preguntas codificadas internamente como `RCT-T1`, `RCT-T2` y `RCT-T3`:

1. **Identificación bajo aleatorización:** resultados potenciales, independencia y diferencia de medias.
2. **Cuatro especificaciones:** comparar RCT simple/estratificado con y sin controles, parámetro identificado y precisión.
3. **Heterogeneidad:** interpretar interacción, efecto base, CATE y centrado.

Cada pregunta tendrá puntaje sugerido, información suficiente y producto esperado. No incluirá solución ni pista.

## Capítulo práctico RCT

### Materiales y reproducibilidad

Se conservarán al inicio los enlaces a do-file, datos, resultados completos de Stata, notebook de Python, Colab y verificación Stata–Python. Los enlaces deben continuar funcionando desde la vista previa local.

### Desarrollo guiado

La práctica se organizará, sin numeración manual, en estas etapas ordenadas:

1. formular pregunta, tratamiento, resultado y unidad de asignación;
2. inspeccionar y preparar los datos;
3. fijar semilla y aleatorizar en Stata;
4. distinguir aleatorización simple y estratificada;
5. construir y leer una tabla de balance;
6. ejecutar la prueba conjunta de balance;
7. estimar RCT simple sin controles;
8. estimar RCT simple con controles;
9. estimar RCT estratificado sin controles adicionales;
10. estimar RCT estratificado con controles adicionales;
11. comparar coeficientes, errores estándar, intervalos y precisión;
12. elegir inferencia según unidad de asignación;
13. decidir cuándo incluir controles;
14. estimar heterogeneidad mediante interacciones;
15. interpretar el efecto base y CATE;
16. aplicar centrado de covariables;
17. replicar en Python/Colab;
18. verificar concordancia Stata–Python.

Cada etapa debe incluir, según corresponda, pregunta, comando, salida central, interpretación, error frecuente y práctica breve. No se pegarán logs completos ni código con elipsis.

### Resultados visibles

Se conservarán y ampliarán, cuando sea necesario:

- tabla descriptiva de datos y asignación;
- tabla canónica de balance;
- prueba conjunta de balance;
- tabla de cuatro especificaciones;
- interpretación de coeficientes y errores estándar;
- resultados de heterogeneidad por sexo, libros y edad;
- resultados con y sin centrado;
- tabla de verificación Stata–Python.

Toda cifra se interpolará desde los CSV existentes. Si una salida requerida no está exportada, primero se ampliará el pipeline y se regenerará.

### Bloques de color

La práctica tendrá como mínimo doce bloques de los tipos:

- **Comando clave**;
- **Salida central**;
- **Interpretación**;
- **Error frecuente**;
- **Resultado clave**;
- **Pregunta tipo examen**.

Los bloques guiarán una lectura continua y no sustituirán encabezados claros.

### Preguntas tipo examen

Se añadirán exactamente cuatro preguntas codificadas como `RCT-S1` a `RCT-S4`:

1. **Aleatorización y balance:** escribir comandos, explicar semilla y diagnosticar tabla/prueba conjunta.
2. **Cuatro modelos:** interpretar una tabla, comparar precisión y escoger especificación.
3. **Inferencia y controles:** detectar errores en errores estándar, clustering y controles postratamiento.
4. **Heterogeneidad y replicación:** interpretar interacción/CATE y diagnosticar una discrepancia Stata–Python.

Cada ejercicio especificará puntaje sugerido, comandos permitidos y producto esperado. No tendrá respuestas ni pistas visibles.

## Clave docente privada

Se creará una clave externa separada para profesora y monitor. No se registrará su nombre, ruta ni contenido dentro del repositorio.

La clave contendrá los siete códigos, y para cada uno:

- respuesta correcta;
- procedimiento;
- criterios de calificación;
- errores frecuentes;
- puntaje sugerido.

La clave usará notación consistente, tendrá permisos restringidos y permanecerá invisible para Git y para el render.

## Pruebas y validación

La ampliación estará completa cuando:

1. todos los contenidos teóricos enumerados permanezcan;
2. teoría contenga ocho o más bloques y exactamente tres preguntas;
3. práctica contenga las dieciocho etapas en orden, doce o más bloques y exactamente cuatro preguntas;
4. ningún encabezado de ambos capítulos tenga numeración manual;
5. las siete preguntas no contengan soluciones, pistas o desplegables;
6. la clave externa contenga siete códigos y cinco componentes por código;
7. la clave y sus identificadores no aparezcan en Git, `docs`, Rmd, YAML o HTML;
8. los CSV canónicos y la verificación Stata–Python sigan coincidiendo;
9. todos los enlaces y descargas respondan correctamente;
10. la suite automatizada pase;
11. ambos capítulos se rendericen y revisen en escritorio y móvil;
12. `docs` no reciba el render ampliado hasta aprobación explícita.

## Límites

- No se eliminarán ejemplos, demostraciones, simulaciones, videos o resultados actuales.
- No se crearán todavía diapositivas.
- No se publicará `docs`.
- No se reescribirá la historia Git para ocultar referencias ya redactadas; ningún contenido de soluciones se ha registrado.
