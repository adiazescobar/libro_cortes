# Ampliación pedagógica de Experimentos aleatorizados Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Ampliar los capítulos teórico y práctico de experimentos aleatorizados sin eliminar contenido, incorporando bloques pedagógicos, siete preguntas tipo examen, desarrollo empírico guiado y una clave docente privada externa.

**Architecture:** `05-RCT.Rmd` conservará todas sus demostraciones y simulaciones, añadiendo capas visuales y evaluación. `06-RCT2.Rmd` mantendrá el pipeline reproducible Stata–Python y reorganizará el flujo en dieciocho etapas sin numeración manual. Pruebas contractuales protegerán contenido, preguntas, fuentes canónicas, privacidad y render.

**Tech Stack:** R Markdown, bookdown/gitbook, R/knitr, Stata 19, Python/Jupyter, CSV canónicos, pytest, CSS existente.

## Global Constraints

- No eliminar ejemplos, demostraciones, simulaciones, videos ni resultados actuales.
- Usar `Y_i(D=1)` y `Y_i(D=0)` con estados explícitos.
- Teoría sin descargas; práctica con descargas inmediatamente después del título.
- Exactamente tres preguntas `RCT-T1`–`RCT-T3` y cuatro `RCT-S1`–`RCT-S4`.
- Ninguna pregunta estudiantil tendrá solución, pista, desplegable o retroalimentación automática.
- La clave se creará en una ubicación docente privada externa proporcionada fuera de este plan.
- El repositorio no contendrá nombre, ruta, identificadores ni contenido de la clave privada.
- Bookdown será la única numeración; no usar números, `PASO` o `Etapa` en encabezados.
- Toda cifra empírica visible provendrá de CSV canónicos o de verificación Stata–Python.
- Mantener concordancia Stata–Python en coeficiente, error estándar, N y R² para los cuatro modelos.
- Renderizar en `/private/tmp/libro_cortes_rct_ampliado`; no publicar `docs`.
- Preservar cambios locales y artefactos ajenos.

---

### Task 1: Contratos de preservación, pedagogía y privacidad

**Files:**
- Create: `tests/test_rct_pedagogy_contract.py`
- Modify: `dofile/06_RCT_Stata/tests/test_rct_chapter.py`

**Interfaces:**
- Consumes: `05-RCT.Rmd`, `06-RCT2.Rmd`, `_bookdown.yml`, CSV y activos actuales.
- Produces: pruebas RED que fijan preservación, bloques, preguntas, etapas, numeración y privacidad.

- [ ] **Step 1: Fijar contenidos teóricos que deben permanecer**

Crear pruebas que exijan fragmentos distintivos de resultados potenciales, descomposición del sesgo, cuatro escenarios, simulación de autoselección/aleatorización, CATE, centrado de Wooldridge, validez, amenazas, videos y referencias. Verificar al menos los encabezados semánticos, no números de línea.

- [ ] **Step 2: Fijar bloques y preguntas teóricas**

Exigir ocho o más fenced divs con clases `.box*`, etiquetas `Intuición`, `Resultado clave`, `Demostración`, `Advertencia` y `Comparación`, y códigos globalmente exactos/únicos `RCT-T1`–`RCT-T3`. Cada código debe vivir en su propio bloque, incluir puntaje y producto esperado, y carecer de marcadores de respuesta.

- [ ] **Step 3: Fijar las dieciocho etapas prácticas**

```python
stages = [
    "Pregunta, tratamiento, resultado y unidad de asignación",
    "Inspección y preparación de los datos",
    "Semilla y asignación aleatoria",
    "Aleatorización simple y estratificada",
    "Tabla de balance",
    "Prueba conjunta de balance",
    "RCT simple sin controles",
    "RCT simple con controles",
    "RCT estratificado sin controles adicionales",
    "RCT estratificado con controles adicionales",
    "Comparación de las cuatro especificaciones",
    "Inferencia y unidad de asignación",
    "Cuándo incluir controles",
    "Heterogeneidad mediante interacciones",
    "Efecto base y CATE",
    "Centrado de covariables",
    "Replicación en Python y Colab",
    "Concordancia Stata–Python",
]
```

Exigir que los dieciocho encabezados H3 aparezcan una vez y en ese orden, sin prefijos manuales.

- [ ] **Step 4: Fijar bloques y preguntas prácticas**

Exigir doce o más bloques, etiquetas `Comando clave`, `Salida central`, `Interpretación`, `Error frecuente`, `Resultado clave`, y exactamente `RCT-S1`–`RCT-S4`. Cada pregunta debe incluir una vez `Puntaje sugerido`, `Comandos permitidos` y `Producto esperado`, sin respuestas.

- [ ] **Step 5: Prohibir numeración manual y exposición privada**

Aplicar a ambos Rmd regex que rechacen encabezados con números, `PASO`, `Paso` o `Etapa`. Escanear recursivamente `docs`, Rmd y YAML contra los identificadores privados proporcionados al ejecutar, construyéndolos en el test a partir de fragmentos para no dejar el token contiguo rastreado.

- [ ] **Step 6: Proteger resultados y descargas**

Mantener pruebas existentes de cuatro modelos, balance, heterogeneidad y verificación. Exigir que todos los enlaces de materiales existan y que el Rmd consuma los CSV canónicos en lugar de resultados transcritos.

- [ ] **Step 7: Ejecutar RED**

Run: `/private/tmp/libro_cortes_rct_venv/bin/python -m pytest -q tests/test_rct_pedagogy_contract.py dofile/06_RCT_Stata/tests/test_rct_chapter.py`

Expected: fallos por bloques, preguntas, dieciocho etapas y numeración manual; contratos reproducibles actuales permanecen verdes.

- [ ] **Step 8: Commit de contratos**

```bash
git add tests/test_rct_pedagogy_contract.py dofile/06_RCT_Stata/tests/test_rct_chapter.py
git commit -m "test: define expanded RCT pedagogy"
```

---

### Task 2: Ampliación conservadora del capítulo teórico

**Files:**
- Modify: `05-RCT.Rmd`
- Test: `tests/test_rct_pedagogy_contract.py`

**Interfaces:**
- Consumes: contenido teórico completo y contratos Task 1.
- Produces: teoría preservada con bloques y tres preguntas.

- [ ] **Step 1: Eliminar numeración manual de encabezados**

Cambiar `## 1)`, `## 2)`, `## 3)` y `## 4)` por títulos semánticos sin prefijo. No cambiar el orden ni el contenido de las cuatro especificaciones.

- [ ] **Step 2: Añadir ocho bloques pedagógicos**

Insertar junto a los conceptos correspondientes:

- intuición del contrafactual promedio;
- resultado de independencia en expectativa;
- demostración de sesgo cero en expectativa;
- advertencia sobre balance muestral;
- comparación de cuatro especificaciones;
- resultado sobre estratos y controles pretratamiento;
- advertencia sobre controles postratamiento y selección de especificación;
- intuición de interacción, efecto base y CATE.

Cada bloque tendrá uno a tres párrafos o una ecuación, sin duplicar demostraciones.

- [ ] **Step 3: Crear `RCT-T1`**

Plantear un RCT individual con resultado potencial, asignación y pequeña diferencia basal fortuita. Pedir identificar el estimando, demostrar insesgadez en expectativa y explicar por qué balance exacto no es requisito. Incluir puntaje y producto esperado.

- [ ] **Step 4: Crear `RCT-T2`**

Presentar cuatro columnas de regresión con el mismo coeficiente aproximado y errores diferentes. Pedir relacionar cada columna con diseño/controles, identificar el parámetro y explicar precisión. No revelar la correspondencia.

- [ ] **Step 5: Crear `RCT-T3`**

Dar un modelo con interacción tratamiento–covariable, covariable no centrada y media conocida. Pedir efecto base, CATE en dos valores, ATE mediante centrado y advertencia interpretativa.

- [ ] **Step 6: Ejecutar pruebas y tejido teórico**

Run: `/private/tmp/libro_cortes_rct_venv/bin/python -m pytest -q tests/test_rct_pedagogy_contract.py -k theory`

Run: `Rscript -e "rmarkdown::render('05-RCT.Rmd', output_dir='/private/tmp/libro_cortes_rct_ampliado/theory')"`

Expected: contratos teóricos verdes y HTML creado.

- [ ] **Step 7: Commit teórico**

```bash
git add 05-RCT.Rmd
git commit -m "docs: add exam practice to RCT theory"
```

---

### Task 3: Desarrollo guiado de la clase práctica y clave privada

**Files:**
- Modify: `06-RCT2.Rmd`
- Modify: `style.css` only if required by the existing visual system.
- Modify pipeline files only if a required visible result is not exported.
- Create externally: private teacher key at the out-of-repository path supplied during execution.
- Test: `tests/test_rct_pedagogy_contract.py`
- Test: `dofile/06_RCT_Stata/tests/test_rct_chapter.py`

**Interfaces:**
- Consumes: Stata/Python artifacts and eighteen stages.
- Produces: práctica desarrollada, cuatro preguntas y clave externa.

- [ ] **Step 1: Reorganizar encabezados sin numeración manual**

Convertir `PASO 1`, `Paso 2`, etc. en los dieciocho títulos semánticos exactos de Task 1. Mantener materiales al comienzo, lecturas y pregunta empírica.

- [ ] **Step 2: Expandir diseño, datos y aleatorización**

Mostrar definición de unidad, tratamiento, resultado, inspección de datos, semilla, asignación simple y estratificada. Incluir código Stata ejecutable y explicar reproducibilidad, probabilidad de asignación y balance fortuito.

- [ ] **Step 3: Expandir balance**

Explicar balance variable por variable, diferencias estandarizadas o métricas disponibles, prueba conjunta `D ~ X`, límites de los p-valores y qué hacer ante desbalance fortuito. Mostrar tabla/prueba desde CSV canónico.

- [ ] **Step 4: Expandir las cuatro especificaciones**

Mostrar los cuatro comandos completos y una tabla comparativa con coeficiente, error estándar, intervalo, N y R². Explicar qué cambia y qué permanece, por qué los controles pretratamiento pueden aumentar precisión y cómo incorporar estratos.

- [ ] **Step 5: Expandir inferencia y controles**

Distinguir asignación individual y por clúster; errores robustos y clustering; grados de libertad cuando corresponda; controles pretratamiento frente a postratamiento.

- [ ] **Step 6: Expandir heterogeneidad y centrado**

Mostrar interacciones por sexo, libros y edad; leer efecto base y suma de coeficientes; presentar CATE; comparar variable centrada/no centrada y la interpretación del coeficiente de tratamiento.

- [ ] **Step 7: Expandir Python/Colab y verificación**

Conservar notebook, badge y videos. Explicar mapeo Stata–Python de fórmula, covarianza robusta, muestra y pesos/estratos si aplican. Mostrar tabla de concordancia desde CSV.

- [ ] **Step 8: Añadir bloques pedagógicos**

Distribuir al menos doce bloques con comandos, salidas, interpretaciones, errores frecuentes y resultados clave. Las cifras se interpolan desde CSV.

- [ ] **Step 9: Crear `RCT-S1`–`RCT-S4`**

- S1: aleatorización, semilla, balance individual y conjunto.
- S2: lectura de tabla de cuatro modelos y precisión.
- S3: inferencia, clustering y controles postratamiento.
- S4: interacción/CATE y discrepancia Stata–Python.

Cada una incluirá puntaje, comandos permitidos y producto esperado, sin respuesta o pista.

- [ ] **Step 10: Crear clave externa**

En la ubicación privada suministrada durante la ejecución, crear siete secciones. Cada código tendrá respuesta correcta, procedimiento, criterios, errores frecuentes y puntaje. Usar notación explícita y permisos `0600`. No mencionar nombre/ruta en archivos rastreados ni informes.

- [ ] **Step 11: Ejecutar pruebas y render aislado**

Run: `/private/tmp/libro_cortes_rct_venv/bin/python -m pytest -q tests/test_rct_pedagogy_contract.py dofile/06_RCT_Stata/tests`

Expected: todos verdes; verificación de cuatro modelos permanece PASS.

- [ ] **Step 12: Commit práctico**

```bash
git add 06-RCT2.Rmd style.css tests/test_rct_pedagogy_contract.py dofile/06_RCT_Stata/tests
git commit -m "docs: restore guided RCT practice"
```

---

### Task 4: Validación académica, reproducible, privada y visual

**Files:**
- Verify: `05-RCT.Rmd`
- Verify: `06-RCT2.Rmd`
- Verify externally: teacher key at the out-of-repository location supplied during execution.
- Verify: `/private/tmp/libro_cortes_rct_ampliado/experimentos-aleatorizados-clase-teorica.html`
- Verify: `/private/tmp/libro_cortes_rct_ampliado/experimentos-aleatorizados-clase-empirica.html`

**Interfaces:**
- Consumes: Tasks 1–3.
- Produces: vistas previas locales verificadas; ninguna publicación en `docs`.

- [ ] **Step 1: Ejecutar suite completa**

Run: `/private/tmp/libro_cortes_rct_venv/bin/python -m pytest -q`

Expected: cero fallos.

- [ ] **Step 2: Verificar reproducibilidad**

Ejecutar tests de `dofile/06_RCT_Stata/tests`; confirmar cuatro modelos Stata–Python en PASS y que Rmd consume los CSV canónicos. Ejecutar Stata/Python únicamente si una salida cambió o un test detecta desactualización.

- [ ] **Step 3: Verificar clave y privacidad**

Validar siete códigos × cinco componentes, notación, puntajes, permisos `0600` e invisibilidad Git. Escanear Rmd, YAML, `docs`, HTML temporal e historial nuevo contra identificadores privados, sin copiar soluciones al informe.

- [ ] **Step 4: Auditar preguntas y rúbricas**

Comprobar información suficiente, única interpretación razonable, correspondencia pregunta–clave, puntajes y productos esperados. Especial atención a clustering, controles postratamiento, CATE y concordancia Stata–Python.

- [ ] **Step 5: Renderizar libro completo**

Run: `Rscript -e "bookdown::render_book('index.Rmd', output_dir='/private/tmp/libro_cortes_rct_ampliado')"`

Expected: exit 0. Si HTML dividido aparece en raíz, copiar solo los dos HTML frescos al temporal y verificar hashes.

- [ ] **Step 6: QA escritorio y móvil**

Teoría: contenido completo, ocho bloques, tres preguntas, videos y simulaciones. Práctica: materiales al inicio, dieciocho etapas, doce bloques, cuatro preguntas, tablas, código, notebook y gráficos. Verificar cero doble numeración, overflow global o respuestas visibles.

- [ ] **Step 7: Verificar enlaces**

Iniciar servidor temporal y comprobar HTTP 200 para ambos capítulos y todos los materiales descargables.

- [ ] **Step 8: Commit de ajustes finales**

Si la revisión encuentra cambios, aplicar TDD, repetir pruebas focales y crear:

```bash
git add 05-RCT.Rmd 06-RCT2.Rmd style.css tests dofile/06_RCT_Stata/tests
git commit -m "fix: address expanded RCT review"
```

- [ ] **Step 9: Entregar vistas previas**

Entregar enlaces locales y mantener `docs` sin publicar.
