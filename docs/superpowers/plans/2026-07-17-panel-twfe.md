# Datos de panel y TWFE Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Dividir y reconstruir el capítulo de panel/TWFE en una pareja teórica–empírica académicamente precisa, reproducible en Stata y uniforme con el libro.

**Architecture:** El capítulo existente conserva su URL y se convierte en teoría. Un capítulo práctico nuevo consume resultados canónicos exportados por un do-file corregido. Un contrato pytest fija contenido, parámetros, sintaxis, privacidad y navegación.

**Tech Stack:** Bookdown/R Markdown, Stata 19, Python/pytest, CSV, PNG.

## Global Constraints

- Conservar `datos-de-panel-did-y-twfe-en-stata.html` para la teoría.
- Usar `Datos de panel y TWFE — Clase teórica` y `Datos de panel y TWFE — Clase empírica`.
- Descargas únicamente al comienzo de la práctica.
- Exactamente tres preguntas teóricas y cuatro empíricas.
- Ninguna solución pública o desplegable.
- Distinguir comparaciones Bacon de pesos sobre efectos grupo-periodo.
- Declarar el parámetro de cada estimador moderno.
- Usar `id` para panel y clustering.
- No presentar tendencias específicas ni event studies TWFE como soluciones automáticas.
- Bookdown controla toda numeración.

---

### Task 1: Contrato de preservación, academia y estructura

**Files:**
- Create: `tests/fixtures/twfe_chapter_baseline.json`
- Create: `tests/test_twfe_pedagogy_contract.py`

**Interfaces:**
- Consumes: `11-TWFE.Rmd`, `_bookdown.yml` y helpers de `test_power_pedagogy_contract.py`.
- Produces: contrato ejecutable para Tasks 2–5.

- [ ] **Step 1: Capture the baseline inventory**

Registrar encabezados, fragmentos conceptuales, comandos, enlaces y figuras distintivos del capítulo actual para exigir que cada unidad relevante sobreviva en la unión de teoría y práctica.

- [ ] **Step 2: Write failing structure tests**

Exigir archivos consecutivos, títulos y anchors estables; materiales primero en práctica; T1–T3 y S1–S4; ausencia de numeración manual y clave pública.

- [ ] **Step 3: Write failing academic tests**

Exigir los marcadores `Goodman-Bacon`, `comparaciones 2×2`, `de Chaisemartin`, `pesos negativos`, `ATT(g,t)`, `interaction-weighted`, `imputación`, `status quo`, y advertencias sobre event studies y tendencias específicas.

- [ ] **Step 4: Write failing code tests**

Prohibir `cluster(i)`, `did_imputation Y i`, `ssc install did_multiplegt,` y matrices consumidas antes de crearse. Exigir `twowayfeweights`, `first_stage(i.id i.t)` y resultados CSV.

- [ ] **Step 5: Verify RED**

Run: `pytest -q tests/test_twfe_pedagogy_contract.py`

Expected: FAIL por ausencia del capítulo práctico, de los resultados y de varias correcciones.

- [ ] **Step 6: Commit**

Run: `git add tests/fixtures/twfe_chapter_baseline.json tests/test_twfe_pedagogy_contract.py && git commit -m "test: define panel and TWFE chapter contract"`

### Task 2: Reconstrucción de la clase teórica

**Files:**
- Modify: `11-TWFE.Rmd`

**Interfaces:**
- Consumes: contrato de Task 1, `11-TWFE-pesos-v2.Rmd` y materiales de las clases 16–17.
- Produces: teoría autocontenida con T1–T3.

- [ ] **Step 1: Implement panel foundations**

Conservar pooled/FE/FD/RE, transformación within, exogeneidad y equivalencia FE–FD con dos periodos, retirando comandos extensos que pertenezcan a la práctica.

- [ ] **Step 2: Implement causal 2×2 and staggered theory**

Separar identidad algebraica de identificación causal; desarrollar adopción simultánea, escalonada, heterogeneidad y las tres familias Bacon.

- [ ] **Step 3: Implement weights and event-study theory**

Integrar residualización de \(D\), pesos sobre efectos grupo-periodo y contaminación de leads/lags TWFE.

- [ ] **Step 4: Implement the method–parameter map**

Incluir la tabla exacta para `csdid`, `eventstudyinteract`, `did_imputation`, `did_multiplegt_dyn` y `did2s`.

- [ ] **Step 5: Add exactly T1–T3**

Cada caja incluye Código, Tipo, Fuente, Enunciado, Puntaje sugerido y Producto esperado; no incluye respuesta, resultado esperado o pista.

- [ ] **Step 6: Verify GREEN for theory**

Run: `pytest -q tests/test_twfe_pedagogy_contract.py -k "theory or title or academic"`

Expected: PASS.

- [ ] **Step 7: Commit**

Run: `git add 11-TWFE.Rmd && git commit -m "feat: rebuild panel and TWFE theory"`

### Task 3: Do-file y resultados canónicos

**Files:**
- Modify: `dofile/11_TWFE/11_stata.do`
- Create: `dofile/11_TWFE/results/panel_estimators.csv`
- Create: `dofile/11_TWFE/results/twfe_2x2.csv`
- Create: `dofile/11_TWFE/results/twfe_staggered.csv`
- Create: `dofile/11_TWFE/results/twfe_eventstudy.csv`
- Create: `dofile/11_TWFE/results/method_parameter_map.csv`
- Create: figures under `dofile/11_TWFE/figures/`

**Interfaces:**
- Consumes: DGP and parameter definitions in the spec.
- Produces: stable public tables/figures consumed by Task 4.

- [ ] **Step 1: Add result-schema tests**

Exigir identificadores de DGP, método, parámetro, horizonte, muestra de comparación, coeficiente, EE y verdad cuando exista.

- [ ] **Step 2: Verify RED**

Run: `pytest -q tests/test_twfe_pedagogy_contract.py -k results`

Expected: FAIL porque los CSV no existen.

- [ ] **Step 3: Correct installation and identifier syntax**

Usar `id`, `vce(cluster id)`, `did_imputation Y id t`, `ssc install did_multiplegt_dyn`, `first_stage(i.id i.t)` y matrices consistentes para `event_plot`.

- [ ] **Step 4: Export foundational results**

Ejecutar pooled/FE/FD/RE y la equivalencia 2×2; exportar `panel_estimators.csv` y `twfe_2x2.csv`.

- [ ] **Step 5: Export staggered diagnostics**

Ejecutar TWFE, `bacondecomp` y `twowayfeweights` en un DGP con efectos dinámicos heterogéneos; exportar resultados interpretables.

- [ ] **Step 6: Export comparable modern estimates**

Ejecutar los métodos instalables de manera estable y almacenar parámetro, horizonte y población. Omitir de una figura común cualquier salida no comparable, explicando la razón en metadatos.

- [ ] **Step 7: Execute Stata**

Run: `/Applications/StataNow/StataSE.app/Contents/MacOS/stata-se -b do dofile/11_TWFE/11_stata.do`

Expected: exit 0, log sin errores y todos los CSV requeridos presentes.

- [ ] **Step 8: Verify GREEN**

Run: `pytest -q tests/test_twfe_pedagogy_contract.py -k "results or stata"`

Expected: PASS.

- [ ] **Step 9: Commit**

Run: `git add dofile/11_TWFE/11_stata.do dofile/11_TWFE/results dofile/11_TWFE/figures && git commit -m "feat: export canonical panel and TWFE results"`

### Task 4: Clase empírica y clave privada

**Files:**
- Create: `11-TWFEStata.Rmd`
- Modify: `_bookdown.yml`
- Create: `claves_privadas/11_TWFE_clave.md`

**Interfaces:**
- Consumes: outputs of Task 3.
- Produces: practice chapter with S1–S4 and private grading key.

- [ ] **Step 1: Register the chapter**

Insertar `11-TWFEStata.Rmd` inmediatamente después de `11-TWFE.Rmd`, sin cambiar el orden de capítulos posteriores.

- [ ] **Step 2: Add materials first**

Enlazar do-file, scripts R/Python, CSV y notebook/Colab al comienzo.

- [ ] **Step 3: Build the guided empirical sequence**

Implementar panel, equivalencia 2×2, adopción escalonada, Bacon, pesos, event study, métodos modernos y checklist con ciclos comando–resultado–interpretación.

- [ ] **Step 4: Add exactly S1–S4**

Cada caja incluye Código, Tipo, Fuente, Enunciado, Puntaje sugerido, Comandos permitidos y Producto esperado, sin respuestas.

- [ ] **Step 5: Add the private key**

Resolver T1–T3 y S1–S4 con rúbricas; confirmar exclusión de `_bookdown.yml` y `docs/`.

- [ ] **Step 6: Verify GREEN**

Run: `pytest -q tests/test_twfe_pedagogy_contract.py`

Expected: PASS.

- [ ] **Step 7: Commit**

Run: `git add 11-TWFEStata.Rmd _bookdown.yml claves_privadas/11_TWFE_clave.md && git commit -m "feat: add panel and TWFE empirical class"`

### Task 5: Verificación integral y vista previa

**Files:**
- Verify: source, outputs, rendered HTML and navigation.

**Interfaces:**
- Consumes: Tasks 1–4.
- Produces: reviewable pair ready for approval.

- [ ] **Step 1: Run the official suite**

Run: `pytest -q tests`

Expected: all tests PASS.

- [ ] **Step 2: Check diffs**

Run: `git diff --check`

Expected: no whitespace errors.

- [ ] **Step 3: Render cleanly**

Run: `Rscript -e "bookdown::render_book('index.Rmd', output_dir='/private/tmp/libro_cortes_twfe_review')"`

Expected: exit 0; old theory URL and new practice URL exist.

- [ ] **Step 4: Inspect output**

Confirmar títulos, navegación consecutiva, descargas iniciales, tablas/gráficos, siete preguntas, ausencia de soluciones y ausencia de doble numeración.

- [ ] **Step 5: Record state**

Actualizar memoria compartida con decisiones, pruebas, render y estado de publicación.

