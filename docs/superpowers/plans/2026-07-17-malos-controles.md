# Malos controles Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Corregir y ampliar la pareja de capítulos sobre malos controles con teoría causal precisa, resultados reproducibles de Stata y evaluación sin respuestas públicas.

**Architecture:** Un contrato automatizado fija la arquitectura, el contenido académico y la separación entre materiales públicos y clave privada. La teoría se reconstruye desde el estimando y los caminos del DAG; la práctica usa tres DGP canónicos y consume resultados exportados por el do-file.

**Tech Stack:** Bookdown/R Markdown, Stata 19, Python/pytest, CSV.

## Global Constraints

- Conservar los anchors `bad-controls-teoria` y `bad-controls-stata`.
- Usar títulos `Malos controles — Clase teórica` y `Malos controles — Clase empírica`.
- Mantener \(Y(D=1)\) y \(Y(D=0)\).
- No incluir respuestas o claves en el libro público.
- Las descargas de la práctica deben aparecer primero.
- Bookdown controla la numeración.

---

### Task 1: Contrato académico y estructural

**Files:**
- Create: `tests/test_bad_controls_contract.py`

**Interfaces:**
- Consumes: helpers públicos de `tests/test_power_pedagogy_contract.py`.
- Produces: contrato ejecutable para teoría, práctica, resultados y privacidad.

- [ ] **Step 1: Write the failing tests**

Crear pruebas para títulos y anchors; materiales primero; tres preguntas `BC-T1`–`BC-T3`; cuatro preguntas `BC-S1`–`BC-S4`; ausencia de respuestas; secuencia de casos; notación; correcciones de ATT, colisionador y proxy; lectura de CSV canónicos; y ausencia de clave en `_bookdown.yml`.

- [ ] **Step 2: Run tests to verify they fail**

Run: `pytest -q tests/test_bad_controls_contract.py`

Expected: FAIL por ausencia del contrato pedagógico y de resultados canónicos.

- [ ] **Step 3: Commit the red contract**

Run: `git add tests/test_bad_controls_contract.py docs/superpowers/specs/2026-07-17-malos-controles-design.md docs/superpowers/plans/2026-07-17-malos-controles.md && git commit -m "test: define bad controls chapter contract"`

### Task 2: Revisión de la clase teórica

**Files:**
- Modify: `09-BadControls.Rmd`

**Interfaces:**
- Consumes: contrato de Task 1 y fuentes de aula.
- Produces: capítulo teórico completo con exactamente tres preguntas.

- [ ] **Step 1: Implement the minimum theory satisfying the contract**

Reorganizar el capítulo según el diseño; corregir ATT, regla temporal, mediadores, colisionadores, controles pretratamiento y signo del sesgo; preservar la demostración de agrupación.

- [ ] **Step 2: Run theory-focused tests**

Run: `pytest -q tests/test_bad_controls_contract.py -k "theory or title or notation"`

Expected: PASS.

- [ ] **Step 3: Commit**

Run: `git add 09-BadControls.Rmd && git commit -m "feat: revise bad controls theory"`

### Task 3: Resultados canónicos de Stata

**Files:**
- Modify: `dofile/10_BadControls/10_stata.do`
- Create: `dofile/10_BadControls/results/bad_controls_estimates.csv`
- Create: `dofile/10_BadControls/results/bad_controls_montecarlo.csv`

**Interfaces:**
- Consumes: tres DGP definidos en el diseño.
- Produces: dos CSV estables que la práctica muestra como tablas.

- [ ] **Step 1: Add failing result contract**

El test debe exigir columnas `case`, `specification`, `estimand`, `estimate`, `se`, `truth` y combinaciones únicas por caso/especificación.

- [ ] **Step 2: Verify RED**

Run: `pytest -q tests/test_bad_controls_contract.py -k results`

Expected: FAIL porque no existen los CSV.

- [ ] **Step 3: Implement and execute Stata**

El do-file debe recrear los tres DGP, almacenar cada especificación, exportar los CSV y conservar las gráficas.

Run: `/Applications/StataNow/StataSE.app/Contents/MacOS/stata-se -b do dofile/10_BadControls/10_stata.do`

Expected: exit 0; ambos CSV existen y contienen estimaciones finitas.

- [ ] **Step 4: Verify GREEN**

Run: `pytest -q tests/test_bad_controls_contract.py -k results`

Expected: PASS.

- [ ] **Step 5: Commit**

Run: `git add dofile/10_BadControls/10_stata.do dofile/10_BadControls/results && git commit -m "feat: export canonical bad controls results"`

### Task 4: Reconstrucción de la clase empírica y clave privada

**Files:**
- Modify: `10-BadControlsStata.Rmd`
- Create: `claves_privadas/10_BadControls_clave.md`

**Interfaces:**
- Consumes: CSV de Task 3.
- Produces: práctica pública autocontenida y clave privada separada.

- [ ] **Step 1: Implement the practice**

Poner materiales al inicio; organizar cada DGP con pregunta, código, resultado e interpretación; leer `bad_controls_estimates.csv` y `bad_controls_montecarlo.csv`; incluir cuatro preguntas `BC-S1`–`BC-S4`.

- [ ] **Step 2: Write the private key**

Resolver `BC-T1`–`BC-T3` y `BC-S1`–`BC-S4` con criterios de calificación. Confirmar que `claves_privadas/` no aparece en `_bookdown.yml`.

- [ ] **Step 3: Run the full contract**

Run: `pytest -q tests/test_bad_controls_contract.py`

Expected: PASS.

- [ ] **Step 4: Commit**

Run: `git add 10-BadControlsStata.Rmd claves_privadas/10_BadControls_clave.md && git commit -m "feat: expand bad controls empirical class"`

### Task 5: Verificación integral

**Files:**
- Verify: all project tests and rendered book.

**Interfaces:**
- Consumes: Tasks 1–4.
- Produces: evidencia de que no hay regresiones ni errores de presentación.

- [ ] **Step 1: Run all tests**

Run: `pytest -q`

Expected: all tests PASS.

- [ ] **Step 2: Render in a clean output directory**

Run: `Rscript -e "bookdown::render_book('index.Rmd', output_dir='/private/tmp/libro_cortes_bad_controls_review')"`

Expected: exit 0 and both bad-controls HTML pages exist.

- [ ] **Step 3: Inspect rendered pages**

Confirmar descargas al inicio de la práctica, tablas visibles, cajas cerradas, títulos uniformes, ausencia de numeración manual y ausencia de soluciones públicas.

- [ ] **Step 4: Record verification**

Actualizar la memoria del proyecto con las decisiones y la evidencia final.
