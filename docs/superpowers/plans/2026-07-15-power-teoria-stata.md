# POWER — teoría y Stata Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Crear una clase teórica de poder estadístico fiel a `POWER.pptx` y ampliar la clase práctica en Stata sin perder contenido, con resultados canónicos, siete preguntas tipo examen y clave docente privada.

**Architecture:** `07-POWER-Teoria.Rmd` concentrará el razonamiento conceptual y los casos de aula; `07-POWER.Rmd` conservará su URL y se convertirá en una práctica guiada. El pipeline de `dofile/07_Power/` exportará resultados canónicos consumidos por la página y, cuando sea viable, verificará una selección de escenarios en Python/R.

**Tech Stack:** R Markdown, bookdown/gitbook, Stata 19, R/knitr, Python/Jupyter, CSV, pytest, CSS existente.

## Global Constraints

- `POWER.pptx` es la fuente principal para secuencia, ejemplos, énfasis y lenguaje de aula.
- `Power_Calculation_3ie.pdf` se usa para comprobar fórmulas, parámetros y supuestos.
- No eliminar fórmulas, escenarios, amenazas, ética, Bertrand–Mullainathan, código ni descargas actuales.
- Crear `07-POWER-Teoria.Rmd`; mantener `07-POWER.Rmd` y su URL práctica actual.
- Insertar teoría antes de práctica en `_bookdown.yml` sin renombrar capítulos posteriores.
- Teoría sin descargas; práctica con descargas inmediatamente después del título.
- Exactamente `POWER-T1`–`POWER-T3` y `POWER-S1`–`POWER-S4`, sin respuestas, pistas ni desplegables.
- Clave externa: siete secciones × cinco componentes, permisos `0600`, invisible para Git y materiales estudiantiles.
- Toda cifra visible calculable debe provenir de resultados canónicos o estar rotulada inequívocamente como escenario hipotético.
- Bookdown será la única numeración; no usar números, `PASO` o `Etapa` en encabezados.
- Renderizar en `/private/tmp/libro_cortes_power`; no publicar `docs` sin aprobación explícita.
- Preservar cambios locales y artefactos ajenos.

---

### Task 1: Contratos de preservación, estructura y privacidad

**Files:**
- Create: `tests/test_power_pedagogy_contract.py`
- Create: `dofile/07_Power/tests/test_power_results.py`
- Test: `07-POWER.Rmd`
- Test: `_bookdown.yml`

**Interfaces:**
- Consumes: capítulo actual, scripts de `dofile/07_Power/`, `_bookdown.yml` y artefactos existentes.
- Produces: contratos RED para teoría, práctica, preservación, resultados, numeración y privacidad.

- [ ] **Step 1: Escribir contrato de preservación del capítulo actual**

Crear una lista de fragmentos distintivos, no palabras genéricas:

```python
PRACTICE_REQUIRED = [
    "power twomeans",
    "power twoproportions",
    "cumplimiento parcial",
    "cluster m1(",
    "Externalidades y efectos de derrame",
    "Belmont Report",
    "Bertrand y Mullainathan",
    "07_stata.do",
    "07_R.R",
    "07_phyton.ipynb",
]
```

Exigir cada fragmento en `07-POWER.Rmd` después de la refactorización.

- [ ] **Step 2: Escribir contrato de arquitectura bookdown**

```python
def test_power_theory_precedes_practice_in_bookdown():
    files = parse_rmd_files(BOOKDOWN)
    assert files.index("07-POWER-Teoria.Rmd") + 1 == files.index("07-POWER.Rmd")
    assert files.index("07-POWER.Rmd") < files.index("08-DID.Rmd")
```

Exigir títulos/anchors únicos y que la práctica conserve `poder-estadistico-stata`.

- [ ] **Step 3: Escribir contratos pedagógicos**

La teoría deberá contener al menos ocho `.box*`, la secuencia conceptual aprobada y exactamente `POWER-T1`–`POWER-T3`. La práctica deberá contener entre 14 y 18 H3 semánticos, al menos doce `.box*` y exactamente `POWER-S1`–`POWER-S4`.

```python
assert question_codes(THEORY, "POWER-T") == ["POWER-T1", "POWER-T2", "POWER-T3"]
assert question_codes(PRACTICE, "POWER-S") == ["POWER-S1", "POWER-S2", "POWER-S3", "POWER-S4"]
```

Cada pregunta debe vivir en una caja independiente y contener metadatos requeridos; rechazar `Respuesta`, `Solución`, `Pista`, `<details`, `hide(` y formulaciones equivalentes.

- [ ] **Step 4: Prohibir numeración manual y transcripción frágil**

Usar regex sobre H2–H4 para rechazar prefijos numéricos, `PASO`, `Paso` o `Etapa`. Exigir que tablas/resultados prácticos lean CSV desde `dofile/07_Power/results/`; los casos hipotéticos deberán contener la frase `escenario hipotético`.

- [ ] **Step 5: Escribir auditoría de privacidad**

Recorrer rutas rastreadas, Rmd, YAML, `docs` y HTML temporal. Construir tokens privados por fragmentos o recibirlos mediante variable de entorno; no imprimir el token en errores.

- [ ] **Step 6: Definir esquema de resultados canónicos**

```python
EXPECTED_COLUMNS = {
    "escenario", "familia", "estimando", "valor",
    "alpha", "power", "asignacion_tratada", "fuente"
}
```

Exigir escenarios mínimos: continuo sin controles, continuo con controles, binario, take-up, atrición, tasa y clúster.

- [ ] **Step 7: Ejecutar RED**

Run: `/private/tmp/libro_cortes_rct_venv/bin/python -m pytest -q tests/test_power_pedagogy_contract.py dofile/07_Power/tests/test_power_results.py`

Expected: fallos por teoría inexistente, orden bookdown, preguntas, etapas, resultados canónicos, descargas al final y numeración manual; preservación actual permanece verde.

- [ ] **Step 8: Commit de contratos**

```bash
git add tests/test_power_pedagogy_contract.py dofile/07_Power/tests/test_power_results.py
git commit -m "test: define POWER lesson contracts"
```

---

### Task 2: Pipeline y resultados canónicos de POWER

**Files:**
- Modify: `dofile/07_Power/07_stata.do`
- Modify: `dofile/07_Power/07_R.R` only when required for matched verification.
- Modify: `dofile/07_Power/07_phyton.ipynb` only when required for matched verification.
- Create: `dofile/07_Power/results/power_resultados.csv`
- Create: `dofile/07_Power/results/power_verificacion.csv`
- Modify: `dofile/07_Power/tests/test_power_results.py`

**Interfaces:**
- Consumes: scenarios and parameters already present in the scripts and classroom cases.
- Produces: `power_resultados.csv` for visible tables and `power_verificacion.csv` for cross-language checks.

- [ ] **Step 1: Audit existing scripts before editing**

Read all four source files completely. Build a private execution ledger with scenario, command, input parameters, expected quantity and current output. Do not record private-key information.

- [ ] **Step 2: Add a failing test for canonical rows**

```python
def test_canonical_power_results_have_one_row_per_required_scenario():
    df = pd.read_csv(RESULTS)
    assert set(REQUIRED_SCENARIOS).issubset(set(df["escenario"]))
    assert df[list(NUMERIC_COLUMNS)].notna().all().all()
```

Run the focal test and confirm it fails because the CSV does not exist.

- [ ] **Step 3: Export Stata results**

Extend `07_stata.do` to create `dofile/07_Power/results/` and export one tidy row per scenario. Capture returned scalars immediately after each `power` command. Use stable scenario identifiers rather than row positions.

The output must include inputs, requested quantity (`N`, `MDE`, clústeres or tamaño de clúster), returned value, command family and source.

- [ ] **Step 4: Run Stata and verify the CSV**

Run: `/Applications/StataNow/StataSE.app/Contents/MacOS/stata-se -b do dofile/07_Power/07_stata.do`

Expected: exit 0, no `r(...)` error in log, and `power_resultados.csv` with all required scenarios.

- [ ] **Step 5: Verify selected cases in Python or R**

Use the same alpha, power, tail convention, allocation, variance and ICC for at least four families: continuous, binary, take-up/attrition and cluster. Export:

```text
escenario,valor_stata,valor_alternativo,diferencia_abs,tolerancia,estado
```

Set explicit tolerances and explain differences caused by rounding or method conventions.

- [ ] **Step 6: Run GREEN and inspect values**

Run: `/private/tmp/libro_cortes_rct_venv/bin/python -m pytest -q dofile/07_Power/tests/test_power_results.py`

Expected: all tests pass; verification rows report `PASS`.

- [ ] **Step 7: Commit pipeline**

```bash
git add dofile/07_Power/07_stata.do dofile/07_Power/07_R.R dofile/07_Power/07_phyton.ipynb dofile/07_Power/results dofile/07_Power/tests/test_power_results.py
git commit -m "feat: export canonical POWER results"
```

---

### Task 3: Clase teórica fiel a `POWER.pptx`

**Files:**
- Create: `07-POWER-Teoria.Rmd`
- Modify: `_bookdown.yml`
- Test: `tests/test_power_pedagogy_contract.py`

**Interfaces:**
- Consumes: approved 19-part sequence, `POWER.pptx`, 3ie formulas and classroom cases.
- Produces: theory chapter rendered before the existing practical chapter.

- [ ] **Step 1: Add the chapter to bookdown**

Insert exactly:

```yaml
  - 07-POWER-Teoria.Rmd      # Poder estadístico — teoría
  - 07-POWER.Rmd             # Poder estadístico — Stata
```

before `08-DID.Rmd`.

- [ ] **Step 2: Create chapter shell and learning objectives**

Use title `# Poder estadístico — Clase teórica {#poder-estadistico-teoria}`. Include objectives, classroom roadmap and a bridge from randomized experiments. No download block.

- [ ] **Step 3: Implement the classroom sequence**

Develop all 19 items from the spec in their approved order. Recreate concepts, not the visual styling of the slides. Include the decision table for type I/type II errors and a clear two-distribution power graphic using an R chunk with reproducible parameters.

- [ ] **Step 4: Derive core formulas**

Show the chain from sampling variance to standard error, critical values, MDE and sample size. State whether each formula is per arm or total and whether the test is one- or two-sided. Derive the roles of `R^2`, take-up, attrition and design effect.

- [ ] **Step 5: Develop classroom cases**

Work the unemployment workshop step by step and present the other approved cases with complete inputs. Preserve values from `POWER.pptx` as historical classroom cases and label any corrections or convention changes transparently.

- [ ] **Step 6: Add blocks and questions**

Add at least eight labeled boxes and exactly three questions covering the specified T1–T3 domains. Questions must be solvable from their own statements and contain no answer markers.

- [ ] **Step 7: Run theory tests and isolated render**

Run: `/private/tmp/libro_cortes_rct_venv/bin/python -m pytest -q tests/test_power_pedagogy_contract.py -k theory`

Run: `Rscript -e "bookdown::render_book('index.Rmd', output_dir='/private/tmp/libro_cortes_power')"`

Expected: theory contracts pass; render reaches `07-POWER-Teoria.Rmd` without warnings from formulas or chunks.

- [ ] **Step 8: Commit theory**

```bash
git add 07-POWER-Teoria.Rmd _bookdown.yml tests/test_power_pedagogy_contract.py
git commit -m "docs: add POWER theory lesson"
```

---

### Task 4: Práctica guiada, evaluación y clave privada

**Files:**
- Modify: `07-POWER.Rmd`
- Modify: `style.css` only if required by existing box/table patterns.
- Modify: `tests/test_power_pedagogy_contract.py`
- Create externally: private POWER answer key.

**Interfaces:**
- Consumes: canonical CSVs from Task 2 and theory concepts from Task 3.
- Produces: guided practical page, four public questions and seven-section private key.

- [ ] **Step 1: Move materials to the beginning**

Replace the final download section with one `.class-materials` block immediately after the title. Preserve every current file link and add canonical CSV links when appropriate.

- [ ] **Step 2: Reorganize into semantic stages**

Create between 14 and 18 H3 stages in the approved order. Use heading text without manual numbers. Keep all original commands and examples inside the relevant stage.

- [ ] **Step 3: Make Stata code sequentially executable**

Define locals before use, avoid accidental redefinition, and ensure each `power` call can run after the previous block. Every command shown must state which quantity Stata solves for.

- [ ] **Step 4: Show canonical tables**

Read `power_resultados.csv` and `power_verificacion.csv` in setup chunks. Generate visible tables programmatically for inputs/results, sensitivity scenarios and cross-language checks. Never paste returned values into prose.

- [ ] **Step 5: Preserve and expand applied cases**

Keep Bertrand–Mullainathan, threats and Belmont content. Place threats/ethics as a design checklist connected to power calculations rather than an unrelated appendix. Keep the four classroom examples and add interpretations.

- [ ] **Step 6: Add blocks and questions**

Add at least twelve labeled boxes and exactly `POWER-S1`–`POWER-S4`. Each question must include full data, `Puntaje sugerido`, `Comandos permitidos` and `Producto esperado` exactly once.

- [ ] **Step 7: Create the external key**

Create seven sections × five components, validate correspondence and set mode `0600`. Do not record the file name, path, identifiers or solutions in tracked artifacts or reports.

- [ ] **Step 8: Run tests and isolated render**

Run: `/private/tmp/libro_cortes_rct_venv/bin/python -m pytest -q tests/test_power_pedagogy_contract.py dofile/07_Power/tests`

Expected: all POWER tests pass; canonical rows and verification remain green.

Render and visually inspect `poder-estadistico-stata.html` in `/private/tmp/libro_cortes_power`.

- [ ] **Step 9: Commit practice**

```bash
git add 07-POWER.Rmd style.css tests/test_power_pedagogy_contract.py
git commit -m "docs: expand guided POWER practice"
```

---

### Task 5: Validación académica, privada, reproducible y visual

**Files:**
- Modify: files from Tasks 1–4 only when validation exposes a tested defect.
- Verify externally: private key.
- Verify: `/private/tmp/libro_cortes_power/poder-estadistico-teoria.html`
- Verify: `/private/tmp/libro_cortes_power/poder-estadistico-stata.html`

**Interfaces:**
- Consumes: all previous tasks.
- Produces: reviewed local previews; no publication in `docs`.

- [ ] **Step 1: Run full test suite**

Run: `/private/tmp/libro_cortes_rct_venv/bin/python -m pytest -q`

Expected: zero failures.

- [ ] **Step 2: Re-run POWER reproducibility**

Confirm Stata log has no errors, canonical schemas are complete, and every verification row is `PASS`. Re-run Stata/R/Python only if inputs or outputs changed.

- [ ] **Step 3: Audit academic content**

Check all formulas against the 3ie guide, all classroom cases against `POWER.pptx`, and all seven questions for sufficient information, unique interpretation and correct rubric correspondence. Distinguish per-arm from total N, MDE from effect size, and units from clústeres.

- [ ] **Step 4: Audit privacy**

Confirm seven sections × five components, matching scores, mode `0600`, invisibility Git and zero hits in tracked files, temporary HTML and new history.

- [ ] **Step 5: Render full book**

Run: `Rscript -e "bookdown::render_book('index.Rmd', output_dir='/private/tmp/libro_cortes_power')"`

Expected: exit 0 and both POWER HTML files freshly generated.

- [ ] **Step 6: QA desktop and mobile**

At 1440×900 and 390×844 verify: no overflow, no double numbering, formulas readable, graphs/tables responsive, three theory questions, four practice questions, no answers, materials at the beginning of practice and no downloads in theory.

- [ ] **Step 7: Verify links**

Serve `/private/tmp/libro_cortes_power` locally. Require HTTP 200 for both chapters and every downloadable material. Do not publish `docs`.

- [ ] **Step 8: Apply tested fixes if required**

For each defect: write or extend a failing contract, reproduce RED, apply the smallest fix, run focal and full suites, re-render and re-check the affected viewport.

- [ ] **Step 9: Commit final adjustments**

If validation changed tracked files:

```bash
git add 07-POWER-Teoria.Rmd 07-POWER.Rmd _bookdown.yml style.css tests dofile/07_Power
git commit -m "fix: address POWER lesson review"
```

- [ ] **Step 10: Deliver previews**

Provide local links to theory and practice, report test counts, reproducibility status and privacy verification. Keep `docs` unpublished until explicit approval.
