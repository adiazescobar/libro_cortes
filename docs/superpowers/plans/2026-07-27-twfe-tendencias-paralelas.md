# Tendencias paralelas en métodos DiD modernos Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Incorporar a los capítulos Panel/TWFE una discusión formal y práctica de tendencias paralelas específica para cada estimador moderno, más un recuadro avanzado de Rambachan–Roth y HonestDiD.

**Architecture:** Un contrato pytest fija los supuestos, grupos de comparación, diagnósticos y límites interpretativos que deben aparecer. La teoría desarrolla el supuesto sobre resultados potenciales y la práctica lo traduce a una matriz método–supuesto–diagnóstico, sin presentar pruebas de pretrends como verificación del contrafactual.

**Tech Stack:** Bookdown/R Markdown, Stata 19, Python/pytest, HonestDiD.

## Global Constraints

- Conservar títulos, anchors y URLs actuales de ambos capítulos.
- Mantener exactamente TWFE-T1–T3 y TWFE-S1–S4, sin soluciones públicas.
- No afirmar que un estimador moderno elimina tendencias paralelas.
- No interpretar ausencia de significancia pretratamiento como prueba del supuesto.
- `honestdid` se presenta como sensibilidad y robustez, no como prueba ni reparación automática.
- La entrada de `honestdid` debe provenir de un event study compatible con el diseño, no del TWFE contaminado.
- Conservar las seis gráficas canónicas y las tablas ya aprobadas.

---

### Task 1: Contrato académico de tendencias paralelas

**Files:**
- Modify: `tests/test_twfe_pedagogy_contract.py`

**Interfaces:**
- Consumes: requisitos de `docs/superpowers/specs/2026-07-17-panel-twfe-design.md`.
- Produces: pruebas rojas para teoría, práctica y HonestDiD.

- [ ] **Step 1: Add the theory contract**

Exigir una definición sobre (Y(D=0)), formulaciones por cohorte, tendencias condicionales, nunca/no-aún tratados, soporte común, no anticipación y distinción supuesto–diagnóstico.

- [ ] **Step 2: Add the method matrix contract**

Exigir filas separadas para TWFE, `csdid`, `eventstudyinteract`, `did_imputation`, `did2s` y `did_multiplegt_dyn`, cada una con supuesto, control, diagnóstico y limitación.

- [ ] **Step 3: Add the Rambachan–Roth contract**

Exigir DOI, identificación parcial, restricciones de magnitud relativa o suavidad, conjuntos de confianza, análisis de sensibilidad y prohibición de usar TWFE contaminado como entrada.

- [ ] **Step 4: Verify RED**

Run: `pytest -q tests/test_twfe_pedagogy_contract.py -k parallel`

Expected: FAIL por ausencia del desarrollo método por método y del recuadro avanzado.

- [ ] **Step 5: Commit**

Run: `git add tests/test_twfe_pedagogy_contract.py && git commit -m "test: define modern DiD parallel trends contract"`

### Task 2: Ampliación teórica

**Files:**
- Modify: `11-TWFE.Rmd`

**Interfaces:**
- Consumes: contrato de Task 1 y Rambachan–Roth (2023).
- Produces: sección teórica y recuadro de lectura avanzada.

- [ ] **Step 1: Define group-time parallel trends**

Añadir la igualdad para (ATT(g,t)) usando nunca o no-aún tratados y explicar cómo cambia el conjunto de comparación.

- [ ] **Step 2: Explain conditional parallel trends**

Separar tendencias incondicionales de tendencias condicionales en covariables pretratamiento, incluyendo soporte común e integración sobre la distribución de (X) de la cohorte tratada.

- [ ] **Step 3: Add the estimator-by-estimator discussion**

Desarrollar TWFE, Callaway–Sant’Anna, Sun–Abraham, BJS, `did2s` y de Chaisemartin–D’Haultfœuille con su población, control y supuesto.

- [ ] **Step 4: Add the advanced reading box**

Explicar identificación parcial, restricciones de magnitud relativa/suavidad, conjuntos robustos y *breakdown value*. Enlazar DOI y rotular la extensión como sensibilidad.

- [ ] **Step 5: Verify theory GREEN**

Run: `pytest -q tests/test_twfe_pedagogy_contract.py -k "parallel and theory"`

Expected: PASS.

- [ ] **Step 6: Commit**

Run: `git add 11-TWFE.Rmd && git commit -m "feat: explain parallel trends across modern DiD methods"`

### Task 3: Aplicación práctica y verificación

**Files:**
- Modify: `11-TWFEStata.Rmd`
- Modify: `dofile/11_TWFE/11_stata.do`
- Modify: `claves_privadas/11_TWFE_clave.md` only if grading criteria need alignment.

**Interfaces:**
- Consumes: theory from Task 2 and existing canonical DGP/results.
- Produces: method matrix, diagnostic workflow and optional HonestDiD example.

- [ ] **Step 1: Add the practical method matrix**

Para cada método, mostrar supuesto, comparación, comando de diagnóstico/placebo y una limitación de interpretación.

- [ ] **Step 2: Add a diagnostic workflow**

Ordenar: graficar resultados observados; definir cohorte/control; estimar event study compatible; revisar preperiodos con intervalos; ejecutar placebos; discutir potencia; realizar sensibilidad.

- [ ] **Step 3: Add HonestDiD as an optional advanced extension**

Mostrar instalación `ssc install honestdid`, verificación `honestdid _plugin_check` y sintaxis basada en matrices compatibles. No exigir ejecución si el plugin compilado no está disponible; rotular claramente el ejemplo.

- [ ] **Step 4: Run focused tests**

Run: `pytest -q tests/test_twfe_pedagogy_contract.py`

Expected: all TWFE tests PASS.

- [ ] **Step 5: Run the full suite**

Run: `pytest -q tests`

Expected: all project tests PASS.

- [ ] **Step 6: Render cleanly**

Run: `Rscript -e "bookdown::render_book('index.Rmd', output_dir='/private/tmp/libro_cortes_twfe_parallel_review')"`

Expected: exit 0; both TWFE pages contain the new sections and current graphs.

- [ ] **Step 7: Inspect and commit**

Confirmar que las fórmulas renderizan, el recuadro no interrumpe la secuencia principal, no hay soluciones públicas y `honestdid` está marcado como opcional.

Run: `git add 11-TWFEStata.Rmd dofile/11_TWFE/11_stata.do claves_privadas/11_TWFE_clave.md && git commit -m "feat: add parallel trends diagnostics to TWFE practice"`
