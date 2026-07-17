# DID and Editorial Standardization Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Corregir la implementación inválida de primeras diferencias en DID y uniformar títulos y numeración de las parejas teoría–clase empírica sin cambiar anchors ni URLs.

**Architecture:** Los contratos pedagógicos serán la especificación ejecutable. El pipeline DID conservará únicamente estimadores válidos para cortes transversales repetidos; Bookdown será la única fuente de numeración y los títulos H1 seguirán un mapeo exacto con anchors estables.

**Tech Stack:** R Markdown, Bookdown, Python/pytest, Stata 19, CSV, Git.

## Global Constraints

- Usar `Tema — Clase teórica` y `Tema — Clase empírica` en las parejas explícitas.
- Conservar exactamente los anchors existentes y el orden de `_bookdown.yml`.
- No fabricar identificadores longitudinales para `base3.dta`.
- No modificar ni publicar `docs/`.
- Mantener preguntas sin respuestas visibles y la clave externa privada.
- Escribir contratos RED antes de modificar archivos de producción.
- Preservar cambios locales ajenos.

---

### Task 1: Contratos RED para DID, títulos y numeración

**Files:**
- Modify: `tests/test_did_pedagogy_contract.py`
- Modify: `dofile/08_DID/tests/test_did_results.py`
- Create: `tests/test_chapter_title_contract.py`
- Test: `03-Parametros.Rmd`, `04-ParametrosStata.Rmd`, `05-RCT.Rmd`, `06-RCT2.Rmd`, `07-POWER-Teoria.Rmd`, `07-POWER.Rmd`, `08-DID.Rmd`, `08-DIDStata.Rmd`, `09-BadControls.Rmd`, `10-BadControlsStata.Rmd`

**Interfaces:**
- Consumes: títulos y anchors H1 existentes; esquemas CSV de DID.
- Produces: contratos que prohíben panel ficticio, exigen tres métodos DID válidos y fijan títulos exactos sin numeración manual.

- [ ] **Step 1: Añadir el contrato contra panel ficticio**

En `tests/test_did_pedagogy_contract.py`, añadir una prueba que exija la frase
`cortes transversales repetidos`, prohíba `id ficticio`, `gen id`, `gen id_pd`,
`xtset id` y `reg D.y D` en la unión del capítulo práctico y el do-file, y exija
que la equivalencia de primeras diferencias quede rotulada como resultado para un
panel genuino, no como estimación sobre `base3.dta`.

- [ ] **Step 2: Corregir el esquema esperado de resultados**

En `dofile/08_DID/tests/test_did_results.py`, retirar
`did_primeras_diferencias` de `REQUIRED_SCENARIOS`, exigir exactamente
`did_manual`, `did_diff` y `did_regresion` para el DiD básico, y eliminar su
comparación en `test_estimators_agree_across_methods`.

- [ ] **Step 3: Crear contrato exacto de títulos y anchors**

En `tests/test_chapter_title_contract.py`, parametrizar el siguiente mapeo:

```python
EXPECTED = {
    "03-Parametros.Rmd": ("Parámetros causales — Clase teórica", "parametros-causales-teoria"),
    "04-ParametrosStata.Rmd": ("Parámetros causales — Clase empírica", "parametros-causales-stata"),
    "05-RCT.Rmd": ("Experimentos aleatorizados — Clase teórica", None),
    "06-RCT2.Rmd": ("Experimentos aleatorizados — Clase empírica", None),
    "07-POWER-Teoria.Rmd": ("Poder estadístico — Clase teórica", "poder-estadistico-teoria"),
    "07-POWER.Rmd": ("Poder estadístico — Clase empírica", "poder-estadistico-stata"),
    "08-DID.Rmd": ("Diferencias en diferencias — Clase teórica", "did-teoria"),
    "08-DIDStata.Rmd": ("Diferencias en diferencias — Clase empírica", "did-stata"),
    "09-BadControls.Rmd": ("Malos controles — Clase teórica", "bad-controls-teoria"),
    "10-BadControlsStata.Rmd": ("Malos controles — Clase empírica", "bad-controls-stata"),
}
```

La prueba debe leer únicamente el primer H1 real de cada archivo, comprobar el
título exacto y verificar que el anchor esperado no cambia.

- [ ] **Step 4: Añadir contrato de numeración**

Reutilizar `_headings` y `_assert_no_manual_numbering` del contrato POWER para
comprobar H2–H4 de los diez capítulos. Añadir además una mutación negativa con
`## 1. Subtítulo` para demostrar que el contrato detecta numeración manual.

- [ ] **Step 5: Ejecutar RED**

Run:

```bash
/private/tmp/libro_cortes_rct_venv/bin/python -m pytest -q \
  tests/test_did_pedagogy_contract.py \
  dofile/08_DID/tests/test_did_results.py \
  tests/test_chapter_title_contract.py
```

Expected: fallos por panel ficticio, escenario adicional y títulos aún no
normalizados; los contratos preexistentes no deben producir errores de sintaxis.

- [ ] **Step 6: Commit**

```bash
git add tests/test_did_pedagogy_contract.py \
  dofile/08_DID/tests/test_did_results.py \
  tests/test_chapter_title_contract.py
git commit -m "test: define DID and chapter title corrections"
```

---

### Task 2: Corregir DID y regenerar resultados canónicos

**Files:**
- Modify: `08-DID.Rmd`
- Modify: `08-DIDStata.Rmd`
- Modify: `dofile/08_DID/08_DID.do`
- Modify: `dofile/08_DID/results/did_resultados.csv`
- Verify: `dofile/08_DID/results/did_verificacion.csv`

**Interfaces:**
- Consumes: contratos de Task 1; `base3.dta`; `hospdd`.
- Produces: capítulo y CSV sin panel artificial, tres estimadores básicos válidos y supuestos de identificación completos.

- [ ] **Step 1: Corregir identificación en teoría**

Sustituir “si y solo si” por una formulación que condicione el ATT a tendencias
paralelas, consistencia, ausencia de anticipación, composición estable y ausencia
de interferencia relevante. Mantener la derivación y la notación
`Y_i(D=1)`/`Y_i(D=0)`.

- [ ] **Step 2: Reescribir primeras diferencias**

En `08-DIDStata.Rmd`, explicar que `base3.dta` son cortes transversales repetidos
y no permite formar `ΔY_i`. Conservar la ecuación como equivalencia teórica para
un panel genuino, sin bloque ejecutable que cree `id`. Cambiar la tabla y el texto
de “cuatro caminos” a tres: manual, `diff` y regresión.

- [ ] **Step 3: Retirar el estimador artificial del do-file**

Eliminar del bloque visible y del export canónico:

```stata
sort t D orden_n
bys t: gen id_pd = _n
xtset id_pd t
quietly reg D.y D
post `res' ("did_primeras_diferencias") ...
```

El do-file debe continuar con `hospdd` inmediatamente después de la regresión
DiD válida.

- [ ] **Step 4: Regenerar Stata**

Run:

```bash
/Applications/StataNow/StataSE.app/Contents/MacOS/stata-se -b do \
  dofile/08_DID/08_DID.do
```

Expected: exit 0; `08_DID.log` sin `r(`; `did_resultados.csv` con diez filas y sin
`did_primeras_diferencias`.

- [ ] **Step 5: Regenerar verificación cruzada**

Run:

```bash
/private/tmp/libro_cortes_rct_venv/bin/python \
  dofile/08_DID/verificar_did.py
```

Expected: tres filas `PASS`.

- [ ] **Step 6: Ejecutar GREEN focal**

Run:

```bash
/private/tmp/libro_cortes_rct_venv/bin/python -m pytest -q \
  tests/test_did_pedagogy_contract.py \
  dofile/08_DID/tests/test_did_results.py
```

Expected: todas pasan.

- [ ] **Step 7: Commit**

```bash
git add 08-DID.Rmd 08-DIDStata.Rmd dofile/08_DID/08_DID.do \
  dofile/08_DID/results/did_resultados.csv \
  dofile/08_DID/results/did_verificacion.csv
git commit -m "fix: remove artificial DID panel"
```

---

### Task 3: Uniformar títulos y eliminar doble numeración

**Files:**
- Modify: `03-Parametros.Rmd`
- Modify: `04-ParametrosStata.Rmd`
- Modify: `05-RCT.Rmd`
- Modify: `06-RCT2.Rmd`
- Modify: `07-POWER-Teoria.Rmd`
- Modify: `07-POWER.Rmd`
- Modify: `08-DID.Rmd`
- Modify: `08-DIDStata.Rmd`
- Modify: `09-BadControls.Rmd`
- Modify: `10-BadControlsStata.Rmd`

**Interfaces:**
- Consumes: mapeo `EXPECTED` de Task 1.
- Produces: títulos uniformes con anchors y URLs intactos; H2–H4 sin numeración manual.

- [ ] **Step 1: Cambiar únicamente los H1**

Aplicar exactamente el mapeo de Task 1. No editar anchors, orden de archivos,
contenido sustantivo ni enlaces.

- [ ] **Step 2: Retirar numeración manual**

Buscar en H2–H4 prefijos numéricos, `Paso` y `Etapa`. Retirar solo esos prefijos,
preservando el texto semántico y cualquier anchor. Bookdown añadirá la numeración
visible.

- [ ] **Step 3: Ejecutar contrato de títulos**

Run:

```bash
/private/tmp/libro_cortes_rct_venv/bin/python -m pytest -q \
  tests/test_chapter_title_contract.py
```

Expected: todas pasan.

- [ ] **Step 4: Ejecutar contratos pedagógicos relacionados**

Run:

```bash
/private/tmp/libro_cortes_rct_venv/bin/python -m pytest -q \
  tests/test_power_pedagogy_contract.py \
  tests/test_did_pedagogy_contract.py
```

Expected: todas pasan y los anchors permanecen intactos.

- [ ] **Step 5: Commit**

```bash
git add 03-Parametros.Rmd 04-ParametrosStata.Rmd 05-RCT.Rmd 06-RCT2.Rmd \
  07-POWER-Teoria.Rmd 07-POWER.Rmd 08-DID.Rmd 08-DIDStata.Rmd \
  09-BadControls.Rmd 10-BadControlsStata.Rmd
git commit -m "docs: standardize theory and empirical class titles"
```

---

### Task 4: Validación integral y vista previa

**Files:**
- Verify: all tracked source files
- Verify: clean render under `/private/tmp/libro_cortes_standardization_review`
- Do not modify: `docs/`

**Interfaces:**
- Consumes: Tasks 1–3.
- Produces: evidencia de pruebas, reproducibilidad, privacidad y render lista para aprobación.

- [ ] **Step 1: Ejecutar suite completa**

Run:

```bash
/private/tmp/libro_cortes_rct_venv/bin/python -m pytest -q
```

Expected: cero fallos.

- [ ] **Step 2: Auditar Stata y CSV**

Confirmar que `08_DID.log` termina en `end of do-file`, no contiene errores
`r(...)`, el CSV no incluye el escenario retirado y las tres verificaciones
siguen en `PASS`.

- [ ] **Step 3: Auditar privacidad**

Ejecutar los contratos de privacidad existentes y buscar fragmentos privados
únicamente mediante los helpers que no imprimen tokens sensibles. Expected:
cero exposición en archivos rastreados y HTML.

- [ ] **Step 4: Renderizar desde una copia limpia**

Crear `/private/tmp/libro_cortes_standardization_review` desde `git archive HEAD`
y ejecutar:

```bash
Rscript -e "bookdown::render_book(
  'index.Rmd',
  output_dir='/private/tmp/libro_cortes_standardization_review/_render'
)"
```

Expected: exit 0 y HTML `poder-estadistico-teoria.html`,
`poder-estadistico-stata.html`, `did-teoria.html` y `did-stata.html`.

- [ ] **Step 5: Inspeccionar HTML**

Verificar:

- títulos exactos en H1 y menú;
- una sola numeración Bookdown por encabezado;
- anchors/URLs sin cambio;
- tres filas de métodos DID válidos;
- ausencia de panel ficticio;
- preguntas y tablas visibles sin respuestas;
- materiales al inicio de clases empíricas.

- [ ] **Step 6: Comprobar alcance Git**

Run:

```bash
git diff --check HEAD~3..HEAD
git diff --name-only HEAD~3..HEAD -- docs
```

Expected: sin errores nuevos y ningún archivo bajo `docs/`.

- [ ] **Step 7: Commit de correcciones finales si fueran necesarias**

Solo si la validación descubre un defecto, crear primero un contrato que falle,
aplicar la corrección mínima, repetir la validación y comprometer:

```bash
git commit -m "fix: address DID and title standardization review"
```

