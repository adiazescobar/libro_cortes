# Exact Matching Introduction Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Convertir `12-ExactMatching.Rmd` en una introducción conceptualmente rigurosa que conecte selección en observables con PSM, sin crear una clase práctica independiente.

**Architecture:** Un contrato automatizado fijará las correcciones académicas y editoriales. Después se reescribirá únicamente el capítulo R Markdown; la implementación de Stata quedará explícitamente pospuesta para la clase empírica de PSM.

**Tech Stack:** R Markdown/Bookdown, Python/pytest, MathJax.

## Global Constraints

- Usar `Y(D=1)` y `Y(D=0)` en toda la notación de resultados potenciales.
- Mantener el capítulo introductorio: sin do-file, descargas ni resultados canónicos propios.
- No afirmar que matching crea aleatorización o resuelve confusión no observada.
- No usar covariables postratamiento.
- Eliminar la regla de cinco controles por tratado.
- Retirar `ssc install nnmatch`, `exact()` y `tc(att)`; reservar `teffects nnmatch`, `ematch()` y `atet` para la práctica posterior.
- Incluir dos preguntas tipo examen sin respuestas públicas ni desplegables.
- No modificar ni publicar `docs/` antes de la aprobación visual de Ana María.

---

### Task 1: Definir el contrato académico y editorial

**Files:**
- Create: `tests/test_exact_matching_intro_contract.py`
- Test: `tests/test_exact_matching_intro_contract.py`

**Interfaces:**
- Consumes: texto UTF-8 de `12-ExactMatching.Rmd`.
- Produces: pruebas que Task 2 debe satisfacer sin depender del HTML renderizado.

- [ ] **Step 1: Crear el contrato en estado RED**

Crear `tests/test_exact_matching_intro_contract.py` con pruebas equivalentes a:

```python
from pathlib import Path
import re

ROOT = Path(__file__).resolve().parents[1]
CHAPTER = ROOT / "12-ExactMatching.Rmd"

def text():
    return CHAPTER.read_text(encoding="utf-8")

def test_introductory_scope_and_title():
    source = text()
    assert source.startswith("# Emparejamiento exacto — Introducción")
    assert "Descargue antes de comenzar" not in source
    assert "dofile/12_" not in source

def test_identification_assumptions_are_explicit():
    source = text().lower()
    for marker in ("no confusión condicional", "soporte común", "sutva", "pretratamiento"):
        assert marker in source
    assert "y_i(d=1)" in source and "y_i(d=0)" in source

def test_forbidden_claims_and_legacy_syntax_are_absent():
    source = text().lower()
    for forbidden in ("la única diferencia restante", "al menos 5:1", "ssc install nnmatch", "exact(", "tc(att)"):
        assert forbidden not in source

def test_manual_example_changes_target_population():
    source = text().lower()
    for marker in ("sin match", "población emparejada", "att", "soporte común"):
        assert marker in source

def test_two_public_exam_questions_without_answers():
    source = text()
    assert len(set(re.findall(r"EXACT-T[12]", source))) == 2
    assert "<details" not in source.lower()
    question_blocks = re.findall(r"(?ms)^:::\s*\{\.boxpregunta\}(.*?)^:::\s*$", source)
    assert len(question_blocks) >= 2
    assert all(not re.search(r"(?i)respuesta\s*:|solución\s*:|rúbrica\s*:", block)
               for block in question_blocks)
```

- [ ] **Step 2: Ejecutar el contrato y comprobar RED semántico**

Run: `pytest -q tests/test_exact_matching_intro_contract.py`

Expected: fallos por título, supuestos, ejemplo y preguntas ausentes; ningún error de importación o sintaxis.

- [ ] **Step 3: Confirmar que el contrato no invade PSM**

Run: `rg -n "13-PSM|14-PSMStata|15-IPW|16-PSM" tests/test_exact_matching_intro_contract.py`

Expected: sin coincidencias; el contrato solo gobierna `12-ExactMatching.Rmd`.

- [ ] **Step 4: Commit del contrato RED**

```bash
git add tests/test_exact_matching_intro_contract.py
git commit -m "test: define exact matching introduction contract"
```

### Task 2: Reescribir y verificar el capítulo introductorio

**Files:**
- Modify: `12-ExactMatching.Rmd`
- Test: `tests/test_exact_matching_intro_contract.py`

**Interfaces:**
- Consumes: contrato de Task 1 y notación editorial del libro.
- Produces: capítulo con anchor conservado, ejemplo manual auditable y puente explícito hacia `#psm`.

- [ ] **Step 1: Reescribir título, metas y problema de identificación**

Usar un encabezado con identificador estable:

```markdown
# Emparejamiento exacto — Introducción {#emparejamiento-exacto}
```

Explicar la descomposición de la diferencia observada en ATT y sesgo de selección. Aclarar que comparar dentro de celdas de `X` solo tiene interpretación causal bajo no confusión condicional, soporte común, SUTVA y covariables pretratamiento.

- [ ] **Step 2: Añadir el ejemplo manual completo**

Incluir una tabla pequeña con identificador, tratamiento, dos covariables discretas, resultado y celda. Debe haber al menos un tratado sin control compatible. Mostrar:

```markdown
\[
\widehat{ATT}_{\mathcal S}
=\frac{1}{N_{T,\mathcal S}}
\sum_{i:D_i=1,\,X_i\in\mathcal S}
\left(Y_i-\overline Y_{0,X_i}\right),
\]
```

Definir `\mathcal S` como soporte común y distinguir este estimando del ATT de todos los tratados.

- [ ] **Step 3: Corregir limitaciones y puente hacia PSM**

Eliminar la regla 5:1 y el bloque operativo antiguo. Añadir bloques separados para: dimensionalidad, confusión no observada, covariables postratamiento, pérdida de soporte e inferencia. Cerrar con Rosenbaum–Rubin: el propensity score es un puntaje de balance bajo los supuestos apropiados, no una garantía automática de balance o causalidad.

- [ ] **Step 4: Añadir evaluación formativa pública**

Crear dos bloques `.boxpregunta`, con códigos `EXACT-T1` y `EXACT-T2`. La primera pregunta debe pedir identificar soporte y estimando en una tabla pequeña; la segunda debe evaluar selección de covariables pretratamiento frente a un mal control. No incluir respuestas, rúbricas ni `<details>`.

- [ ] **Step 5: Ejecutar GREEN focalizado y suite completa**

Run: `pytest -q tests/test_exact_matching_intro_contract.py`

Expected: todas las pruebas del archivo pasan.

Run: `pytest -q tests`

Expected: cero fallos.

- [ ] **Step 6: Verificar formato y render completo**

Run: `git diff --check`

Expected: sin salida.

Run: `Rscript -e "bookdown::render_book('index.Rmd', output_dir='/private/tmp/libro_cortes_exact_matching_review')"`

Expected: exit 0 y archivo `/private/tmp/libro_cortes_exact_matching_review/emparejamiento-exacto.html` o el slug producido por el anchor/título.

Comprobar en el HTML: título, ecuaciones, tabla manual, bloques de color, dos preguntas y enlace al capítulo PSM; confirmar ausencia de sintaxis antigua y respuestas.

- [ ] **Step 7: Commit de implementación**

```bash
git add 12-ExactMatching.Rmd
git commit -m "feat: rebuild exact matching introduction"
```
