# Bloque fundacional del libro Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Dejar listos la Prueba de entrada, la Introducción y Stata para principiantes como bloque fundacional académicamente revisado, robusto y coherente con el patrón visual aprobado.

**Architecture:** La prueba diagnóstica separa contenido académico, claves y comportamiento verificable mediante una matriz de auditoría y pruebas del HTML fuente. La introducción se reorganiza sin perder sus activos pedagógicos. Stata para principiantes adopta el componente de materiales al inicio y verifica sus ejemplos contra una ejecución real del do-file.

**Tech Stack:** R Markdown, bookdown/gitbook, R (`webexercises`, `knitr`, `kableExtra`), JavaScript, CSS, Stata 19, Python/pytest, CSV.

## Global Constraints

- La Prueba de entrada evalúa únicamente prerrequisitos.
- La prueba conserva cuatro áreas y tendrá cinco preguntas por área, 20 en total.
- Cada pregunta tendrá una sola respuesta correcta, justificación académica y retroalimentación posterior a la calificación.
- El render no instalará paquetes.
- La Introducción no tendrá bloque de descargas.
- Stata para principiantes tendrá materiales inmediatamente después del título.
- Los resultados de Stata visibles no se transcribirán sin verificación contra una ejecución real.
- Los renders se escribirán en `/private/tmp/libro_cortes_foundational_render`, no en `docs/`.
- Los cambios locales preexistentes y artefactos de render permanecerán fuera de los commits.

---

### Task 1: Matriz de auditoría académica de la Prueba de entrada

**Files:**
- Create: `docs/audits/prueba_entrada_academica.csv`
- Create: `tests/test_entrada_academica.py`
- Modify: `00-PruebaEntrada.Rmd:173-397`

**Interfaces:**
- Consumes: las 18 preguntas actuales y los prerrequisitos definidos en la especificación.
- Produces: una matriz de 20 ítems con `id`, `seccion`, `competencia`, `dificultad`, `tipo`, `clave`, `justificacion` y `estado`; preguntas alineadas uno a uno con la matriz.

- [ ] **Step 1: Crear la prueba fallida del contrato académico**

```python
import csv
import re
from collections import Counter
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
RMD = (ROOT / "00-PruebaEntrada.Rmd").read_text(encoding="utf-8")
AUDIT = ROOT / "docs/audits/prueba_entrada_academica.csv"


def test_quiz_has_twenty_balanced_questions():
    numbers = re.findall(r'question-number">Pregunta (\d+)\.', RMD)
    assert numbers == [str(i) for i in range(1, 21)]
    sections = re.findall(r'<div class="quiz-section" data-section="([^"]+)">', RMD)
    assert sections == ["Estadística básica", "Regresión lineal", "Causalidad", "Stata"]


def test_academic_audit_is_complete():
    with AUDIT.open(newline="", encoding="utf-8") as handle:
        rows = list(csv.DictReader(handle))
    assert len(rows) == 20
    assert Counter(row["seccion"] for row in rows) == {
        "Estadística básica": 5,
        "Regresión lineal": 5,
        "Causalidad": 5,
        "Stata": 5,
    }
    assert all(row["estado"] == "aprobada" for row in rows)
    assert all(row["clave"] and row["justificacion"] for row in rows)
```

- [ ] **Step 2: Ejecutar y confirmar el fallo**

Run: `/private/tmp/libro_cortes_rct_venv/bin/python -m pytest tests/test_entrada_academica.py -q`

Expected: falla porque hay 18 preguntas, faltan tildes en los nombres de sección y no existe la matriz.

- [ ] **Step 3: Crear la matriz de 20 competencias**

Crear `docs/audits/prueba_entrada_academica.csv` con cinco ítems por sección:

- Estadística: regla 68–95–99.7, valor p, error tipo I, interpretación frecuentista del IC, varianza.
- Regresión: pendiente ceteris paribus, (R^2), supuestos de MCO, significancia a niveles convencionales, modelo semilogarítmico.
- Causalidad: correlación no causal, selección, asignación aleatoria, contrafactual, comparación participantes/no participantes.
- Stata: inspeccionar observaciones, interpretar `regress`, sintaxis de regresión, `generate ... if`, y recuperar `r(mean)` después de `summarize`.

Para cada fila registrar dificultad `básica` o `intermedia`, la clave exacta y una justificación de al menos una oración.

- [ ] **Step 4: Revisar y corregir las 18 preguntas existentes**

Aplicar tildes y notación consistente: `hipótesis`, `más`, `estándar`, `regresión`, `correlación`, (H_0), (R^2), (eta_1). Ajustar la pregunta del intervalo para decir “en muestreos repetidos, aproximadamente 95% de los intervalos construidos con este procedimiento contendrían el parámetro”.

En la pregunta semilogarítmica, formular la opción correcta como “aproximadamente (100\beta_1\)% por una unidad adicional de (X)” para evitar que el estudiante seleccione una afirmación incompleta.

- [ ] **Step 5: Añadir dos preguntas de Stata**

Añadir las preguntas 19 y 20:

```markdown
<div class="question-box">
<span class="question-number">Pregunta 19.</span> ¿Qué comando crea `ingreso_alto` igual a 1 únicamente para observaciones con `ingreso` mayor que 1000?

`r mcq(c("replace ingreso_alto = 1 if ingreso > 1000", answer = "generate ingreso_alto = 1 if ingreso > 1000", "if ingreso > 1000 generate ingreso_alto = 1", "create ingreso_alto where ingreso > 1000"))`

`r hide("Ver explicación")`
`generate` crea una variable nueva y `if` restringe las observaciones a las que se asigna el valor. Las demás quedarán como valores perdidos hasta que se definan explícitamente.
`r unhide()`
</div>

<div class="question-box">
<span class="question-number">Pregunta 20.</span> Después de ejecutar `summarize salario`, ¿cómo muestra Stata la media almacenada por el comando?

`r mcq(c("display e(mean)", answer = "display r(mean)", "display _b[mean]", "show mean(salario)"))`

`r hide("Ver explicación")`
`summarize` es un comando de clase `r`; guarda la media en `r(mean)` hasta que otro comando sobrescriba esos resultados.
`r unhide()`
</div>
```

- [ ] **Step 6: Ejecutar la prueba académica**

Run: `/private/tmp/libro_cortes_rct_venv/bin/python -m pytest tests/test_entrada_academica.py -q`

Expected: `2 passed`.

- [ ] **Step 7: Commit de la auditoría académica**

```bash
git add 00-PruebaEntrada.Rmd docs/audits/prueba_entrada_academica.csv tests/test_entrada_academica.py
git commit -m "docs: audit entrance test prerequisites"
```

---

### Task 2: Instrucciones, puntaje y robustez de la Prueba de entrada

**Files:**
- Modify: `00-PruebaEntrada.Rmd:1-170,398-674`
- Modify: `style.css`
- Create: `tests/test_entrada_structure.py`

**Interfaces:**
- Consumes: 20 preguntas aprobadas y cuatro contenedores `.quiz-section`.
- Produces: instrucciones iniciales, puntaje 0–20 y 0–5 por sección, recomendaciones específicas y render sin instalación automática.

- [ ] **Step 1: Escribir pruebas fallidas de estructura**

```python
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
TEXT = (ROOT / "00-PruebaEntrada.Rmd").read_text(encoding="utf-8")


def test_quiz_never_installs_during_render():
    assert "install.packages" not in TEXT
    assert 'stop("Falta el paquete webexercises' in TEXT


def test_instructions_precede_quiz():
    assert TEXT.index("## Antes de comenzar {-}") < TEXT.index('<div id="prueba-entrada-quiz">')
    for phrase in ["20 preguntas", "15–20 minutos", "diagnóstica", "sin consultar materiales"]:
        assert phrase in TEXT


def test_scoring_contract_is_present():
    assert 'id="btn-finalizar"' in TEXT
    assert 'id="score-result"' in TEXT
    assert "Estadística básica" in TEXT
    assert "Regresión lineal" in TEXT
```

- [ ] **Step 2: Ejecutar y confirmar el fallo**

Run: `/private/tmp/libro_cortes_rct_venv/bin/python -m pytest tests/test_entrada_structure.py -q`

Expected: falla por `install.packages`, encabezado inicial ausente y textos sin tildes.

- [ ] **Step 3: Sustituir instalación automática por fallo explícito**

```r
if (!requireNamespace("webexercises", quietly = TRUE)) {
  stop("Falta el paquete webexercises. Instálalo con install.packages('webexercises') antes de renderizar el libro.")
}
library(webexercises)
```

- [ ] **Step 4: Añadir instrucciones antes del quiz**

Crear `## Antes de comenzar {-}` con propósito diagnóstico, 20 preguntas, cuatro áreas, duración de 15–20 minutos, una sola sesión sin consultar materiales y explicación de que la retroalimentación aparecerá al pulsar el botón final.

- [ ] **Step 5: Ajustar puntaje y recomendaciones**

Mantener el conteo mediante elementos `webex-correct`, pero fijar explícitamente cinco preguntas por `.quiz-section`. Mostrar total `/20`, cada sección `/5` y recomendaciones:

- 0–2: repaso prioritario;
- 3: repaso recomendado;
- 4–5: preparación suficiente.

Los campos sin responder deben contarse como incorrectos únicamente al finalizar y deben listarse como pendientes antes de revelar explicaciones.

- [ ] **Step 6: Añadir estilos al sistema visual**

Agregar a `style.css` clases `.diagnostic-intro`, `.quiz-section`, `.question-box`, `.scoring-guide` y `.score-recommendation` usando `--paper`, `--ink`, `--line` y `--accent`. Añadir `@media (max-width: 700px)` para botones a ancho completo y opciones sin desbordamiento.

- [ ] **Step 7: Ejecutar pruebas del capítulo**

Run: `/private/tmp/libro_cortes_rct_venv/bin/python -m pytest tests/test_entrada_academica.py tests/test_entrada_structure.py -q`

Expected: `5 passed`.

- [ ] **Step 8: Commit de robustez e interfaz**

```bash
git add 00-PruebaEntrada.Rmd style.css tests/test_entrada_structure.py
git commit -m "feat: make entrance diagnostic robust and actionable"
```

---

### Task 3: Reorganización académica de la Introducción

**Files:**
- Modify: `01-intro.Rmd`
- Create: `tests/test_intro_contract.py`

**Interfaces:**
- Consumes: ejemplos, imágenes, videos y chunks existentes.
- Produces: progresión fundacional sin encabezados duplicados y con hechos corregidos.

- [ ] **Step 1: Escribir el contrato de estructura**

```python
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
TEXT = (ROOT / "01-intro.Rmd").read_text(encoding="utf-8")


def test_intro_sections_are_ordered():
    headings = [
        "## Objetivos y mapa del capítulo {-}",
        "## Por qué importa la inferencia causal {-}",
        "## Cómo formular una pregunta causal {-}",
        "## Contrafactual y resultados potenciales {-}",
        "## El problema fundamental {-}",
        "## Diferencia observada y sesgo de selección {-}",
        "## Comparaciones que no identifican causalidad {-}",
        "## Estrategias del curso {-}",
        "## Mapa del libro y puente a Stata {-}",
    ]
    positions = [TEXT.index(h) for h in headings]
    assert positions == sorted(positions)


def test_intro_factual_and_copy_fixes():
    assert "México, 1990" not in TEXT
    assert "Lanzado en México en 1997" in TEXT
    assert "Por últimpo" not in TEXT
    assert "## DESCARGA" not in TEXT
```

- [ ] **Step 2: Ejecutar y confirmar el fallo**

Run: `/private/tmp/libro_cortes_rct_venv/bin/python -m pytest tests/test_intro_contract.py -q`

Expected: las dos pruebas fallan.

- [ ] **Step 3: Reorganizar encabezados y transiciones**

Unificar las dos secciones iniciales de motivación bajo `## Por qué importa la inferencia causal {-}`. Mover la formulación de preguntas antes del desarrollo de resultados potenciales. Agrupar contrafactual, visualización y resultados potenciales. Mantener el cuento de Borges, Progresa, imágenes, checklist, video y ejemplos sustantivos.

- [ ] **Step 4: Corregir contenido académico y factual**

- Cambiar el encabezado a “Progresa (México, 1997)”.
- Separar con viñetas los efectos directos e indirectos de la microeconometría.
- Corregir “Por últimpo”.
- En el ejemplo del hospital, declarar que el tratamiento es ir al hospital y que (Y_i(d)=1) significa recuperación; presentar los tres perfiles como heterogeneidad, no como recomendación clínica.
- Evitar afirmar que el grupo de control individual es literalmente el contrafactual de otra persona; explicar que aproxima el contrafactual promedio bajo supuestos de comparabilidad.
- Mantener la descomposición diferencia observada = efecto causal + sesgo de selección y remitir la demostración completa a Parámetros causales.

- [ ] **Step 5: Añadir mapa y puente**

Cerrar con una tabla compacta de familias del curso: experimentos, selección en observables, panel/DiD, IV y RDD. Añadir un puente que explique que el capítulo siguiente desarrolla las herramientas de Stata requeridas para reproducir los ejercicios.

- [ ] **Step 6: Ejecutar pruebas y render aislado**

Run: `/private/tmp/libro_cortes_rct_venv/bin/python -m pytest tests/test_intro_contract.py -q`

Expected: `2 passed`.

- [ ] **Step 7: Commit de la Introducción**

```bash
git add 01-intro.Rmd tests/test_intro_contract.py
git commit -m "docs: restructure causal inference introduction"
```

---

### Task 4: Materiales y resultados visibles en Stata para principiantes

**Files:**
- Modify: `02-StataBasics.Rmd`
- Modify: `dofile/Clase0_StataBasics/Clase00_Stata.do`
- Create: `dofile/Clase0_StataBasics/results/stata_basics_results.csv`
- Create: `tests/test_stata_basics_contract.py`

**Interfaces:**
- Consumes: `hh_98.dta`, do-file, script R y notebook Python existentes.
- Produces: materiales al inicio, tabla canónica verificada de resultados y secuencia comando–salida–interpretación.

- [ ] **Step 1: Escribir pruebas fallidas del contrato práctico**

```python
import csv
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
RMD = (ROOT / "02-StataBasics.Rmd").read_text(encoding="utf-8")


def test_materials_are_first():
    assert RMD.index("## Materiales para la clase {-}") < RMD.index("### Objetivos de aprendizaje {-}")
    assert "## DESCARGA LOS DOCUMENTOS {-}" not in RMD


def test_download_files_exist():
    base = ROOT / "dofile/Clase0_StataBasics"
    required = ["Clase00_Stata.do", "clase0_R.R", "clase0_phyton.ipynb", "hh_98.dta"]
    assert all((base / name).is_file() for name in required)


def test_canonical_results_schema():
    path = ROOT / "dofile/Clase0_StataBasics/results/stata_basics_results.csv"
    with path.open(newline="", encoding="utf-8-sig") as handle:
        columns = set(next(csv.reader(handle)))
    assert {"ejemplo", "variable", "valor", "N"} <= columns
```

- [ ] **Step 2: Ejecutar y confirmar los fallos**

Run: `/private/tmp/libro_cortes_rct_venv/bin/python -m pytest tests/test_stata_basics_contract.py -q`

Expected: fallan ubicación de materiales y CSV canónico; existen los cuatro descargables.

- [ ] **Step 3: Añadir materiales al inicio**

Reutilizar `.class-materials` inmediatamente después del título con enlaces a `Clase00_Stata.do`, `hh_98.dta`, `clase0_R.R`, `clase0_phyton.ipynb` y Colab. Eliminar la sección final duplicada.

- [ ] **Step 4: Exportar resultados canónicos desde Stata**

Añadir al do-file una sección determinista que cargue `hh_98.dta`, ejecute `summarize` sobre tres variables realmente presentes, almacene media y N mediante `postfile` y exporte `results/stata_basics_results.csv`. Validar primero los nombres con `describe`; si las variables del capítulo no existen, adaptar los ejemplos del capítulo a variables reales, sin generar datos sustitutos.

- [ ] **Step 5: Mostrar salidas junto a comandos**

En macros, loops, programas y `postfile`, añadir bloques de salida breves y verificables. Para el ejemplo con datos, leer `stata_basics_results.csv`, validar columnas y mostrar una tabla con variable, media y N. Debajo explicar qué resultado almacenó Stata y cómo se recuperó.

- [ ] **Step 6: Revisar equivalencias y nombres**

Mantener R y Python como cierre comparativo, no intercalados con la progresión de Stata. Añadir un enlace canónico nuevo `clase0_python.ipynb` únicamente si se crea una copia compatible; conservar el enlace al nombre histórico `clase0_phyton.ipynb` hasta publicar la transición.

- [ ] **Step 7: Ejecutar Stata y pruebas**

Run: `/Applications/StataNow/StataSE.app/Contents/MacOS/stata-se -b do Clase00_Stata.do` desde `dofile/Clase0_StataBasics`.

Expected: código 0 y CSV no vacío.

Run: `/private/tmp/libro_cortes_rct_venv/bin/python -m pytest tests/test_stata_basics_contract.py -q`

Expected: `3 passed`.

- [ ] **Step 8: Commit del capítulo práctico**

```bash
git add 02-StataBasics.Rmd dofile/Clase0_StataBasics/Clase00_Stata.do dofile/Clase0_StataBasics/results/stata_basics_results.csv tests/test_stata_basics_contract.py
git commit -m "feat: standardize Stata basics practice"
```

---

### Task 5: Render y revisión funcional del bloque fundacional

**Files:**
- Verify: `00-PruebaEntrada.Rmd`
- Verify: `01-intro.Rmd`
- Verify: `02-StataBasics.Rmd`
- Verify: `style.css`
- Verify: `/private/tmp/libro_cortes_foundational_render/`

**Interfaces:**
- Consumes: tres capítulos terminados y pruebas del bloque.
- Produces: render temporal, evidencia visual y aprobación para continuar con Parámetros causales.

- [ ] **Step 1: Ejecutar todas las pruebas**

Run: `/private/tmp/libro_cortes_rct_venv/bin/python -m pytest tests/test_entrada_academica.py tests/test_entrada_structure.py tests/test_intro_contract.py tests/test_stata_basics_contract.py dofile/06_RCT_Stata/tests -q`

Expected: todas las pruebas pasan.

- [ ] **Step 2: Renderizar fuera de `docs/`**

Run: `Rscript -e "bookdown::render_book('index.Rmd', 'bookdown::gitbook', output_dir='/private/tmp/libro_cortes_foundational_render')"`

Expected: código 0; existen HTML para Prueba de entrada, Intro y Stata para principiantes.

- [ ] **Step 3: Probar la Prueba de entrada en navegador**

Responder una combinación conocida: cinco correctas en Estadística, cuatro en Regresión, tres en Causalidad, dos en Stata y seis sin responder distribuidas en opciones no seleccionadas. Pulsar finalizar y comprobar total, subtotales, recomendaciones y aparición de explicaciones. Repetir la calificación y confirmar que el puntaje no se duplica.

- [ ] **Step 4: Revisar escritorio y móvil**

Con anchos de 1440 px y 390 px comprobar:

- opciones, botón y puntaje de la prueba;
- jerarquía de la Introducción y ausencia de duplicaciones;
- descargas visibles en la primera pantalla de Stata;
- tabla de resultados y bloques de salida legibles.

- [ ] **Step 5: Verificar integridad final**

Run: `git diff --check`

Expected: sin salida.

Run: `git status --short`

Expected: cambios del bloque comprometidos; `docs/`, artefactos de render y modificaciones locales preexistentes no fueron añadidos.

- [ ] **Step 6: Entregar revisión**

Entregar los tres enlaces locales, resumen de cambios académicos de la prueba, conteo de pruebas y capturas de escritorio/móvil. Detenerse para aprobación de Ana María antes de comenzar Parámetros causales.
