# Piloto RCT: estructura teórica, recursos y resultados Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Aplicar al módulo RCT los patrones aprobados para clases teóricas y prácticas, manteniendo completas las demostraciones y mostrando al inicio de la práctica las descargas y los resultados principales de Stata.

**Architecture:** `05-RCT.Rmd` se reorganiza mediante encabezados y transiciones sin eliminar contenido sustantivo. `06-RCT2.Rmd` obtiene un bloque inicial de recursos y consume archivos canónicos exportados por Stata para las tablas principales; las verificaciones automáticas impiden renderizar cifras ausentes o inconsistentes. `style.css` aporta un único componente visual reutilizable para los materiales.

**Tech Stack:** R Markdown, bookdown/gitbook, R (`haven`, `dplyr`, `knitr`, `kableExtra`), Stata 19, CSS, Python/pytest para la verificación Stata–Python.

## Global Constraints

- Las demostraciones matemáticas completas permanecerán en el cuerpo de `05-RCT.Rmd`.
- `05-RCT.Rmd` no tendrá bloque de descargas.
- El bloque **Materiales para la clase** aparecerá en `06-RCT2.Rmd` después de los objetivos y antes de la pregunta empírica.
- Las tablas publicadas no contendrán cifras transcritas manualmente.
- El libro mostrará descriptivas/balance, estimaciones centrales y heterogeneidad o robustez indispensable; los resultados auxiliares quedarán en archivos descargables.
- Los archivos descargables y las tablas deben usar la misma base `dofile/06_RCT_Stata/data.dta` y la misma muestra analítica.
- Las diapositivas quedan fuera de este plan; posteriormente deberán incluir todos los resultados de la clase práctica.
- No se publicará en `docs/` durante este piloto: el render se hará en `/private/tmp/libro_cortes_rct_render`.

---

### Task 1: Contrato automatizado de recursos y tablas prácticas

**Files:**
- Create: `dofile/06_RCT_Stata/tests/test_practical_chapter_contract.py`
- Test: `dofile/06_RCT_Stata/tests/test_practical_chapter_contract.py`

**Interfaces:**
- Consumes: texto de `06-RCT2.Rmd` y archivos bajo `dofile/06_RCT_Stata/`.
- Produces: pruebas que fijan la posición de descargas, la existencia de archivos y las fuentes canónicas exigidas por el capítulo.

- [ ] **Step 1: Escribir las pruebas fallidas**

```python
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
CHAPTER = ROOT / "06-RCT2.Rmd"


def test_materials_precede_empirical_question():
    text = CHAPTER.read_text(encoding="utf-8")
    assert "## Materiales para la clase {-}" in text
    assert text.index("## Materiales para la clase {-}") < text.index("## Pregunta empírica {-}")
    assert "## DESCARGA LOS DOCUMENTOS {-}" not in text


def test_download_targets_exist():
    required = [
        "clase6_stata.do",
        "clase6_R.R",
        "clase6_python.ipynb",
        "data.dta",
        "results/resultados_stata.csv",
        "results/verificacion_stata_python.csv",
    ]
    base = ROOT / "dofile/06_RCT_Stata"
    assert all((base / path).is_file() for path in required)


def test_chapter_reads_canonical_stata_results():
    text = CHAPTER.read_text(encoding="utf-8")
    assert 'read.csv("dofile/06_RCT_Stata/results/resultados_stata.csv"' in text
```

- [ ] **Step 2: Ejecutar las pruebas y confirmar el fallo esperado**

Run: `/private/tmp/libro_cortes_rct_venv/bin/python -m pytest dofile/06_RCT_Stata/tests/test_practical_chapter_contract.py -q`

Expected: al menos `test_materials_precede_empirical_question` falla porque el bloque continúa al final y `test_chapter_reads_canonical_stata_results` falla porque la tabla aún se calcula en R.

- [ ] **Step 3: Confirmar que las pruebas existentes siguen verdes antes de editar**

Run: `/private/tmp/libro_cortes_rct_venv/bin/python -m pytest dofile/06_RCT_Stata/tests/test_rct_python.py dofile/06_RCT_Stata/tests/test_verify_stata_python.py -q`

Expected: `10 passed`.

- [ ] **Step 4: Commit del contrato fallido**

```bash
git add dofile/06_RCT_Stata/tests/test_practical_chapter_contract.py
git commit -m "test: define practical chapter publishing contract"
```

---

### Task 2: Bloque inicial de materiales descargables

**Files:**
- Modify: `06-RCT2.Rmd:1-55,801-824`
- Modify: `style.css`
- Test: `dofile/06_RCT_Stata/tests/test_practical_chapter_contract.py`

**Interfaces:**
- Consumes: archivos existentes de Stata, R, Python, datos y resultados.
- Produces: sección inicial `## Materiales para la clase {-}` y componente CSS `.class-materials`.

- [ ] **Step 1: Añadir el bloque después del cierre de objetivos**

Insertar en `06-RCT2.Rmd`, antes del chunk `setup-cap6`:

```markdown
## Materiales para la clase {-}

::: {.class-materials}
**Descarga antes de comenzar**

- [Do-file de Stata](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/06_RCT_Stata/clase6_stata.do)
- [Base `data.dta`](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/06_RCT_Stata/data.dta)
- [Script de R](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/06_RCT_Stata/clase6_R.R)
- [Notebook de Python](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/06_RCT_Stata/clase6_python.ipynb)
- [Resultados completos de Stata](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/06_RCT_Stata/results/resultados_stata.csv)
- [Verificación Stata–Python](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/06_RCT_Stata/results/verificacion_stata_python.csv)

<a class="colab-link" href="https://colab.research.google.com/github/adiazescobar/libro_cortes/blob/main/dofile/06_RCT_Stata/clase6_python.ipynb" target="_blank" rel="noopener">
  <img src="https://colab.research.google.com/assets/colab-badge.svg" alt="Abrir notebook en Google Colab">
</a>
:::
```

- [ ] **Step 2: Eliminar la sección final duplicada**

Eliminar desde `## DESCARGA LOS DOCUMENTOS {-}` hasta el final de la lista de materiales. Conservar cualquier contenido pedagógico anterior al encabezado.

- [ ] **Step 3: Añadir estilos responsivos**

Agregar al final de `style.css`:

```css
.class-materials {
  background: #f7f3ea;
  border: 1px solid var(--line);
  border-left: 5px solid var(--accent);
  border-radius: 10px;
  margin: 1.25em 0 1.75em;
  padding: 1em 1.15em;
}

.class-materials ul {
  display: grid;
  gap: 0.55em 1.2em;
  grid-template-columns: repeat(2, minmax(0, 1fr));
  list-style: none;
  margin: 0.8em 0;
  padding: 0;
}

.class-materials li a {
  display: block;
  font-weight: 600;
}

.class-materials .colab-link img { height: 20px; }

@media (max-width: 700px) {
  .class-materials ul { grid-template-columns: 1fr; }
}
```

- [ ] **Step 4: Ejecutar el contrato de posición y archivos**

Run: `/private/tmp/libro_cortes_rct_venv/bin/python -m pytest dofile/06_RCT_Stata/tests/test_practical_chapter_contract.py::test_materials_precede_empirical_question dofile/06_RCT_Stata/tests/test_practical_chapter_contract.py::test_download_targets_exist -q`

Expected: `2 passed`.

- [ ] **Step 5: Commit del bloque de materiales**

```bash
git add 06-RCT2.Rmd style.css
git commit -m "feat: surface practical class downloads first"
```

---

### Task 3: Tablas centrales provenientes de resultados Stata

**Files:**
- Modify: `dofile/06_RCT_Stata/clase6_stata.do`
- Modify: `06-RCT2.Rmd:294-505,533-668`
- Modify: `dofile/06_RCT_Stata/tests/test_practical_chapter_contract.py`
- Create: `dofile/06_RCT_Stata/results/balance_stata.csv`
- Create: `dofile/06_RCT_Stata/results/heterogeneidad_stata.csv`

**Interfaces:**
- Consumes: `data.dta` y las mismas definiciones de `D`, `y`, `mujer`, `pregrado`, `maestria`, `semestre` usadas por el do-file.
- Produces: CSV canónicos `balance_stata.csv`, `resultados_stata.csv` y `heterogeneidad_stata.csv`; tablas renderizadas inmediatamente después del código Stata correspondiente.

- [ ] **Step 1: Ampliar el contrato con validación de esquema**

Agregar a `test_practical_chapter_contract.py`:

```python
import csv


def _columns(path):
    with path.open(newline="", encoding="utf-8-sig") as handle:
        return set(next(csv.reader(handle)))


def test_canonical_stata_tables_have_required_columns():
    results = ROOT / "dofile/06_RCT_Stata/results"
    assert {"variable", "media_tratado", "media_control", "diferencia", "p_value"} <= _columns(results / "balance_stata.csv")
    assert {"modelo", "termino", "coeficiente", "error_estandar", "N", "R2"} <= _columns(results / "resultados_stata.csv")
    assert {"moderador", "termino", "coeficiente", "error_estandar", "N"} <= _columns(results / "heterogeneidad_stata.csv")
```

- [ ] **Step 2: Ejecutar la prueba y confirmar el fallo por archivos ausentes**

Run: `/private/tmp/libro_cortes_rct_venv/bin/python -m pytest dofile/06_RCT_Stata/tests/test_practical_chapter_contract.py::test_canonical_stata_tables_have_required_columns -q`

Expected: FAIL con `FileNotFoundError` para `balance_stata.csv`.

- [ ] **Step 3: Exportar balance y heterogeneidad desde Stata**

En `clase6_stata.do`, reutilizar `Table_Balance_raw.dta` y exportar una versión con encabezados canónicos:

```stata
use "Table_Balance_raw.dta", clear
rename mean_T media_tratado
rename mean_C media_control
rename diff diferencia
rename pval p_value
keep variable media_tratado media_control diferencia p_value N_T N_C
export delimited using "results/balance_stata.csv", replace
```

Después de `reg y D##i.mujer, vce(robust)`, exportar la interacción principal:

```stata
tempname hetpost
postfile `hetpost' str20 moderador str40 termino double coeficiente error_estandar N using "results/heterogeneidad_stata.dta", replace
post `hetpost' ("genero") ("D#Mujer") (_b[1.D#1.mujer]) (_se[1.D#1.mujer]) (e(N))
postclose `hetpost'
use "results/heterogeneidad_stata.dta", clear
export delimited using "results/heterogeneidad_stata.csv", replace
```

- [ ] **Step 4: Ejecutar Stata y validar los CSV**

Run: `/Applications/StataNow/StataSE.app/Contents/MacOS/stata-se -b do clase6_stata.do` desde `dofile/06_RCT_Stata`.

Expected: código de salida 0; existen los tres CSV y no contienen observaciones vacías.

Run: `/private/tmp/libro_cortes_rct_venv/bin/python -m pytest dofile/06_RCT_Stata/tests/test_practical_chapter_contract.py::test_canonical_stata_tables_have_required_columns -q`

Expected: `1 passed`.

- [ ] **Step 5: Sustituir las tablas centrales calculadas en R por lectores validados**

Añadir al setup de `06-RCT2.Rmd`:

```r
read_required_csv <- function(path, required) {
  if (!file.exists(path)) stop("Falta el resultado canónico de Stata: ", path)
  out <- read.csv(path, check.names = FALSE)
  missing <- setdiff(required, names(out))
  if (length(missing)) stop("Faltan columnas en ", path, ": ", paste(missing, collapse = ", "))
  if (!nrow(out)) stop("El resultado canónico de Stata está vacío: ", path)
  out
}

stata_main <- read_required_csv(
  "dofile/06_RCT_Stata/results/resultados_stata.csv",
  c("modelo", "termino", "coeficiente", "error_estandar", "N", "R2")
)
```

Leer `balance_stata.csv` en el chunk de balance, `resultados_stata.csv` en los cuatro escenarios y `heterogeneidad_stata.csv` después de la especificación por género. Formatear con `kable()`/`kable_styling()` y añadir debajo una interpretación de magnitud, precisión y relación con el diseño. Mantener los gráficos de heterogeneidad calculados desde `data.dta` porque son visualizaciones pedagógicas, no tablas canónicas.

- [ ] **Step 6: Ejecutar el contrato completo y la verificación numérica existente**

Run: `/private/tmp/libro_cortes_rct_venv/bin/python -m pytest dofile/06_RCT_Stata/tests -q`

Expected: todas las pruebas pasan.

Run: `/private/tmp/libro_cortes_rct_venv/bin/python dofile/06_RCT_Stata/verify_stata_python.py`

Expected: cuatro filas con `estado = PASS`.

- [ ] **Step 7: Commit de tablas canónicas**

```bash
git add 06-RCT2.Rmd dofile/06_RCT_Stata/clase6_stata.do dofile/06_RCT_Stata/results/balance_stata.csv dofile/06_RCT_Stata/results/heterogeneidad_stata.csv dofile/06_RCT_Stata/results/resultados_stata.csv dofile/06_RCT_Stata/tests/test_practical_chapter_contract.py
git commit -m "feat: publish central Stata results in RCT practice"
```

---

### Task 4: Estructura común de la clase teórica RCT

**Files:**
- Modify: `05-RCT.Rmd`
- Create: `dofile/06_RCT_Stata/tests/test_theory_chapter_contract.py`

**Interfaces:**
- Consumes: contenido sustantivo existente de `05-RCT.Rmd`.
- Produces: encabezados y transiciones acordes con la estructura teórica aprobada, sin bloque de descargas ni eliminación de demostraciones.

- [ ] **Step 1: Crear pruebas de estructura y preservación**

```python
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
TEXT = (ROOT / "05-RCT.Rmd").read_text(encoding="utf-8")


def test_theory_sections_are_ordered():
    headings = [
        "## Pregunta causal {-}",
        "## Intuición y motivación {-}",
        "## Notación, parámetros y estimandos {-}",
        "## Supuestos de identificación {-}",
        "## Desarrollo teórico y demostraciones {-}",
        "## Amenazas, limitaciones y errores comunes {-}",
        "## Resumen {-}",
        "## Preguntas para clase {-}",
        "## Puente a la clase práctica {-}",
        "## Referencias {-}",
    ]
    positions = [TEXT.index(heading) for heading in headings]
    assert positions == sorted(positions)


def test_theory_has_no_download_block():
    assert "DESCARGA LOS DOCUMENTOS" not in TEXT
    assert "Materiales para la clase" not in TEXT


def test_core_derivations_remain():
    required = [
        "sesgo de selección",
        r"\text{Cov}(D,M)",
        "RCT simple, sin estratos, sin controles",
        "RCT estratificado + controles adicionales",
        "El truco de centrar (Wooldridge)",
    ]
    assert all(item in TEXT for item in required)
```

- [ ] **Step 2: Ejecutar y confirmar el fallo de encabezados**

Run: `/private/tmp/libro_cortes_rct_venv/bin/python -m pytest dofile/06_RCT_Stata/tests/test_theory_chapter_contract.py -q`

Expected: `test_theory_sections_are_ordered` falla en el primer encabezado ausente; las pruebas de no descargas y derivaciones pasan.

- [ ] **Step 3: Reorganizar mediante encabezados y transiciones**

Editar `05-RCT.Rmd` para introducir la secuencia aprobada. Mantener intactas las ecuaciones, simulaciones y derivaciones sustantivas; mover bloques completos únicamente cuando sea necesario para ubicar intuición antes de formalización. Añadir:

```markdown
## Pregunta causal {-}

¿Cuándo la diferencia observada entre tratados y controles identifica el efecto causal promedio, y cómo debe cambiar la estimación cuando el diseño incorpora estratos o controles pretratamiento?

## Puente a la clase práctica {-}

En la clase práctica aplicaremos estas cuatro especificaciones a un experimento de aula con 70 observaciones y asignación dentro de semestre. Verificaremos balance, compararemos precisión y evaluaremos heterogeneidad sin cambiar el estimando causal.

## Referencias {-}

Conservar aquí las lecturas ya citadas al comienzo y las referencias sustantivas utilizadas en las demostraciones.
```

En `## Supuestos de identificación {-}`, distinguir explícitamente aleatorización, SUTVA, ausencia de atrición diferencial y respeto de los estratos. En las secciones de estimación, separar identificación de precisión e inferencia.

- [ ] **Step 4: Ejecutar pruebas de estructura y diff de preservación**

Run: `/private/tmp/libro_cortes_rct_venv/bin/python -m pytest dofile/06_RCT_Stata/tests/test_theory_chapter_contract.py -q`

Expected: `3 passed`.

Run: `git diff --word-diff=porcelain -- 05-RCT.Rmd`

Expected: no desaparecen ecuaciones ni los cuatro escenarios; las eliminaciones corresponden a encabezados sustituidos o texto trasladado.

- [ ] **Step 5: Commit de la estructura teórica**

```bash
git add 05-RCT.Rmd dofile/06_RCT_Stata/tests/test_theory_chapter_contract.py
git commit -m "docs: standardize RCT theory chapter structure"
```

---

### Task 5: Render y revisión visual del piloto completo

**Files:**
- Verify: `05-RCT.Rmd`
- Verify: `06-RCT2.Rmd`
- Verify: `style.css`
- Verify: `/private/tmp/libro_cortes_rct_render/experimentos-aleatorizados-clase-teórica.html`
- Verify: `/private/tmp/libro_cortes_rct_render/experimentos-aleatorizados-clase-empírica.html`

**Interfaces:**
- Consumes: capítulos, CSS y CSV canónicos terminados.
- Produces: render temporal validado y evidencia visual de escritorio y pantalla angosta.

- [ ] **Step 1: Ejecutar todas las pruebas y verificaciones**

Run: `/private/tmp/libro_cortes_rct_venv/bin/python -m pytest dofile/06_RCT_Stata/tests -q`

Expected: todas las pruebas pasan.

Run: `/private/tmp/libro_cortes_rct_venv/bin/python dofile/06_RCT_Stata/verify_stata_python.py`

Expected: cuatro modelos con `estado = PASS`.

- [ ] **Step 2: Renderizar el libro fuera de `docs/`**

Run: `Rscript -e "bookdown::render_book('index.Rmd', 'bookdown::gitbook', output_dir='/private/tmp/libro_cortes_rct_render')"`

Expected: código de salida 0 y ambos HTML del módulo RCT existen en `/private/tmp/libro_cortes_rct_render`.

- [ ] **Step 3: Servir y revisar escritorio**

Run: `python3 -m http.server 8765 --directory /private/tmp/libro_cortes_rct_render`

Abrir la clase empírica y comprobar que el bloque de materiales aparece sin desplazamiento, los enlaces responden y las tres tablas centrales son legibles junto a su código e interpretación. Abrir la clase teórica y comprobar que no hay bloque de descargas y que las demostraciones permanecen visibles.

- [ ] **Step 4: Revisar pantalla angosta**

Con ancho aproximado de 390 px, comprobar que los enlaces se apilan, las cifras no se superponen y las tablas permiten lectura o desplazamiento horizontal controlado.

- [ ] **Step 5: Verificar integridad del repositorio**

Run: `git diff --check`

Expected: sin salida.

Run: `git status --short`

Expected: los archivos fuente del piloto están comprometidos; los cambios locales preexistentes y artefactos de render no se incluyen en commits del piloto.

- [ ] **Step 6: Registrar evidencia final**

No modificar `docs/`. Entregar a Ana María los enlaces locales de los dos capítulos, el conteo de pruebas, los cuatro estados Stata–Python y capturas de escritorio/pantalla angosta para decidir si el patrón se extiende al resto del libro.
