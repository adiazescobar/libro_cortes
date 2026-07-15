# Parámetros causales Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Estandarizar los capítulos teórico y práctico de Parámetros causales, incorporar CATE, producir resultados reproducibles en Stata y aplicar la notación de clase a todo el libro.

**Architecture:** `03-Parametros.Rmd` concentrará la exposición conceptual y las demostraciones; `dofile/04_ParametrosStata/04_stata.do` será la fuente canónica de cifras, tablas y gráficos; `04-ParametrosStata.Rmd` consumirá esas salidas y mostrará solo resultados pedagógicamente esenciales. Pruebas contractuales verificarán estructura, notación, descargas y correspondencia entre Rmd y resultados exportados.

**Tech Stack:** R Markdown, bookdown/gitbook, Stata 19, CSV, PNG, Python 3/pytest, R/knitr.

## Global Constraints

- Usar exclusivamente `Y_i(D=1)` y `Y_i(D=0)`, o `Y(D=1)` y `Y(D=0)` cuando no sea necesario el índice.
- Incluir ATE, ATT, ATU y CATE, distinguiendo parámetros de estimadores.
- El capítulo teórico no tendrá descargas; el práctico tendrá descargas inmediatamente después del título.
- Los dos videos teóricos permanecen; el prompt largo se convierte en una actividad breve y evaluable.
- La página práctica muestra resultados esenciales; los resultados completos permanecen descargables.
- Las futuras diapositivas prácticas incluirán todos los resultados, pero no forman parte de este plan.
- El do-file de Stata será la única fuente de las cifras publicadas.
- Los Monte Carlo usarán 1.000 repeticiones, 80.000 observaciones por repetición y semillas documentadas.
- Los renders se escribirán en `/private/tmp/libro_cortes_parametros_render`, nunca en `docs/`.
- Los cambios locales preexistentes y artefactos no relacionados permanecerán fuera de los commits.

---

### Task 1: Contratos académicos y estructurales

**Files:**
- Create: `tests/test_parametros_theory_contract.py`
- Create: `tests/test_parametros_stata_contract.py`
- Create: `tests/test_potential_outcomes_notation.py`

**Interfaces:**
- Consumes: la especificación `docs/superpowers/specs/2026-07-15-parametros-causales-design.md`.
- Produces: contratos ejecutables para las dos páginas y para la notación global.

- [ ] **Step 1: Escribir el contrato fallido del capítulo teórico**

```python
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
TEXT = (ROOT / "03-Parametros.Rmd").read_text(encoding="utf-8")


def test_theory_sections_follow_approved_order():
    headings = [
        "## Objetivos y lecturas {-}",
        "## Pregunta causal y población de interés {-}",
        "## Resultados potenciales {-}",
        "## El problema fundamental {-}",
        "## ATE, ATT, ATU y CATE {-}",
        "## Diferencia observada y sesgo de selección {-}",
        "## Supuestos de identificación {-}",
        "## Comparación antes-después {-}",
        "## SUTVA {-}",
        "## Síntesis {-}",
        "## Ejercicios {-}",
        "## Puente a la práctica {-}",
        "## Referencias {-}",
    ]
    positions = [TEXT.index(heading) for heading in headings]
    assert positions == sorted(positions)


def test_cate_and_aggregation_are_present():
    assert "CATE(x)" in TEXT
    assert "ATE=\\mathbb{E}[CATE(X_i)]" in TEXT
    assert "ATE=P(D_i=1)ATT+P(D_i=0)ATU" in TEXT


def test_theory_keeps_videos_but_not_long_chatgpt_prompt():
    assert TEXT.count("youtube.com/embed/") == 2
    assert "PROMPT DE CHATGPT PARA REFLEXIÓN PROFUNDA" not in TEXT
    for phrase in ["estimando", "contrafactual faltante", "supuesto", "amenazas"]:
        assert phrase in TEXT.lower()
```

- [ ] **Step 2: Escribir el contrato fallido del capítulo práctico**

```python
import csv
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
TEXT = (ROOT / "04-ParametrosStata.Rmd").read_text(encoding="utf-8")
BASE = ROOT / "dofile/04_ParametrosStata"


def test_downloads_are_first_and_complete():
    assert TEXT.index("## Materiales para la clase {-}") < TEXT.index("## Objetivos {-}")
    required = [
        "04_stata.do", "04_data.dta", "04_R.R", "04_phyton.ipynb",
        "results/parameters_results.csv", "results/monte_carlo_summary.csv",
        "04_stata.log",
    ]
    assert all((BASE / path).is_file() for path in required)


def test_page_consumes_canonical_results():
    assert 'read.csv("dofile/04_ParametrosStata/results/parameters_results.csv"' in TEXT
    assert 'read.csv("dofile/04_ParametrosStata/results/monte_carlo_summary.csv"' in TEXT
    assert "Linear regression                               Number of obs" not in TEXT
    assert "..." not in TEXT


def test_results_schema():
    with (BASE / "results/parameters_results.csv").open(newline="", encoding="utf-8-sig") as handle:
        columns = set(next(csv.reader(handle)))
    assert {"escenario", "estimando", "valor", "N"} <= columns
```

- [ ] **Step 3: Escribir la prueba fallida de notación global**

```python
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]


def test_all_chapters_use_class_potential_outcomes_notation():
    forbidden = re.compile(r"Y(?:_i)?\((?:1|0)\)")
    offenders = {}
    for path in ROOT.glob("*.Rmd"):
        matches = forbidden.findall(path.read_text(encoding="utf-8"))
        if matches:
            offenders[path.name] = matches
    assert offenders == {}
```

- [ ] **Step 4: Ejecutar y confirmar los fallos correctos**

Run: `/private/tmp/libro_cortes_rct_venv/bin/python -m pytest -q tests/test_parametros_theory_contract.py tests/test_parametros_stata_contract.py tests/test_potential_outcomes_notation.py`

Expected: fallan por estructura antigua, archivos de resultados inexistentes y 78 usos de notación abreviada en 13 capítulos.

- [ ] **Step 5: Commit de pruebas contractuales**

```bash
git add tests/test_parametros_theory_contract.py tests/test_parametros_stata_contract.py tests/test_potential_outcomes_notation.py
git commit -m "test: define causal parameters chapter contracts"
```

---

### Task 2: Revisión académica de Parámetros causales (teoría)

**Files:**
- Modify: `03-Parametros.Rmd`
- Test: `tests/test_parametros_theory_contract.py`

**Interfaces:**
- Consumes: contrato teórico de Task 1 y la muestra de ocho perfiles ya presente.
- Produces: capítulo teórico completo con notación de clase y puente explícito a Stata.

- [ ] **Step 1: Reorganizar encabezados y apertura**

Reescribir la apertura con los trece encabezados del contrato. En “Pregunta causal” usar como hilo conductor: “¿Cuál es el efecto de participar en un programa de capacitación sobre el salario de la población elegible?”. Definir población, tratamiento binario y resultado antes de introducir notación.

- [ ] **Step 2: Formalizar resultados potenciales y observación**

Incluir exactamente:

```markdown
Para cada unidad (i), (Y_i(D=1)) es el salario que tendría bajo tratamiento y (Y_i(D=0)) el salario que tendría sin tratamiento. El resultado observado satisface

\[
Y_i=D_iY_i(D=1)+(1-D_i)Y_i(D=0).
\]
```

Explicar que solo uno de los dos resultados potenciales se observa y que el faltante es el contrafactual individual.

- [ ] **Step 3: Definir los cuatro parámetros y sus relaciones**

Incluir las definiciones:

```latex
ATE=\mathbb{E}[Y_i(D=1)-Y_i(D=0)]
ATT=\mathbb{E}[Y_i(D=1)-Y_i(D=0)\mid D_i=1]
ATU=\mathbb{E}[Y_i(D=1)-Y_i(D=0)\mid D_i=0]
CATE(x)=\mathbb{E}[Y_i(D=1)-Y_i(D=0)\mid X_i=x]
```

Derivar `ATE=P(D_i=1)ATT+P(D_i=0)ATU` y `ATE=\mathbb{E}[CATE(X_i)]`. Aclarar que `X` debe ser pretratamiento para interpretar el CATE como heterogeneidad entre subgrupos definidos antes de la intervención.

- [ ] **Step 4: Conservar y completar el ejemplo de ocho personas**

Mantener `yd0`, `yd1` y `D`; añadir un indicador pedagógico `X` que vale 0 para las primeras cuatro filas y 1 para las últimas cuatro, definido como atributo pretratamiento del ejemplo. Añadir columnas de resultado observado y efecto individual, y preguntas para ATE, ATT, ATU, `CATE(0)`, `CATE(1)`, diferencia naïve y sesgo.

- [ ] **Step 5: Presentar la demostración completa del sesgo**

Mostrar la suma y resta de `\mathbb{E}[Y_i(D=0)\mid D_i=1]` hasta obtener:

```latex
\mathbb{E}[Y_i\mid D_i=1]-\mathbb{E}[Y_i\mid D_i=0]
=ATT+\mathbb{E}[Y_i(D=0)\mid D_i=1]
-\mathbb{E}[Y_i(D=0)\mid D_i=0].
```

Interpretar signo positivo, negativo y cero sin afirmar que el sesgo siempre sobreestima.

- [ ] **Step 6: Corregir los supuestos de identificación**

Separar:

```latex
(Y_i(D=1),Y_i(D=0))\perp D_i
```

de

```latex
(Y_i(D=1),Y_i(D=0))\perp D_i\mid X_i,
\qquad 0<P(D_i=1\mid X_i=x)<1.
```

Explicar que IV, RDD y DiD no “cumplen independencia” en general: identifican parámetros bajo relevancia/exclusión, continuidad y tendencias paralelas, respectivamente.

- [ ] **Step 7: Corregir antes-después y SUTVA**

Usar `Y_{it}(D=0)` y `Y_{it}(D=1)` para separar tiempo de tratamiento. Definir el contrafactual faltante como `Y_{i1}(D=0)`. Presentar SUTVA como ausencia de interferencia y tratamiento bien definido, incluyendo vacunación, redes y dosis como ejemplos.

- [ ] **Step 8: Conservar videos y reemplazar el prompt**

Mantener los dos `iframe`. Sustituir el bloque de prompt por una actividad de cuatro respuestas: estimando objetivo, contrafactual faltante, supuesto requerido y dos amenazas. Añadir síntesis, tres ejercicios y puente explícito a `04-ParametrosStata.Rmd`.

- [ ] **Step 9: Ejecutar la prueba teórica**

Run: `/private/tmp/libro_cortes_rct_venv/bin/python -m pytest -q tests/test_parametros_theory_contract.py`

Expected: todas las pruebas del archivo pasan.

- [ ] **Step 10: Commit del capítulo teórico**

```bash
git add 03-Parametros.Rmd tests/test_parametros_theory_contract.py
git commit -m "docs: restructure causal parameters theory"
```

---

### Task 3: Pipeline canónico de Stata y CATE por subgrupo

**Files:**
- Modify: `dofile/04_ParametrosStata/04_stata.do`
- Modify: `dofile/04_ParametrosStata/04_data.dta` only if the source needs a persistent `X`; prefer generating `X` in the do-file.
- Create: `dofile/04_ParametrosStata/results/parameters_results.csv`
- Create: `dofile/04_ParametrosStata/results/monte_carlo_summary.csv`
- Create: `dofile/04_ParametrosStata/results/monte_carlo_draws.dta`
- Modify: `dofile/04_ParametrosStata/sesgo_con_seleccion.png`
- Modify: `dofile/04_ParametrosStata/sesgo_con_aleatorizacion.png`
- Create: `dofile/04_ParametrosStata/comparacion_escenarios.png`
- Modify: `dofile/04_ParametrosStata/04_stata.log`
- Test: `tests/test_parametros_stata_contract.py`

**Interfaces:**
- Consumes: `04_data.dta` with `yd0`, `yd1` and `D`.
- Produces: CSV con resultados puntuales, CSV de resúmenes Monte Carlo, base de repeticiones, tres gráficos y log completo.

- [ ] **Step 1: Crear resultados puntuales mediante `postfile`**

Al inicio del do-file fijar rutas relativas, crear `results`, generar `X=(_n>4)`, `y` y `tau`, y usar:

```stata
capture mkdir "results"
gen byte X = (_n > 4)
label define grupo_pre 0 "Grupo A" 1 "Grupo B"
label values X grupo_pre
gen double y = D*yd1 + (1-D)*yd0
gen double tau = yd1 - yd0

tempname pointpost
postfile `pointpost' str24 escenario str16 estimando double valor long N using "results/parameters_results.dta", replace
```

Guardar ATE, ATT, ATU, `CATE_X0`, `CATE_X1`, NAIVE, sesgo y coeficiente de regresión. Exportar al final con `export delimited using "results/parameters_results.csv", replace`.

- [ ] **Step 2: Hacer explícito el experimento de duplicación**

Conservar `expand 10000`, pero etiquetar el escenario `datos_duplicados` y explicar en comentarios que se replican perfiles idénticos: aumenta `N` nominal, no la información independiente, y deja intactos estimandos y sesgo.

- [ ] **Step 3: Implementar el experimento de asignación aleatoria**

Restaurar la población expandida, fijar `set seed 87634`, generar `D=(runiform()<0.5)` y guardar estimandos bajo `aleatorizacion_unica`. No afirmar igualdad exacta; registrar la desviación `NAIVE-ATE` y el sesgo respecto a ATT.

- [ ] **Step 4: Reemplazar los loops frágiles por un programa de simulación**

Definir un programa que devuelva escalares:

```stata
capture program drop one_rep
program define one_rep, rclass
    syntax, POPulation(string) Scenario(string)
    use "`population'", clear
    drop D
    if "`scenario'" == "seleccion" {
        quietly summarize yd0
        generate double p = invlogit((yd0-r(mean))/2)
        generate byte D = runiform() < p
    }
    else generate byte D = runiform() < .5
    generate double y = D*yd1 + (1-D)*yd0
    generate double tau = yd1-yd0
    quietly summarize tau if D==1
    scalar att = r(mean)
    quietly summarize y if D==1
    scalar y1 = r(mean)
    quietly summarize y if D==0
    return scalar sesgo = y1-r(mean)-att
end

simulate sesgo=r(sesgo), reps(1000) seed(12345): one_rep, population("`population'") scenario("seleccion")
generate str16 escenario = "seleccion"
generate long rep = _n
save "results/monte_carlo_seleccion.dta", replace

simulate sesgo=r(sesgo), reps(1000) seed(87634): one_rep, population("`population'") scenario("aleatorizacion")
generate str16 escenario = "aleatorizacion"
generate long rep = _n
append using "results/monte_carlo_seleccion.dta"
save "results/monte_carlo_draws.dta", replace
```

Ejecutar 1.000 repeticiones por escenario con semillas distintas y almacenar `rep`, `escenario` y `sesgo` en una única base.

- [ ] **Step 5: Exportar resúmenes y gráficos**

Calcular por escenario `N`, media, desviación estándar, p5, mediana y p95; exportar `monte_carlo_summary.csv`. Guardar `monte_carlo_draws.dta`. Generar histogramas con el mismo eje y colores consistentes, y un gráfico comparativo con ambos escenarios.

- [ ] **Step 6: Ejecutar Stata 19 de principio a fin**

Run: `/Applications/StataNow/StataSE.app/Contents/MacOS/stata-se -b do 04_stata.do`

Working directory: `dofile/04_ParametrosStata/`

Expected: código de salida 0; `04_stata.log` termina sin `r(...)`; existen dos CSV, una base de simulaciones y tres PNG; cada escenario tiene 1.000 repeticiones.

- [ ] **Step 7: Ejecutar el contrato de resultados**

Run: `/private/tmp/libro_cortes_rct_venv/bin/python -m pytest -q tests/test_parametros_stata_contract.py::test_results_schema`

Expected: la prueba del esquema canónico pasa; las pruebas de estructura del Rmd se reservan para Task 4.

- [ ] **Step 8: Commit del pipeline Stata**

```bash
git add dofile/04_ParametrosStata/04_stata.do dofile/04_ParametrosStata/04_stata.log dofile/04_ParametrosStata/results dofile/04_ParametrosStata/sesgo_con_seleccion.png dofile/04_ParametrosStata/sesgo_con_aleatorizacion.png dofile/04_ParametrosStata/comparacion_escenarios.png
git commit -m "feat: make causal parameters results reproducible"
```

---

### Task 4: Reestructuración de Parámetros causales en Stata

**Files:**
- Modify: `04-ParametrosStata.Rmd`
- Test: `tests/test_parametros_stata_contract.py`

**Interfaces:**
- Consumes: `parameters_results.csv`, `monte_carlo_summary.csv` y tres gráficos de Task 3.
- Produces: página práctica compacta con resultados verificados y enlaces funcionales.

- [ ] **Step 1: Añadir materiales inmediatamente después del título**

Crear `## Materiales para la clase {-}` con enlaces a do-file, base, R, notebook histórico `04_phyton.ipynb`, Colab, log, ambos CSV y gráficos. Eliminar el bloque de descargas del final.

- [ ] **Step 2: Cargar y validar resultados canónicos**

Añadir un chunk oculto:

```r
point <- read.csv("dofile/04_ParametrosStata/results/parameters_results.csv", check.names = FALSE)
mc <- read.csv("dofile/04_ParametrosStata/results/monte_carlo_summary.csv", check.names = FALSE)
stopifnot(all(c("escenario", "estimando", "valor", "N") %in% names(point)))
stopifnot(all(c("escenario", "N", "media", "sd", "p5", "p50", "p95") %in% names(mc)))
```

- [ ] **Step 3: Aplicar la secuencia pedagógica estándar**

Para descripción, regresión, parámetros, duplicación, aleatorización y Monte Carlo usar, en ese orden, subtítulos o etiquetas visibles **Pregunta**, **Comando**, **Resultado**, **Interpretación** y **Práctica breve**. Incluir código Stata ejecutable sin pegar consolas completas.

- [ ] **Step 4: Mostrar tablas esenciales desde CSV**

Construir una tabla puntual para ATE, ATT, ATU, CATE(0), CATE(1), NAIVE y sesgo; otra para duplicación y aleatorización; y una tabla Monte Carlo con media, desviación y cuantiles. Usar `knitr::kable` y redondear únicamente para presentación.

- [ ] **Step 5: Insertar gráficos e interpretación**

Incluir los dos histogramas y `comparacion_escenarios.png`. Explicar que selección desplaza la distribución del sesgo y que aleatorización la centra aproximadamente en cero, manteniendo variabilidad entre repeticiones.

- [ ] **Step 6: Añadir ejercicios y síntesis**

Añadir ejercicios para cambiar la regla de selección, comparar CATE por otro `X` pretratamiento y reducir `N` para observar varianza. Cerrar con cuatro lecciones: identificación, heterogeneidad, precisión frente a sesgo y aleatorización en expectativa.

- [ ] **Step 7: Ejecutar pruebas prácticas**

Run: `/private/tmp/libro_cortes_rct_venv/bin/python -m pytest -q tests/test_parametros_stata_contract.py`

Expected: todas las pruebas pasan.

- [ ] **Step 8: Commit del capítulo práctico**

```bash
git add 04-ParametrosStata.Rmd tests/test_parametros_stata_contract.py
git commit -m "docs: standardize causal parameters practice"
```

---

### Task 5: Normalización de resultados potenciales en todo el libro

**Files:**
- Modify: `01-intro.Rmd`
- Modify: `03-Parametros.Rmd`
- Modify: `05-RCT.Rmd`
- Modify: `06-RCT2.Rmd`
- Modify: `08-DID.Rmd`
- Modify: `09-BadControls.Rmd`
- Modify: `12-ExactMatching.Rmd`
- Modify: `13-PSM.Rmd`
- Modify: `15-IPW.Rmd`
- Modify: `18-IV.Rmd`
- Modify: `19-IVStata.Rmd`
- Modify: `20-RDD.Rmd`
- Modify: `21-RDDStata.Rmd`
- Test: `tests/test_potential_outcomes_notation.py`

**Interfaces:**
- Consumes: los 78 usos abreviados detectados por Task 1.
- Produces: una única convención de notación en todos los Rmd del libro.

- [ ] **Step 1: Sustituir notación caso por caso**

Cambiar `Y_i(1)` por `Y_i(D=1)`, `Y_i(0)` por `Y_i(D=0)`, `Y(1)` por `Y(D=1)` y `Y(0)` por `Y(D=0)`. Revisar cada ecuación después del cambio para evitar duplicaciones como `Y_i(D=D=1)` y conservar índices temporales, por ejemplo `Y_{it}(D=0)`.

- [ ] **Step 2: Revisar prosa vinculada**

Asegurar que “1” y “0” se describan como estados de tratamiento, no como periodos. En capítulos DiD conservar separados los subíndices temporales y el argumento `D=`.

- [ ] **Step 3: Ejecutar la prueba global**

Run: `/private/tmp/libro_cortes_rct_venv/bin/python -m pytest -q tests/test_potential_outcomes_notation.py`

Expected: `1 passed` y diccionario de infractores vacío.

- [ ] **Step 4: Commit de notación global**

```bash
git add 01-intro.Rmd 03-Parametros.Rmd 05-RCT.Rmd 06-RCT2.Rmd 08-DID.Rmd 09-BadControls.Rmd 12-ExactMatching.Rmd 13-PSM.Rmd 15-IPW.Rmd 18-IV.Rmd 19-IVStata.Rmd 20-RDD.Rmd 21-RDDStata.Rmd tests/test_potential_outcomes_notation.py
git commit -m "docs: standardize potential outcomes notation across book"
```

---

### Task 6: Render, control visual y revisión académica final

**Files:**
- Verify: `03-Parametros.Rmd`
- Verify: `04-ParametrosStata.Rmd`
- Verify: `/private/tmp/libro_cortes_parametros_render/parametros-causales-teoria.html`
- Verify: `/private/tmp/libro_cortes_parametros_render/parametros-causales-stata.html`

**Interfaces:**
- Consumes: capítulos y resultados aprobados en Tasks 2–5.
- Produces: vistas previas locales verificadas, sin publicación en `docs/`.

- [ ] **Step 1: Ejecutar toda la batería automatizada**

Run: `/private/tmp/libro_cortes_rct_venv/bin/python -m pytest -q`

Expected: todas las pruebas pasan, incluidas las 41 preexistentes y las nuevas.

- [ ] **Step 2: Verificar limpieza textual**

Run: `git diff --check`

Expected: sin salida.

Run: `rg -n "\.pull-(left|right)|PROMPT DE CHATGPT|\.\.\." 03-Parametros.Rmd 04-ParametrosStata.Rmd`

Expected: sin coincidencias.

- [ ] **Step 3: Renderizar el libro en carpeta temporal**

Run: `Rscript -e "bookdown::render_book('index.Rmd', output_dir='/private/tmp/libro_cortes_parametros_render')"`

Expected: `Output created: /private/tmp/libro_cortes_parametros_render/index.html` sin errores.

- [ ] **Step 4: Verificar HTML y enlaces**

Inspeccionar que los encabezados, ecuaciones, CATE, tablas y gráficos estén presentes, que no haya sintaxis de slides visible y que las descargas respondan desde la vista previa local.

- [ ] **Step 5: Revisar escritorio y móvil**

Abrir ambos HTML en anchos de escritorio y móvil. Verificar tablas sin desbordamiento, ecuaciones legibles, gráficos con etiquetas visibles, descargas antes de objetivos solo en la práctica y videos responsivos.

- [ ] **Step 6: Auditoría académica final**

Comprobar: coherencia de ATE/ATT/ATU/CATE; identidad de agregación; signo de la descomposición del sesgo; independencia más positividad; separación tiempo/tratamiento; SUTVA; interpretación de duplicación y aleatorización; correspondencia exacta entre CSV y tablas.

- [ ] **Step 7: Commit de ajustes finales, solo si fueron necesarios**

```bash
git add 03-Parametros.Rmd 04-ParametrosStata.Rmd tests dofile/04_ParametrosStata
git commit -m "fix: address causal parameters final review"
```

- [ ] **Step 8: Entregar vistas previas para aprobación**

Iniciar un servidor local sobre `/private/tmp/libro_cortes_parametros_render` y entregar enlaces a los dos capítulos. No copiar nada a `docs/` hasta recibir aprobación explícita.
