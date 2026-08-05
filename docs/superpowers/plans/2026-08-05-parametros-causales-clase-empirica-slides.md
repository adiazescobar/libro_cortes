# Parámetros causales — Clase empírica Slides Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Crear una presentación Xaringan autocontenida de 48–55 diapositivas y un notebook de Google Colab que reproduzcan fielmente la clase empírica de parámetros causales, incluyendo outputs verificados de Stata y las actividades recuperadas de Canvas.

**Architecture:** Stata seguirá siendo la fuente canónica de datos y resultados. Un script de extracción convertirá el log y los CSV canónicos en fragmentos legibles para Xaringan; un notebook independiente reproducirá los procedimientos principales en Python y validará sus resultados contra Stata. La presentación ensamblará contenido, outputs y actividades sin modificar el libro ni otras clases.

**Tech Stack:** Stata 19, R/R Markdown, Xaringan, CSS, pagedown/Chrome, Python 3, pandas, NumPy, SciPy, statsmodels, nbformat/nbconvert.

## Global Constraints

- Trabajar directamente, sin agentes ni llamadas pagadas de API.
- No crear PPTX.
- No modificar presentaciones existentes ni materiales de Regresión Discontinua.
- Seguir `04-ParametrosStata.Rmd` en orden, ejemplos, comandos, variables, resultados y conclusiones.
- Ejecutar Stata y Python; no inventar ni transcribir resultados sin verificación.
- Mantener 1 hora y 45 minutos y aproximadamente 48–55 diapositivas.
- Mostrar pregunta, comando, output, interpretación y conexión causal.
- Incorporar `Pausa 1` y la tarea `ATE` con la identidad algebraica corregida.
- Producir HTML principal, PDF de respaldo y notebook compatible con Colab.

---

### Task 1: Crear la carpeta aislada y el contrato de contenido

**Files:**
- Create: `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/Slides/2026_02/Clase4_ParametrosCausales/README.md`
- Create: `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/Slides/2026_02/Clase4_ParametrosCausales/content_manifest.csv`
- Create: `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/Slides/2026_02/Clase4_ParametrosCausales/tests/test_content_contract.py`

**Interfaces:**
- Consumes: `04-ParametrosStata.Rmd`, `04_stata.do`, `Pausa 1`, tarea `ATE`.
- Produces: `content_manifest.csv` con columnas `order,section,question,stata_command,output_source,interpretation_source,slide_role`.

- [ ] **Step 1: Crear una prueba de contrato que exija las 13 secciones aprobadas**

```python
import csv
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]

def test_manifest_has_approved_sections_in_order():
    rows = list(csv.DictReader((ROOT / "content_manifest.csv").open()))
    sections = list(dict.fromkeys(row["section"] for row in rows))
    assert sections == [
        "objetivos", "datos", "construccion", "descripcion",
        "medias_regresion", "parametros", "sesgo", "duplicacion",
        "aleatorizacion", "monte_carlo", "sintesis", "canvas", "colab"
    ]
```

- [ ] **Step 2: Ejecutar la prueba y confirmar que falla porque no existe el manifiesto**

Run: `python3 -m pytest tests/test_content_contract.py -q`

Expected: FAIL por ausencia de `content_manifest.csv`.

- [ ] **Step 3: Crear la carpeta, README y manifiesto con cada comando del capítulo en su orden exacto**

El manifiesto debe incluir por separado `use/describe/generate/list`, tabulaciones y resúmenes, `ttest`, `regress/lincom`, programa `estimadores`, parámetros directos, sesgo, duplicación, aleatorización y las dos simulaciones.

- [ ] **Step 4: Añadir contratos para Pausa 1 y la identidad algebraica**

```python
def test_canvas_activity_contract():
    text = (ROOT / "content_manifest.csv").read_text()
    for token in ["ATE", "ATT", "ATU", "NAIVE", "sesgo de selección", "1-pi"]:
        assert token in text
    assert "1+pi" not in text
```

- [ ] **Step 5: Ejecutar las pruebas**

Run: `python3 -m pytest tests/test_content_contract.py -q`

Expected: PASS.

---

### Task 2: Ejecutar Stata y construir outputs canónicos para diapositivas

**Files:**
- Modify: `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/libro_cortes/dofile/04_ParametrosStata/04_stata.do` solo si una salida necesaria no queda identificable; no cambiar el proceso generador.
- Create: `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/Slides/2026_02/Clase4_ParametrosCausales/stata/slide_outputs.do`
- Create: `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/Slides/2026_02/Clase4_ParametrosCausales/stata/slide_outputs.log`
- Create: `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/Slides/2026_02/Clase4_ParametrosCausales/data/parameters_results.csv`
- Create: `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/Slides/2026_02/Clase4_ParametrosCausales/data/monte_carlo_summary.csv`
- Create: `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/Slides/2026_02/Clase4_ParametrosCausales/tests/test_stata_outputs.py`

**Interfaces:**
- Consumes: `04_data.dta` y el do-file canónico.
- Produces: log con marcadores `SLIDE_OUTPUT_BEGIN/END` y CSV numéricos copiados desde una ejecución fresca.

- [ ] **Step 1: Escribir pruebas numéricas contra los valores canónicos del libro**

```python
import pandas as pd
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]

def value(df, scenario, estimand):
    return float(df.loc[(df.escenario == scenario) &
                        (df.estimando == estimand), "valor"].iloc[0])

def test_point_estimands_match_book():
    df = pd.read_csv(ROOT / "data/parameters_results.csv")
    assert abs(value(df, "datos_originales", "ATE") - 0.75) < 1e-12
    assert abs(value(df, "datos_originales", "NAIVE") - 6.75) < 1e-12
    assert abs(value(df, "datos_originales", "SESGO_ATT") - 6.0) < 1e-12
```

- [ ] **Step 2: Ejecutar la prueba y confirmar que falla por archivos ausentes**

Run: `python3 -m pytest tests/test_stata_outputs.py -q`

Expected: FAIL.

- [ ] **Step 3: Crear `slide_outputs.do` como wrapper que ejecuta el do-file canónico desde su carpeta**

El wrapper debe abrir un log de texto, ejecutar `04_stata.do`, volver a cargar `04_data.dta` y emitir bloques claramente delimitados para `describe`, `list`, `tabulate`, `summarize`, `ttest`, `regress` y `lincom`.

- [ ] **Step 4: Ejecutar Stata 19 una sola vez**

Run: `/Applications/StataNow/StataSE.app/Contents/MacOS/stata-se -b do slide_outputs.do`

Expected: código de salida 0, `Pipeline canónico completado`, sin `r(...)` no resuelto.

- [ ] **Step 5: Copiar los CSV y tres PNG generados a la carpeta de la presentación**

Los PNG serán `sesgo_con_seleccion.png`, `sesgo_con_aleatorizacion.png` y `comparacion_escenarios.png`.

- [ ] **Step 6: Ejecutar pruebas numéricas y comprobar presencia de marcadores del log**

Run: `python3 -m pytest tests/test_stata_outputs.py -q`

Expected: PASS.

---

### Task 3: Generar fragmentos limpios del output de Stata

**Files:**
- Create: `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/Slides/2026_02/Clase4_ParametrosCausales/R/build_stata_fragments.R`
- Create: `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/Slides/2026_02/Clase4_ParametrosCausales/generated/stata_fragments.Rds`
- Create: `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/Slides/2026_02/Clase4_ParametrosCausales/generated/stata_tables.Rds`
- Create: `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/Slides/2026_02/Clase4_ParametrosCausales/tests/test_fragments.R`

**Interfaces:**
- Consumes: `slide_outputs.log` y CSV canónicos.
- Produces: lista R nombrada con fragmentos `describe`, `list`, `tab_D`, `tab_X_D`, `summary`, `ttest`, `regress`, `lincom` y tablas de parámetros/escenarios/Monte Carlo.

- [ ] **Step 1: Escribir prueba R que exija cada fragmento y las estadísticas mínimas**

```r
x <- readRDS("generated/stata_fragments.Rds")
stopifnot(all(c("describe", "list", "tab_D", "tab_X_D", "summary",
                "ttest", "regress", "lincom") %in% names(x)))
stopifnot(grepl("Number of obs", x$regress, fixed = TRUE))
stopifnot(grepl("Robust", x$regress, fixed = TRUE))
```

- [ ] **Step 2: Ejecutar y confirmar el fallo inicial**

Run: `Rscript tests/test_fragments.R`

Expected: FAIL por ausencia de RDS.

- [ ] **Step 3: Implementar extracción por marcadores sin alterar los números**

El script debe eliminar prompts redundantes y paginación, pero conservar encabezados, N, medias, coeficientes, errores estándar, estadísticos, valores p e intervalos.

- [ ] **Step 4: Construir tablas desde CSV, nunca desde números escritos a mano**

- [ ] **Step 5: Ejecutar la prueba**

Run: `Rscript R/build_stata_fragments.R && Rscript tests/test_fragments.R`

Expected: PASS.

---

### Task 4: Reconstruir y validar el notebook de Google Colab

**Files:**
- Create: `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/Slides/2026_02/Clase4_ParametrosCausales/Parametros_Causales_Colab.ipynb`
- Create: `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/Slides/2026_02/Clase4_ParametrosCausales/python/build_notebook.py`
- Create: `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/Slides/2026_02/Clase4_ParametrosCausales/tests/test_notebook.py`

**Interfaces:**
- Consumes: URL pública de `04_data.dta` y CSV canónicos locales durante QA.
- Produces: notebook ejecutado con outputs persistidos y aserciones Stata–Python.

- [ ] **Step 1: Escribir pruebas de estructura y reproducibilidad**

```python
import nbformat
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]

def test_notebook_is_colab_portable():
    nb = nbformat.read(ROOT / "Parametros_Causales_Colab.ipynb", 4)
    source = "\n".join("".join(c.source) for c in nb.cells)
    assert "raw.githubusercontent.com/adiazescobar/libro_cortes/main" in source
    assert "/Users/" not in source
    assert all(c.execution_count is not None for c in nb.cells if c.cell_type == "code")
```

- [ ] **Step 2: Ejecutar y confirmar el fallo inicial**

Run: `python3 -m pytest tests/test_notebook.py -q`

Expected: FAIL.

- [ ] **Step 3: Construir el notebook en el orden del capítulo**

Las celdas deben cubrir carga, `X/y/tau`, descripción, diferencia de medias, OLS HC1, ATE/ATT/ATU/CATE, naïve/sesgo, duplicación, una aleatorización y Monte Carlo.

- [ ] **Step 4: Añadir comparación automática con Stata**

Usar tolerancia `1e-10` para estimandos deterministas y documentar que las secuencias pseudoaleatorias de Stata y NumPy no coinciden observación por observación; comparar propiedades y estimandos objetivo, no sorteos idénticos.

- [ ] **Step 5: Ejecutar completamente con nbconvert**

Run: `jupyter nbconvert --to notebook --execute Parametros_Causales_Colab.ipynb --output Parametros_Causales_Colab.executed.ipynb --ExecutePreprocessor.timeout=600`

Expected: 0 errores.

- [ ] **Step 6: Reemplazar el notebook final por la copia ejecutada y correr pruebas**

Run: `python3 -m pytest tests/test_notebook.py -q`

Expected: PASS.

---

### Task 5: Crear la identidad visual y el esqueleto Xaringan

**Files:**
- Create: `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/Slides/2026_02/Clase4_ParametrosCausales/Parametros_Causales_Clase_Empirica.Rmd`
- Create: `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/Slides/2026_02/Clase4_ParametrosCausales/identity.css`
- Create: `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/Slides/2026_02/Clase4_ParametrosCausales/tests/test_slides_contract.py`

**Interfaces:**
- Consumes: `Clase1_v3/identity.css`, manifiesto, RDS y notebook.
- Produces: Xaringan con helpers para código, output, pregunta, respuesta y tabla.

- [ ] **Step 1: Escribir contratos de título, secciones, conteo y prohibición de PPTX**

```python
from pathlib import Path
ROOT = Path(__file__).resolve().parents[1]

def test_slide_source_contract():
    text = (ROOT / "Parametros_Causales_Clase_Empirica.Rmd").read_text()
    assert "Parámetros causales" in text
    assert "Implementación en Google Colab" in text
    assert "Pausa 1" in text
    assert "(1-\\pi)(ATT-ATU)" in text
    assert not list(ROOT.glob("*.pptx"))
```

- [ ] **Step 2: Ejecutar y confirmar fallo inicial**

Run: `python3 -m pytest tests/test_slides_contract.py -q`

Expected: FAIL.

- [ ] **Step 3: Adaptar la identidad visual de `Clase1_v3`**

Conservar Fraunces/Inter/IBM Plex Mono, crema/navy/azul/terracota, 16:9 y pies discretos. Añadir clases específicas `.stata-command`, `.stata-output`, `.interpretation`, `.student-question`, `.answer-reveal` y tablas compactas legibles.

- [ ] **Step 4: Crear las 13 secciones y el conteo objetivo de 48–55 diapositivas**

- [ ] **Step 5: Ejecutar contratos**

Run: `python3 -m pytest tests/test_slides_contract.py -q`

Expected: PASS.

---

### Task 6: Completar la secuencia Stata y las interpretaciones

**Files:**
- Modify: `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/Slides/2026_02/Clase4_ParametrosCausales/Parametros_Causales_Clase_Empirica.Rmd`
- Create: `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/Slides/2026_02/Clase4_ParametrosCausales/notas_docentes.md`

**Interfaces:**
- Consumes: fragmentos y tablas verificadas.
- Produces: bloque principal completo de Stata, con notas docentes y preguntas reveladas después.

- [ ] **Step 1: Implementar datos, construcción y descripción**

Incluir outcome `y`, tratamiento `D`, referencia `D=0`, grupo `X`, resultados potenciales y límite de observabilidad real.

- [ ] **Step 2: Implementar `ttest`, regresión robusta y `lincom`**

Mostrar orientación del signo de `ttest`, constante, coeficiente, error estándar, IC y supuesto causal.

- [ ] **Step 3: Implementar programa `estimadores`, ATE/ATT/ATU/CATE y descomposición**

- [ ] **Step 4: Implementar duplicación y aleatorización**

Explicar por qué N nominal no añade información independiente y por qué una realización aleatoria no coincide exactamente con ATE.

- [ ] **Step 5: Implementar Monte Carlo con los tres gráficos y tabla canónica**

- [ ] **Step 6: Añadir notas docentes con tiempos acumulados**

La suma debe ser 105 minutos e incluir 10–12 minutos para la actividad Canvas.

- [ ] **Step 7: Correr los contratos de contenido**

Run: `python3 -m pytest tests/test_content_contract.py tests/test_slides_contract.py -q`

Expected: PASS.

---

### Task 7: Incorporar Pausa 1, tarea ATE y sección Colab

**Files:**
- Modify: `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/Slides/2026_02/Clase4_ParametrosCausales/Parametros_Causales_Clase_Empirica.Rmd`
- Modify: `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/Slides/2026_02/Clase4_ParametrosCausales/notas_docentes.md`

**Interfaces:**
- Consumes: tabla Canvas, respuestas verificadas y notebook ejecutado.
- Produces: taller sin respuestas iniciales, revelación posterior, derivación correcta y comparaciones Stata–Python.

- [ ] **Step 1: Crear diapositiva de Pausa 1 con la tabla original de ocho personas**

- [ ] **Step 2: Separar las cinco preguntas de las diapositivas de solución**

Los resultados serán `0.75, 0.75, 0.75, 6.75, 6.00` y cada solución mostrará las unidades incluidas en el promedio.

- [ ] **Step 3: Demostrar la identidad correcta**

Partir de `ATE = pi*ATT + (1-pi)*ATU` y llegar a `Naïve = ATE + sesgo de selección + (1-pi)(ATT-ATU)`.

- [ ] **Step 4: Crear la sección Colab con equivalencias principales**

Comparar Stata/Python para construcción, descriptivos, diferencia de medias, OLS robusta, parámetros y Monte Carlo. No repetir todas las celdas del notebook si no caben; mostrar las principales y enlazar el notebook.

- [ ] **Step 5: Añadir enlace directo de Colab basado en la futura ubicación final del notebook**

- [ ] **Step 6: Ejecutar todos los contratos**

Run: `python3 -m pytest tests -q`

Expected: PASS.

---

### Task 8: Renderizar HTML/PDF y hacer QA visual completo

**Files:**
- Create: `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/Slides/2026_02/Clase4_ParametrosCausales/Parametros_Causales_Clase_Empirica.html`
- Create: `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/Slides/2026_02/Clase4_ParametrosCausales/Parametros_Causales_Clase_Empirica.pdf`
- Create: `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/Slides/2026_02/Clase4_ParametrosCausales/qa_render/`
- Create: `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/Slides/2026_02/Clase4_ParametrosCausales/qa_report.txt`

**Interfaces:**
- Consumes: Rmd, CSS, datos, figuras y notebook finales.
- Produces: entregables renderizados y evidencia de revisión de cada diapositiva.

- [ ] **Step 1: Renderizar HTML en UTF-8**

Run: `LANG=es_MX.UTF-8 LC_ALL=es_MX.UTF-8 Rscript -e 'rmarkdown::render("Parametros_Causales_Clase_Empirica.Rmd")'`

Expected: HTML creado sin warnings de recursos faltantes.

- [ ] **Step 2: Imprimir PDF con Chrome**

Run: `Rscript -e 'pagedown::chrome_print("Parametros_Causales_Clase_Empirica.html", output="Parametros_Causales_Clase_Empirica.pdf", timeout=180)'`

Expected: PDF con el mismo número de páginas que diapositivas.

- [ ] **Step 3: Renderizar cada diapositiva a PNG**

Usar el flujo probado de `Clase1_v3` para generar `qa_render/slide-XX.png`.

- [ ] **Step 4: Revisar individualmente todas las diapositivas a tamaño completo**

Registrar en `qa_report.txt`: número, título, código legible, output legible, desbordamiento, figuras y corrección aplicada.

- [ ] **Step 5: Corregir todos los defectos y volver a renderizar**

No aceptar bloques cortados, títulos en dos líneas accidentales, fuentes pequeñas, tablas fuera del lienzo ni outputs ilegibles.

- [ ] **Step 6: Verificar rutas y conteos**

Run: `python3 -m pytest tests -q`

Run: `pdfinfo Parametros_Causales_Clase_Empirica.pdf`

Expected: todas las pruebas pasan; 48–55 páginas; ningún recurso roto.

---

### Task 9: Auditoría final y entrega local

**Files:**
- Modify: `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/Slides/2026_02/Clase4_ParametrosCausales/README.md`
- Modify: `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/Slides/2026_02/Clase4_ParametrosCausales/qa_report.txt`

**Interfaces:**
- Consumes: todos los entregables.
- Produces: reporte final reproducible y rutas de entrega.

- [ ] **Step 1: Ejecutar una verificación fresca de Stata, notebook, pruebas y render**

No volver a ejecutar Stata si el do-file y los datos no cambiaron desde Task 2; verificar hashes y reutilizar la ejecución certificada.

- [ ] **Step 2: Comparar una última vez los resultados Stata, libro y Python**

Documentar separadamente diferencias por convención de t-test, HC1 y generadores pseudoaleatorios.

- [ ] **Step 3: Confirmar que no se modificó RDD ni otras presentaciones**

Run: `find ../ -type f -newer README.md` y contrastar únicamente contra la carpeta nueva; usar Git cuando corresponda.

- [ ] **Step 4: Completar README con comandos de reproducción**

- [ ] **Step 5: Entregar sin publicar**

Informar ruta de `.Rmd`, `.html`, `.pdf`, `.ipynb`, número de diapositivas, estado de Stata, coincidencia con el libro, ejecución de Colab y diferencias Stata–Python. Esperar aprobación explícita antes de subir a GitHub.
