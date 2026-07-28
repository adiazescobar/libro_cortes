# Lecturas centrales en todos los capítulos — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Añadir al comienzo de cada página docente enlaces directos al PDF pertinente de Bernal y Peña y al capítulo temático de *Causal Inference: The Mixtape*.

**Architecture:** Los bloques se incorporan directamente en cada Rmd para que cada página sea autocontenida. Un contrato parametrizado conserva el mapa archivo–capítulo, verifica ubicación, etiquetas y destinos locales; el render final comprueba que Bookdown copie los PDF y que los enlaces web sigan respondiendo.

**Tech Stack:** Bookdown/R Markdown, Markdown/Pandoc, pytest, Python 3, PDF locales, HTTP HEAD/GET para auditoría de enlaces.

## Global Constraints

- Aplicar la regla a todas las páginas docentes de `_bookdown.yml`, excepto `index.Rmd`.
- En clases empíricas, materiales y descargas permanecen antes de `Lecturas centrales`.
- Usar PDF separados por capítulo para Bernal y Peña; no sustituir el capítulo 6 por el libro completo.
- Usar exclusivamente URLs canónicas `https://mixtape.scunning.com/NN-slug` para Cunningham.
- Copiar PDF locales sin modificar su contenido y con nombres ASCII en `lecturas/bernal-pena/`.
- No actualizar `docs/` sin aprobación expresa.

---

### Task 1: Contrato de cobertura y mapa de referencias

**Files:**
- Create: `tests/test_central_readings_contract.py`
- Read: `_bookdown.yml`
- Read: `docs/superpowers/specs/2026-07-28-lecturas-centrales-design.md`

**Interfaces:**
- Consumes: lista de Rmd de `_bookdown.yml` y mapa aprobado en la especificación.
- Produces: constantes `EXPECTED_READINGS`, `BERNAL_LOCAL` y pruebas reutilizables por las tareas siguientes.

- [ ] **Step 1: Escribir la prueba fallida de cobertura**

```python
EXPECTED_READINGS = {
    "00-PruebaEntrada.Rmd": ([2, 3], ["04-potential_outcomes"]),
    "01-intro.Rmd": ([2, 3], ["01-introduction", "04-potential_outcomes"]),
    "02-StataBasics.Rmd": ([2], ["02-probability_and_regression"]),
    "03-Parametros.Rmd": ([2, 3], ["04-potential_outcomes"]),
    "04-ParametrosStata.Rmd": ([2, 3], ["04-potential_outcomes"]),
    "05-RCT.Rmd": ([4], ["04-potential_outcomes"]),
    "06-RCT2.Rmd": ([4], ["04-potential_outcomes"]),
    "07-POWER-Teoria.Rmd": ([4], ["04-potential_outcomes"]),
    "07-POWER.Rmd": ([4], ["04-potential_outcomes"]),
    "08-DID.Rmd": ([5], ["09-difference_in_differences"]),
    "08-DIDStata.Rmd": ([5], ["09-difference_in_differences"]),
    "09-BadControls.Rmd": ([3], ["03-directed_acyclical_graphs"]),
    "10-BadControlsStata.Rmd": ([3], ["03-directed_acyclical_graphs"]),
    "11-TWFE.Rmd": ([5], ["08-panel_data", "09-difference_in_differences"]),
    "11-TWFEStata.Rmd": ([5], ["08-panel_data", "09-difference_in_differences"]),
    "12-ExactMatching.Rmd": ([6], ["05-matching_and_subclassification"]),
    "13-PSM.Rmd": ([6], ["05-matching_and_subclassification"]),
    "14-PSMStata.Rmd": ([6], ["05-matching_and_subclassification"]),
    "15-IPW.Rmd": ([6], ["05-matching_and_subclassification"]),
    "16-PSM_IPW_SinteticosConsolidado.Rmd": ([6], ["05-matching_and_subclassification", "10-synthetic_control"]),
    "18-IV.Rmd": ([7], ["07-instrumental_variables"]),
    "19-IVStata.Rmd": ([7], ["07-instrumental_variables"]),
    "20-RDD.Rmd": ([8], ["06-regression_discontinuity"]),
    "21-RDDStata.Rmd": ([8], ["06-regression_discontinuity"]),
}

def test_every_teaching_page_has_a_reading_mapping():
    files = parse_rmd_files(ROOT / "_bookdown.yml")
    assert set(files) - {"index.Rmd"} == set(EXPECTED_READINGS)
```

- [ ] **Step 2: Ejecutar la prueba y confirmar que falla por ausencia de bloques**

Run: `python3 -m pytest -q tests/test_central_readings_contract.py`

Expected: FAIL para las páginas que todavía no contienen `Lecturas centrales`.

- [ ] **Step 3: Añadir pruebas de contenido, posición y destinos**

```python
def test_each_page_has_both_reference_families():
    for filename, (chapters, mixtape_slugs) in EXPECTED_READINGS.items():
        text = (ROOT / filename).read_text(encoding="utf-8")
        assert "**Lecturas centrales**" in text
        for chapter in chapters:
            assert f"Bernal y Peña — capítulo {chapter}" in text
        for slug in mixtape_slugs:
            assert f"https://mixtape.scunning.com/{slug}" in text

def test_local_pdf_targets_exist():
    for path in BERNAL_LOCAL.values():
        assert (ROOT / path).is_file()
```

- [ ] **Step 4: Ejecutar y conservar el fallo rojo**

Run: `python3 -m pytest -q tests/test_central_readings_contract.py`

Expected: FAIL por bloques y PDF aún ausentes.

- [ ] **Step 5: Commit del contrato rojo**

```bash
git add tests/test_central_readings_contract.py
git commit -m "test: define central readings contract"
```

### Task 2: Reunir y normalizar los PDF de Bernal y Peña

**Files:**
- Create: `lecturas/bernal-pena/capitulo-05.pdf`
- Create: `lecturas/bernal-pena/capitulo-06.pdf`
- Create: `lecturas/bernal-pena/capitulo-07.pdf`
- Create: `lecturas/bernal-pena/capitulo-08.pdf`
- Test: `tests/test_central_readings_contract.py`

**Interfaces:**
- Consumes: copias del archivo histórico del curso y enlaces Dropbox existentes para capítulos 2–4.
- Produces: destinos estables que los bloques y el render pueden resolver.

- [ ] **Step 1: Copiar capítulos disponibles con metadatos intactos**

Run:

```bash
mkdir -p lecturas/bernal-pena
cp -X "/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/canvas_extracted/web_resources/Multimedia cargada/Capitulo 5 Bernal y PeÃ±a.pdf" lecturas/bernal-pena/capitulo-05.pdf
cp -X "/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/canvas_extracted/web_resources/Multimedia cargada/Capitulo 7 Bernal y Peña.pdf" lecturas/bernal-pena/capitulo-07.pdf
cp -X "/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/canvas_extracted/web_resources/Multimedia cargada/Capitulo 8 Bernal Peña.pdf" lecturas/bernal-pena/capitulo-08.pdf
```

- [ ] **Step 2: Localizar el capítulo 6 en archivos autorizados del curso**

Run:

```bash
find /Users/adiazescobar/Dropbox/ClasesR/EconometriaAV -type f -iname '*.pdf' -print | rg -i 'cap[ií]tulo.?6|capitulo.?6|matching|emparejamiento'
```

Expected: una copia separada del capítulo 6. Copiarla con `cp -X <ruta-encontrada> lecturas/bernal-pena/capitulo-06.pdf`.

If absent: detener solamente las ediciones de Exact Matching/PSM/IPW, informar a Ana María la ruta esperada y solicitar la copia. Continuar con los demás temas no debe crear enlaces falsos.

- [ ] **Step 3: Validar que cada archivo sea un PDF legible y corresponda al capítulo**

Run:

```bash
pdfinfo lecturas/bernal-pena/capitulo-05.pdf
pdfinfo lecturas/bernal-pena/capitulo-06.pdf
pdfinfo lecturas/bernal-pena/capitulo-07.pdf
pdfinfo lecturas/bernal-pena/capitulo-08.pdf
pdftotext lecturas/bernal-pena/capitulo-06.pdf - | head -80
```

Expected: `PDF version` válido y texto que identifique el capítulo 6 y el método de emparejamiento.

- [ ] **Step 4: Ejecutar la prueba de destinos locales**

Run: `python3 -m pytest -q tests/test_central_readings_contract.py -k local_pdf`

Expected: PASS.

- [ ] **Step 5: Commit de recursos**

```bash
git add lecturas/bernal-pena tests/test_central_readings_contract.py
git commit -m "docs: add Bernal Pena chapter readings"
```

### Task 3: Añadir bloques a fundamentos, parámetros, RCT y poder

**Files:**
- Modify: `00-PruebaEntrada.Rmd`
- Modify: `01-intro.Rmd`
- Modify: `02-StataBasics.Rmd`
- Modify: `03-Parametros.Rmd`
- Modify: `04-ParametrosStata.Rmd`
- Modify: `05-RCT.Rmd`
- Modify: `06-RCT2.Rmd`
- Modify: `07-POWER-Teoria.Rmd`
- Modify: `07-POWER.Rmd`
- Test: `tests/test_central_readings_contract.py`

**Interfaces:**
- Consumes: enlaces Dropbox de capítulos 2–4 y URLs Mixtape del mapa.
- Produces: nueve páginas con caja uniforme y orden correcto.

- [ ] **Step 1: Insertar el bloque canónico en cada página**

Ejemplo para RCT:

```markdown
::: {.boxinfo}
**Lecturas centrales**

- [Bernal y Peña — capítulo 4 (PDF)](https://www.dropbox.com/s/vxpgxt22pvphwx3/Capitulo%204%20Bernal%20y%20Pe%C3%B1a.pdf?dl=1)
- [Cunningham — capítulo 4: Potential Outcomes](https://mixtape.scunning.com/04-potential_outcomes)
:::
```

Para capítulos 2 y 3 usar respectivamente:

```text
https://www.dropbox.com/s/zsqa2gcbbgdi5i3/Capitulo%202%20Bernal%20y%20Pe%C3%B1a.pdf?dl=1
https://www.dropbox.com/s/837u3ea36r7t5me/Capitulo%203%20Bernal%20y%20Pe%C3%B1a.pdf?dl=1
```

- [ ] **Step 2: Verificar ubicación en prácticas**

Run: `python3 -m pytest -q tests/test_central_readings_contract.py -k 'position or fundamentals'`

Expected: PASS; materiales permanecen antes de lecturas en páginas empíricas.

- [ ] **Step 3: Commit del primer bloque temático**

```bash
git add 00-PruebaEntrada.Rmd 01-intro.Rmd 02-StataBasics.Rmd 03-Parametros.Rmd 04-ParametrosStata.Rmd 05-RCT.Rmd 06-RCT2.Rmd 07-POWER-Teoria.Rmd 07-POWER.Rmd
git commit -m "docs: add central readings to foundational chapters"
```

### Task 4: Añadir bloques a DID, malos controles y TWFE

**Files:**
- Modify: `08-DID.Rmd`
- Modify: `08-DIDStata.Rmd`
- Modify: `09-BadControls.Rmd`
- Modify: `10-BadControlsStata.Rmd`
- Modify: `11-TWFE.Rmd`
- Modify: `11-TWFEStata.Rmd`
- Test: `tests/test_central_readings_contract.py`

**Interfaces:**
- Consumes: `lecturas/bernal-pena/capitulo-05.pdf`, capítulo 3 Dropbox y Mixtape 3/8/9.
- Produces: seis páginas con referencias de panel, DID y DAG correctamente diferenciadas.

- [ ] **Step 1: Insertar cajas según el mapa aprobado**

Bloque DID:

```markdown
::: {.boxinfo}
**Lecturas centrales**

- [Bernal y Peña — capítulo 5 (PDF)](lecturas/bernal-pena/capitulo-05.pdf)
- [Cunningham — capítulo 9: Difference-in-Differences](https://mixtape.scunning.com/09-difference_in_differences)
:::
```

TWFE añade también `https://mixtape.scunning.com/08-panel_data`; malos controles usa capítulo 3 de ambos libros.

- [ ] **Step 2: Ejecutar contrato para los seis archivos**

Run: `python3 -m pytest -q tests/test_central_readings_contract.py -k 'did or bad_controls or twfe'`

Expected: PASS.

- [ ] **Step 3: Commit del bloque**

```bash
git add 08-DID.Rmd 08-DIDStata.Rmd 09-BadControls.Rmd 10-BadControlsStata.Rmd 11-TWFE.Rmd 11-TWFEStata.Rmd
git commit -m "docs: add central readings to DID and panel chapters"
```

### Task 5: Añadir bloques a matching, PSM, IPW y sintéticos

**Files:**
- Modify: `12-ExactMatching.Rmd`
- Modify: `13-PSM.Rmd`
- Modify: `14-PSMStata.Rmd`
- Modify: `15-IPW.Rmd`
- Modify: `16-PSM_IPW_SinteticosConsolidado.Rmd`
- Test: `tests/test_central_readings_contract.py`

**Interfaces:**
- Consumes: `lecturas/bernal-pena/capitulo-06.pdf` validado y Mixtape 5/10.
- Produces: cinco páginas con referencia común a matching y referencia adicional a control sintético donde corresponde.

- [ ] **Step 1: Insertar el bloque común de matching**

```markdown
::: {.boxinfo}
**Lecturas centrales**

- [Bernal y Peña — capítulo 6 (PDF)](lecturas/bernal-pena/capitulo-06.pdf)
- [Cunningham — capítulo 5: Matching and Subclassification](https://mixtape.scunning.com/05-matching_and_subclassification)
:::
```

- [ ] **Step 2: Añadir Mixtape 10 al capítulo consolidado**

```markdown
- [Cunningham — capítulo 10: Synthetic Control](https://mixtape.scunning.com/10-synthetic_control)
```

- [ ] **Step 3: Ejecutar contrato de matching**

Run: `python3 -m pytest -q tests/test_central_readings_contract.py -k 'matching or psm or ipw or synthetic'`

Expected: PASS.

- [ ] **Step 4: Commit del bloque**

```bash
git add 12-ExactMatching.Rmd 13-PSM.Rmd 14-PSMStata.Rmd 15-IPW.Rmd 16-PSM_IPW_SinteticosConsolidado.Rmd
git commit -m "docs: add central readings to matching chapters"
```

### Task 6: Añadir bloques a IV y RDD

**Files:**
- Modify: `18-IV.Rmd`
- Modify: `19-IVStata.Rmd`
- Modify: `20-RDD.Rmd`
- Modify: `21-RDDStata.Rmd`
- Test: `tests/test_central_readings_contract.py`

**Interfaces:**
- Consumes: capítulos locales 7/8 y Mixtape 7/6.
- Produces: cuatro páginas con lecturas específicas de IV y RDD.

- [ ] **Step 1: Insertar bloques según el mapa**

IV usa `lecturas/bernal-pena/capitulo-07.pdf` y `https://mixtape.scunning.com/07-instrumental_variables`; RDD usa `lecturas/bernal-pena/capitulo-08.pdf` y `https://mixtape.scunning.com/06-regression_discontinuity`.

- [ ] **Step 2: Ejecutar contrato IV/RDD**

Run: `python3 -m pytest -q tests/test_central_readings_contract.py -k 'instrumental or rdd'`

Expected: PASS.

- [ ] **Step 3: Commit del bloque**

```bash
git add 18-IV.Rmd 19-IVStata.Rmd 20-RDD.Rmd 21-RDDStata.Rmd
git commit -m "docs: add central readings to IV and RDD chapters"
```

### Task 7: Auditoría completa y vista previa

**Files:**
- Modify if needed: `tests/test_central_readings_contract.py`
- Create outside repo: `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/libro_cortes_readings_review_20260728/`

**Interfaces:**
- Consumes: todas las páginas y recursos de Tasks 1–6.
- Produces: suite verde, enlaces válidos y vista previa completa sin publicar.

- [ ] **Step 1: Ejecutar contratos y suite completa**

Run:

```bash
python3 -m pytest -q tests/test_central_readings_contract.py
python3 -m pytest -q
git diff --check
```

Expected: todas las pruebas PASS y `git diff --check` sin salida.

- [ ] **Step 2: Verificar enlaces web**

Run:

```bash
python3 - <<'PY'
import requests
from tests.test_central_readings_contract import MIXTAPE_URLS, BERNAL_REMOTE
for url in sorted(MIXTAPE_URLS | BERNAL_REMOTE):
    response = requests.get(url, timeout=30, allow_redirects=True)
    assert response.status_code == 200, (response.status_code, url)
    print(response.status_code, response.url)
PY
```

Expected: HTTP 200 para todas las URLs finales.

- [ ] **Step 3: Renderizar sin tocar `docs/`**

Run:

```bash
Rscript -e 'bookdown::render_book("index.Rmd", output_dir="/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/libro_cortes_readings_review_20260728")'
```

Expected: exit 0.

- [ ] **Step 4: Auditar HTML y recursos**

Run:

```bash
python3 - <<'PY'
from pathlib import Path
from bs4 import BeautifulSoup

root = Path('/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/libro_cortes_readings_review_20260728')
pages = list(root.glob('*.html'))
assert pages
for page in pages:
    soup = BeautifulSoup(page.read_text(encoding='utf-8'), 'html.parser')
    for link in soup.select('a[href*="lecturas/bernal-pena/"]'):
        target = root / link['href'].split('#', 1)[0]
        assert target.is_file(), (page.name, link['href'])
for chapter in (5, 6, 7, 8):
    assert (root / f'lecturas/bernal-pena/capitulo-{chapter:02d}.pdf').is_file()
print('HTML y PDF locales: PASS')
PY
```

Expected: cero páginas sin bloque y cero destinos rotos.

- [ ] **Step 5: Commit de ajustes de verificación**

```bash
git add tests/test_central_readings_contract.py
git commit -m "test: verify central readings across book"
```

- [ ] **Step 6: Entregar enlaces de revisión**

Reportar la carpeta completa y enlaces directos a una página teórica y una empírica de cada familia. Declarar explícitamente que `docs/` no cambió.
