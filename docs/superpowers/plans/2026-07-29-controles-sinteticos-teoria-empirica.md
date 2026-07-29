# Controles sintéticos — Theory and Empirical Classes Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Incorporar antes de IV una clase teórica rigurosa y una clase empírica reproducible de controles sintéticos, usando Prop 99, `synth`, diagnósticos placebo y resultados visibles de Stata.

**Architecture:** El do-file de Stata será la única fuente de resultados canónicos y exportará tablas y gráficas a un directorio exclusivo. Los dos R Markdown consumirán esos artefactos: la teoría explicará identificación, soporte e inferencia; la práctica guiará la aplicación completa y validará automáticamente los archivos antes de mostrarlos. Un contrato pytest cubrirá estructura, contenido, concordancia numérica, navegación y privacidad.

**Tech Stack:** Stata 19 (`synth`; `synth_runner` solo opcional), R Markdown/Bookdown, CSV, PNG, Python 3/pytest, Git.

## Global Constraints

- Trabajar directamente en `main`, por preferencia explícita de Ana María.
- Crear `17-SyntheticControls.Rmd` y `17-SyntheticControlsStata.Rmd` antes de `18-IV.Rmd` en `_bookdown.yml`.
- Usar exactamente los títulos `Controles sintéticos — Clase teórica` y `Controles sintéticos — Clase empírica`.
- Usar los anchors `controles-sinteticos` y `controles-sinteticos-stata`; no cambiar nombres, anchors ni URL de IV o RDD.
- Usar `Y(D=1)` y `Y(D=0)` en ambos capítulos.
- La clase empírica comienza con materiales, lecturas centrales y metas, en ese orden.
- Mostrar tablas, resultados y gráficas de Stata dentro de la página empírica.
- Incluir tres preguntas tipo examen en teoría y cuatro en práctica, sin respuestas públicas ni desplegables.
- Mantener la clave únicamente en `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/claves_privadas/17_SyntheticControls_clave.md` y fuera del historial de Git.
- Definir 1989 como primer periodo tratado; la Proposición 99 fue aprobada en noviembre de 1988.
- Mostrar todos los placebos y usar, para la comparación restringida, `RMSPE_pre_placebo <= 5 * RMSPE_pre_California`.
- Usar 1980 como placebo temporal sin utilizar información posterior a la fecha ficticia para ajustar ese modelo.
- No presentar el promedio simple de controles como control sintético ni llamar p-valor convencional a la proporción placebo.
- No eliminar los materiales heredados en `dofile/16_PSM_IPW_Sinteticos/` durante este ciclo.
- No actualizar, publicar ni versionar productos nuevos dentro de `docs/`; el preview debe quedar fuera del repositorio.

---

### Task 1: Crear el contrato ejecutable de los dos capítulos

**Files:**
- Create: `tests/test_synthetic_controls_contract.py`
- Modify: `tests/test_chapter_title_contract.py`
- Modify: `tests/test_central_readings_contract.py`

**Interfaces:**
- Consumes: `docs/superpowers/specs/2026-07-29-controles-sinteticos-teoria-empirica-design.md`.
- Produces: constantes `THEORY`, `PRACTICE`, `DOFILE`, `RESULTS`, `PRIVATE_KEY` y requisitos automatizados usados como puerta de aceptación por todas las tareas posteriores.

- [ ] **Step 1: Escribir el contrato inicialmente fallido para archivos, navegación, títulos y orden de la práctica.**

```python
from pathlib import Path
import csv
import math

import test_power_pedagogy_contract as base

ROOT = base.ROOT
THEORY = ROOT / "17-SyntheticControls.Rmd"
PRACTICE = ROOT / "17-SyntheticControlsStata.Rmd"
DOFILE = ROOT / "dofile/17_SyntheticControls/01_synthetic_controls.do"
RESULTS = ROOT / "dofile/17_SyntheticControls/results"
PRIVATE_KEY = Path.home() / "Dropbox/ClasesR/EconometriaAV/claves_privadas/17_SyntheticControls_clave.md"

def read(path):
    return path.read_text(encoding="utf-8")

def rows(name):
    with (RESULTS / name).open(encoding="utf-8", newline="") as handle:
        return list(csv.DictReader(handle))

def test_pair_is_inserted_before_iv_with_stable_anchors():
    book = base.parse_rmd_files(ROOT / "_bookdown.yml")
    assert book.index("17-SyntheticControls.Rmd") < book.index("17-SyntheticControlsStata.Rmd") < book.index("18-IV.Rmd")
    assert read(THEORY).startswith("# Controles sintéticos — Clase teórica {#controles-sinteticos}")
    assert read(PRACTICE).startswith("# Controles sintéticos — Clase empírica {#controles-sinteticos-stata}")

def test_practice_starts_with_materials_readings_and_goals():
    text = read(PRACTICE)
    assert text.index("## Materiales para la clase") < text.index("**Lecturas centrales**") < text.index("**Metas de aprendizaje**") < 5000
```

- [ ] **Step 2: Añadir pruebas académicas, de artefactos y de privacidad con requisitos concretos.**

```python
def test_theory_covers_identification_support_and_inference():
    text = read(THEORY).lower()
    for marker in ["y(d=1)", "y(d=0)", "envolvente convexa", "no anticipación", "interferencia", "rmspe", "placebo", "leave-one-out"]:
        assert marker in text, marker
    assert read(THEORY).count("::: {.boxexam}") == 3

def test_practice_uses_real_synth_and_complete_diagnostics():
    do = read(DOFILE)
    page = read(PRACTICE)
    for marker in ["synth cigsale", "trunit(3)", "trperiod(1989)", "synth_weights.csv", "synth_predictor_balance.csv", "synth_rmspe.csv", "synth_placebos.csv", "synth_leave_one_out.csv"]:
        assert marker in do, marker
        assert marker in page, marker
    assert "promedio simple" in page.lower()
    assert read(PRACTICE).count("::: {.boxexam}") == 4

def test_private_key_stays_outside_repository():
    assert PRIVATE_KEY.is_file()
    assert ROOT not in PRIVATE_KEY.parents
    assert not (ROOT / "claves_privadas/17_SyntheticControls_clave.md").exists()
    public = read(THEORY) + read(PRACTICE)
    for label in ["SC-T1", "SC-T2", "SC-T3", "SC-S1", "SC-S2", "SC-S3", "SC-S4"]:
        assert label in read(PRIVATE_KEY)
    assert "Uso exclusivo de la profesora y el monitor" not in public
```

- [ ] **Step 3: Añadir pruebas numéricas para pesos convexos, reconstrucción y regla placebo.**

```python
def test_weights_are_convex_and_reconstruction_matches_synth():
    weights = rows("synth_weights.csv")
    positive = [float(r["weight"]) for r in weights if float(r["weight"]) > 1e-8]
    assert positive
    assert all(w >= 0 for w in positive)
    assert abs(sum(float(r["weight"]) for r in weights) - 1) < 1e-6
    paths = rows("synth_paths.csv")
    assert max(abs(float(r["synthetic"]) - float(r["manual_synthetic"])) for r in paths) < 1e-8

def test_rmspe_and_placebo_filter_are_reproducible():
    rmspe = rows("synth_rmspe.csv")
    ca = next(r for r in rmspe if r["unit"] == "California")
    assert float(ca["pre_rmspe"]) > 0
    assert math.isclose(float(ca["ratio"]), float(ca["post_rmspe"]) / float(ca["pre_rmspe"]), rel_tol=1e-9)
    placebos = rows("synth_placebos.csv")
    cutoff = 5 * float(ca["pre_rmspe"])
    assert all((r["eligible"] == "1") == (float(r["pre_rmspe"]) <= cutoff) for r in placebos)
```

- [ ] **Step 4: Extender los contratos globales de títulos y lecturas.** Añadir a `EXPECTED` de `test_chapter_title_contract.py` ambos archivos y anchors. Añadir a `EXPECTED_READINGS` de `test_central_readings_contract.py`:

```python
"17-SyntheticControls.Rmd": ([6], ["10-synthetic_control"]),
"17-SyntheticControlsStata.Rmd": ([6], ["10-synthetic_control"]),
```

y agregar `"17-SyntheticControlsStata.Rmd"` al conjunto explícito de páginas empíricas.

- [ ] **Step 5: Ejecutar el contrato para confirmar que falla por los archivos aún inexistentes.**

Run: `python3 -m pytest -q tests/test_synthetic_controls_contract.py tests/test_chapter_title_contract.py tests/test_central_readings_contract.py`

Expected: FAIL en los dos Rmd, el do-file, resultados y clave todavía inexistentes; las páginas actuales deben conservar sus resultados previos.

- [ ] **Step 6: Commit del contrato rojo.**

```bash
git add tests/test_synthetic_controls_contract.py tests/test_chapter_title_contract.py tests/test_central_readings_contract.py
git commit -m "test: define synthetic controls chapter contract"
```

### Task 2: Construir la estimación canónica y sus artefactos básicos

**Files:**
- Create: `dofile/17_SyntheticControls/01_synthetic_controls.do`
- Copy: `dofile/16_PSM_IPW_Sinteticos/synth_smoking.dta` to `dofile/17_SyntheticControls/synth_smoking.dta`
- Create: `dofile/17_SyntheticControls/results/synth_weights.csv`
- Create: `dofile/17_SyntheticControls/results/synth_predictor_balance.csv`
- Create: `dofile/17_SyntheticControls/results/synth_paths.csv`
- Create: `dofile/17_SyntheticControls/results/synth_rmspe.csv`
- Create: `dofile/17_SyntheticControls/synth_raw_series.png`
- Create: `dofile/17_SyntheticControls/synth_actual_vs_synthetic.png`
- Create: `dofile/17_SyntheticControls/synth_gap.png`

**Interfaces:**
- Consumes: long panel `state year cigsale lnincome beer age15to24 retprice` for 39 states, 1970–2000.
- Produces: CSV schemas `state_id,state,weight`; `predictor,treated,synthetic`; `year,treated,synthetic,manual_synthetic,gap,post`; `unit,pre_rmspe,post_rmspe,ratio`.

- [ ] **Step 1: Copiar la base sin borrar ni renombrar el original.**

Run: `mkdir -p dofile/17_SyntheticControls/results`

Run: `cp dofile/16_PSM_IPW_Sinteticos/synth_smoking.dta dofile/17_SyntheticControls/synth_smoking.dta`

Expected: ambos archivos existen y `shasum` informa el mismo hash.

- [ ] **Step 2: Escribir el encabezado defensivo del do-file y auditar la muestra.**

```stata
version 19.0
clear all
set more off
set seed 1298

capture confirm file "synth_smoking.dta"
if _rc {
    capture confirm file "dofile/17_SyntheticControls/synth_smoking.dta"
    if !_rc cd "dofile/17_SyntheticControls"
}
confirm file "synth_smoking.dta"
capture which synth
if _rc {
    di as error "Falta synth. Instálelo una vez con: ssc install synth"
    exit 499
}
use synth_smoking.dta, clear
isid state year
assert inrange(year, 1970, 2000)
encode state, gen(state_id)
assert state_id == 3 if state == "California"
xtset state_id year
```

- [ ] **Step 3: Estimar California sintética con tratamiento desde 1989 y conservar la salida nativa.**

```stata
synth cigsale lnincome beer(1984(1)1988) age15to24 retprice ///
    cigsale(1975) cigsale(1980) cigsale(1988), ///
    trunit(3) trperiod(1989) xperiod(1980(1)1988) nested ///
    keep(results/california_synth_native.dta) replace
```

- [ ] **Step 4: Exportar pesos y balance desde las matrices devueltas por `synth`.** Extraer `e(W_weights)` y `e(X_balance)`, combinar los identificadores con las etiquetas de estado y exportar exactamente:

```text
state_id,state,weight
```

y

```text
predictor,treated,synthetic
```

Añadir aserciones Stata `assert weight >= -1e-10` y `assert abs(sum_weight-1)<0.002` antes de exportar; la segunda tolerancia admite únicamente el redondeo a tres decimales de `e(W_weights)` en la versión SSC de `synth`.

- [ ] **Step 5: Reconstruir la trayectoria con los pesos y verificarla contra la salida nativa.** Combinar los pesos con el panel donante, calcular `manual_synthetic=sum(weight*cigsale)` por año, unir con California y con `_Y_synthetic` de `california_synth_native.dta`, y exigir:

```stata
gen double reconstruction_error = abs(synthetic-manual_synthetic)
summ reconstruction_error, meanonly
assert r(max) < 1e-8
gen double gap = treated-synthetic
gen byte post = year>=1989
export delimited year treated synthetic manual_synthetic gap post using results/synth_paths.csv, replace
```

- [ ] **Step 6: Calcular RMSPE pre, post y razón para California.**

```stata
gen double gap_sq = gap^2
summ gap_sq if year<1989, meanonly
scalar pre_rmspe = sqrt(r(mean))
summ gap_sq if year>=1989, meanonly
scalar post_rmspe = sqrt(r(mean))
scalar rmspe_ratio = post_rmspe/pre_rmspe
```

Exportar una primera fila `California,pre_rmspe,post_rmspe,ratio` a `results/synth_rmspe.csv`.

- [ ] **Step 7: Generar las tres gráficas canónicas con línea vertical en 1989.** Producir serie bruta, California frente a sintético y brecha; usar títulos en español, leyendas legibles, eje de paquetes per cápita y exportar los nombres definidos en Files.

- [ ] **Step 8: Ejecutar Stata y validar el resultado básico.**

Run: `/Applications/StataNow/StataSE.app/Contents/MacOS/stata-se -b do 01_synthetic_controls.do`

Working directory: `dofile/17_SyntheticControls/`

Expected: exit 0, ningún `r(...)` final, tres CSV y tres PNG; el máximo error de reconstrucción es menor que `1e-8`.

- [ ] **Step 9: Ejecutar pruebas focalizadas y commit.**

Run: `python3 -m pytest -q tests/test_synthetic_controls_contract.py -k "weights or real_synth"`

```bash
git add dofile/17_SyntheticControls tests/test_synthetic_controls_contract.py
git commit -m "feat: estimate canonical Prop 99 synthetic control"
```

### Task 3: Añadir placebos e inferencia de sensibilidad

**Files:**
- Modify: `dofile/17_SyntheticControls/01_synthetic_controls.do`
- Create: `dofile/17_SyntheticControls/results/synth_placebos.csv`
- Create: `dofile/17_SyntheticControls/results/synth_time_placebo.csv`
- Create: `dofile/17_SyntheticControls/results/synth_leave_one_out.csv`
- Create: `dofile/17_SyntheticControls/synth_placebo_gaps.png`
- Create: `dofile/17_SyntheticControls/synth_rmspe_ratios.png`
- Create: `dofile/17_SyntheticControls/synth_time_placebo.png`
- Create: `dofile/17_SyntheticControls/synth_leave_one_out.png`

**Interfaces:**
- Consumes: misma especificación principal, `pre_rmspe` de California y lista de donantes elegibles.
- Produces: `synth_placebos.csv` con `unit_id,unit,pre_rmspe,post_rmspe,ratio,eligible`; `synth_time_placebo.csv` con `year,gap`; `synth_leave_one_out.csv` con `omitted_state,year,gap`.

- [ ] **Step 1: Escribir una prueba fallida que exija cobertura completa de placebos y leave-one-out.**

```python
def test_placebos_cover_donors_and_leave_one_out_covers_positive_weights():
    placebos = rows("synth_placebos.csv")
    assert len({r["unit"] for r in placebos}) == 39
    assert sum(r["unit"] == "California" for r in placebos) == 1
    positive = {r["state"] for r in rows("synth_weights.csv") if float(r["weight"]) > 1e-8}
    loo = {r["omitted_state"] for r in rows("synth_leave_one_out.csv")}
    assert loo == positive
```

- [ ] **Step 2: Ejecutar la prueba y confirmar falla por CSV inexistentes.**

Run: `python3 -m pytest -q tests/test_synthetic_controls_contract.py -k "placebo or leave_one_out"`

Expected: FAIL por `synth_placebos.csv` y `synth_leave_one_out.csv`.

- [ ] **Step 3: Programar placebos espaciales con la misma especificación.** Iterar cada `state_id` como unidad tratada, excluirla del donor pool de su propia corrida, mantener `trperiod(1989)` y los mismos predictores/años, calcular sus RMSPE y guardar una fila por unidad. Después definir:

```stata
scalar placebo_cutoff = 5*pre_rmspe_california
gen byte eligible = pre_rmspe <= placebo_cutoff
assert eligible == (pre_rmspe <= 5*pre_rmspe_california)
```

El bucle debe registrar fallas de convergencia por unidad y terminar con error si falta cualquiera de las 39 unidades; no debe omitir silenciosamente placebos.

- [ ] **Step 4: Calcular la proporción placebo descriptiva.** Crear en el log y el CSV una comparación de la razón de California frente a todas las unidades elegibles. Etiquetarla como `proporción de placebos elegibles con razón al menos tan grande`, no como p-valor convencional.

- [ ] **Step 5: Estimar el placebo temporal de 1980 sin fuga de información.** Reestimar con `trperiod(1980)`, usando `xperiod(1972(1)1979)` y únicamente predictores/resultados observados hasta 1979; exportar la brecha 1970–1988 a `synth_time_placebo.csv` y dibujar `synth_time_placebo.png` con línea en 1980.

- [ ] **Step 6: Ejecutar leave-one-out sobre cada donante con peso positivo.** Para cada estado positivo, reestimar California excluyéndolo del donor pool, exportar la brecha anual y verificar que el conjunto de `omitted_state` coincide exactamente con los pesos positivos de `synth_weights.csv`.

- [ ] **Step 7: Generar las cuatro gráficas de inferencia y sensibilidad.** Mostrar: brechas de todos los placebos con California destacada; distribución de razones RMSPE elegibles; placebo temporal; abanico leave-one-out con especificación principal destacada.

- [ ] **Step 8: Ejecutar Stata y validar esquemas y finitud.**

Run: `/Applications/StataNow/StataSE.app/Contents/MacOS/stata-se -b do 01_synthetic_controls.do`

Expected: 39 unidades en placebos, una fila de California, `eligible` consistente con el umbral 5×, ningún RMSPE no finito y un leave-one-out por donante de peso positivo.

- [ ] **Step 9: Ejecutar el contrato focalizado y commit.**

Run: `python3 -m pytest -q tests/test_synthetic_controls_contract.py -k "rmspe or placebo or leave_one_out"`

```bash
git add dofile/17_SyntheticControls tests/test_synthetic_controls_contract.py
git commit -m "feat: add synthetic-control placebos and sensitivity"
```

### Task 4: Escribir la clase teórica y su clave privada

**Files:**
- Create: `17-SyntheticControls.Rmd`
- Create outside repository: `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/claves_privadas/17_SyntheticControls_clave.md`

**Interfaces:**
- Consumes: diseño aprobado y notación uniforme del libro.
- Produces: capítulo teórico autocontenido y respuestas/rúbrica privada `SC-T1`–`SC-T3`.

- [ ] **Step 1: Crear el esqueleto con título, lecturas y metas.**

```markdown
# Controles sintéticos — Clase teórica {#controles-sinteticos}

::: {.boxinfo}
**Lecturas centrales**

- [Bernal y Peña — capítulo 6 (PDF)](lecturas/bernal-pena/capitulo-06.pdf)
- [Cunningham — capítulo 10: Synthetic Control](https://mixtape.scunning.com/10-synthetic_control)
:::

::: {.boxinfo}
**Metas de aprendizaje**

- Explicar por qué una combinación convexa puede construir un contrafactual más creíble que un único control.
- Distinguir ajuste pretratamiento, identificación e inferencia placebo.
- Diagnosticar soporte, contaminación del donor pool y sensibilidad a donantes influyentes.
:::
```

- [ ] **Step 2: Desarrollar la secuencia problema → solución → estimador.** Incluir la combinación convexa, la matriz de pesos `W`, la importancia de predictores `V`, el estimando `Y(D=1)-Y(D=0)` y un ejemplo numérico pequeño que sume uno sin duplicar la aplicación Prop 99.

- [ ] **Step 3: Desarrollar credibilidad y soporte.** Explicar buen ajuste pretratamiento, no anticipación, interferencia/spillovers, contaminación de donantes, estabilidad contrafactual, envolvente convexa e interpolación. Declarar explícitamente que una regresión de prebrechas no constituye una prueba de validez.

- [ ] **Step 4: Desarrollar inferencia y sensibilidad.** Definir RMSPE pre y post, razón post/pre, placebos espaciales, umbral 5× declarado ex ante, placebo temporal y leave-one-out. Separar la proporción placebo de un p-valor asintótico convencional.

- [ ] **Step 5: Añadir bloques didácticos y tres preguntas.** Usar al menos un bloque de intuición, supuesto/credibilidad, advertencia, lectura avanzada y conexión estimando-gráfica. Crear `SC-T1`, `SC-T2`, `SC-T3` como `::: {.boxexam}` sin soluciones ni elementos desplegables.

- [ ] **Step 6: Crear la clave privada fuera del repositorio.** Incluir encabezado `Uso exclusivo de la profesora y el monitor`, respuesta esperada, errores frecuentes y criterios de calificación para `SC-T1`, `SC-T2`, `SC-T3`, `SC-S1`, `SC-S2`, `SC-S3`, `SC-S4`. No ejecutar `git add` sobre la clave.

- [ ] **Step 7: Ejecutar pruebas de teoría, títulos, lecturas y privacidad.**

Run: `python3 -m pytest -q tests/test_synthetic_controls_contract.py tests/test_chapter_title_contract.py tests/test_central_readings_contract.py -k "theory or title or reading or private"`

Expected: las pruebas de teoría pasan; las pruebas dependientes de la práctica pueden seguir fallando.

- [ ] **Step 8: Commit solo del capítulo público.**

```bash
git add 17-SyntheticControls.Rmd
git commit -m "feat: add synthetic controls theory class"
```

### Task 5: Construir la clase empírica y conectarla al libro

**Files:**
- Create: `17-SyntheticControlsStata.Rmd`
- Modify: `_bookdown.yml`
- Modify: `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/claves_privadas/17_SyntheticControls_clave.md`

**Interfaces:**
- Consumes: CSV/PNG canónicos de Tasks 2–3 y clave iniciada en Task 4.
- Produces: página práctica renderizable, navegación antes de IV y preguntas `SC-S1`–`SC-S4` con rúbrica privada.

- [ ] **Step 1: Crear el comienzo de la página en el orden obligatorio.**

```markdown
# Controles sintéticos — Clase empírica {#controles-sinteticos-stata}

## Materiales para la clase {-}

- [Do-file completo](dofile/17_SyntheticControls/01_synthetic_controls.do)
- [Datos de Prop 99](dofile/17_SyntheticControls/synth_smoking.dta)
- [Log completo de Stata](dofile/17_SyntheticControls/01_synthetic_controls.log)

::: {.boxinfo}
**Lecturas centrales**

- [Bernal y Peña — capítulo 6 (PDF)](lecturas/bernal-pena/capitulo-06.pdf)
- [Cunningham — capítulo 10: Synthetic Control](https://mixtape.scunning.com/10-synthetic_control)
:::

::: {.boxinfo}
**Metas de aprendizaje**

- Estimar y reconstruir California sintética.
- Auditar ajuste, placebos y sensibilidad a donantes.
- Interpretar la brecha como un estimando dinámico bajo los supuestos del diseño.
:::
```

- [ ] **Step 2: Añadir un chunk oculto que importe y valide todos los resultados.** Exigir nombres exactos de columnas, pesos no negativos que sumen uno, coincidencia manual menor que `1e-8`, 39 unidades placebo, regla 5× y existencia de las siete gráficas antes de renderizar.

```r
sc_dir <- "dofile/17_SyntheticControls"
sc_weights <- read.csv(file.path(sc_dir, "results/synth_weights.csv"), check.names = FALSE)
sc_balance <- read.csv(file.path(sc_dir, "results/synth_predictor_balance.csv"), check.names = FALSE)
sc_paths <- read.csv(file.path(sc_dir, "results/synth_paths.csv"), check.names = FALSE)
sc_rmspe <- read.csv(file.path(sc_dir, "results/synth_rmspe.csv"), check.names = FALSE)
sc_placebos <- read.csv(file.path(sc_dir, "results/synth_placebos.csv"), check.names = FALSE)
sc_loo <- read.csv(file.path(sc_dir, "results/synth_leave_one_out.csv"), check.names = FALSE)
stopifnot(
  abs(sum(sc_weights$weight)-1) < 1e-6,
  min(sc_weights$weight) >= -1e-10,
  max(abs(sc_paths$synthetic-sc_paths$manual_synthetic)) < 1e-8,
  length(unique(sc_placebos$unit)) == 39
)
```

- [ ] **Step 3: Escribir la secuencia empírica completa.** Presentar pregunta/datos, gráfico bruto, especificación, comando `synth`, tabla de pesos, balance de predictores, reconstrucción manual, trayectoria, brecha y tabla RMSPE. Cada salida debe tener interpretación sustantiva y no limitarse a copiar el log.

- [ ] **Step 4: Explicar inferencia y sensibilidad con resultados visibles.** Mostrar todos los placebos, luego la comparación restringida por 5×, la proporción descriptiva, el placebo 1980 y leave-one-out. Indicar por qué un buen ajuste pretratamiento es necesario pero no suficiente.

- [ ] **Step 5: Añadir cuatro preguntas públicas y terminar la clave privada.** Crear `SC-S1`–`SC-S4` sobre pesos/soporte, ajuste, brecha/estimando y placebos/sensibilidad. Confirmar que el capítulo no contiene `solución`, `<details>` ni respuestas. Completar fuera del repositorio sus rúbricas.

- [ ] **Step 6: Insertar ambos capítulos en `_bookdown.yml`.** El fragmento debe quedar exactamente:

```yaml
  - 15-IPW.Rmd
  - 16-PSM_IPW_SinteticosConsolidado.Rmd
  - 17-SyntheticControls.Rmd
  - 17-SyntheticControlsStata.Rmd
  - 18-IV.Rmd
```

- [ ] **Step 7: Ejecutar contratos focalizados.**

Run: `python3 -m pytest -q tests/test_synthetic_controls_contract.py tests/test_chapter_title_contract.py tests/test_central_readings_contract.py`

Expected: PASS completo.

- [ ] **Step 8: Commit de práctica y navegación.**

```bash
git add 17-SyntheticControlsStata.Rmd _bookdown.yml
git commit -m "feat: add Prop 99 empirical synthetic controls class"
```

### Task 6: Verificar resultados, render y ausencia de filtraciones

**Files:**
- Test: `tests/test_synthetic_controls_contract.py`
- Test: all files under `tests/`
- Output outside repository: `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/libro_cortes_synth_review_20260729/`

**Interfaces:**
- Consumes: implementación completa y resultados canónicos regenerables.
- Produces: evidencia final de pruebas, HTML de revisión y auditoría de privacidad/publicación.

- [ ] **Step 1: Regenerar resultados desde cero con Stata.**

Run: `/Applications/StataNow/StataSE.app/Contents/MacOS/stata-se -b do 01_synthetic_controls.do`

Working directory: `dofile/17_SyntheticControls/`

Expected: exit 0; log sin error terminal; todos los CSV y PNG se regeneran; reconstrucción `<1e-8`.

- [ ] **Step 2: Ejecutar el contrato focalizado.**

Run: `python3 -m pytest -q tests/test_synthetic_controls_contract.py tests/test_chapter_title_contract.py tests/test_central_readings_contract.py`

Expected: PASS.

- [ ] **Step 3: Ejecutar la suite completa y revisar formato.**

Run: `python3 -m pytest -q`

Run: `git diff --check`

Expected: toda la suite PASS y ninguna advertencia de whitespace.

- [ ] **Step 4: Renderizar una copia de revisión fuera de `docs/`.** Crear una configuración temporal que conserve la misma lista `rmd_files` y cambie únicamente `output_dir` a `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/libro_cortes_synth_review_20260729`; ejecutar `bookdown::render_book("index.Rmd", config_file="/private/tmp/bookdown-synth-preview.yml")`.

- [ ] **Step 5: Auditar visualmente los dos HTML.** Verificar materiales al inicio de la práctica, tablas legibles, siete gráficas, bloques de color, notación, tres y cuatro preguntas, ausencia de doble numeración y navegación correcta hacia IPW e IV.

- [ ] **Step 6: Auditar privacidad e historial nuevo.**

Run: `git ls-files | rg '17_SyntheticControls_clave|claves_privadas'`

Expected: ninguna salida.

Run: `git log --all --oneline -- claves_privadas/17_SyntheticControls_clave.md`

Expected: ninguna salida.

- [ ] **Step 7: Confirmar que no se publicó.**

Run: `git status --short -- docs`

Expected: ningún producto renderizado nuevo dentro de `docs/`; el único cambio permitido allí es la documentación de diseño/plan ya versionada.

- [ ] **Step 8: Commit de cualquier ajuste exclusivamente derivado de verificación.** Si las pruebas o el render exigieron cambios, añadir solo los archivos de controles sintéticos y sus contratos, volver a ejecutar Steps 1–7 y usar:

```bash
git commit -m "fix: verify synthetic controls teaching pair"
```

No hacer push ni publicar el libro.
