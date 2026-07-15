# Ampliación pedagógica de Parámetros causales Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Ampliar los capítulos teórico y práctico de Parámetros causales con desarrollo guiado, bloques de color y siete preguntas tipo examen sin respuestas visibles, acompañadas por una clave docente privada.

**Architecture:** Los dos Rmd estudiantiles conservarán la teoría, los resultados canónicos y los gráficos aprobados, pero añadirán capas pedagógicas reutilizando el sistema visual existente. Las preguntas se identificarán mediante códigos estables y se probará que no contengan soluciones. La clave se escribirá fuera del repositorio y de `docs`, en la carpeta docente privada de Dropbox.

**Tech Stack:** R Markdown, bookdown/gitbook, CSS existente, Stata 19, CSV canónicos, Python/pytest, Markdown privado.

## Global Constraints

- Usar exclusivamente `Y_i(D=1)` y `Y_i(D=0)`, o las versiones sin índice.
- Conservar ATE, ATT, ATU, CATE, las identidades, supuestos y gráficos ya aprobados.
- Mantener descargas al inicio de la práctica y ninguna descarga en teoría.
- Añadir exactamente tres preguntas teóricas `T-P1`–`T-P3` y cuatro prácticas `S-P1`–`S-P4`.
- No incluir respuestas, pistas, retroalimentación automática ni elementos desplegables en las páginas estudiantiles.
- Guardar la clave únicamente en una ubicación privada externa comunicada fuera del repositorio.
- No añadir la clave al repositorio, `_bookdown.yml`, `docs` ni enlaces estudiantiles.
- Toda cifra empírica visible seguirá proviniendo de CSV exportados por Stata.
- Las futuras diapositivas incluirán todos los resultados, pero quedan fuera de este plan.
- Renderizar en `/private/tmp/libro_cortes_parametros_ampliado`; no publicar `docs`.
- Preservar cambios locales y artefactos ajenos.

---

### Task 1: Contratos de profundidad, bloques, preguntas y privacidad

**Files:**
- Create: `tests/test_parametros_pedagogy_contract.py`
- Modify: `tests/test_parametros_stata_contract.py`

**Interfaces:**
- Consumes: `03-Parametros.Rmd`, `04-ParametrosStata.Rmd`, `_bookdown.yml` y la especificación aprobada.
- Produces: pruebas RED que fijan profundidad mínima, códigos de preguntas, bloques y ausencia de soluciones.

- [ ] **Step 1: Escribir el contrato del capítulo teórico**

```python
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
THEORY = (ROOT / "03-Parametros.Rmd").read_text(encoding="utf-8")
PRACTICE = (ROOT / "04-ParametrosStata.Rmd").read_text(encoding="utf-8")


def test_theory_has_colored_learning_blocks():
    assert THEORY.count("::: {.box") >= 6
    for label in ["Intuición", "Resultado clave", "Advertencia", "Ejemplo guiado"]:
        assert label in THEORY


def test_theory_has_exactly_three_exam_questions_without_answers():
    codes = re.findall(r"T-P[1-3]", THEORY)
    assert sorted(set(codes)) == ["T-P1", "T-P2", "T-P3"]
    for code in set(codes):
        block = THEORY.split(code, 1)[1].split(":::", 1)[0].lower()
        assert "puntaje sugerido" in block
        assert not any(word in block for word in ["respuesta:", "solución:", "pista:", "details>", "hide("])
```

- [ ] **Step 2: Escribir el contrato del capítulo práctico**

```python
def test_practice_restores_twelve_guided_stages():
    stages = [
        "Preparación de los datos", "Descripción por grupos", "Diferencia de medias",
        "Regresión simple", "Programa `estimadores`", "ATE, ATT, ATU y CATE",
        "Descomposición del sesgo", "Duplicación de observaciones",
        "Asignación aleatoria", "Monte Carlo con selección",
        "Monte Carlo con aleatorización", "Comparación gráfica",
    ]
    positions = [PRACTICE.index(stage) for stage in stages]
    assert positions == sorted(positions)


def test_practice_has_required_blocks_and_exam_questions():
    for label in ["Comando clave", "Salida central", "Interpretación", "Error frecuente", "Resultado clave"]:
        assert label in PRACTICE
    assert PRACTICE.count("::: {.box") >= 10
    codes = re.findall(r"S-P[1-4]", PRACTICE)
    assert sorted(set(codes)) == ["S-P1", "S-P2", "S-P3", "S-P4"]
```

- [ ] **Step 3: Prohibir exposición de la clave y respuestas**

```python
def test_student_material_never_exposes_private_key():
    combined = THEORY + PRACTICE + (ROOT / "_bookdown.yml").read_text(encoding="utf-8")
    assert all(token not in combined for token in PRIVATE_KEY_IDENTIFIERS)
    assert "<details" not in THEORY + PRACTICE
    assert "Ver respuesta" not in THEORY + PRACTICE
```

- [ ] **Step 4: Fortalecer el contrato práctico de código**

Exigir en `tests/test_parametros_stata_contract.py` que la página contenga comandos ejecutables para `generate y`, `generate tau`, creación de `X`, `ttest`, `regress`, `program define estimadores`, `summarize ... if`, `expand 10000`, `set seed`, `simulate` y ambas reglas de asignación. El contrato debe rechazar `...` dentro de bloques Stata.

- [ ] **Step 5: Ejecutar y confirmar RED**

Run: `/private/tmp/libro_cortes_rct_venv/bin/python -m pytest -q tests/test_parametros_pedagogy_contract.py tests/test_parametros_stata_contract.py`

Expected: fallos por ausencia de bloques, códigos, etapas y programa detallado; los contratos canónicos existentes permanecen verdes.

- [ ] **Step 6: Commit de contratos**

```bash
git add tests/test_parametros_pedagogy_contract.py tests/test_parametros_stata_contract.py
git commit -m "test: define expanded parameters pedagogy"
```

---

### Task 2: Bloques y preguntas tipo examen en teoría

**Files:**
- Modify: `03-Parametros.Rmd`
- Test: `tests/test_parametros_pedagogy_contract.py`

**Interfaces:**
- Consumes: contenido teórico aprobado y contrato de Task 1.
- Produces: seis o más bloques visuales y tres preguntas sin solución.

- [ ] **Step 1: Añadir bloques visuales sin alterar las demostraciones**

Reutilizar `boxinfo`, `boxnote`, `boxwarning` y `boxejercicio`. Insertar bloques titulados:

- `💡 Intuición: una unidad, dos resultados potenciales`;
- `✅ Resultado clave: del ATT y ATU al ATE`;
- `✅ Resultado clave: del CATE al ATE`;
- `⚠️ Advertencia: más datos no corrigen selección`;
- `🔎 Ejemplo guiado: ocho personas, cuatro parámetros`;
- `⚠️ Advertencia: ignorabilidad sin positividad no basta`.

Cada bloque tendrá entre uno y tres párrafos o una ecuación; no duplicará secciones completas.

- [ ] **Step 2: Añadir `T-P1`**

Crear un bloque `boxejercicio` con una situación de capacitación dirigida a desempleados de larga duración. Pedir escoger entre ATE, ATT, ATU y CATE, justificar la población relevante y escribir el estimando con `Y_i(D=1)` y `Y_i(D=0)`. Puntaje sugerido: 4 puntos.

- [ ] **Step 3: Añadir `T-P2`**

Dar `E[Y|D=1]=12`, `E[Y|D=0]=7` y `ATT=2`; pedir derivar el sesgo de selección y explicar su signo mediante la descomposición completa. Las cifras son datos del enunciado, no resultados empíricos del capítulo. Puntaje sugerido: 5 puntos.

- [ ] **Step 4: Añadir `T-P3`**

Presentar una intervención escolar con adopción voluntaria, una escuela sin tratados comparables y posibles derrames entre compañeros. Pedir diagnosticar independencia, positividad y SUTVA, y proponer una modificación de diseño. Puntaje sugerido: 6 puntos.

- [ ] **Step 5: Ejecutar pruebas y tejido aislado**

Run: `/private/tmp/libro_cortes_rct_venv/bin/python -m pytest -q tests/test_parametros_theory_contract.py tests/test_parametros_pedagogy_contract.py`

Expected: contratos teóricos pasan; los prácticos de Task 1 todavía fallan.

Run: `Rscript -e "rmarkdown::render('03-Parametros.Rmd', output_dir='/private/tmp/libro_cortes_parametros_ampliado/theory')"`

Expected: HTML creado sin errores.

- [ ] **Step 6: Commit de ampliación teórica**

```bash
git add 03-Parametros.Rmd
git commit -m "docs: add exam practice to parameters theory"
```

---

### Task 3: Recuperar desarrollo práctico y crear evaluación

**Files:**
- Modify: `04-ParametrosStata.Rmd`
- Modify: `style.css` only if existing boxes cannot express the approved pattern.
- Create externally: ubicación privada externa comunicada fuera del repositorio
- Test: `tests/test_parametros_pedagogy_contract.py`
- Test: `tests/test_parametros_stata_contract.py`

**Interfaces:**
- Consumes: CSV y gráficos canónicos existentes; código completo de `dofile/04_ParametrosStata/04_stata.do`.
- Produces: práctica desarrollada en doce etapas, cuatro preguntas y clave privada externa.

- [ ] **Step 1: Expandir preparación y descripción**

Mostrar bloques Stata completos para cargar datos, inspeccionar `describe/list`, crear `X`, `y` y `tau`, etiquetar variables, ejecutar `tabulate`, `summarize` y `bysort`. Añadir bloques de comando, salida e interpretación; todas las cifras de salida se interpolan desde `parameters_results.csv`.

- [ ] **Step 2: Expandir `ttest` y regresión**

Mostrar sintaxis ejecutable, dirección de la resta en `ttest`, equivalencia con `regress y D, robust`, interpretación de `_cons`, `_b[D]`, error estándar e intervalo. Añadir un bloque `Error frecuente` sobre el signo de `mean(0)-mean(1)` frente al coeficiente de `D`.

- [ ] **Step 3: Presentar el programa completo**

Incluir una versión ejecutable de:

```stata
capture program drop estimadores
program define estimadores
    syntax varlist(min=3 max=3)
    tokenize `varlist'
    local tau `1'
    local y `2'
    local d `3'
    quietly summarize `tau'
    scalar ATE = r(mean)
    quietly summarize `tau' if `d' == 1
    scalar ATT = r(mean)
    quietly summarize `tau' if `d' == 0
    scalar ATU = r(mean)
    display "ATE = " ATE
    display "ATT = " ATT
    display "ATU = " ATU
end
```

Explicar `program`, `syntax`, `tokenize`, locales, `r(mean)` y escalares. El código de clase puede ser pedagógicamente más corto que el pipeline de exportación, pero debe calcular correctamente los estimandos que anuncia.

- [ ] **Step 4: Expandir parámetros, sesgo, duplicación y aleatorización**

Mostrar cálculos separados con `summarize ... if`, CATE por `X`, medias observadas, NAIVE y `NAIVE-ATT`. Mostrar `expand 10000`, volver a ejecutar el programa y explicar por qué no aumenta información independiente. Mostrar `set seed 87634`, generación aleatoria de `D`, recreación de `y` y diferencia entre realización muestral e insesgadez.

- [ ] **Step 5: Expandir ambos Monte Carlo**

Explicar población de 80.000, programa `one_rep`, regla `invlogit` de selección, regla `runiform()<.5`, `simulate`, semillas y almacenamiento. Se pueden presentar fragmentos consecutivos del do-file canónico, pero juntos deben formar una ruta reproducible y no contener elipsis.

- [ ] **Step 6: Conservar y ampliar lectura de gráficos**

Mantener los tres PNG. Añadir preguntas guiadas sobre centro, dispersión, cuantiles, masa a cada lado de cero y comparación de escenarios, interpolando medias y cuantiles desde `monte_carlo_summary.csv`.

- [ ] **Step 7: Añadir `S-P1` a `S-P4`**

- `S-P1` (5 puntos): interpretar output de regresión y relación con diferencia de medias.
- `S-P2` (6 puntos): calcular ATE, ATT, ATU, CATE(0), CATE(1) y sesgo desde una tabla nueva de seis unidades.
- `S-P3` (6 puntos): corregir cuatro errores en un programa Stata de estimandos.
- `S-P4` (7 puntos): diseñar cambio en regla de selección, anticipar centro/dispersión y escribir pseudocódigo Stata ejecutable.

Ningún bloque contendrá solución, pista o desplegable.

- [ ] **Step 8: Crear la clave privada externa**

Crear la clave en la ubicación privada externa comunicada fuera del repositorio, con encabezado de confidencialidad. Para cada código `T-P1`–`T-P3` y `S-P1`–`S-P4`, incluir cinco subtítulos: `Respuesta correcta`, `Procedimiento`, `Criterios de calificación`, `Errores frecuentes`, `Puntaje sugerido`. No incluir el archivo en Git.

- [ ] **Step 9: Ejecutar pruebas focales y verificar privacidad**

Run: `/private/tmp/libro_cortes_rct_venv/bin/python -m pytest -q tests/test_parametros_pedagogy_contract.py tests/test_parametros_stata_contract.py`

Expected: todos pasan.

Run: verificar que el índice y el estado de Git no incluyan identificadores de la clave privada externa.

Expected: sin salida porque la clave está fuera del repositorio.

- [ ] **Step 10: Commit de ampliación práctica**

```bash
git add 04-ParametrosStata.Rmd style.css tests/test_parametros_pedagogy_contract.py tests/test_parametros_stata_contract.py
git commit -m "docs: restore guided parameters practice"
```

---

### Task 4: Verificación integral y vistas previas

**Files:**
- Verify: `03-Parametros.Rmd`
- Verify: `04-ParametrosStata.Rmd`
- Verify externally: ubicación privada externa comunicada fuera del repositorio
- Verify: `/private/tmp/libro_cortes_parametros_ampliado/parametros-causales-teoria.html`
- Verify: `/private/tmp/libro_cortes_parametros_ampliado/parametros-causales-stata.html`

**Interfaces:**
- Consumes: Tasks 1–3.
- Produces: render local ampliado, clave privada validada y revisión académica/visual final.

- [ ] **Step 1: Ejecutar suite completa**

Run: `/private/tmp/libro_cortes_rct_venv/bin/python -m pytest -q`

Expected: cero fallos.

- [ ] **Step 2: Verificar clave privada**

Comprobar que existen siete códigos únicos y cinco componentes por código. Buscar frases distintivas de las soluciones en los Rmd y HTML estudiantiles; la búsqueda debe ser vacía. Confirmar que `_bookdown.yml`, Git y `docs` no contienen la ruta o el nombre de la clave.

- [ ] **Step 3: Verificar fuentes de cifras y código completo**

Confirmar que las cifras empíricas visibles se interpolan desde `point` o `mc`; los únicos números literales permitidos en ejercicios son datos hipotéticos explícitos. Buscar elipsis dentro de fences Stata y comandos incompletos.

- [ ] **Step 4: Renderizar libro completo**

Run: `Rscript -e "bookdown::render_book('index.Rmd', output_dir='/private/tmp/libro_cortes_parametros_ampliado')"`

Expected: exit 0. Si bookdown vuelve a generar HTML dividido en la raíz, copiar únicamente los HTML frescos al directorio temporal y documentar el workaround; no tocar `docs`.

- [ ] **Step 5: Revisar teoría en escritorio y móvil**

Verificar ritmo visual, seis bloques diferenciados, tres preguntas completas, videos accesibles, ecuaciones sin desbordamiento y ausencia de respuestas.

- [ ] **Step 6: Revisar práctica en escritorio y móvil**

Verificar las doce etapas, legibilidad de código, scroll interno de tablas, diez o más bloques, cuatro preguntas, tres gráficos intactos, descargas al inicio y ausencia de respuestas.

- [ ] **Step 7: Auditoría académica de preguntas y clave**

Confirmar que cada pregunta tiene una sola interpretación razonable, información suficiente, dificultad coherente y puntaje consistente con la clave. Revisar que la clave no use una respuesta distinta a la solicitada ni otorgue puntaje por elementos no pedidos.

- [ ] **Step 8: Revisión final y commit de ajustes**

Si la revisión detecta cambios necesarios, aplicar únicamente esos ajustes, ejecutar sus pruebas focales y crear:

```bash
git add 03-Parametros.Rmd 04-ParametrosStata.Rmd style.css tests
git commit -m "fix: address expanded parameters review"
```

- [ ] **Step 9: Entregar enlaces locales**

Iniciar servidor en un puerto libre sobre `/private/tmp/libro_cortes_parametros_ampliado` y entregar los dos enlaces. Mantener `docs` sin publicar.
