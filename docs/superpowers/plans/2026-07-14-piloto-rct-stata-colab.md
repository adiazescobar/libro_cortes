# Piloto RCT Stata–Colab Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Convertir el módulo RCT en el patrón canónico teoría/empírico y demostrar equivalencia automática entre cuatro modelos estimados en Stata y Python sobre la misma `data.dta`.

**Architecture:** Se conserva casi intacto el contenido sustantivo de `05-RCT.Rmd`. El flujo reproducible vive en `dofile/06_RCT_Stata/`: Stata y Python exportan CSV con el mismo contrato, un comparador puro produce una tabla de verificación y `06-RCT2.Rmd` consume esa tabla sin cifras manuales.

**Tech Stack:** bookdown/gitbook, R/knitr, Stata 19, Python 3, pandas, statsmodels, pytest, CSS.

## Global Constraints

- Stata primero; Python/Colab después.
- Ambos lenguajes leen `dofile/06_RCT_Stata/data.dta`.
- No inventar ni transcribir cifras manualmente.
- Preservar el contenido, secuencia, ejemplos y simulaciones de `05-RCT.Rmd`.
- No tocar `06-RCT2-DRAFT.Rmd` ni cambios locales ajenos.
- No enviar correos ni usar `gmail_creds.json`.
- Coeficientes y SE: diferencia absoluta `< 1e-3`; N exacto; R² `< 1e-2`.

---

### Task 1: Congelar el contrato y las transformaciones RCT

**Files:**
- Create: `dofile/06_RCT_Stata/tests/test_rct_python.py`
- Create: `dofile/06_RCT_Stata/rct_python.py`
- Test: `dofile/06_RCT_Stata/tests/test_rct_python.py`

**Interfaces:**
- Consumes: `data.dta` con `resultado`, `grupo`, `genero`, `programa`, `semestre`, `edad`, `libros`.
- Produces: `prepare_rct_data(path) -> pandas.DataFrame` y `fit_main_models(df) -> pandas.DataFrame`.

- [ ] **Step 1: Escribir pruebas de preparación y contrato**

Comprobar que `D`, `y`, `mujer`, `pregrado`, `maestria` y semestre se construyen como en Stata; que las cuatro especificaciones se llaman `m1_simple`, `m2_controles`, `m3_estratos`, `m4_completo`; y que el resultado contiene exactamente:

```python
EXPECTED_COLUMNS = [
    "modelo", "termino", "coeficiente", "error_estandar",
    "N", "R2", "prueba", "estadistico", "p_value"
]
```

- [ ] **Step 2: Ejecutar la prueba y confirmar que falla**

Run:

```bash
python3 -m pytest dofile/06_RCT_Stata/tests/test_rct_python.py -v
```

Expected: `ModuleNotFoundError: No module named 'rct_python'`.

- [ ] **Step 3: Implementar preparación y estimación mínima**

Usar `pd.read_stata`, categorías explícitas de semestre y `statsmodels.formula.api.ols(...).fit(cov_type="HC1")`. Exportar al menos el término `D` de cada modelo, con `N` y `R2` repetidos por fila.

- [ ] **Step 4: Verificar las pruebas**

Run: el mismo comando del Step 2.  
Expected: todas las pruebas `PASSED`.

- [ ] **Step 5: Commit**

```bash
git add dofile/06_RCT_Stata/rct_python.py dofile/06_RCT_Stata/tests/test_rct_python.py
git commit -m "test: define RCT estimation contract"
```

### Task 2: Convertir el notebook en una réplica Colab reproducible

**Files:**
- Modify: `dofile/06_RCT_Stata/clase6_python.ipynb`
- Modify: `dofile/06_RCT_Stata/rct_python.py`
- Create: `dofile/06_RCT_Stata/results/resultados_python.csv`
- Test: `dofile/06_RCT_Stata/tests/test_rct_python.py`

**Interfaces:**
- Consumes: `prepare_rct_data()` y `fit_main_models()`.
- Produces: `results/resultados_python.csv` con el contrato canónico.

- [ ] **Step 1: Añadir una prueba de exportación determinista**

La prueba ejecutará `export_python_results(data_path, output_path)`, releerá el CSV y exigirá cuatro pares únicos `modelo`–`termino` para `D`, columnas exactas y ausencia de valores faltantes en coeficiente, SE, N y R².

- [ ] **Step 2: Ejecutar la prueba y confirmar que falla**

Expected: fallo porque `export_python_results` no existe.

- [ ] **Step 3: Implementar el exportador**

Crear `export_python_results(data_path: Path, output_path: Path) -> Path`, crear el directorio padre y escribir UTF-8 con `index=False` y precisión completa.

- [ ] **Step 4: Actualizar el notebook**

El notebook debe:

1. instalar/importar dependencias;
2. descargar `data.dta` desde GitHub cuando corre en Colab y usar la copia local cuando corre desde el repositorio;
3. mostrar preparación, balance y cuatro modelos;
4. llamar al mismo exportador;
5. no usar datos fallback ni resultados escritos a mano.

- [ ] **Step 5: Ejecutar pruebas y notebook**

Run:

```bash
python3 -m pytest dofile/06_RCT_Stata/tests/test_rct_python.py -v
jupyter nbconvert --to notebook --execute dofile/06_RCT_Stata/clase6_python.ipynb --output /private/tmp/clase6_python.executed.ipynb --ExecutePreprocessor.timeout=300
```

Expected: pruebas `PASSED`; notebook ejecutado sin error.

- [ ] **Step 6: Commit**

```bash
git add dofile/06_RCT_Stata/clase6_python.ipynb dofile/06_RCT_Stata/rct_python.py dofile/06_RCT_Stata/tests/test_rct_python.py dofile/06_RCT_Stata/results/resultados_python.csv
git commit -m "feat: make RCT Colab replication reproducible"
```

### Task 3: Exportar el contrato equivalente desde Stata

**Files:**
- Modify: `dofile/06_RCT_Stata/clase6_stata.do`
- Create: `dofile/06_RCT_Stata/results/resultados_stata.csv`

**Interfaces:**
- Consumes: `data.dta` y las macros `$X` ya usadas por la clase.
- Produces: `results/resultados_stata.csv` con los cuatro modelos y el término `D`.

- [ ] **Step 1: Añadir validaciones al inicio del do-file**

Usar `confirm file "data.dta"` y `confirm variable` para todas las columnas requeridas. Ante error, imprimir un mensaje explícito y terminar con código distinto de cero.

- [ ] **Step 2: Añadir un bloque de exportación después de `eststo m1 ... m4`**

Usar `postfile` para guardar, por modelo, `_b[D]`, `_se[D]`, `e(N)` y `e(r2)`. Guardar strings vacíos para pruebas no aplicables y exportar con:

```stata
export delimited using "results/resultados_stata.csv", replace
```

- [ ] **Step 3: Ejecutar Stata en batch**

Run desde `dofile/06_RCT_Stata/`:

```bash
/Applications/StataNow/StataSE.app/Contents/MacOS/stata-se -b do clase6_stata.do
```

Expected: exit code 0, log sin `r(...)` y CSV con cuatro filas del término `D`.

- [ ] **Step 4: Validar esquema sin modificar el archivo**

Run:

```bash
python3 -c "import pandas as pd; p='results/resultados_stata.csv'; d=pd.read_csv(p); assert len(d)==4; assert d['N'].notna().all(); print(d.to_string(index=False))"
```

Expected: cuatro modelos con columnas canónicas.

- [ ] **Step 5: Commit**

```bash
git add dofile/06_RCT_Stata/clase6_stata.do dofile/06_RCT_Stata/results/resultados_stata.csv
git commit -m "feat: export canonical RCT results from Stata"
```

### Task 4: Comparar Stata y Python automáticamente

**Files:**
- Create: `dofile/06_RCT_Stata/verify_stata_python.py`
- Create: `dofile/06_RCT_Stata/tests/test_verify_stata_python.py`
- Create: `dofile/06_RCT_Stata/results/verificacion_stata_python.csv`

**Interfaces:**
- Consumes: los dos CSV canónicos.
- Produces: `compare_results(stata_path, python_path) -> DataFrame` y CSV con diferencias y estado.

- [ ] **Step 1: Escribir pruebas para PASS y FAIL**

Casos obligatorios: coincidencia dentro de tolerancias; N diferente; coeficiente fuera de tolerancia; clave ausente; columna requerida ausente.

- [ ] **Step 2: Ejecutar pruebas y confirmar fallo**

Expected: fallo por módulo inexistente.

- [ ] **Step 3: Implementar comparación**

Hacer `outer merge` por `modelo,termino`, calcular `coef_abs_diff`, `se_abs_diff`, `N_igual`, `R2_abs_diff` y asignar `PASS` únicamente si todos los criterios aplicables se cumplen. Claves ausentes son `FAIL`.

- [ ] **Step 4: Ejecutar pruebas y comparación real**

```bash
python3 -m pytest dofile/06_RCT_Stata/tests/test_verify_stata_python.py -v
python3 dofile/06_RCT_Stata/verify_stata_python.py
```

Expected: pruebas `PASSED`; cuatro filas reales con `estado=PASS`.

- [ ] **Step 5: Commit**

```bash
git add dofile/06_RCT_Stata/verify_stata_python.py dofile/06_RCT_Stata/tests/test_verify_stata_python.py dofile/06_RCT_Stata/results/verificacion_stata_python.csv
git commit -m "feat: verify RCT Stata and Python estimates"
```

### Task 5: Aplicar el patrón editorial sin desarmar la teoría

**Files:**
- Modify: `05-RCT.Rmd`
- Modify: `06-RCT2.Rmd`

**Interfaces:**
- Consumes: contenido existente y CSV de verificación.
- Produces: pareja explícita clase teórica/clase empírica.

- [ ] **Step 1: Crear una comprobación estructural**

Usar `rg` para registrar los encabezados actuales y exigir un solo encabezado `#` por archivo.

- [ ] **Step 2: Editar mínimamente `05-RCT.Rmd`**

Cambiar solo el título a “Experimentos aleatorizados — Clase teórica”, mapear el objetivo inicial, añadir al final las secciones realmente ausentes —errores comunes, resumen y preguntas— y reemplazar cajas antiguas por clases canónicas sin reordenar demostraciones, cuatro escenarios, CATE ni simulaciones.

- [ ] **Step 3: Reorganizar `06-RCT2.Rmd` alrededor del flujo empírico**

Conservar los ejemplos Stata, tablas de balance, cuatro escenarios, heterogeneidad y centrado. Añadir secciones de datos, réplica Colab y verificación. Reemplazar resultados R ocultos que compitan con el nuevo contrato por lectura de archivos generados.

- [ ] **Step 4: Insertar tabla automática**

El chunk R debe leer `dofile/06_RCT_Stata/results/verificacion_stata_python.csv`, detenerse si no existe o contiene `FAIL`, y renderizar coeficientes, SE, diferencias y estado.

- [ ] **Step 5: Verificar estructura**

```bash
rg -n '^# ' 05-RCT.Rmd 06-RCT2.Rmd
rg -n '^## ' 05-RCT.Rmd 06-RCT2.Rmd
```

Expected: exactamente un `#` por capítulo; secciones empíricas canónicas presentes.

- [ ] **Step 6: Commit**

```bash
git add 05-RCT.Rmd 06-RCT2.Rmd
git commit -m "docs: align RCT theory and empirical chapters"
```

### Task 6: Incorporar estilo visual y acceso a Colab

**Files:**
- Modify: `style.css`
- Modify: `06-RCT2.Rmd`

**Interfaces:**
- Produces: cajas `box-stata`, `box-colab`, `box-verificacion`, `box-cuidado`, `box-ejercicios`, `box-resumen`.

- [ ] **Step 1: Añadir únicamente el sistema tipográfico y las cajas reutilizables**

Adaptar Fraunces, Hanken Grotesk, JetBrains Mono, colores y ancho 840 px desde el libro de Econometría II. Mantener selectores existentes de navegación y audio.

- [ ] **Step 2: Añadir botón Colab**

Enlazar a:

```text
https://colab.research.google.com/github/adiazescobar/libro_cortes/blob/main/dofile/06_RCT_Stata/clase6_python.ipynb
```

Abrir en pestaña nueva y mantener un enlace directo alternativo al `.ipynb`.

- [ ] **Step 3: Comprobar selectores y enlaces**

```bash
rg -n 'box-(stata|colab|verificacion|cuidado|ejercicios|resumen)' style.css 06-RCT2.Rmd
rg -n 'colab.research.google.com/github/adiazescobar/libro_cortes' 06-RCT2.Rmd
```

- [ ] **Step 4: Commit**

```bash
git add style.css 06-RCT2.Rmd
git commit -m "style: add canonical RCT Stata and Colab callouts"
```

### Task 7: Render y revisión integral

**Files:**
- Generated/verify: `docs/experimentos-aleatorizados-clase-teorica.html`
- Generated/verify: `docs/experimentos-aleatorizados-clase-empirica.html`

**Interfaces:**
- Consumes: capítulos, CSS y resultados verificados.
- Produces: libro HTML navegable sin regresiones estructurales.

- [ ] **Step 1: Ejecutar toda la verificación numérica desde cero**

Ejecutar Stata, notebook, pytest y comparador en ese orden. Expected: cuatro `PASS`.

- [ ] **Step 2: Renderizar bookdown**

```bash
Rscript -e "bookdown::render_book('index.Rmd', 'bookdown::gitbook')"
```

Expected: exit code 0 y ambos capítulos HTML generados.

- [ ] **Step 3: Revisar enlaces y contenido generado**

Confirmar en HTML el botón Colab, los tres archivos de descarga, cuatro filas de verificación y ausencia de `FAIL`.

- [ ] **Step 4: Revisión visual**

Abrir el render local y revisar escritorio y ancho móvil: índice, títulos, tablas, overflow de código, cajas, botón Colab y navegación anterior/siguiente.

- [ ] **Step 5: Revisar el diff final**

```bash
git status --short
git diff --check
git diff --stat HEAD~6..HEAD
```

Excluir expresamente cambios previos ajenos y archivos temporales.

- [ ] **Step 6: Commit de artefactos renderizados solo si el repositorio ya los versiona**

```bash
git status --short docs
git add docs/experimentos-aleatorizados-clase-teorica.html docs/experimentos-aleatorizados-clase-empirica.html docs/style.css
git commit -m "build: render verified RCT pilot"
```

Antes de ejecutar `git add`, confirmar los nombres reales generados por bookdown. No añadir `docs/404.html`, `docs/search_index.json`, `econometria-avanzada.rds` ni ningún otro archivo que ya estuviera modificado antes del piloto. Expected: el estado restante contiene únicamente los cambios previos de Ana María identificados antes del piloto.
