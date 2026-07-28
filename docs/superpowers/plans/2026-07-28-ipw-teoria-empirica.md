# IPW — clases teórica y empírica — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Reconstruir IPW como una pareja teoría–práctica rigurosa, reproducible y consistente con el estándar editorial del libro.

**Architecture:** `15-IPW.Rmd` contiene identificación, estimandos y teoría de ponderación. `16-PSM_IPW_SinteticosConsolidado.Rmd` conserva su anchor legado pero se convierte en la práctica reproducible; un do-file genera CSV, gráficos y log. El contenido de controles sintéticos se preserva fuera del libro en un borrador independiente.

**Tech Stack:** R Markdown/Bookdown, Stata 19 `teffects`, CSV, PNG, pytest, Python/BeautifulSoup para QA.

## Global Constraints

- Trabajar directamente en `main` por preferencia previa de Ana María.
- Preservar `ipw.html` y `psm-ipw-sinteticos.html`.
- Usar (Y(D=1)) y (Y(D=0)).
- Descargas antes de lecturas en la práctica.
- Resultados visibles deben provenir de Stata.
- Preguntas públicas sin respuestas; clave privada fuera de `_bookdown.yml`.
- No actualizar `docs/` sin aprobación.

---

### Task 1: Preservar controles sintéticos y establecer contratos

**Files:**
- Create: `17-SyntheticControls-DRAFT.Rmd`
- Create: `tests/test_ipw_pedagogy_contract.py`
- Read: `16-PSM_IPW_SinteticosConsolidado.Rmd`

**Interfaces:**
- Consumes: sección `Parte III: Synthetic Control Method` y materiales asociados.
- Produces: borrador completo fuera de `_bookdown.yml` y contrato rojo para IPW.

- [ ] Extraer al borrador la explicación, código, gráficos, descargas, ejercicios y referencias de controles sintéticos.
- [ ] Verificar que `17-SyntheticControls-DRAFT.Rmd` no aparezca en `_bookdown.yml`.
- [ ] Crear pruebas que exijan títulos exactos, anchors estables, materiales antes de lecturas, HT y Hájek correctamente diferenciados, ATE/ATT, ESS, balance, positividad, AIPW/IPWRA, siete preguntas y clave privada.
- [ ] Ejecutar `python3 -m pytest -q tests/test_ipw_pedagogy_contract.py` y confirmar FAIL.
- [ ] Commit: `test: define IPW pedagogy contract`.

### Task 2: Reconstruir la clase teórica

**Files:**
- Modify: `15-IPW.Rmd`
- Test: `tests/test_ipw_pedagogy_contract.py`

**Interfaces:**
- Consumes: diseño aprobado y notación global.
- Produces: capítulo teórico autocontenido con preguntas T1–T3.

- [ ] Reescribir el título como `# Ponderación por probabilidad inversa — Clase teórica {#ipw}`.
- [ ] Desarrollar pregunta causal, CIA, positividad, identidades de reponderación y pesos ATE/ATT.
- [ ] Definir HT sin normalizar y Hájek normalizado con fórmulas distintas.
- [ ] Añadir pesos estabilizados, balance y `ESS=(sum w)^2/sum(w^2)`.
- [ ] Explicar trimming/winsorización como cambios de diseño, no arreglos automáticos.
- [ ] Desarrollar AIPW/IPWRA y límites de doble robustez.
- [ ] Añadir comparación con matching, tres preguntas y referencias.
- [ ] Ejecutar contratos teóricos y commit `feat: rebuild IPW theory chapter`.

### Task 3: Construir do-file y resultados canónicos

**Files:**
- Modify: `dofile/16_PSM_IPW_Sinteticos/02_ipw_stata.do`
- Create: `dofile/16_PSM_IPW_Sinteticos/results/ipw_estimates.csv`
- Create: `dofile/16_PSM_IPW_Sinteticos/results/ipw_weight_diagnostics.csv`
- Create: `dofile/16_PSM_IPW_Sinteticos/results/ipw_balance.csv`
- Create: `dofile/16_PSM_IPW_Sinteticos/results/ipw_positivity_simulation.csv`
- Create: `dofile/16_PSM_IPW_Sinteticos/ipw_support.png`
- Create: `dofile/16_PSM_IPW_Sinteticos/ipw_weights_dist.png`
- Create: `dofile/16_PSM_IPW_Sinteticos/ipw_positivity_weak.png`
- Create: `dofile/16_PSM_IPW_Sinteticos/ipw_demo.log`

**Interfaces:**
- Consumes: `base6.dta`, `$Xmust`, seed 1298.
- Produces: estimaciones y diagnósticos importables por R Markdown.

- [ ] Calcular diferencia cruda, HT/Hájek manual para ATE y ATT.
- [ ] Ejecutar `teffects ipw`, `teffects aipw` y `teffects ipwra` para ATE y ATET.
- [ ] Calcular percentiles de pesos, máximo, suma y ESS.
- [ ] Exportar balance antes/después con diferencias estandarizadas.
- [ ] Generar las tres gráficas con nombres estables.
- [ ] Crear simulación de positividad débil con efecto verdadero conocido.
- [ ] Ejecutar Stata 19 en batch y exigir exit 0.
- [ ] Validar que los CSV no tengan columnas vacías ni valores no numéricos inesperados.
- [ ] Commit `feat: generate canonical IPW results`.

### Task 4: Reconstruir la clase empírica

**Files:**
- Modify: `16-PSM_IPW_SinteticosConsolidado.Rmd`
- Test: `tests/test_ipw_pedagogy_contract.py`

**Interfaces:**
- Consumes: CSV/PNG generados en Task 3.
- Produces: práctica autocontenida con preguntas S1–S4.

- [ ] Cambiar solo el título a `Ponderación por probabilidad inversa — Clase empírica` y conservar `{#psm-ipw-sinteticos}`.
- [ ] Colocar descargas, lecturas centrales y metas al comienzo.
- [ ] Importar CSV con chunks ocultos y validar esquemas con `stopifnot`.
- [ ] Desarrollar flujo estimando → PS → pesos → diagnósticos → efecto → doble robustez.
- [ ] Mostrar todas las tablas y gráficas centrales.
- [ ] Incorporar simulación de positividad débil y cambio de población al restringir soporte.
- [ ] Añadir cuatro preguntas tipo examen sin soluciones.
- [ ] Ejecutar contrato práctico y commit `feat: rebuild IPW empirical chapter`.

### Task 5: Crear clave privada

**Files:**
- Create: `claves_privadas/15_IPW_clave.md`
- Test: `tests/test_ipw_pedagogy_contract.py`

**Interfaces:**
- Consumes: T1–T3 y S1–S4.
- Produces: soluciones y rúbricas para profesora/monitor.

- [ ] Escribir siete soluciones con rúbrica de cinco puntos.
- [ ] Probar correspondencia uno a uno y ausencia del archivo en `_bookdown.yml` y HTML.
- [ ] Commit `docs: add private IPW answer key`.

### Task 6: Verificar capítulos y suite

**Files:**
- Modify if needed: `tests/test_chapter_title_contract.py`
- Modify if needed: `tests/test_central_readings_contract.py`
- Test: toda la suite.

**Interfaces:**
- Consumes: capítulos y resultados finales.
- Produces: contratos verdes y compatibilidad con reglas globales.

- [ ] Añadir el par IPW al contrato global de títulos.
- [ ] Actualizar el mapa de lecturas sin cambiar los destinos aprobados.
- [ ] Ejecutar `python3 -m pytest -q tests/test_ipw_pedagogy_contract.py`.
- [ ] Ejecutar `python3 -m pytest -q`.
- [ ] Ejecutar `git diff --check`.
- [ ] Commit `test: verify IPW chapter pair` si hubo cambios de pruebas.

### Task 7: Render y auditoría visual

**Files:**
- Create outside repo: `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/libro_cortes_ipw_review_20260728/`

**Interfaces:**
- Consumes: libro completo verificado.
- Produces: preview local revisable sin publicación.

- [ ] Renderizar con `bookdown::render_book("index.Rmd", "bookdown::gitbook", output_dir=...)`.
- [ ] Verificar `ipw.html` y `psm-ipw-sinteticos.html`.
- [ ] Comprobar descargas, cuatro tablas, tres gráficos, siete preguntas y cero soluciones públicas.
- [ ] Confirmar que `17-SyntheticControls-DRAFT.Rmd` no fue renderizado.
- [ ] Reportar rutas, resultados Stata, número final de pruebas y que `docs/` permanece intacto.
