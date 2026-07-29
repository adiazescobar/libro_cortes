# IPW con `reg` y postestimación de balance — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Ampliar la clase empírica de IPW para reproducir Hájek con `reg` ponderada y auditar balance ATE/ATT después de estimar.

**Architecture:** El do-file sigue siendo la única fuente de resultados: exporta estimaciones y balance en CSV, además de un gráfico de diferencias estandarizadas. El R Markdown valida e importa esos artefactos, muestra la equivalencia puntual y guía la postestimación sin duplicar números manualmente.

**Tech Stack:** Stata 19 (`logit`, `reg`, `teffects`, `tebalance`), R Markdown/Bookdown, CSV, PNG, pytest.

## Global Constraints

- Trabajar directamente en `main` por preferencia explícita de Ana María.
- Preservar `psm-ipw-sinteticos.html` y `{#psm-ipw-sinteticos}`.
- Analizar ATE y ATT por separado.
- Toda cifra pública debe derivarse de resultados canónicos generados por Stata.
- La equivalencia `reg`–Hájek se exige para el estimador puntual, no para los errores estándar.
- Balance observable no demuestra CIA ni descarta confusión no observada.
- La clave permanece fuera del repositorio en `../claves_privadas/15_IPW_clave.md`.
- No actualizar ni publicar `docs/` sin aprobación.

---

### Task 1: Extender el contrato pedagógico

**Files:**
- Modify: `tests/test_ipw_pedagogy_contract.py`

**Interfaces:**
- Consumes: diseño `docs/superpowers/specs/2026-07-29-ipw-reg-balance-design.md`.
- Produces: requisitos ejecutables para comandos, CSV, equivalencia y gráfico.

- [ ] Añadir una prueba que exija literalmente `reg y2 D [pw=w_ate], vce(robust)` y `reg y2 D [pw=w_att], vce(robust)` en do-file y práctica.
- [ ] Añadir una prueba que exija en `ipw_estimates.csv` las filas `reg ponderada` para ATE y ATT y compruebe `abs(reg-Hajek)<1e-8` y `abs(reg-teffects_ipw)<1e-8`.
- [ ] Añadir una prueba de esquema largo para `ipw_balance.csv`: `estimand,covariate,metric,raw,weighted`, dos estimandos, seis covariables y métricas `smd`/`variance_ratio`.
- [ ] Añadir una prueba que exija `ipw_balance_ate_att.png`, `tebalance summarize`, `tebalance density`, la frase “balance observable no demuestra CIA” y orientación para reespecificar sin mirar el efecto.
- [ ] Ejecutar `python3 -m pytest -q tests/test_ipw_pedagogy_contract.py` y confirmar fallas específicas nuevas.
- [ ] Commit: `test: require IPW regression equivalence and balance audit`.

### Task 2: Generar resultados de Stata

**Files:**
- Modify: `dofile/16_PSM_IPW_Sinteticos/02_ipw_stata.do`
- Modify: `dofile/16_PSM_IPW_Sinteticos/results/ipw_estimates.csv`
- Modify: `dofile/16_PSM_IPW_Sinteticos/results/ipw_balance.csv`
- Create: `dofile/16_PSM_IPW_Sinteticos/ipw_balance_ate_att.png`

**Interfaces:**
- Consumes: `base6.dta`, `w_ate`, `w_att`, `$Xmust`.
- Produces: dos coeficientes `reg`, balance largo ATE/ATT y gráfico canónico.

- [ ] Después de los cálculos Hájek, ejecutar las dos regresiones ponderadas con intercepto y `vce(robust)`; almacenar `_b[D]` y `_se[D]` en `ipw_estimates.csv` con estimador `reg ponderada`.
- [ ] Para cada combinación de estimando y covariable, calcular diferencia estandarizada cruda, diferencia estandarizada ponderada, razón de varianza cruda y razón de varianza ponderada. Para ATE usar `w_ate`; para ATT usar `w_att`.
- [ ] Exportar 24 filas a `ipw_balance.csv` con esquema `estimand,covariate,metric,raw,weighted`.
- [ ] Construir `ipw_balance_ate_att.png` con $|SMD|$ crudo y ponderado, paneles ATE/ATT y línea de referencia 0.10.
- [ ] Ejecutar `teffects ipw ..., ate`, `tebalance summarize`, `tebalance density personas`; repetir con `atet`.
- [ ] Ejecutar Stata 19 en batch y verificar ausencia de `r(...)` en el final del log.
- [ ] Validar CSV con Python: columnas exactas, valores finitos y equivalencias dentro de `1e-8`.
- [ ] Ejecutar el contrato focalizado y commit `feat: add weighted regression and balance diagnostics`.

### Task 3: Integrar la secuencia pedagógica

**Files:**
- Modify: `16-PSM_IPW_SinteticosConsolidado.Rmd`
- Modify: `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/claves_privadas/15_IPW_clave.md`

**Interfaces:**
- Consumes: nuevos CSV y `ipw_balance_ate_att.png`.
- Produces: explicación pública y ajuste privado de la rúbrica.

- [ ] Actualizar validaciones de esquema y número de filas en el chunk oculto.
- [ ] Tras HT/Hájek, añadir “El mismo resultado con `reg`”: código ATE/ATT, tabla dinámica con Hájek–`reg`–`teffects ipw` y advertencia sobre inferencia.
- [ ] Reemplazar la sección de balance por “Postestimación: ¿quedó balanceado?” con comandos ATE/ATET, tablas dinámicas de SMD y razones de varianza y el nuevo gráfico.
- [ ] Explicar 0.10 como referencia descriptiva, no prueba; revisar covariables individualmente; balance observable no demuestra CIA.
- [ ] Explicar el ciclo cuando persiste desequilibrio: revisar temporalidad y teoría causal, añadir no linealidades/interacciones justificadas, reestimar PS/pesos, repetir soporte y balance, sin seleccionar por el efecto.
- [ ] Ampliar IPW-S3 y su rúbrica privada para evaluar ese ciclo, conservando cuatro preguntas públicas.
- [ ] Ejecutar el contrato focalizado y render mínimo con `/private/tmp/bookdown-ipw-preview.yml`.
- [ ] Commit: `feat: teach IPW regression equivalence and post-balance`.

### Task 4: Verificación integral

**Files:**
- Test: `tests/test_ipw_pedagogy_contract.py`
- Output outside repo: `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/libro_cortes_ipw_review_20260729/`

**Interfaces:**
- Consumes: implementación completa.
- Produces: evidencia reproducible y preview local.

- [ ] Ejecutar nuevamente Stata 19 y comparar los CSV regenerados con los versionados.
- [ ] Ejecutar `python3 -m pytest -q tests/test_ipw_pedagogy_contract.py`.
- [ ] Ejecutar `python3 -m pytest -q` y `git diff --check`.
- [ ] Renderizar los capítulos IPW al directorio de revisión fechado.
- [ ] Auditar en HTML: materiales antes de lecturas y metas; tabla de equivalencia; balance ATE/ATT; gráfico; cuatro preguntas; cero clave privada.
- [ ] Confirmar `git status --short -- docs` sin cambios nuevos y reportar que no se publicó.
