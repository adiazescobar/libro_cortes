# TWFE Empirical Pedagogical Sequence Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Hacer visible la heterogeneidad del DGP y convertir la tabla de supuestos en una síntesis posterior a la presentación de los estimadores modernos.

**Architecture:** Se modifica únicamente la narrativa de `11-TWFEStata.Rmd`; los resultados canónicos del do-file permanecen porque ya generan el DGP correcto. Las pruebas verifican contenido, orden, conservación de activos y render.

**Tech Stack:** Bookdown/R Markdown, Stata syntax, pytest.

## Global Constraints

- Conservar URLs, título, descargas, seis gráficas, resultados y siete preguntas.
- Mantener HonestDiD como sensibilidad y nunca alimentarlo con TWFE contaminado.

---

### Task 1: Hacer auditable el DGP y reordenar la síntesis

**Files:**
- Modify: `11-TWFEStata.Rmd`
- Test: `tests/test_twfe_pedagogy_contract.py`

**Interfaces:**
- Consumes: `dofile/11_TWFE/results/twfe_staggered.csv` y las seis figuras existentes.
- Produces: capítulo empírico con secuencia DGP → problema → soluciones → síntesis → diagnóstico.

- [ ] Añadir al código visible la construcción de `tau`, `Y0` y `Y`, y comprobaciones por cohorte/exposición.
- [ ] Mover la matriz de supuestos después de las cinco alternativas modernas.
- [ ] Ubicar el flujo diagnóstico después de la matriz y antes de HonestDiD.
- [ ] Ejecutar `pytest -q tests`, `git diff --check` y renderizar el libro en `/private/tmp/libro_cortes_twfe_pedagogy_review`.
