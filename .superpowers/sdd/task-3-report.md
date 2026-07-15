# Task 3 — Informe de implementación

## Estado

Completado. `dofile/04_ParametrosStata/04_stata.do` es la fuente canónica de los resultados puntuales, los draws y resúmenes Monte Carlo, y los tres gráficos. No se modificó el Rmd ni `04_data.dta`.

## Cambios implementados

- Se genera `X=(_n>4)` antes de cualquier tratamiento o expansión y se reportan `CATE_X0` y `CATE_X1`.
- Un `postfile` guarda ATE, ATT, ATU, ambos CATE, NAIVE, sesgo respecto a ATT, desviación respecto a ATE y el coeficiente de `D` en la regresión, para datos originales, datos duplicados y una aleatorización única.
- La duplicación conserva perfiles idénticos: pasa de 8 a 80.000 observaciones nominales sin alterar los estimandos puntuales.
- `one_rep` es un programa `rclass`; `simulate` ejecuta 1.000 repeticiones con selección y 1.000 con aleatorización sobre una población de 80.000 observaciones.
- Se guardan los 2.000 draws en una sola base y se calculan N, media, desviación estándar, p5, mediana y p95 por escenario.
- Los dos histogramas usan el mismo eje horizontal y colores consistentes; el tercer gráfico compara las dos densidades.
- El contrato se amplió para exigir los tres gráficos y la estructura/conteo de los resultados Monte Carlo.

## TDD y depuración

La fase RED produjo tres fallos esperados: no existían los CSV, la base de draws ni el gráfico comparativo. La primera ejecución real de Stata encontró `r(110)` porque el temporal de población retenía `y` y `tau`, mientras `one_rep` debía generarlas en cada repetición. Se corrigió la causa en el origen: el temporal ahora se guarda sin esas variables. Las ejecuciones posteriores terminaron con `Pipeline canónico completado` y sin códigos `r(...)`.

## Ejecución y pruebas

Comando de ejecución real:

```text
/Applications/StataNow/StataSE.app/Contents/MacOS/stata-se -b do 04_stata.do
```

Directorio de trabajo: `dofile/04_ParametrosStata/`.

Contrato Task 3:

```text
/private/tmp/libro_cortes_rct_venv/bin/python -m pytest -q \
  ../../tests/test_parametros_stata_contract.py::test_canonical_do_file_exports_exactly_the_page_artifacts \
  ../../tests/test_parametros_stata_contract.py::test_results_schema \
  ../../tests/test_parametros_stata_contract.py::test_monte_carlo_outputs_have_complete_scenarios \
  ../../tests/test_parametros_stata_contract.py::test_all_three_stata_graphs_exist
```

Resultado: `4 passed`.

## Auto-revisión de cifras

- Datos originales y duplicados: ATE = 0.75, ATT = 0.75, ATU = 0.75, NAIVE = 6.75 y sesgo respecto a ATT = 6.00.
- Heterogeneidad pretratamiento: CATE(X=0) = 1.25 y CATE(X=1) = 0.25.
- Aleatorización única: NAIVE = 0.750989 y NAIVE−ATE = 0.000989; no se afirma igualdad exacta.
- Monte Carlo con selección: media del sesgo = 3.940896, DE = 0.015928, p5 = 3.915118 y p95 = 3.966087.
- Monte Carlo con aleatorización: media del sesgo = −0.000405, DE = 0.021963, p5 = −0.037663 y p95 = 0.034870.
- Cada escenario contiene exactamente 1.000 repeticiones.

## Archivos producidos

- `results/parameters_results.csv`
- `results/monte_carlo_summary.csv`
- `results/monte_carlo_draws.dta`
- `sesgo_con_seleccion.png`
- `sesgo_con_aleatorizacion.png`
- `comparacion_escenarios.png`
- `04_stata.log`

## Preocupaciones residuales

- `04_stata.log` está cubierto por la regla global `*.log` de `.gitignore`; se añade al commit con `git add -f` porque el brief lo exige.
- El contrato completo contiene pruebas del Rmd asignadas a Task 4; la verificación de esta tarea se limita deliberadamente a las cuatro pruebas del pipeline Stata.
