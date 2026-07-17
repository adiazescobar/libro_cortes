# Task 2 — Corrección DID y resultados canónicos

## Estado

DONE

## Alcance implementado

- `08-DID.Rmd`: la identificación del ATT queda condicionada a tendencias
  paralelas, consistencia, ausencia de anticipación, composición estable y
  ausencia de interferencia relevante, preservando la notación
  `Y_i(D=1)`/`Y_i(D=0)`.
- `08-DIDStata.Rmd`: `base3.dta` se describe como cortes transversales
  repetidos; la primera diferencia queda como equivalencia teórica para un
  panel genuino; la tabla presenta solo los tres estimadores válidos.
- `dofile/08_DID/08_DID.do`: se retiraron tanto el bloque visible que construía
  un identificador artificial como su fila en el export canónico. El flujo pasa
  de la regresión DiD válida a `hospdd`.
- `dofile/08_DID/results/did_resultados.csv`: regenerado por Stata con diez
  escenarios, sin `did_primeras_diferencias`.
- `dofile/08_DID/results/did_verificacion.csv`: regenerado por la verificación
  independiente con tres estados `PASS` (sin cambio sustantivo en Git).

No se cambiaron títulos ni archivos de `docs/`. Los cambios ajenos presentes en
el árbol de trabajo se dejaron intactos y fuera del commit.

## Contratos de prueba

El baseline `10b22da` contenía una contradicción: `PRACTICE_REQUIRED` exigía el
literal `reg D.y D`, mientras el contrato del panel ficticio prohibía ese mismo
literal. Se retiró únicamente esa exigencia obsoleta; las cinco mutaciones que
comprueban la prohibición siguen activas.

El snapshot se ajustó quirúrgicamente: nueve hashes de prosa y dos de código
vinculados a los supuestos incompletos, los cuatro métodos y el panel artificial
se reemplazaron por ocho hashes de prosa y uno de código del contenido aprobado.
No se regeneró el fixture completo ni se alteraron las demás unidades.

## Evidencia

- Stata:
  `/Applications/StataNow/StataSE.app/Contents/MacOS/stata-se -b do dofile/08_DID/08_DID.do`
  terminó con código 0.
- `08_DID.log`: sin códigos de error `r(#)`.
- CSV canónico: 11 líneas (encabezado + 10 escenarios) y ninguna fila
  `did_primeras_diferencias`.
- Python:
  `/private/tmp/libro_cortes_rct_venv/bin/python dofile/08_DID/verificar_did.py`
  produjo tres `PASS`.
- GREEN focal:
  `/private/tmp/libro_cortes_rct_venv/bin/python -m pytest -q tests/test_did_pedagogy_contract.py dofile/08_DID/tests/test_did_results.py`
  produjo `33 passed`.

## Self-review

- La derivación y la ecuación de primeras diferencias permanecen visibles.
- No queda ningún comando ejecutable que cree un panel en `base3.dta`.
- El export usa identificadores estables y contiene exactamente los tres
  estimadores básicos válidos.
- Los títulos se conservaron sin cambios.
- `git diff --check` no reporta errores.
- Las diferencias numéricas mínimas en `ptrends` y `granger` provienen de la
  regeneración de Stata y no cambian su interpretación.

## Preocupaciones

Ninguna pendiente dentro del alcance.
