# Task 1 report: contratos académicos y estructurales

## Estado

DONE_WITH_CONCERNS

## Commit

- `1035a40 test: define causal parameters chapter contracts`
- Alcance del commit: exclusivamente los tres archivos de pruebas solicitados (83 inserciones).

## Implementación

Se añadieron, sin modificar capítulos ni archivos de producción:

- `tests/test_parametros_theory_contract.py`: fija el orden aprobado del capítulo teórico, exige CATE y sus identidades de agregación, conserva los dos videos y rechaza el prompt largo.
- `tests/test_parametros_stata_contract.py`: exige descargas completas al inicio, consumo de resultados canónicos, ausencia de consola transcrita/código incompleto y el esquema mínimo del CSV.
- `tests/test_potential_outcomes_notation.py`: prohíbe globalmente `Y_i(1)`, `Y_i(0)`, `Y(1)` y `Y(0)` en los capítulos Rmd.

## TDD y pruebas

Comando ejecutado antes y después del commit:

```bash
/private/tmp/libro_cortes_rct_venv/bin/python -m pytest -q tests/test_parametros_theory_contract.py tests/test_parametros_stata_contract.py tests/test_potential_outcomes_notation.py
```

Resultado fresco posterior al commit: `7 failed in 0.14s`, que es el estado RED requerido.

Los fallos se deben a las razones esperadas:

1. La estructura y los encabezados aprobados todavía no existen en `03-Parametros.Rmd`.
2. CATE y las identidades de agregación todavía no aparecen.
3. El prompt largo de ChatGPT todavía está presente; los dos videos sí se conservan.
4. La página práctica todavía no tiene la estructura de materiales/objetivos exigida.
5. La página todavía no consume los dos CSV canónicos.
6. `results/parameters_results.csv` todavía no existe, por lo que tampoco puede validarse su esquema.
7. La notación abreviada prohibida todavía aparece en 13 capítulos.

También se ejecutó `git diff --cached --check` antes del commit, sin errores.

## Auto-revisión

- Los contenidos coinciden literalmente con los contratos especificados en el brief.
- Las pruebas leen archivos reales y no usan mocks.
- No se añadió código de producción ni se intentó hacer pasar los contratos.
- `git show --name-only HEAD` confirma que el commit contiene solo los tres tests solicitados.
- El worktree sucio preexistente se preservó: ninguno de sus artefactos fue modificado, añadido al índice o incluido en el commit.
- No encontré defectos accionables en los tres tests frente al brief.

## Preocupaciones

- El brief anticipa 78 usos de notación abreviada en 13 capítulos, pero el conteo reproducible del árbol actual es **138 usos en 13 capítulos**. La cantidad de capítulos coincide y el contrato detecta correctamente todos los usos; parece que el número 78 quedó desactualizado.
- Los siete fallos son intencionales y deben permanecer hasta que las tareas posteriores implementen los capítulos y los resultados canónicos.

## Corrección posterior a revisión

Se corrigieron los hallazgos **Important** y **Minor** sin modificar capítulos ni el do-file:

- El contrato práctico ahora exige que `Materiales para la clase` sea el primer H2 después del título, que cada archivo requerido exista y aparezca como destino de un enlace Markdown.
- El contrato teórico prohíbe secciones de materiales/descargas y enlaces a archivos descargables.
- El contrato de reproducibilidad lee `04_stata.do` y exige que exporte exactamente los dos CSV canónicos y los dos gráficos dentro de `results/`.
- Se exigen definiciones completas de ATE, ATT, ATU y CATE con la notación `Y_i(D=...)`.
- Se fijaron las dos URLs originales exactas de YouTube y se exige una sección breve de actividad con estimando, contrafactual faltante, supuesto y dos amenazas, sin el prompt largo.
- La expresión global ahora detecta variantes abreviadas con espacios, otros subíndices y argumento `d`; una prueba de control confirma que no rechaza `Y_{it}(D=...)`.

Comando ejecutado:

```bash
/private/tmp/libro_cortes_rct_venv/bin/python -m pytest -q tests/test_parametros_theory_contract.py tests/test_parametros_stata_contract.py tests/test_potential_outcomes_notation.py
```

Salida resumida fresca: `9 failed, 2 passed in 0.31s`. Los tres archivos permanecen en RED por los incumplimientos esperados: estructura/definiciones/actividad teórica pendientes; materiales, enlaces y exportaciones canónicas pendientes; y notación abreviada todavía presente en 15 capítulos. Los dos checks que pasan validan la ausencia actual de descargas en teoría y que la regex distingue correctamente las abreviaturas prohibidas de la notación temporal válida.
