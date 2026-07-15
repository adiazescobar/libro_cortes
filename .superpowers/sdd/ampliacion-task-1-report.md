# Ampliación pedagógica — Task 1

## Alcance

- Se creó `tests/test_parametros_pedagogy_contract.py` con contratos RED para seis bloques teóricos, tres preguntas teóricas reales, doce etapas prácticas, diez bloques prácticos, cuatro preguntas prácticas y privacidad de la clave docente.
- Se reforzó `tests/test_parametros_stata_contract.py` para inspeccionar exclusivamente bloques cercados como `stata` y exigir el flujo ejecutable completo, ambas reglas de asignación y ausencia de `...`.
- No se modificaron `03-Parametros.Rmd`, `04-ParametrosStata.Rmd`, `_bookdown.yml`, claves externas ni otros archivos de producción.

## Diseño y auto-revisión

- Los códigos `T-P1`–`T-P3` y `S-P1`–`S-P4` sólo cuentan si aparecen dentro de un bloque pedagógico `.box`; una mención narrativa o una lista de códigos no satisface el contrato.
- Cada pregunta debe ocupar su propio bloque y contener su código una sola vez. Las teóricas exigen `puntaje sugerido` y prohíben marcadores de respuesta, solución, pista o contenido desplegable.
- Los comandos sólo cuentan dentro de cercas `stata`, anclados al inicio de una línea ejecutable. Una mención en prosa no satisface el contrato.
- La prohibición de `...` se aplica dentro de cada bloque Stata, además del contrato global ya existente.
- Se preservaron todos los cambios ajenos que ya estaban presentes en el árbol de trabajo.

## Verificación RED

Comando solicitado:

```text
/private/tmp/libro_cortes_rct_venv/bin/python -m pytest -q tests/test_parametros_pedagogy_contract.py tests/test_parametros_stata_contract.py
```

Resultado: **6 failed, 11 passed**. Los seis fallos son los esperados por ausencia de bloques, preguntas, etapas, comandos completos y reglas de asignación en el material actual. La prueba de privacidad y los contratos ya satisfechos permanecen verdes.

Regresión canónica:

```text
/private/tmp/libro_cortes_rct_venv/bin/python -m pytest -q tests/test_parametros_theory_contract.py tests/test_parametros_stata_contract.py -k 'not complete_executable and not both_executable'
```

Resultado: **16 passed, 2 deselected**.

Validación sintáctica:

```text
/private/tmp/libro_cortes_rct_venv/bin/python -m py_compile tests/test_parametros_pedagogy_contract.py tests/test_parametros_stata_contract.py
```

Resultado: exit code 0.

## Preocupaciones

- El brief exige explícitamente `generate y` y `generate tau`; por ello las abreviaturas Stata `gen y` y `gen tau` no satisfacen el contrato. Esta rigidez es intencional para la presentación pedagógica aprobada.
- La regla con selección ya no exige literalmente `generate D = X`: acepta `generate`, `gen` o `replace` cuando la expresión para `D` depende de `X` o `yd0` mediante una condición o una probabilidad. La regla aleatoria exige una instrucción ejecutable con `runiform() < .5` (también acepta `0.5` o `1/2`).

## Correcciones posteriores a revisión

- Los códigos se detectan globalmente con los patrones abiertos `T-P\d+` y `S-P\d+`. El contrato rechaza códigos fuera del rango, duplicados, códigos fuera de un bloque pedagógico y más de un código por bloque.
- Tanto las preguntas teóricas como las prácticas rechazan, sin distinguir mayúsculas, marcadores de respuesta, solución, pista, `details`, `hide(` y `Ver respuesta`.
- Las comprobaciones globales de privacidad y contenido desplegable tampoco distinguen mayúsculas.
- Se añadieron casos de control del matcher de asignación para confirmar que menciones narrativas y `generate D = X` no bastan, mientras que condiciones y probabilidades dependientes de `X` o `yd0` sí cuentan.

Verificación focal posterior a revisión:

```text
/private/tmp/libro_cortes_rct_venv/bin/python -m pytest -q tests/test_parametros_pedagogy_contract.py tests/test_parametros_stata_contract.py
```

Resultado: **6 failed, 12 passed**. Los seis fallos siguen siendo el RED intencional por contenido pedagógico aún no implementado; las doce pruebas restantes, incluida la autoprueba de reglas de asignación, pasan.

Validación adicional:

```text
git diff --check
/private/tmp/libro_cortes_rct_venv/bin/python -m py_compile tests/test_parametros_pedagogy_contract.py tests/test_parametros_stata_contract.py
```

Resultado: ambos comandos terminaron con exit code 0 y sin salida de error.
