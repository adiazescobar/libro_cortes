# Informe — ampliación Task 2 (teoría)

## Alcance implementado

- Se modificó únicamente `03-Parametros.Rmd` en el contenido docente.
- Se añadieron los seis bloques visuales solicitados reutilizando `boxinfo`,
  `boxnote` y `boxwarning`, sin alterar las demostraciones existentes.
- Se añadieron tres bloques `boxejercicio`, con los códigos únicos y ordenados
  `T-P1`, `T-P2` y `T-P3`, sus puntajes sugeridos y sin respuesta, pista ni
  contenido desplegable.
- No se modificaron `04-ParametrosStata.Rmd`, CSS, claves, videos, notación ni
  estructura de secciones.

## TDD y pruebas

Fase RED:

```text
/private/tmp/libro_cortes_rct_venv/bin/python -m pytest -q \
  tests/test_parametros_theory_contract.py \
  tests/test_parametros_pedagogy_contract.py
```

Resultado inicial: `4 failed, 8 passed`. Las dos fallas teóricas correspondían
a los bloques y preguntas ausentes; las otras dos eran los contratos prácticos
que el brief declara fuera de alcance.

Fase GREEN:

```text
/private/tmp/libro_cortes_rct_venv/bin/python -m pytest -q \
  tests/test_parametros_theory_contract.py \
  tests/test_parametros_pedagogy_contract.py
```

Resultado: `2 failed, 10 passed`. Las únicas fallas restantes son las dos de
práctica previstas por el brief.

Prueba focal pertinente:

```text
/private/tmp/libro_cortes_rct_venv/bin/python -m pytest -q \
  tests/test_parametros_theory_contract.py \
  tests/test_parametros_pedagogy_contract.py \
  -k 'theory or answer_marker or student_material'
```

Resultado: `10 passed, 2 deselected`.

## Tejido aislado

El comando literal del brief llegó al tejido del Markdown, pero falló al
resolver `libs/jquery-3.6.0`: la configuración preexistente espera que las
dependencias sean descendientes del directorio de salida, condición que no se
cumple al escribir directamente en `/private/tmp`.

Se verificó el capítulo mediante una salida HTML autocontenida:

```text
Rscript -e "rmarkdown::render(
  '03-Parametros.Rmd',
  output_format=rmarkdown::html_document(self_contained=TRUE),
  output_dir='/private/tmp/libro_cortes_parametros_ampliado/theory',
  knit_root_dir=getwd(), clean=TRUE,
  envir=new.env(parent=globalenv()))"
```

Resultado: exit 0 y archivo
`/private/tmp/libro_cortes_parametros_ampliado/theory/03-Parametros.html`
(629 KB). La inspección del HTML confirmó una aparición de cada código de
pregunta y de los títulos pedagógicos muestreados. Pandoc emitió advertencias
no bloqueantes al no poder descargar los dos videos de YouTube en el entorno
sin red; ambos iframes se conservan.

## Auto-revisión de preguntas

- `T-P1` obliga a elegir el parámetro a partir de la población decisional y a
  escribirlo en resultados potenciales; no revela que la elección pertinente
  es el ATT.
- `T-P2` entrega cifras como datos del enunciado y pide reconstruir la
  descomposición y el signo del sesgo; no muestra el cálculo ni su resultado.
- `T-P3` separa amenazas a independencia, positividad y SUTVA, y exige una
  modificación de diseño; no anticipa el diagnóstico ni una propuesta.

## Preocupaciones

- Las dos fallas prácticas permanecen deliberadamente fuera del alcance de
  Task 2.
- El comando literal de tejido externo es incompatible con el `lib_dir`
  preexistente; el tejido autocontenido ofrece la verificación aislada sin
  modificar configuración ni CSS.
