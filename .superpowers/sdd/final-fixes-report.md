# Informe de correcciones finales

Fecha: 2026-07-15

## Alcance

- `01-intro.Rmd`: el diagnóstico de RDD ahora exige ausencia plausible de ordenamiento preciso y continuidad de resultados potenciales; el puente respeta la secuencia Introducción → Stata → Parámetros.
- `02-StataBasics.Rmd`: equivalencias ejecutables, precisión sobre scalars numéricos/string, tildes, preparación del entorno, modificación segura con `generate`/`replace`/`keep`/`drop`, checklist final y patrón objetivo → comando → salida → interpretación → ejercicio para macros, loops y programas.
- `00-PruebaEntrada.Rmd` y `docs/audits/prueba_entrada_academica.csv`: contrafactual definido para la misma unidad bajo la condición alternativa.
- Pruebas de contrato ampliadas para cristalizar estas decisiones.

## TDD: rojo

Comando exacto:

```sh
python3 -m pytest tests/test_intro_contract.py tests/test_stata_basics_contract.py tests/test_entrada_academica.py -q
```

Salida antes de modificar los Rmd:

```text
..FF....FFFFF...F                                                        [100%]
8 failed, 9 passed in 0.30s
```

Los ocho fallos correspondieron a RDD, cronología, tildes, equivalencias, scalars, flujo elemental de datos, patrones pedagógicos y contrafactual del quiz.

## TDD: verde focalizado

Mismo comando, después de implementar:

```text
.................                                                        [100%]
17 passed in 0.06s
```

## Suite completa

Comando exacto:

```sh
python3 -m pytest -q
```

Salida:

```text
.......................................                                  [100%]
39 passed in 4.22s
```

## Render aislado

Comando exacto:

```sh
mkdir -p /private/tmp/libro_cortes_final_renders
Rscript -e 'for (f in c("00-PruebaEntrada.Rmd", "01-intro.Rmd", "02-StataBasics.Rmd")) { message("RENDER ", f); rmarkdown::render(f, output_format = "html_document", output_file = sub("[.]Rmd$", ".html", f), output_dir = "/private/tmp/libro_cortes_final_renders", knit_root_dir = getwd(), quiet = FALSE, clean = TRUE, envir = new.env(parent = globalenv())) }'
```

Salidas finales:

```text
Output created: /private/tmp/libro_cortes_final_renders/00-PruebaEntrada.html
Output created: /private/tmp/libro_cortes_final_renders/01-intro.html
Output created: /private/tmp/libro_cortes_final_renders/02-StataBasics.html
```

Tamaños verificados:

```text
637K  /private/tmp/libro_cortes_final_renders/00-PruebaEntrada.html
4.4M  /private/tmp/libro_cortes_final_renders/01-intro.html
638K  /private/tmp/libro_cortes_final_renders/02-StataBasics.html
```

Pandoc emitió advertencias no bloqueantes al intentar incrustar recursos externos de Spotify, YouTube y el badge de Colab porque el entorno no resolvió esos dominios. Los tres HTML se generaron con éxito y los recursos locales sí fueron procesados.

## Control de alcance

Se preservaron los cambios ajenos y los artefactos no rastreados. El commit incluye solamente los tres Rmd corregidos, la matriz académica, las tres pruebas de contrato y este informe.
