# Informe — Tarea 3: Introducción

## Alcance implementado

- Se reorganizó `01-intro.Rmd` en nueve secciones fundacionales ordenadas.
- Se unificó la motivación y se ubicó la formulación de la pregunta antes de resultados potenciales.
- Se preservaron el cuento de Borges, Progresa, imágenes, checklist, Spotify, YouTube y ejemplos sustantivos.
- Se corrigieron la fecha de Progresa, la errata, el ejemplo hospitalario y la interpretación del grupo de control.
- Se incorporó la descomposición de la diferencia observada y se remitió su desarrollo al capítulo Parámetros causales.
- Se agregó una tabla compacta de familias de identificación y el puente al capítulo de Stata.
- No se modificaron la Prueba de entrada ni Stata Basics y no se añadió un bloque de descargas.

## Pruebas

- Contrato específico: `2 passed`.
- Suite completa: `27 passed`.
- Render aislado HTML: exitoso en `/private/tmp/task3-intro.html`.
- `git diff --check`: limpio después de corregir un espacio final detectado en la auto-revisión.

## Auto-revisión

- Los nueve encabezados exigidos aparecen una sola vez y en el orden contractual.
- La notación usa consistentemente $Y_i(1)$ y $Y_i(0)$.
- El control se describe como aproximación al contrafactual promedio solo bajo comparabilidad.
- Las comparaciones simples se presentan como descriptivas, no como causalmente válidas por sí solas.
- El render emitió únicamente advertencias de red al intentar acceder a los embeds externos de Spotify y YouTube; el documento se generó.

## Corrección posterior de revisión académica

- Se reemplazó la pregunta diagnóstica de RDD por una formulación que explicita las dos condiciones relevantes: ausencia de ordenamiento preciso alrededor del umbral y continuidad de los resultados potenciales.
- Se añadió una prueba contractual para fijar la nueva formulación y excluir la pregunta anterior, que era demasiado fuerte.
- Comando: `/private/tmp/libro_cortes_rct_venv/bin/python -m pytest tests/test_intro_contract.py -q`
- Salida: `3 passed in 0.04s`.
