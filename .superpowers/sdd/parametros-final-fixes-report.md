# Correcciones finales — Parámetros causales

Fecha: 2026-07-15

## Cambios

- `04-ParametrosStata.Rmd` incorpora objetivos, conocimientos previos y puente al capítulo de RCT.
- Las frecuencias y medias observadas por tratamiento se exportan desde Stata como `N_D0`, `N_D1`, `MEDIA_Y_D0` y `MEDIA_Y_D1` en el artefacto canónico.
- Todas las cifras empíricas visibles del capítulo práctico se interpolan desde los CSV canónicos mediante objetos R.
- La etiqueta visible del notebook dice Python, manteniendo `04_phyton.ipynb`; los dos videos teóricos tienen títulos descriptivos y `$N$` usa delimitadores matemáticos.
- `01-intro.Rmd`, `18-IV.Rmd` y `20-RDD.Rmd` sustituyen la notación genérica `D=d` por pares explícitos `D=1`/`D=0`.
- Los contratos cubren estructura y secuencia, enlaces, etiqueta Python, títulos de iframes, ausencia de cifras transcritas, exportaciones de Stata y variantes prohibidas de notación.

## TDD

Pruebas RED focalizadas:

```bash
/private/tmp/libro_cortes_rct_venv/bin/python -m pytest -q \
  tests/test_parametros_theory_contract.py \
  tests/test_parametros_stata_contract.py \
  tests/test_potential_outcomes_notation.py
```

Resultado inicial: 5 fallos por artefactos/estructura/etiqueta/cifras/notación y 1 fallo separado por títulos faltantes en los iframes. Tras la implementación y regeneración: `18 passed`.

## Stata 19

Ejecutado desde `dofile/04_ParametrosStata`:

```bash
/Applications/StataNow/StataSE.app/Contents/MacOS/stata-se -b do 04_stata.do
```

Resultado: exit 0; el log cerró el 15-jul-2026 a las 11:54:30 con `Pipeline canónico completado`. Se regeneraron CSV, DTA, log y gráficos. El log se normalizó después de la ejecución para retirar espacios finales generados por Stata sin cambiar su contenido.

## Suite y limpieza

```bash
/private/tmp/libro_cortes_rct_venv/bin/python -m pytest -q
# 59 passed in 4.13s

git diff --check
# sin salida
```

Las búsquedas de notación genérica prohibida y de las frases numéricas transcritas tampoco devolvieron coincidencias.

## Render y HTML

```bash
Rscript -e "bookdown::render_book('index.Rmd', output_dir='/private/tmp/libro_cortes_parametros_render')"
```

Resultado: exit 0 y `Output created: /private/tmp/libro_cortes_parametros_render/index.html`. Como en la ejecución anterior, `bookdown` dejó los HTML divididos en la raíz; solo los dos capítulos de parámetros se copiaron al directorio temporal para comprobarlos. No se publicó ni copió contenido a `docs/`.

Checks HTML básicos confirmaron:

- objetivos, conocimientos previos y puente en la práctica;
- etiqueta visible `Notebook de Python`;
- títulos descriptivos de ambos iframes;
- cifras R evaluadas, sin código inline residual;
- existencia no vacía de los siete materiales descargables principales.

## Preocupación residual

Persiste la peculiaridad preexistente de `bookdown`: el `output_dir` recibe el índice, pero los HTML divididos se generan en la raíz. La vista previa temporal quedó completa para estos dos capítulos y `docs/` permanece sin publicar.
