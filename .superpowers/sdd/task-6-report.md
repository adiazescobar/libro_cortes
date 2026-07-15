# Task 6 — cierre, render y revisión final

Fecha: 2026-07-15

## Ajustes finales

- `03-Parametros.Rmd`: los dos CATE del primer ejercicio quedaron en delimitadores matemáticos.
- `tests/test_parametros_stata_contract.py`: el contrato lee `monte_carlo_draws.dta` y verifica columnas exactas (`escenario`, `rep`, `sesgo`), 2.000 filas, 1.000 por escenario, `rep` única de 1 a 1.000 y ausencia de valores perdidos.
- `tests/test_potential_outcomes_notation.py`: se añadieron los fixtures abreviados `Y(D)` y `Y_i(D)`.
- `dofile/04_ParametrosStata/04_stata.do`: los tres gráficos usan el mismo soporte y etiquetan su extremo negativo (`xmin`), cero y la secuencia positiva hasta `xmax`.
- `style.css`: en pantallas de hasta 700 px, las tablas anchas quedan dentro del ancho disponible y ofrecen scroll horizontal interno.

## Stata y artefactos reproducibles

Comando ejecutado desde `dofile/04_ParametrosStata`:

```bash
/Applications/StataNow/StataSE.app/Contents/MacOS/stata-se -b do 04_stata.do
```

El log cerró el 15-jul-2026 a las 11:32:41 con `Pipeline canónico completado`. Se regeneraron `04_stata.log`, los tres PNG y los DTA de resultados. `monte_carlo_draws.dta` contiene 2.000 filas, 1.000 por escenario, `rep=1,...,1000` sin duplicados dentro de escenario y cero valores perdidos.

## Pruebas y limpieza

```bash
/private/tmp/libro_cortes_rct_venv/bin/python -m pytest -q
# 54 passed in 5.36s

git diff --check
# sin salida

rg -n "\.pull-(left|right)|PROMPT DE CHATGPT|\.\.\." 03-Parametros.Rmd 04-ParametrosStata.Rmd
# sin coincidencias
```

## Render y HTML

Comando final:

```bash
Rscript -e "bookdown::render_book('index.Rmd', output_dir='/private/tmp/libro_cortes_parametros_render')"
```

Resultado: exit 0 y `Output created: /private/tmp/libro_cortes_parametros_render/index.html`. Este proyecto tiene una peculiaridad preexistente: `bookdown` genera los HTML divididos en la raíz y solo copia parte al `output_dir`; por ello se copiaron los HTML recién renderizados al directorio temporal con `cp -p ./*.html /private/tmp/libro_cortes_parametros_render/`. No se copió ni publicó nada en `docs/`.

Vistas previas:

- `http://127.0.0.1:8765/parametros-causales-teoria.html`
- `http://127.0.0.1:8765/parametros-causales-stata.html`
- archivos locales en `/private/tmp/libro_cortes_parametros_render/`

El servidor se inició con `python3 -m http.server 8765 --bind 127.0.0.1`. Los siete enlaces descargables de la práctica respondieron HTTP 200: do-file, base, script R, notebook, log y los dos CSV.

## Revisión visual

- Escritorio 1440x1000: encabezados, ecuaciones, tablas, tres gráficos y navegación legibles; sin desbordamiento global.
- Móvil 390x844: `body` y documento permanecen en 390 px; videos de 323 px dentro de un contenedor de 360 px; imágenes ajustadas al contenedor.
- La primera inspección detectó columnas recortadas en tablas anchas. Tras el ajuste CSS, cada tabla ocupa 360 px y conserva su contenido completo mediante `overflow-x:auto` (`scrollWidth` de 418 o 595 px según la tabla), sin ampliar el documento.
- Capturas: `/private/tmp/parametros-teoria-desktop.png`, `/private/tmp/parametros-stata-desktop.png`, `/private/tmp/parametros-teoria-mobile-fresh.png` y `/private/tmp/parametros-stata-mobile-table-fixed2.png`.
- No aparece sintaxis de slides ni prompts residuales. Los materiales descargables están antes de los objetivos solo en el capítulo práctico.

## Auditoría académica

- ATE, ATT, ATU y CATE usan poblaciones objetivo distintas y resultados potenciales consistentes; CATE condiciona en una variable pretratamiento.
- Las dos identidades de agregación del ATE están presentes y son correctas.
- La descomposición es `naïve = ATT + selección`; el texto interpreta correctamente sesgo positivo, negativo o nulo.
- Independencia incondicional/condicional se acompaña de positividad y soporte común; no se confunde con los supuestos de IV, RDD o DiD.
- La sección antes-después separa tiempo y tratamiento y define correctamente el contrafactual posterior.
- SUTVA cubre ausencia de interferencia y tratamiento bien definido.
- Duplicar observaciones no modifica identificación; la aleatorización centra el sesgo en cero en expectativa, sin eliminar variación muestral.
- Los valores visibles se leen de los CSV canónicos y coinciden con ellos: ATE/ATT/ATU originales 0,75; CATE(0)=1,25; CATE(1)=0,25; naïve 6,75; sesgo 6; Monte Carlo con 1.000 repeticiones por escenario.

## Preocupación residual

La única incidencia es el copiado incompleto de HTML al `output_dir` por el flujo actual de `bookdown`; la vista previa solicitada quedó completa mediante el copiado documentado. `docs/` permanece sin publicar.
