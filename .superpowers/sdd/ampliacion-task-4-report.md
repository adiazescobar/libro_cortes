# Task 4 — Verificación integral y vistas previas

## Estado

Completado. Se verificaron los capítulos teórico y práctico, la clave docente
externa, el render ampliado y las vistas de escritorio y móvil. No se publicó ni
se modificó deliberadamente el contenido de `docs`.

## Pruebas y TDD

La suite inicial produjo `68 passed`. La auditoría académica detectó una sola
ambigüedad real: una pregunta práctica dejaba abierta la regla de asignación,
pero su rúbrica calificaba una regla y una predicción únicas. Se añadió primero
una prueba focal; la fase RED falló por la especificación ausente. Luego se hizo
el cambio mínimo en el enunciado para fijar la regla evaluada y pedir código
Stata ejecutable.

Verificación focal GREEN:

```text
1 passed
20 passed
```

La segunda cifra corresponde conjuntamente a los contratos pedagógico y de
Stata. La suite completa final se ejecutó después del render y del QA.

## Privacidad y clave externa

- La clave existe fuera del repositorio y tiene permisos `0600`.
- Contiene siete códigos únicos: tres teóricos y cuatro prácticos.
- Cada código contiene exactamente cinco componentes no vacíos y en el orden
  requerido.
- Los puntajes de las siete preguntas coinciden con sus rúbricas.
- No hay una ruta de la clave en `_bookdown.yml` ni un archivo de clave en el
  índice de Git.
- Los dos HTML estudiantiles no contienen marcadores privados, respuestas ni
  frases sustantivas distintivas copiadas de las soluciones.
- La documentación interna de diseño no forma parte de `_bookdown.yml`; no se
  trató como contenido estudiantil ni se publicó.

Este informe omite deliberadamente respuestas, cálculos de la clave y nombres
de archivos privados.

## Fuentes de cifras y código

- Las cifras empíricas visibles de la práctica provienen de los objetos `point`
  o `mc`; los números literales de ejercicios son datos hipotéticos explícitos.
- Los bloques Stata no contienen elipsis ni comandos truncados.
- El flujo visible incluye construcción del resultado, diferencia de medias,
  regresión, programa de estimadores, selección, aleatorización y simulación.
- Las tres figuras canónicas cargan con dimensiones naturales completas.

## Render

Comando ejecutado:

```bash
Rscript -e "bookdown::render_book('index.Rmd', output_dir='/private/tmp/libro_cortes_parametros_ampliado')"
```

Resultado: código 0. Bookdown volvió a producir los HTML divididos frescos en la
raíz del proyecto durante el render; se copiaron únicamente los dos HTML
objetivo al directorio temporal, después de terminar el proceso. No se copió ni
publicó contenido en `docs`.

## QA visual

### Teoría

- Escritorio y móvil sin desbordamiento global.
- Diez bloques visuales en total, incluyendo más de los seis exigidos.
- Tres preguntas completas y diferenciadas.
- Dos videos embebidos con URL y título accesible.
- Cero ecuaciones con desbordamiento en móvil.
- Sin marcadores de respuestas privadas.

### Práctica

- Doce etapas, 38 bloques visuales y cuatro preguntas.
- Descargas al inicio y once materiales visibles.
- Tres gráficos cargados e íntegros.
- Escritorio y móvil sin desbordamiento global ni imágenes fuera del viewport.
- En móvil, las dos tablas anchas y once bloques de código ofrecen scroll
  horizontal interno (`overflow-x: auto`).
- Sin marcadores de respuestas privadas.

Se conservaron capturas de evidencia en el directorio temporal con prefijo
`qa-`; no forman parte del repositorio.

## Auditoría académica

Las siete preguntas tienen información suficiente, una respuesta razonable
según lo pedido, dificultad y puntaje coherentes, y rúbricas que no otorgan
puntaje por elementos ajenos al enunciado. El único desfase detectado fue
corregido mediante TDD en la cuarta pregunta práctica.

## Servidor y enlaces

Servidor local sobre `/private/tmp/libro_cortes_parametros_ampliado`, enlazado a
`127.0.0.1` en el puerto `8765`:

- `http://127.0.0.1:8765/parametros-causales-teoria.html`
- `http://127.0.0.1:8765/parametros-causales-stata.html`

## Preocupaciones residuales

- El workaround de HTML dividido sigue siendo necesario por el comportamiento
  de bookdown con `output_dir` en este proyecto.
- El repositorio conserva numerosos artefactos sucios previos y generados; no se
  incluyeron en el ajuste ni en el commit de esta tarea.
