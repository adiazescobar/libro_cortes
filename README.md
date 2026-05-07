# Cortes Transversales

Libro de apoyo para el curso avanzado de microeconometría aplicada.

## Estructura

- Capítulos fuente: archivos `*.Rmd` en la raíz del proyecto.
- Configuración de compilación: `_bookdown.yml` y `_output.yml`.
- Material computacional: `dofile/`, organizado por capítulo o sesión.
- Sitio compilado: `docs/`.

## Reproducibilidad

Los do-files deben poder ejecutarse desde su propia carpeta o desde la raíz del libro. Las bases usadas en clase están en `dofile/` y los scripts evitan rutas absolutas a computadores específicos.

Para compilar el libro:

```r
bookdown::render_book("index.Rmd")
```
