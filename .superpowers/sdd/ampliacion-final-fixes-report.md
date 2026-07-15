# Ajustes finales de la ampliación de Parámetros

## Estado

Se corrigieron los hallazgos accionables de la revisión final sin publicar ni
copiar contenido de la clave docente. La práctica ahora presenta output
canónico suficiente para S-P1, las cuatro preguntas usan metadatos uniformes y
la notación de resultados potenciales quedó normalizada también en la clave
externa.

## TDD y output de Stata

Primero se añadieron contratos para exigir error estándar robusto e intervalo
de confianza de `D`, una tabla visible construida desde el CSV y los campos
`Comandos permitidos` y `Producto esperado` en cada pregunta. La ejecución RED
produjo tres fallos esperados.

El do-file exporta estimandos etiquetados para coeficientes, errores estándar
robustos e intervalos de confianza de `D` y de la constante. Una prueba de
integración detectó que la anchura original de la variable de etiquetas
truncaba tres nombres largos; se añadió una regresión RED específica, se amplió
esa anchura y se reejecutó el pipeline completo con Stata 19. Los contratos
focales pasaron después de regenerar los artefactos.

## Preguntas prácticas

S-P1 a S-P4 contienen exactamente una línea de `Comandos permitidos` y una de
`Producto esperado`, en ese orden. El contenido es específico para la tarea y
coincide con lo que solicita cada enunciado. S-P1 incluye una tabla interpolada
desde el CSV canónico; no se transcribieron cifras.

## Clave externa y privacidad

- Se preservó la clave fuera del repositorio y con permisos `0600`.
- Se verificaron siete códigos únicos y cinco componentes no vacíos por código.
- La notación abreviada de resultados potenciales quedó en cero; todas las
  referencias afectadas usan estados explícitos `D=1` o `D=0`.
- Git no rastrea ni muestra archivos de clave o solución.
- El HTML temporal no contiene identificadores privados ni marcadores de
  respuestas.

No se reescribió la historia de Git ni se borraron artefactos no rastreados.
Como observación no bloqueante, nombres o rutas ya redactados podrían permanecer
en la historia local. La clave y sus soluciones nunca fueron committed.

## Verificación

- Stata 19: pipeline canónico completo, con cierre normal del log.
- Pruebas focales posteriores a la corrección de truncamiento: `3 passed`.
- Suite completa previa al cierre: `73 passed`.
- Render completo: código 0. Por el comportamiento conocido de bookdown, el
  render fresco apareció como HTML único en la raíz y se copió únicamente al
  directorio temporal.
- QA HTML focal: tabla robusta con cinco columnas, cuatro códigos únicos, cuatro
  pares de metadatos y cero marcadores privados.

### Corrección de empaquetado

Sin volver a renderizar, se copió el HTML dividido fresco de la práctica desde
la raíz del proyecto a
`/private/tmp/libro_cortes_parametros_ampliado/parametros-causales-stata.html`.
Origen y destino tienen SHA-256
`26c4274f9d896226f0fa31d506cd4f1a38ddb3e8c44b3bdfe2889f1e7f5c7ad0`, y el
timestamp del destino es posterior al del origen. El archivo empacado contiene
una tabla robusta y exactamente cuatro campos `Comandos permitidos` y cuatro
campos `Producto esperado`. El HTML de teoría ya era idéntico entre origen y
destino, por lo que no fue necesario copiarlo de nuevo.

## Preocupaciones residuales

El workaround de salida de bookdown sigue siendo necesario. El árbol de trabajo
contiene cambios y artefactos previos ajenos a esta corrección; no se alteraron
ni se incluyeron en el commit temático.
