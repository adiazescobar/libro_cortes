# Secuencia pedagógica de TWFE empírico

## Objetivo

Reordenar la clase empírica para que el estudiante observe primero un DGP con adopción escalonada y efectos heterogéneos, entienda por qué falla TWFE, conozca las alternativas modernas y solo entonces use la tabla de supuestos como síntesis.

## Diseño aprobado

1. Mostrar explícitamente la función `tau` del DGP y comprobar que varía entre cohortes y con la exposición.
2. Mantener la secuencia del problema: TWFE, Bacon, pesos causales y event study contaminado.
3. Presentar individualmente `csdid`, `eventstudyinteract`, `did_imputation`, `did_multiplegt_dyn` y `did2s`.
4. Ubicar la tabla “¿Qué supone cada estimador?” después de esas soluciones.
5. Cerrar con diagnóstico de tendencias paralelas y la extensión opcional HonestDiD.

## Restricciones

- Conservar descargas, seis gráficas, resultados visibles y siete preguntas.
- No cambiar URLs ni títulos de capítulos.
- No presentar pretrends u HonestDiD como prueba de tendencias paralelas.
- El ejemplo principal debe combinar adopción escalonada y heterogeneidad cohorte-tiempo.
