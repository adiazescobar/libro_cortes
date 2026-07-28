# Emparejamiento exacto — Diseño del capítulo introductorio

## Objetivo

Convertir `12-ExactMatching.Rmd` en una introducción conceptual breve a los métodos de selección en observables. El capítulo debe explicar qué intenta recuperar el emparejamiento exacto, bajo cuáles supuestos y por qué la dimensionalidad motiva el propensity score, sin convertirse en una clase práctica independiente.

## Lugar en la secuencia del curso

- Va después de Panel y TWFE.
- Funciona como puente hacia `PSM — Clase teórica`.
- No tendrá capítulo empírico propio.
- La implementación con `teffects nnmatch` y `ematch()` se presentará al inicio de la futura clase empírica de PSM, donde podrá compararse con el emparejamiento por propensity score.

## Estructura pedagógica

1. **El problema observacional.** Descomponer la diferencia de medias en ATT y sesgo de selección usando la notación uniforme `Y(D=1)` y `Y(D=0)`.
2. **La idea de las celdas exactas.** Comparar unidades con iguales covariables pretratamiento, dejando claro que el diseño no crea aleatorización.
3. **Supuestos de identificación.** Presentar no confusión condicional, overlap, SUTVA y temporalidad correcta de las covariables.
4. **Ejemplo manual.** Construir una tabla pequeña de tratados y controles, formar celdas, identificar tratados sin match y calcular el ATT para la población emparejada.
5. **Qué cambia al perder observaciones.** Distinguir el ATT original del efecto para tratados dentro del soporte común.
6. **Maldición de dimensionalidad.** Mostrar cómo crecen las celdas y por qué las variables continuas vuelven escasos los matches exactos.
7. **Lo que matching no resuelve.** Confusión no observada, covariables postratamiento, falta de overlap e inferencia.
8. **Puente hacia PSM.** Introducir el propensity score como puntaje de balance, sin afirmar que su estimación garantiza balance o identificación.
9. **Evaluación formativa.** Incluir dos preguntas tipo examen sin respuestas públicas ni elementos desplegables.

## Correcciones académicas vinculantes

- No afirmar que, después del match, “la única diferencia restante es el tratamiento”. La interpretación causal depende de no confusión condicional y los demás supuestos.
- No describir el emparejamiento como si convirtiera automáticamente el estudio observacional en un experimento.
- Eliminar la regla no sustentada de cinco controles por tratado.
- No usar covariables determinadas por el tratamiento ni proxies postratamiento.
- Explicar que descartar tratados sin control cambia la población para la cual se identifica el efecto.
- Diferenciar match exacto puro de nearest-neighbor matching con restricciones exactas.
- Retirar el bloque operativo antiguo `ssc install nnmatch` / `exact()` / `tc(att)` del capítulo introductorio.
- Cuando se implemente en la clase empírica, usar la interfaz oficial de Stata: `teffects nnmatch`, `ematch()` y `atet` cuando el estimando sea el efecto sobre tratados.

## Presentación

- Título uniforme: `Emparejamiento exacto — Introducción`.
- Conservar el capítulo conciso y apoyado en bloques de color: meta, intuición, advertencia y pregunta.
- No crear descargas, resultados canónicos ni do-file exclusivos para este capítulo.
- Mantener las URLs actuales salvo que el render confirme que el cambio de título altera el enlace; si existe riesgo, fijar el identificador actual explícitamente.

## Verificación

- Pruebas de contenido para las correcciones académicas y las dos preguntas.
- Confirmación de que no queda sintaxis antigua de `nnmatch`.
- Suite completa del libro.
- Render completo en una carpeta temporal y revisión visual del capítulo.
- No publicar `docs/` hasta la aprobación explícita de Ana María.

## Fuentes orientadoras

- Rosenbaum, P. R. y Rubin, D. B. (1983), “The Central Role of the Propensity Score in Observational Studies for Causal Effects”, *Biometrika* 70(1): 41–55.
- Imbens, G. W. y Rubin, D. B. (2015), *Causal Inference for Statistics, Social, and Biomedical Sciences*, capítulo 12.
- StataCorp (2025), *Causal Inference and Treatment-Effects Reference Manual*, entrada `teffects nnmatch`.
