# Clave privada — Propensity score matching

Uso exclusivo de la profesora y el monitor. No incluir en `_bookdown.yml` ni en `docs/`.

## PSM-T1

- Balance de observables no prueba CIA: puede permanecer un confusor no observado.
- Contraejemplo: motivación afecta participación y resultado, pero no está en (X).
- Rúbrica: distinción identificación/diagnóstico (2), contraejemplo (2), conclusión (1).

## PSM-T2

- Tras descartar tratados, no se identifica automáticamente el ATT de todos los tratados.
- El parámetro corresponde al ATT de los tratados dentro de la región de soporte.
- Debe reportar proporción descartada, características y límites de validez externa.
- Rúbrica: parámetro (2), población (1), reporte (2).

## PSM-T3

- NN(1): menor distancia y más varianza; reemplazo puede concentrar pesos.
- NN(5): menor varianza, posible aumento de sesgo.
- Kernel: usa más controles; depende del ancho de banda y es menos transparente.
- Rúbrica: sesgo-varianza (2), pesos (1), transparencia (1), diagnóstico (1).

## PSM-S1

- Armonizar ATET/ATT, logit/probit, número de vecinos, soporte/caliper y tratamiento de empates.
- No exigir errores estándar iguales: las implementaciones incorporan incertidumbre de forma distinta.
- En la corrida validada: `psmatch2` 0.294 y `teffects psmatch` 0.288.
- Rúbrica: cinco decisiones (3), inferencia (1), interpretación (1).

## PSM-S2

- LASSO de tratamiento optimiza predicción, no identificación causal.
- Educación puede predecir fuertemente los resultados potenciales aunque prediga poco (D).
- Debe quedar en `ainclude()` como confusor obligatorio; LASSO selecciona entre candidatos pretratamiento adicionales.
- `telasso` aporta ortogonalidad y doble robustez, pero no reemplaza el DAG.
- Rúbrica: falla (2), especificación (2), límite de LASSO (1).
