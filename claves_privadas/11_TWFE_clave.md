# Clave privada — Datos de panel y TWFE

Uso exclusivo de la profesora y el monitor. No incluir en `_bookdown.yml` ni en `docs/`.

## TWFE-T1

- Mostrar que con \(T=2\), demeaning y primera diferencia son transformaciones proporcionales y producen el mismo coeficiente.
- Separar identidad algebraica de tendencias paralelas, consistencia, no anticipación, composición estable e interferencia.
- Rúbrica: álgebra (2), equivalencia (1), supuestos (2).

## TWFE-T2

- Comparaciones: tratada–nunca, temprana–tardía antes de adopción y tardía–temprana ya tratada.
- La última resta resultados que ya contienen tratamiento.
- Bacon describe comparaciones; de Chaisemartin–D’Haultfœuille, pesos sobre efectos grupo-periodo.
- Rúbrica: tabla (2), contaminación (2), distinción (1).

## TWFE-T3

- Parámetro recomendado: efecto dinámico por tiempo relativo o \(ATT(g,t)\) agregado con anticipación explícita.
- Estrategia válida si alinea población, control y horizonte; Sun–Abraham, CS o imputación pueden ser defendibles según el objetivo.
- Rúbrica: parámetro (2), estrategia (1), controles (1), horizonte/agregación (1).

## TWFE-S1

- Pooled y RE sesgados en el DGP por correlación con \(\alpha_i\); FE y FD cerca de 3.
- Debe declarar `xtset id t` y reportar variación *within*.
- Rúbrica: estructura (1), estimaciones (2), tabla (1), interpretación (1).

## TWFE-S2

- Los cuatro coeficientes deben coincidir salvo redondeo.
- La igualdad no prueba identificación; revisar los cinco supuestos causales.
- Rúbrica: cálculo manual (1), tres regresiones (2), igualdad (1), identificación (1).

## TWFE-S3

- `bacondecomp`: composición en DiD 2×2.
- `twowayfeweights`: pesos sobre efectos causales y robustez a heterogeneidad.
- Debe identificar tardía contra temprana ya tratada como potencialmente contaminada.
- Rúbrica: ejecución (2), diferencia conceptual (2), diagnóstico (1).

## TWFE-S4

- `csdid` estándar no es la primera opción si el tratamiento se apaga.
- `did_multiplegt_dyn` es defendible para cambios de tratamiento y efectos dinámicos.
- Debe justificar *stayers*/controles, rezagos, nivel de cluster y pocos clústeres.
- Rúbrica: parámetro (1), comando (1), controles (1), dinámica (1), inferencia (1).
