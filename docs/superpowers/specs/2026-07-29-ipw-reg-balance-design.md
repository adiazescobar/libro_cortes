# IPW con `reg` y postestimación de balance — Diseño

## Objetivo

Ampliar la clase empírica de IPW para que el estudiante pueda reproducir el estimador Hájek mediante una regresión ponderada ordinaria y comprobar, después de ponderar, si las covariables observadas quedaron balanceadas. El análisis se hará por separado para ATE y ATT porque cada conjunto de pesos representa una población objetivo distinta.

## Equivalencia con regresión ponderada

El do-file estimará:

```stata
reg y2 D [pw=w_ate], vce(robust)
reg y2 D [pw=w_att], vce(robust)
```

Con intercepto y un indicador binario de tratamiento, el coeficiente de `D` es la diferencia entre las medias ponderadas y coincide numéricamente con el estimador Hájek manual correspondiente. La página mostrará juntos Hájek manual, `reg` ponderada y `teffects ipw` para ATE y ATT.

La equivalencia exigida es del estimador puntual, no de la inferencia. Los errores estándar robustos de `reg` tratan los pesos estimados como dados; `teffects ipw` reconoce que el propensity score fue estimado. Esta diferencia se explicará explícitamente.

## Postestimación de balance

El post-análisis responderá: **¿la ponderación equilibró las covariables pretratamiento en la población objetivo?** Tendrá tres componentes:

1. `tebalance summarize` después de `teffects ipw`, tanto para ATE como para ATET.
2. `tebalance density` para inspeccionar distribuciones completas de covariables seleccionadas.
3. Cálculo manual reproducible de diferencias estandarizadas y razones de varianza antes y después de ponderar.

El do-file exportará una tabla de balance en formato largo con las columnas `estimand`, `covariate`, `metric`, `raw` y `weighted`. Incluirá diferencias estandarizadas y razones de varianza para ATE y ATT. También generará un gráfico de diferencias estandarizadas absolutas antes/después, separado por estimando, y conservará las densidades de soporte ya existentes.

La página usará 0.10 como referencia descriptiva para diferencias estandarizadas absolutas, no como prueba estadística ni regla automática. Explicará que:

- balance observable no demuestra CIA ni descarta confusión no observada;
- los valores p dependen del tamaño muestral y no son el criterio principal;
- buen balance del propensity score no reemplaza revisar cada covariable;
- si persiste desequilibrio, se reespecifica el modelo del tratamiento usando razonamiento causal y flexibilidad funcional, se recalculan los pesos y se repiten todos los diagnósticos;
- no se selecciona la especificación mirando cuál produce el efecto preferido.

## Cambios visibles

- El do-file tendrá una sección explícita “Mismo estimador con `reg`”.
- La tabla principal incorporará las dos regresiones ponderadas.
- La práctica tendrá una sección “Postestimación: ¿quedó balanceado?” después de construir los pesos y antes de interpretar el efecto.
- Se mostrarán tablas de balance ATE y ATT, un gráfico comparativo y los comandos nativos de Stata.
- Una pregunta tipo examen existente se ampliará para exigir diagnóstico y reespecificación cuando una covariable permanezca desbalanceada; el número total de preguntas seguirá siendo cuatro.

## Reproducibilidad y pruebas

Stata 19 regenerará los CSV, el log y las figuras. Los contratos comprobarán:

- presencia de ambas regresiones ponderadas;
- coincidencia numérica de `reg`, Hájek y `teffects ipw` dentro de tolerancia;
- balance exportado para ATE y ATT con ambas métricas;
- presencia de `tebalance summarize` y `tebalance density`;
- resultados visibles derivados de archivos canónicos, sin números duplicados manualmente;
- ausencia de respuestas públicas y de la clave privada dentro del repositorio o del render.

No se actualizará `docs/` ni se publicará el libro sin aprobación expresa.
