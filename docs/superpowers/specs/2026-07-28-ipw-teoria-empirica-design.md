# Diseño: IPW — clases teórica y empírica

## Objetivo

Reconstruir el módulo de ponderación por probabilidad inversa como una pareja consecutiva de capítulos, académicamente rigurosa y reproducible, siguiendo el patrón aprobado del libro.

## Estructura y URLs

- `15-IPW.Rmd`: **Ponderación por probabilidad inversa — Clase teórica**, anchor `ipw`, URL estable `ipw.html`.
- `16-PSM_IPW_SinteticosConsolidado.Rmd`: **Ponderación por probabilidad inversa — Clase empírica**, anchor legado `psm-ipw-sinteticos`, URL estable `psm-ipw-sinteticos.html`.
- El contenido existente de controles sintéticos se preserva en un archivo independiente para su revisión académica posterior; no se mezcla con IPW.
- El bloque de PSM duplicado se elimina del capítulo 16 porque PSM ya tiene su pareja completa.

## Clase teórica

La secuencia será:

1. pregunta causal y estimando antes de construir pesos;
2. CIA, consistencia/SUTVA y positividad;
3. identidad de reponderación para (E[Y(D=1)]) y (E[Y(D=0)]);
4. pesos para ATE y ATT;
5. estimador Horvitz–Thompson no normalizado;
6. estimador Hájek normalizado;
7. pesos estabilizados y qué conservan;
8. balance ponderado y tamaño efectivo de muestra;
9. pesos extremos, positividad práctica y cambio de población al recortar;
10. AIPW e IPWRA como estimadores doblemente robustos;
11. comparación conceptual con matching;
12. preguntas tipo examen sin respuestas públicas.

La teoría debe corregir explícitamente:

- la fórmula Hájek actualmente rotulada como Horvitz–Thompson;
- la afirmación absoluta de que IPW usa toda la muestra;
- la recomendación mecánica de winsorizar o recortar;
- la inferencia de que buen balance prueba CIA;
- la idea de que doble robustez significa inmunidad a toda mala especificación.

Se mantiene la notación global (Y(D=1)) y (Y(D=0)).

## Clase empírica

La página comienza con:

1. materiales descargables;
2. lecturas centrales de Bernal y Peña, capítulo 6, y Cunningham, capítulo 5;
3. metas de aprendizaje;
4. pregunta empírica y estimando.

El flujo principal usa `base6.dta` para conservar continuidad con PSM:

- diferencia cruda;
- propensity score con el mismo conjunto pretratamiento;
- soporte y distribución de pesos;
- IPW manual para ATE y ATT;
- Horvitz–Thompson y Hájek;
- `teffects ipw`, armonizando estimando y modelo;
- `tebalance summarize` y `tebalance density`;
- máximo, percentiles, suma de pesos y tamaño efectivo;
- `teffects aipw` e `ipwra` como extensiones.

La base heredada tiene buena superposición. Por eso se añade una simulación separada con positividad débil para mostrar cómo una observación puede dominar IPW y cómo cambian estimación, precisión y población al aplicar restricciones justificadas. La simulación tendrá respuesta causal conocida y no sustituirá el ejemplo principal.

## Diagnósticos y resultados visibles

La clase empírica mostrará en la página:

- tabla de diferencia cruda, IPW manual, `teffects ipw`, AIPW e IPWRA;
- tabla de diagnóstico de pesos: mínimo, p50, p90, p95, p99, máximo y tamaño efectivo;
- tabla de balance antes y después;
- gráfica de propensity scores por tratamiento;
- histograma o densidad de pesos;
- gráfica de la simulación de positividad débil.

Todos los números visibles provienen de Stata o de CSV exportados por el do-file. No se transcriben resultados inventados.

## Inferencia y sensibilidad

- `teffects` será la referencia para inferencia que incorpora la estimación del propensity score.
- La regresión manual ponderada se presenta como demostración de mecánica, no como reemplazo automático de la inferencia de `teffects`.
- Recorte, truncamiento o winsorización deben declarar el umbral, el número de observaciones afectadas y la población resultante.
- Se explicará que estabilizar pesos puede mejorar escala/variabilidad, pero no repara falta de positividad.
- El tamaño efectivo se calculará como ((\sum_i w_i)^2/\sum_i w_i^2).

## Doble robustez

La clase distinguirá:

- `teffects ipw`: requiere un modelo de tratamiento adecuado;
- `teffects aipw`: combina funciones de resultado y tratamiento;
- `teffects ipwra`: ajuste de regresión ponderado;
- `telasso`: extensión avanzada ya introducida, no eje central de esta clase.

“Doblemente robusto” se describirá como consistencia si al menos uno de los dos modelos nuisance está correctamente especificado, bajo los restantes supuestos causales y condiciones regulares; no como protección contra falta de overlap, confusión no observada o variables postratamiento.

## Preguntas y clave privada

- Tres preguntas teóricas y cuatro empíricas tipo examen.
- Sin respuestas desplegables ni soluciones en HTML.
- Clave separada para profesora y monitor en `claves_privadas/15_IPW_clave.md`.
- La clave no entra en `_bookdown.yml` ni `docs/`.

## Preservación de controles sintéticos

Antes de reescribir el capítulo consolidado se extraerán:

- explicación y comandos de control sintético;
- enlaces a `03_synthetic_controls_stata.do` y `synth_smoking.dta`;
- gráficos existentes;
- referencias y ejercicios relacionados.

Se guardarán en `17-SyntheticControls-DRAFT.Rmd`, fuera de `_bookdown.yml`, con una advertencia editorial de que requiere revisión. No se perderá el material y no se presentará todavía como capítulo aprobado.

## Verificación

- Contrato de títulos, anchors, descargas, lecturas, notación y preguntas.
- Contrato de fórmulas HT/Hájek, ATT/ATE y tamaño efectivo.
- Ejecución completa del do-file en Stata 19.
- Validación de CSV y gráficos generados.
- Suite completa de pytest.
- Render completo en una carpeta de revisión de Dropbox.
- Auditoría de enlaces, tablas, gráficos y ausencia de soluciones.
- `docs/` no se actualiza sin aprobación expresa.
