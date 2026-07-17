# Datos de panel y TWFE — diseño académico y pedagógico

## Objetivo

Dividir el capítulo actual de datos de panel, DiD y TWFE en una pareja teórica–empírica uniforme con el resto del libro, conservar la secuencia de las clases 16–17 y corregir los matices conceptuales y errores de código identificados en la revisión académica.

## Decisiones aprobadas

- Crear dos capítulos consecutivos:
  - `Datos de panel y TWFE — Clase teórica`;
  - `Datos de panel y TWFE — Clase empírica`.
- Conservar la URL pública actual `datos-de-panel-did-y-twfe-en-stata.html` para la clase teórica mediante un anchor explícito.
- Crear una URL nueva y estable para la práctica.
- Mantener la progresión desde fundamentos de panel hasta DiD con adopción escalonada.
- Usar los materiales de las clases 16–17 y preservar los ejemplos relevantes del capítulo existente.
- Poner las descargas al comienzo de la práctica; la teoría no contiene descargas.
- Mostrar resultados canónicos producidos por Stata en la página práctica.
- Incluir tres preguntas teóricas y cuatro empíricas, sin respuestas visibles ni desplegables.
- Guardar una clave separada para la profesora y el monitor, fuera del libro publicado.
- Delegar la numeración de encabezados a Bookdown.

## Correcciones académicas obligatorias

### Equivalencia 2×2

La igualdad algebraica entre DiD manual, regresión DiD, primeras diferencias y TWFE en el diseño balanceado de dos grupos y dos periodos debe distinguirse de la identificación causal. La interpretación causal requiere tendencias paralelas, consistencia, ausencia de anticipación, composición estable y ausencia de interferencia relevante.

### Comparaciones Bacon y pesos causales

- Goodman-Bacon (2021) se usa para mostrar las comparaciones DiD 2×2 que componen TWFE bajo variación en el momento de adopción.
- Los pesos negativos sobre efectos causales grupo-periodo se presentan con el marco de de Chaisemartin y D’Haultfœuille (2020).
- El texto no atribuye a Bacon el resultado general de pesos negativos sobre \(ATT(g,t)\).
- `bacondecomp` y `twowayfeweights` aparecen como diagnósticos complementarios, no intercambiables.

### Estimadores heterogeneity-robust

El libro no afirmará que todos los métodos modernos estiman el mismo \(ATT(g,t)\). Para cada método debe declararse el parámetro:

| Método | Parámetro principal que se enseñará |
|---|---|
| Callaway–Sant’Anna / `csdid` | \(ATT(g,t)\) y agregaciones explícitas |
| Sun–Abraham / `eventstudyinteract` | promedios *interaction-weighted* por tiempo relativo |
| Borusyak–Jaravel–Spiess / `did_imputation` | efectos de evento mediante imputación |
| de Chaisemartin–D’Haultfœuille / `did_multiplegt_dyn` | efectos dinámicos en diseños generales, incluidos tratamientos no absorbentes |
| Gardner / `did2s` | efectos dinámicos o agregados definidos por la segunda etapa |

La comparación conjunta solo colocará en una misma figura parámetros y horizontes compatibles.

### Event studies y tendencias

- Un event study TWFE tradicional no se presenta como diagnóstico limpio bajo efectos heterogéneos.
- Se explicará que leads y lags pueden mezclar efectos de otros periodos y producir pretrends aparentes.
- Las tendencias específicas por grupo no se presentan como reparación automática de tendencias paralelas; pueden absorber efectos, extrapolar una forma funcional injustificada y cambiar el estimando.
- La inspección de pretrends se presenta como diagnóstico, no como prueba definitiva de tendencias paralelas.

### Código Stata

- Usar `id`, nunca `i`, como identificador del panel y nivel de clustering en el DGP canónico.
- Instalar `did_multiplegt_dyn` con `ssc install did_multiplegt_dyn, replace`.
- Usar `vce(cluster id)` o la sintaxis equivalente exigida por cada comando.
- Especificar efectos fijos explícitos en `did2s`, por ejemplo `first_stage(i.id i.t)`.
- Guardar matrices con los mismos nombres que consume `event_plot`.
- Ejecutar cada ruta canónica antes de mostrarla en el libro.

## Arquitectura de la clase teórica

1. Pregunta de panel y fuentes de variación.
2. Pooled OLS, efectos fijos, primeras diferencias y efectos aleatorios.
3. Transformación *within* y supuestos de exogeneidad.
4. Equivalencia FE–FD con dos periodos.
5. Equivalencia DiD–FD–TWFE en 2×2 y condiciones de identificación causal.
6. Panel largo con adopción simultánea.
7. Adopción escalonada y heterogeneidad.
8. Tres familias de comparaciones Bacon.
9. De la residualización de \(D\) a pesos sobre efectos grupo-periodo.
10. Contaminación del event study TWFE.
11. Mapa de parámetros y estimadores heterogeneity-robust.
12. Checklist conceptual, síntesis y tres preguntas tipo examen.

La versión `11-TWFE-pesos-v2.Rmd` aporta el desarrollo algebraico de la residualización y los pesos, pero se corrigen sus referencias de comandos y se integra sin crear un capítulo redundante.

## Arquitectura de la clase empírica

1. Materiales para la clase.
2. Declarar y describir un panel con `xtset`, `xtdes` y `xtsum`.
3. Comparar pooled, FE, FD y RE en un DGP reproducible.
4. Verificar numéricamente la equivalencia 2×2.
5. Mostrar adopción simultánea y luego escalonada.
6. Ejecutar e interpretar `bacondecomp`.
7. Ejecutar e interpretar `twowayfeweights`.
8. Construir un event study TWFE y mostrar su contaminación.
9. Estimar alternativas modernas, agrupadas por parámetro.
10. Presentar tablas y gráficos canónicos exportados por Stata.
11. Aplicar el checklist de elección del estimador.
12. Incluir cuatro preguntas tipo examen y puente a R/Python.

Cada módulo práctico sigue: pregunta → estimando → DGP/datos → predicción previa → comando → resultado → interpretación → advertencia.

## Checklist antes de escoger estimador

La práctica debe exigir responder:

1. ¿El tratamiento es binario, continuo o multivaluado?
2. ¿Es absorbente o puede prenderse y apagarse?
3. ¿Existen nunca tratados, no-aún-tratados, *stayers* o *quasi-stayers*?
4. ¿Interesa un ATT global, \(ATT(g,t)\), efecto dinámico, acumulado o actual frente al *status quo*?
5. ¿Puede haber anticipación?
6. ¿El resultado depende de rezagos del tratamiento?
7. ¿Cuál es el nivel de asignación y de clustering?
8. ¿Hay pocos clústeres?
9. ¿Las covariables son pretratamiento o están afectadas por el tratamiento?
10. ¿Los horizontes y poblaciones son comparables entre estimadores?

## Resultados reproducibles

El do-file canónico debe exportar, como mínimo:

- comparación pooled/FE/FD/RE;
- equivalencia 2×2;
- coeficiente TWFE y ATT verdadero en el DGP escalonado;
- composición Bacon;
- diagnóstico de pesos grupo-periodo;
- event study TWFE;
- estimaciones modernas que se puedan ejecutar de forma estable en Stata 19;
- metadatos de parámetro, muestra de comparación y horizonte.

El Rmd práctico lee los archivos exportados; no transcribe cifras manualmente.

## Pruebas y verificación

- Contrato de títulos, anchors y orden en `_bookdown.yml`.
- Preservación de unidades académicas relevantes del capítulo actual.
- Pruebas que distingan Bacon de pesos sobre \(ATT(g,t)\).
- Pruebas del mapa método–parámetro.
- Detección de `cluster(i)`, identificadores incorrectos, instalación errónea y matrices inexistentes.
- Exactamente T1–T3 y S1–S4, sin soluciones públicas.
- Clave privada excluida del libro y de `docs`.
- Ejecución limpia del do-file canónico.
- Suite completa del proyecto.
- Render en directorio temporal y revisión de tablas, gráficos, descargas, navegación y numeración.

## Fuera de alcance

- Una demostración exhaustiva de todos los estimadores modernos.
- Forzar todos los métodos a una única gráfica cuando sus parámetros no son comparables.
- Presentar una única recomendación universal para todo diseño escalonado.
- Reemplazar el capítulo de DID ya aprobado.
