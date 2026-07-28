# Diseño: lecturas centrales en todos los capítulos

## Objetivo

Cada página docente del libro debe mostrar al comienzo dos referencias centrales:

1. un enlace directo al PDF separado del capítulo pertinente de Bernal y Peña; y
2. un enlace al capítulo web pertinente de *Causal Inference: The Mixtape*, de Scott Cunningham.

La regla aplica tanto a clases teóricas como empíricas. La prueba de entrada y Stata para principiantes también reciben referencias introductorias. `index.Rmd` conserva la bibliografía general y no necesita duplicar el bloque de clase.

## Ubicación y presentación

- Título y metas de aprendizaje.
- En clases empíricas, bloque de materiales y descargas.
- Inmediatamente después, bloque uniforme `Lecturas centrales`.
- Luego comienza el contenido sustantivo.

El bloque utiliza la misma caja de color en todo el libro. Los textos indican autor, número y título del capítulo; no usan rótulos genéricos como “aquí”. Los enlaces de Bernal y Peña abren o descargan el PDF, y los de Cunningham abren el capítulo web.

## Mapa tema–lectura

| Archivos | Bernal y Peña | Cunningham |
|---|---|---|
| `00-PruebaEntrada.Rmd` | capítulos 2–3 | cap. 4, *Potential Outcomes* |
| `01-intro.Rmd` | capítulos 2–3 | caps. 1 y 4 |
| `02-StataBasics.Rmd` | capítulo 2 | cap. 2, *Probability and Regression Review* |
| `03-Parametros.Rmd`, `04-ParametrosStata.Rmd` | capítulos 2–3 | cap. 4, *Potential Outcomes* |
| `05-RCT.Rmd`, `06-RCT2.Rmd` | capítulo 4 | cap. 4, secciones experimentales |
| `07-POWER-Teoria.Rmd`, `07-POWER.Rmd` | capítulo 4 | cap. 4, secciones experimentales e inferencia |
| `08-DID.Rmd`, `08-DIDStata.Rmd` | capítulo 5 | cap. 9, *Difference-in-Differences* |
| `09-BadControls.Rmd`, `10-BadControlsStata.Rmd` | capítulo 3 | cap. 3, *Directed Acyclic Graphs* |
| `11-TWFE.Rmd`, `11-TWFEStata.Rmd` | capítulo 5 | caps. 8–9, *Panel Data* y *Difference-in-Differences* |
| `12-ExactMatching.Rmd`, `13-PSM.Rmd`, `14-PSMStata.Rmd`, `15-IPW.Rmd` | capítulo 6 | cap. 5, *Matching and Subclassification* |
| `16-PSM_IPW_SinteticosConsolidado.Rmd` | capítulo 6 | caps. 5 y 10, matching y control sintético |
| `18-IV.Rmd`, `19-IVStata.Rmd` | capítulo 7 | cap. 7, *Instrumental Variables* |
| `20-RDD.Rmd`, `21-RDDStata.Rmd` | capítulo 8 | cap. 6, *Regression Discontinuity* |

## Fuentes de los PDF

- Capítulos 2, 3 y 4: enlaces directos de Dropbox recuperados del curso anterior.
- Capítulos 5, 7 y 8: copias ya existentes en los archivos del curso; se copiarán con nombres ASCII estables a `lecturas/bernal-pena/`.
- Capítulo 6: debe localizarse una copia separada autorizada en los archivos del curso antes de completar PSM/IPW. No se inventará una URL ni se sustituirá silenciosamente por el libro completo.

Los PDF locales formarán parte de los recursos del libro para que sigan disponibles al publicar. No se alterará su contenido.

## Enlaces de Mixtape

- `https://mixtape.scunning.com/01-introduction`
- `https://mixtape.scunning.com/02-probability_and_regression`
- `https://mixtape.scunning.com/03-directed_acyclical_graphs`
- `https://mixtape.scunning.com/04-potential_outcomes`
- `https://mixtape.scunning.com/05-matching_and_subclassification`
- `https://mixtape.scunning.com/06-regression_discontinuity`
- `https://mixtape.scunning.com/07-instrumental_variables`
- `https://mixtape.scunning.com/08-panel_data`
- `https://mixtape.scunning.com/09-difference_in_differences`
- `https://mixtape.scunning.com/10-synthetic_control`

## Validación

Una prueba de contrato verificará que:

- cada Rmd docente tenga el bloque;
- aparezca cerca del comienzo, respetando materiales primero en las clases empíricas;
- contenga al menos un enlace directo de Bernal y Peña y un enlace `mixtape.scunning.com`;
- los PDF locales existan y se copien al render;
- no haya enlaces vacíos, `LINK`, `TODO` ni referencias a la clave privada.

El libro se renderizará en una carpeta de revisión de Dropbox. No se actualizará `docs/` sin aprobación expresa.
