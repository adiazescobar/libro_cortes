# Corrección DID y estandarización editorial del libro

**Fecha:** 2026-07-17  
**Estado:** diseño aprobado por Ana María  
**Repositorio:** `libro_cortes`

## Objetivo

Corregir el problema académico detectado en la implementación de primeras
diferencias de DID y uniformar la presentación del libro: títulos de clases
teóricas y empíricas con la misma convención, y subtítulos sin numeración manual
que compita con Bookdown.

## Convención editorial aprobada

Los capítulos que forman una pareja teoría–aplicación usarán:

- `Tema — Clase teórica`
- `Tema — Clase empírica`

Se conservarán sin cambios todos los anchors existentes y, por tanto, las URLs.
Los capítulos introductorios o integrados que no sean una pareja teoría–aplicación
conservarán su título sustantivo actual.

En esta iteración se normalizan las parejas ya explícitas:

| Archivo | Título |
|---|---|
| `03-Parametros.Rmd` | `Parámetros causales — Clase teórica` |
| `04-ParametrosStata.Rmd` | `Parámetros causales — Clase empírica` |
| `05-RCT.Rmd` | `Experimentos aleatorizados — Clase teórica` |
| `06-RCT2.Rmd` | `Experimentos aleatorizados — Clase empírica` |
| `07-POWER-Teoria.Rmd` | `Poder estadístico — Clase teórica` |
| `07-POWER.Rmd` | `Poder estadístico — Clase empírica` |
| `08-DID.Rmd` | `Diferencias en diferencias — Clase teórica` |
| `08-DIDStata.Rmd` | `Diferencias en diferencias — Clase empírica` |
| `09-BadControls.Rmd` | `Malos controles — Clase teórica` |
| `10-BadControlsStata.Rmd` | `Malos controles — Clase empírica` |

Los capítulos posteriores que hoy son unidades integradas —TWFE, matching, PSM,
IPW, IV y RDD— no se reclasifican artificialmente en esta corrección. Cuando se
separen y estandaricen, adoptarán la misma convención.

Bookdown será la única fuente de numeración. Ningún encabezado H2–H4 en POWER,
DID ni las parejas normalizadas comenzará con números, `Paso` o `Etapa`. Los
anchors explícitos y el orden de `_bookdown.yml` no cambiarán.

## Corrección académica de DID

`base3.dta` contiene cortes transversales repetidos: 8.000 observaciones, cuatro
celdas grupo×periodo de 2.000 filas, y ningún identificador individual. No se
creará un panel emparejando filas por su posición u orden de nacimiento.

Se eliminarán:

- el `id` artificial;
- `xtset` y `reg D.y D` aplicados a `base3.dta`;
- la fila canónica `did_primeras_diferencias`;
- la afirmación de que cuatro métodos empíricos reproducen el estimador.

La equivalencia entre DiD y primeras diferencias se conservará como resultado
teórico condicionado: es válida cuando se observa un panel genuino de las mismas
unidades en dos periodos. La clase indicará que `base3.dta` no permite demostrarla
empíricamente. Los tres métodos válidos sobre esta base serán el cálculo manual,
`diff` y la regresión saturada con interacción.

La identificación del ATT se formulará bajo tendencias paralelas **y** los demás
supuestos necesarios: consistencia, ausencia de anticipación, composición estable
y ausencia de interferencia relevante. Se eliminará la frase “si y solo si”.

## Resultados y pruebas

Los CSV canónicos se regenerarán desde Stata. El esquema y los contratos dejarán
de exigir `did_primeras_diferencias` y pasarán a prohibir patrones que fabriquen
identificadores longitudinales en `base3.dta`.

Se añadirán contratos para comprobar:

1. títulos exactos y anchors intactos;
2. ausencia de numeración manual en encabezados;
3. ausencia de `id` artificial, `xtset` y primeras diferencias sobre `base3.dta`;
4. tres métodos DID válidos y concordantes;
5. formulación completa de los supuestos de identificación;
6. preservación de preguntas, bloques, materiales y resultados restantes.

## Validación y publicación

La entrega requiere:

- ciclo RED→GREEN de los contratos nuevos;
- Stata con exit 0 y log sin errores;
- verificación cruzada Stata–Python en `PASS`;
- suite completa verde;
- render completo desde una copia limpia;
- inspección de los HTML de POWER y DID para numeración, títulos y tablas;
- auditoría de privacidad de la clave docente.

Los HTML de DID no se copiarán a `docs` ni se publicarán hasta recibir aprobación
explícita de Ana María sobre la vista previa corregida.

