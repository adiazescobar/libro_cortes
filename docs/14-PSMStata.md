# Propensity score matching — Clase empírica {#psm-stata}

::: {.boxinfo}
**Metas de aprendizaje**

- Interpretar logit y probit con efectos marginales (`margins`).
- Implementar el flujo principal con `psmatch2`.
- Diagnosticar selección, soporte y balance antes de interpretar el ATT.
- Replicar el estimando con `teffects psmatch` y explicar diferencias.
- Usar LASSO como herramienta de alta dimensión sin delegarle la selección causal.
:::

## Materiales de la clase {-}

Descarga los archivos antes de comenzar:

- [Do-file de la clase del 15 de octubre, con `margins` y LASSO](dofile/16_PSM_IPW_Sinteticos/01_psm_stata.do)
- [Log completo de ese do-file](dofile/16_PSM_IPW_Sinteticos/psm_demo.log)
- [Base de datos de la práctica](dofile/16_PSM_IPW_Sinteticos/base6.dta)
- [Do-file guiado alternativo](dofile/16_PSM_IPW_Sinteticos/01_psm_stata_CLASSROOM.do) y [su log](dofile/16_PSM_IPW_Sinteticos/psm_classroom.log)

Instala `psmatch2` una sola vez:

```stata
ssc install psmatch2
help psmatch2
```

`pstest` y `psgraph` se instalan con el mismo paquete.

::: {.boxinfo}
**Lecturas centrales**

- [Bernal y Peña — capítulo 6 (PDF)](lecturas/bernal-pena/capitulo-06.pdf)
- [Cunningham — capítulo 5: Matching and Subclassification](https://mixtape.scunning.com/05-matching_and_subclassification)
:::

## Pregunta causal y estimando {-}

La práctica pregunta: **¿cuál es el efecto del tratamiento sobre quienes efectivamente lo recibieron?** El estimando principal es

$$ATT=E[Y(D=1)-Y(D=0)\mid D=1].$$

Antes de ejecutar un comando, clasificamos las variables:

```stata
global Xmust "personas orden_n ocupado_jefe educa_jefe ingresos_hogar_jefe hombre"
global Xflex "c.personas#c.personas c.educa_jefe#c.educa_jefe ///
              c.ingresos_hogar_jefe#c.ingresos_hogar_jefe ///
              i.ocupado_jefe#i.hombre"
```

`Xmust` contiene los **confusores obligatorios** justificados antes de estimar. `Xflex` reúne términos pretratamiento candidatos para capturar no linealidades. Ninguna variable postratamiento puede entrar en cualquiera de los dos grupos.

::: {.boxwarning}
LASSO no descubre los confusores. Una variable causalmente necesaria no se vuelve opcional porque su coeficiente predictivo sea pequeño.
:::

## Mirar la selección antes de corregirla {-}

```stata
use base6.dta, clear
table D, statistic(count D) statistic(mean y2 personas ocupado_jefe educa_jefe)
logit D $Xmust
predict double pscore, pr
```

Salida inicial de la base heredada:

```text
Tratamiento       N       Media de Y
D=0            2,048       -0.976
D=1            1,952       -0.644
Diferencia cruda              0.332

Logit de D: LR chi2(6) = 64.57; p < 0.001
```

La prueba conjunta confirma selección observable, pero la diferencia cruda todavía mezcla efecto y composición. No interpretamos $0.332$ como causal.

## Probit, logit y efectos marginales {-}

El propensity score es $P(D=1\mid X)=G(X\beta)$. En el probit $G$ es la normal acumulada y en el logit es $\exp(X\beta)/(1+\exp(X\beta))$. Los coeficientes no son efectos sobre la probabilidad. Para una $x_1$ continua el efecto es

$$\frac{\partial P(D=1\mid X)}{\partial x_1}=g(X\beta)\,\beta_1,$$

que cambia de hogar en hogar. `margins, dydx(*)` reporta el promedio $\frac{1}{n}\sum_i g(x_i\hat\beta)\hat\beta_1$ (efecto marginal promedio). Para una $x_2$ binaria el efecto es la diferencia $G(X\beta)\vert_{x_2=1}-G(X\beta)\vert_{x_2=0}$; por eso las binarias se escriben con `i.`. `margins` reemplaza al viejo `dprobit`.

```stata
probit D personas orden_n i.ocupado_jefe educa_jefe ingresos_hogar_jefe i.hombre
predict double xb_probit, xb
margins, dydx(*)              // promedio de g(x_i b)*b
margins, dydx(*) atmeans      // evaluado en las medias de X

* Verificación a mano
gen double em_educa = normalden(xb_probit) * _b[educa_jefe]
summ em_educa
```

```text
                    Probit coef.   AME (margins)   AME a mano   MEM (atmeans)
personas              -0.0546        -0.0215                       -0.0218
orden_n                0.2289         0.0901                        0.0913
1.ocupado_jefe         0.2054         0.0807                        0.0814
educa_jefe            -0.0151        -0.0059        -0.0059         -0.0060
ingresos_hogar_jefe    0.0008         0.0003                        0.0003
1.hombre               0.0413         0.0163         0.0163         0.0165
```

Un año más de educación del jefe reduce la probabilidad de tratamiento en 0.6 puntos porcentuales en promedio. El cálculo a mano coincide con `margins` hasta el séptimo decimal, también para `hombre` con la diferencia $G(\cdot\mid hombre=1)-G(\cdot\mid hombre=0)$. El logit da coeficientes distintos (por ejemplo $-0.024$ para educación) pero efectos marginales casi iguales ($-0.0059$), y la correlación entre los dos propensity scores es 1.000.

## Visualizar soporte común {-}

```stata
twoway (kdensity pscore if D==1, lcolor(navy) lwidth(medthick)) ///
       (kdensity pscore if D==0, lcolor(maroon) lwidth(medthick)), ///
       legend(order(1 "Tratados" 2 "Controles")) ///
       xtitle("Propensity score") ytitle("Densidad")
graph export pscore_distribution.png, replace
```

![Distribución del propensity score por tratamiento](dofile/16_PSM_IPW_Sinteticos/pscore_distribution.png)

La figura muestra superposición amplia. Eso permite construir contrafactuales observables, pero también anticipa que el ajuste puede ser modesto. La clase discutirá por qué un ejemplo con selección estadística no necesariamente tiene gran sesgo en el resultado.

## Estimación principal con `psmatch2` {-}

Usamos logit, ATT, un vecino, reemplazo y soporte común:

```stata
psmatch2 D $Xmust, outcome(y2) logit neighbor(1) common ties
scalar att_psm = r(att)
psgraph
pstest $Xmust, treated(D) both graph
graph export psm_balance.png, replace
```

```text
Variable   Muestra       Tratados    Controles    Diferencia
y2         Sin ajustar    -0.644      -0.976         0.332
           ATT            -0.643      -0.936         0.294

Tratados fuera del soporte: 5 de 1,952
```

Aquí el ATT cambia poco. Esa es una conclusión sustantiva del ejemplo, no una prueba de que PSM siempre sea innecesario: las covariables predicen participación, pero en esta base explican poca parte de la diferencia en $Y$.

### Diagnóstico de balance {-}

```text
Muestra       Pseudo R2    MeanBias    MedBias
Sin ajustar      0.012        8.5         9.0
Emparejada       0.001        2.3         1.4
```

<p><img src="dofile/16_PSM_IPW_Sinteticos/psm_balance.png" alt="Balance de covariables antes y después del matching" style="max-width:100%;"></p>

Leemos la tabla covariable por covariable. Buscamos diferencias estandarizadas pequeñas —como guía, menores a 10%— y también revisamos varianzas y distribuciones. No decidimos balance con los $p$-valores de pruebas t.

::: {.boxkey}
El matching mejora el diseño observado, pero el balance solo se refiere a las covariables medidas e incluidas. No prueba CIA.
:::

## Decisiones del algoritmo y sensibilidad {-}

```stata
* Cinco vecinos: menor varianza potencial, mayor distancia promedio
psmatch2 D $Xmust, outcome(y2) logit neighbor(5) common ties

* Kernel: usa más controles con pesos por cercanía
psmatch2 D $Xmust, outcome(y2) logit kernel kerneltype(epan) ///
    bwidth(0.06) common

* Caliper expresado en la escala elegida por psmatch2
psmatch2 D $Xmust, outcome(y2) logit neighbor(1) ///
    caliper(0.02) common ties
```

| Especificación | Qué cambia | Diagnóstico obligatorio |
|---|---|---|
| NN(1), reemplazo | Prioriza cercanía | Reutilización de controles |
| NN(5) | Usa más información | Distancia del quinto vecino |
| Kernel | Suaviza con muchos controles | Sensibilidad al bandwidth |
| Caliper | Rechaza matches lejanos | Tratados descartados y nuevo estimando |

No escogemos la fila con menor $p$-valor. Una especificación es defendible cuando sus decisiones estaban justificadas, conserva una población relevante y mejora el balance.

## Verificación con `teffects psmatch` {-}

Para comparar debemos alinear el **mismo estimando**, modelo del tratamiento y número de vecinos:

```stata
teffects psmatch (y2) (D $Xmust, logit), atet nneighbor(1)
estimates store te_psm
```

```text
Estimando: ATET
Método: propensity-score matching, logit, un vecino
ATET estimado: 0.288 (frente a 0.294 con psmatch2 alineado)
Inferencia: errores estándar ajustados por la estimación del propensity score
```

No esperamos igualdad mecánica. `psmatch2` y `teffects psmatch` pueden resolver de manera distinta los empates, el soporte, la reutilización de controles y la estimación de la varianza. Si los coeficientes difieren, primero comprobamos estimando, muestra, enlace logit/probit, vecinos, caliper y empates; no concluimos automáticamente que uno está “mal”.

::: {.boxexam}
**Ejercicio tipo examen 1.** `psmatch2` entrega 0.294 y `teffects psmatch` 0.288. Enumere cinco decisiones que debe armonizar antes de interpretar la diferencia. ¿Exigiría que los errores estándar coincidieran? Justifique.
:::

## Machine learning con disciplina causal {-}

### La estrategia incorrecta {-}

```stata
* NO usar como selector causal automático
lasso probit D $X $Xflex, selection(cv) rseed(1298)
lassocoef
```

Esta regresión optimiza predicción de $D$. En `base6` LASSO saca `educa_jefe` y deja `educa2`: el confusor que justificamos con teoría sale del modelo por razones puramente predictivas. Puede omitir un predictor importante de $Y(0)$ o $Y(1)$ y seleccionar un predictor casi exclusivo del tratamiento que empeore la superposición. Por eso no usamos el conjunto seleccionado como una lista descubierta de confusores.

### Estrategia de la clase: LASSO solo sobre los términos flexibles {-}

Los confusores obligatorios van entre paréntesis y LASSO no puede sacarlos. LASSO solo decide qué cuadrados e interacciones entran al propensity score. Como `psmatch2` no acepta notación de factores, creamos los términos a mano.

```stata
gen double personas2  = personas^2
gen double educa2     = educa_jefe^2
gen double ingresos2  = ingresos_hogar_jefe^2
gen double ocup_hom   = ocupado_jefe*hombre
gen double educa_ingr = educa_jefe*ingresos_hogar_jefe
global Xflex "personas2 educa2 ingresos2 ocup_hom educa_ingr"

lasso probit D ($X) $Xflex, selection(cv) rseed(1298)
lassocoef
global Xsel "`e(allvars_sel)'"

psmatch2 D $Xsel, outcome(y2) n(1) common ai(1)
pstest $X, treated(D) both
```

```text
LASSO (validación cruzada, 10 folds) mantiene las 6 de $X y agrega ingresos2
ATT psmatch2 con $Xsel = 0.282 (EE 0.047); 6 tratados fuera de soporte
MeanBias: 8.5 sin emparejar, 1.8 emparejado
```

### Benchmark doblemente robusto: `telasso` {-}

`telasso` estima con LASSO el modelo del resultado y el del tratamiento y construye un estimador ortogonal y doblemente robusto. `ainclude()` deja fijos los confusores obligatorios.

```stata
telasso (y2 $X $Xflex, ainclude($X)) (D $X $Xflex, ainclude($X)), ///
        atet rseed(1298)
lassocoef (., for(D)) (., for(y2) tlevel(0)) (., for(y2) tlevel(1))
```

```text
ATET telasso = 0.332 (EE robusto 0.034)
Controles candidatos = 11; con el método plugin no entra ningún término de $Xflex
```

La comparación tiene tres columnas conceptualmente distintas:

| Estimador | Función en la clase | Qué no debemos afirmar |
|---|---|---|
| `psmatch2` | Diseño visible, soporte y balance | Que el matching elimina confusión no observada |
| `teffects psmatch` | Verificación nativa e inferencia ajustada | Que replica exactamente cada decisión de `psmatch2` |
| `telasso` | Benchmark de alta dimensión, ortogonal y doblemente robusto | Que LASSO conoce el DAG o descubre causalidad |

::: {.boxexam}
**Ejercicio tipo examen 2.** Una estudiante permite que LASSO elimine educación porque su coeficiente en el modelo de tratamiento es pequeño. Educación predice fuertemente el resultado. Explique por qué esa decisión puede aumentar sesgo y proponga una especificación causalmente defendible.
:::

## Cuando PSM puede empeorar el diseño {-}

King y Nielsen advierten que podar observaciones únicamente por distancia en el propensity score puede aumentar desequilibrios en otras dimensiones de $X$. En el laboratorio repetimos el diagnóstico para cada algoritmo y comparamos la distribución conjunta de covariables, no solo la del puntaje.

::: {.boxwarning}
Si el balance empeora, el soporte se vuelve muy estrecho o el efecto depende fuertemente del algoritmo, el resultado correcto no es ocultar esa especificación. Es reconocer que el diseño observacional no sostiene una conclusión robusta.
:::

## Lista de verificación para entregar {-}

1. Definir ATT o ATE antes de estimar.
2. Justificar confusores obligatorios y excluir variables postratamiento.
3. Mostrar soporte común y describir unidades descartadas.
4. Reportar balance antes y después, no solo el efecto.
5. Presentar `psmatch2` como estimación principal y `teffects psmatch` como contraste armonizado.
6. Si se usa LASSO, separar selección predictiva de razonamiento causal y reportar `telasso` como extensión.
7. Discutir sensibilidad a algoritmo, vecinos, caliper y población con soporte.

## Lecturas y ayuda de Stata {-}

- Leuven, E. y Sianesi, B. (2003). `PSMATCH2`, módulo de Stata disponible en SSC.
- `help teffects psmatch` y `help tebalance`.
- `help telasso` y la documentación de Stata sobre *Treatment-effects estimation using lasso*.
- King, G. y Nielsen, R. (2019). “Why Propensity Scores Should Not Be Used for Matching”, *Political Analysis*.
