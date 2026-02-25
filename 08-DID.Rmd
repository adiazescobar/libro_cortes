
# Diferencias en Diferencias (Teoría)

::: {.boxinfo}
**🎯 Objetivos de la clase**

* Entender la lógica detrás del estimador **Difference-in-Differences (DID)**.
* Analizar estadísticamente la evolución de una variable de resultado para grupos de **tratamiento** y **control** en dos periodos de tiempo.
* Implementar DID en **Stata** de tres formas:

  1. Cálculo manual paso a paso.
  2. Usando el comando `diff`.
  3. Mediante regresiones.
:::

---

## Preparación de los datos {-}

```stata
clear all
set mem 150m
capture log close 
use "datafiles/base3.dta"
```

Verificamos cómo están definidas las variables de **tiempo** y **tratamiento**:

```stata
tab t
tab D
tab t D
```

La variable de resultado es `ha_nchs`, el número de desviaciones estándar que el niño está por encima o por debajo de la media del grupo relevante. En el do-file la llamaremos simplemente `y`.

---

## Estadísticas descriptivas {-}

### Promedios por periodo y grupo {-}

```stata
sum y
sum y if t==0
sum y if t==1
bysort t: sum y
bysort D: sum y
bysort D t: sum y

table D t, c(mean y sd y)
```

Definimos etiquetas y graficamos:

```stata
label define t 0 "t=0" 1 "t=1", replace
label value t t 
label define D 0 "Control" 1 "Tratado", replace
label value D D

graph dot y, over(D) by(t) vertical
```

Evolución temporal:

```stata
preserve
collapse y, by(t D)
twoway (line y t if D==1) (line y t if D==0), ///
       legend(label(1 "Tratados") label(2 "Controles"))
restore
```

---

## Comparación de medias {-}

```stata
ttest y if t == 0, by(D)
ttest y if t == 1, by(D)
```

**Interpretación preliminar:**

* Hay una diferencia preexistente entre tratados y controles en el periodo inicial.
* La diferencia aumenta después del tratamiento.
* DID nos permitirá aislar el efecto neto del tratamiento controlando por diferencias iniciales.

---

## Estimador DID paso a paso {-}

Definimos los promedios:

```stata
sum y if D == 0 & t == 0
scalar y_c0 = r(mean)

sum y if D == 0 & t == 1
scalar y_c1 = r(mean)

sum y if D == 1 & t == 0
scalar y_t0 = r(mean)

sum y if D == 1 & t == 1
scalar y_t1 = r(mean)
```

Cálculo manual del estimador DID:

$$
\widehat{DD} = \big( \bar{Y}_{T,1} - \bar{Y}_{T,0}\big) - \big( \bar{Y}_{C,1} - \bar{Y}_{C,0}\big)
$$

```stata
scalar DD = (y_t1 - y_t0) - (y_c1 - y_c0)
di DD
```

---

## Usando el comando `diff` {-}

```stata
ssc inst diff, replace
diff y, t(D) p(t)
```

---

## Implementación con regresiones {-}

El modelo DID se puede estimar con una regresión incluyendo interacción entre tratamiento y tiempo:

$$
Y_{it} = \alpha + \beta D_i + \gamma t_t + \delta (D_i \times t_t) + \varepsilon_{it}
$$

Donde:

* $\delta$ es el estimador DID.

En Stata:

```stata
reg y D##t
```

Comparación con regresión simple:

```stata
reg y D
```

---

## Extensión con panel {-}

Definimos datos de panel y usamos notación de diferencias:

```stata
xtset id t
reg D.y D.D
```

---

## Interpretación {-}

El coeficiente asociado a la interacción $D \times t$ captura el **efecto causal estimado** bajo el supuesto de **tendencias paralelas**.

En este caso, el coeficiente fue **0.18** y significativo, lo que indica que la diferencia en talla para la edad entre tratados y controles aumentó en **0.18 desviaciones estándar** después de la aplicación del programa.

---
