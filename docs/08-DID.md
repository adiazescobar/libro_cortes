
# Diferencias en Diferencias (Teoría) {#did-teoria}

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

Descarga la base de datos y el do-file de esta clase:

::: {.boxejercicio}
📁 **Archivos de la clase**

* [base3.dta](dofile/08_DID/base3.dta) — base de datos (8 000 observaciones, niños con dos periodos de seguimiento)
* [08_DID.do](dofile/08_DID/08_DID.do) — do-file completo
:::

```stata
clear all
set mem 150m
capture log close
cd "RUTA_DE_TU_CARPETA/dofile/08_DID"
use "base3.dta"
```

Verificamos cómo están definidas las variables de **tiempo** y **tratamiento**:

```stata
tab t
tab D
tab t D
```

Las variables clave de la base son:

| Variable | Descripción |
|---|---|
| `y` | Talla para la edad (z-score): desviaciones estándar respecto a la media del grupo |
| `ha_nchs` | Ídem (nombre original NCHS — es la misma información que `y`) |
| `D` | Indicador de tratamiento (1 = tratado, 0 = control) |
| `t` | Periodo (0 = antes, 1 = después) |
| `orden_n` | Orden de nacimiento del niño en el hogar |

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

La base no incluye un identificador individual explícito, así que lo generamos antes de declarar el panel:

```stata
* Crear identificador individual (panel balanceado: 4 000 niños × 2 periodos)
sort t orden_n
by t: gen id = _n

xtset id t
reg D.y D.D
```

---

## Interpretación {-}

El coeficiente asociado a la interacción $D \times t$ captura el **efecto causal estimado** bajo el supuesto de **tendencias paralelas**.

En este caso, el coeficiente fue **0.18** y significativo, lo que indica que la diferencia en talla para la edad entre tratados y controles aumentó en **0.18 desviaciones estándar** después de la aplicación del programa.

---
