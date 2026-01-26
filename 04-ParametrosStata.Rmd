# Estimadores Causales en Secciones Transversales 

::: {.boxinfo}
## 📚 Lecturas obligatorias{-}

- Lectura 4 – Capítulo 4 del libro de Scott Cunningham:  
  👉 [Mixtape: Potential Outcomes](https://mixtape.scunning.com/04-potential_outcomes)

- Documento de referencia sobre RCT:  
  📄 `RCT.pdf`
:::

*🎯 Objetivo de la clase* 

Aprenderemos a estimar y comparar distintos **estimadores del efecto causal**:

- ATE (Efecto Promedio en la Población)
- ATT (Efecto Promedio en los Tratados)
- ATU (Efecto Promedio en los No Tratados)
- Estimador Naïve (diferencia observada entre grupos)

Repasamos conceptos de la clase anterior: resultados potenciales, estimadores de media y regresión simple.


---

*📥 Cargar la base y generar la variable de resultado observado* 

```stata
use "04_data.dta", clear

* Generamos el resultado observado según el tratamiento recibido
gen y = D*yd1 + (1-D)*yd0

label var y "Salarios en millones de pesos"
label define D 0 "Control" 1 "Tratados"
label value D D
numlabel, add
```

---

*📊 Estadísticas descriptivas por grupo*

```stata
tab D

          D |      Freq.     Percent        Cum.
------------+-----------------------------------
 0. Control |          4       50.00       50.00
1. Tratados |          4       50.00      100.00
------------+-----------------------------------
      Total |          8      100.00
      
      
sum y

    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
           y |          8       7.625    3.700869          3         12


bysort D: sum y
------------------------------------------------------------------------------------------
-> D = 0. Control

    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
           y |          4        4.25    .9574271          3          5

------------------------------------------------------------------------------------------
-> D = 1. Tratados

    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
           y |          4          11    .8164966         10         12

sum y if D == 0

    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
           y |          4        4.25    .9574271          3          5

sum y if D == 1

    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
           y |          4          11    .8164966         10         12

```

Nos permite observar las diferencias **promedio** entre grupos tratados y de control.

---

 *📎 Diferencia de medias*

```stata
ttest y, by(D)

Two-sample t test with equal variances
------------------------------------------------------------------------------
   Group |     Obs        Mean    Std. err.   Std. dev.   [95% conf. interval]
---------+--------------------------------------------------------------------
0. Contr |       4        4.25    .4787136    .9574271     2.72652     5.77348
1. Trata |       4          11    .4082483    .8164966    9.700772    12.29923
---------+--------------------------------------------------------------------
Combined |       8       7.625    1.308455    3.700869    4.530996      10.719
---------+--------------------------------------------------------------------
    diff |               -6.75    .6291529               -8.289482   -5.210518
------------------------------------------------------------------------------
    diff = mean(0. Contr) - mean(1. Trata)                        t = -10.7287
H0: diff = 0                                     Degrees of freedom =        6

    Ha: diff < 0                 Ha: diff != 0                 Ha: diff > 0
 Pr(T < t) = 0.0000         Pr(|T| > |t|) = 0.0000          Pr(T > t) = 1.0000

reg y D, robust
```

*📎 Regresioón Simple*
Modelo:
\[ Y = \alpha + \tau D + \varepsilon \]

- \( \tau \) representa el estimador naïve (diferencia de medias observada).
- No es causal si hay **sesgo de selección**.


```stata
Linear regression                               Number of obs     =          8
                                                F(1, 6)           =     115.11
                                                Prob > F          =     0.0000
                                                R-squared         =     0.9505
                                                Root MSE          =     .88976

------------------------------------------------------------------------------
             |               Robust
           y | Coefficient  std. err.      t    P>|t|     [95% conf. interval]
-------------+----------------------------------------------------------------
           D |       6.75   .6291529    10.73   0.000     5.210518    8.289482
       _cons |       4.25   .4787136     8.88   0.000      3.07863     5.42137
------------------------------------------------------------------------------
```
---

*🧮 Estimación de ATE, ATT y ATU*

```stata
gen tau = yd1 - yd0   // efecto individual
display "ATE: "
sum tau
scalar ATE = r(mean)
di "ATE = " ATE

sum tau if D == 1
scalar ATT = r(mean)
di "ATT = " ATT

sum tau if D == 0
scalar ATU = r(mean)
di "ATU = " ATU
```

---

*🧯 Comparación con el estimador naïve*

```stata
sum y if D==1
scalar ybar_1 = r(mean)

sum y if D==0
scalar ybar_0 = r(mean)

scalar NAIVE = ybar_1 - ybar_0
di "Naive = " NAIVE

di "Sesgo de selección = " NAIVE - ATT
```


*⚙️ Programa para estimadores*

```stata
cap prog drop estimadores
program define estimadores
    args tau y D

    di "--- Calculando estimadores ---"
    quietly {
        sum `tau'
        scalar ATE = r(mean)
        sum `tau' if `D' == 1
        scalar ATT = r(mean)
        sum `tau' if `D' == 0
        scalar ATU = r(mean)
        sum `y' if `D' == 1
        scalar ybar_1 = r(mean)
        sum `y' if `D' == 0
        scalar ybar_0 = r(mean)
        scalar NAIVE = ybar_1 - ybar_0
    }
    di "ATE = " ATE
    di "ATT = " ATT
    di "ATU = " ATU
    di "Naive = " NAIVE
    di "Sesgo de Selección = " NAIVE - ATT
end

* Llamamos el programa:
estimadores tau y D
```



*🧪 Experimento 1: ¿Qué pasa si aumento el tamaño muestral? *

```stata
drop y tau
expand 10000

* Generar nuevamente el resultado
gen y = D*yd1 + (1-D)*yd0
gen tau = yd1 - yd0

estimadores tau y D
```

🔍 **Moraleja:** Aumentar el tamaño no elimina el sesgo si hay selección.

---

*🧪 Experimento 2: Asignación aleatoria *

```stata
drop y D tau
set seed 87634
gen D = (uniform() > 0.5)
gen y = D*yd1 + (1-D)*yd0

* Repetimos estimadores
gen tau = yd1 - yd0

estimadores tau y D
...
```

🎯 Con asignación aleatoria:
\[ ATE \approx ATT \approx ATU \approx Naïve \]


---

*🧠 Reflexiones finales*

- El estimador naïve es solo válido si hay **asignación aleatoria**.
- La diferencia entre ATT y Naïve nos permite cuantificar el **sesgo de selección**.
- El ATE no es igual al ATT si hay **heterogeneidad en el tratamiento**.
- El tamaño muestral no soluciona problemas de sesgo.

---

## DESCARGA LOS DOCUMENTOS {-}

**Descargar Stata do file**:
[Descargar Stata](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/04_ParametrosStata/04_stata.do)

**Descargar R script**:
[Descargar R](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/04_ParametrosStata/04_R.R)

**Descargar Python Notebook**:
[Descargar Python](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/04_ParametrosStata/04_phyton.ipynb)

[![Abrir en Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/adiazescobar/libro_cortes/blob/main/dofile/04_ParametrosStata/04_phyton.ipynb)

**Descarga los Datos**:
[Descargar Datos](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/04_ParametrosStata/04_data.dta)
