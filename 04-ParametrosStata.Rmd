# Parámetros Causales en Stata {#parametros-causales-stata}

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

*🧪 Experimento 3: Simulación Monte Carlo - ¿El sesgo persiste siempre?*

**¿Qué es una simulación Monte Carlo?**

Es repetir un proceso aleatorio **muchas veces** (ej: 1000 repeticiones) para observar:
- La distribución de nuestros estimadores
- Si en promedio acertamos (sesgo)
- Qué tan dispersas son nuestras estimaciones (varianza)

**¿Por qué es útil?**

Nos permite **ver** qué pasaría si pudiéramos repetir nuestro estudio 1000 veces para analizar las propiedades estadísticas de nuestros estimadores. En la vida real solo tenemos una muestra, pero en simulación podemos explorar "¿qué pasaría si...?"

---

**📊 Escenario 1: Simulación con SELECCIÓN (viola independencia)**

Simulamos 1000 estudios donde las unidades **se auto-seleccionan** al tratamiento usando **nuestros datos de clase**:

**⚠️ ¿Por qué eliminamos el D original en cada simulación?**

Si NO eliminamos el D original, todas las 1000 simulaciones usarían exactamente la misma asignación de tratamiento y darían el mismo resultado. El Monte Carlo no tendría sentido.

Lo que queremos es simular "¿qué pasaría si repetimos el estudio 1000 veces?":
- En cada simulación mantenemos los mismos **resultados potenciales** (yd0, yd1)
- Pero **re-asignamos el tratamiento** de forma diferente (con sesgo de selección)

```stata
* Primero, preparamos los datos de clase expandidos
use "04_data.dta", clear

* Expandimos a 80,000 observaciones
expand 10000

* Guardamos los datos expandidos
tempfile datos_expandidos
save `datos_expandidos', replace

* Ahora creamos base para almacenar resultados de 1000 simulaciones
clear all
set seed 12345
set obs 1000
gen sim_id = _n
gen SESGO = .

* Loop de Monte Carlo
forvalues i = 1/1000 {

    quietly {
        preserve

        * Cargar datos expandidos de clase
        use `datos_expandidos', clear

        * IMPORTANTE: Eliminamos el D original y creamos uno nuevo en cada simulación
        * Si no hacemos esto, todas las simulaciones darían el mismo resultado
        drop D

        * SELECCIÓN: Los que tienen mejor yd0 se tratan más
        * Calcular media de yd0 para centrar
        sum yd0
        scalar mean_yd0 = r(mean)

        gen prob_D = invlogit((yd0 - mean_yd0)/2)  // Mayor yd0 → mayor prob de D=1
        gen D = (uniform() < prob_D)

        * Generar resultado observado y efecto individual
        gen y = D*yd1 + (1-D)*yd0
        gen tau = yd1 - yd0

        * Calcular estimadores (misma nomenclatura que en clase)
        sum tau
        scalar ATE = r(mean)

        sum tau if D==1
        scalar ATT = r(mean)

        sum y if D==1
        scalar ybar_1 = r(mean)
        sum y if D==0
        scalar ybar_0 = r(mean)
        scalar NAIVE = ybar_1 - ybar_0

        restore

        * Guardar el sesgo de esta simulación
        * Recordar: NAIVE = ATT + SESGO, por lo tanto SESGO = NAIVE - ATT
        replace SESGO = NAIVE - ATT in `i'
    }

    * Mostrar progreso cada 100 simulaciones
    if mod(`i', 100) == 0 {
        di "Simulación `i' de 1000 completada"
    }
}

* Resultados de la simulación CON SELECCIÓN
di _n "=== RESULTADOS CON SELECCIÓN (viola independencia) ==="
sum SESGO
di "Sesgo promedio del estimador Naive: " r(mean)
di "El sesgo persiste incluso con muchas observaciones!"

* Gráfico
histogram SESGO, ///
    xline(0, lcolor(red) lwidth(thick)) ///
    title("Distribución del Sesgo del Estimador Naive") ///
    subtitle("1000 simulaciones con SELECCIÓN - Datos de clase") ///
    xtitle("SESGO = NAIVE - ATT") ///
    note("Línea roja = sesgo cero (lo ideal)")
graph export "sesgo_con_seleccion.png", replace
```

📌 **Interpretación:**
- El histograma muestra que el estimador naive está **sistemáticamente sesgado**
- En promedio, el sesgo NO es cero (la distribución no está centrada en 0)
- **Aumentar el tamaño muestral NO elimina este sesgo** (problema de identificación)

---

**📊 Escenario 2: Simulación con ALEATORIZACIÓN (cumple independencia)**

Ahora simulamos 1000 estudios donde el tratamiento se asigna **aleatoriamente** usando **nuestros datos de clase**:

```stata
* Cargar datos de clase y expandir (si no está ya cargado del escenario anterior)
use "04_data.dta", clear
expand 10000
tempfile datos_expandidos
save `datos_expandidos', replace

* Crear base para almacenar resultados
clear all
set seed 12345
set obs 1000
gen sim_id = _n
gen SESGO = .

* Loop de Monte Carlo
forvalues i = 1/1000 {

    quietly {
        preserve

        * Cargar datos expandidos de clase
        use `datos_expandidos', clear

        * IMPORTANTE: Eliminamos el D original y creamos uno nuevo en cada simulación
        * Si no hacemos esto, todas las simulaciones darían el mismo resultado
        drop D

        * ALEATORIZACIÓN: D es independiente de yd0 y yd1
        gen D = (uniform() < 0.5)   // 50% tratamiento, 50% control

        * Generar resultado observado y efecto individual
        gen y = D*yd1 + (1-D)*yd0
        gen tau = yd1 - yd0

        * Calcular estimadores (misma nomenclatura que en clase)
        sum tau
        scalar ATE = r(mean)

        sum tau if D==1
        scalar ATT = r(mean)

        sum y if D==1
        scalar ybar_1 = r(mean)
        sum y if D==0
        scalar ybar_0 = r(mean)
        scalar NAIVE = ybar_1 - ybar_0

        restore

        * Guardar el sesgo de esta simulación
        * Recordar: NAIVE = ATT + SESGO, por lo tanto SESGO = NAIVE - ATT
        replace SESGO = NAIVE - ATT in `i'
    }

    if mod(`i', 100) == 0 {
        di "Simulación `i' de 1000 completada"
    }
}

* Resultados de la simulación CON ALEATORIZACIÓN
di _n "=== RESULTADOS CON ALEATORIZACIÓN (cumple independencia) ==="
sum SESGO
di "Sesgo promedio del estimador Naive: " r(mean)
di "El sesgo es aproximadamente CERO!"

* Gráfico
histogram SESGO, ///
    xline(0, lcolor(green) lwidth(thick)) ///
    title("Distribución del Sesgo del Estimador Naive") ///
    subtitle("1000 simulaciones con ALEATORIZACIÓN - Datos de clase") ///
    xtitle("SESGO = NAIVE - ATT") ///
    note("Línea verde = sesgo cero. ¡La distribución está centrada en cero!")
graph export "sesgo_con_aleatorizacion.png", replace
```

📌 **Interpretación:**
- El histograma muestra que el estimador naive **NO tiene sesgo sistemático**
- La distribución está centrada en cero (sesgo promedio ≈ 0)
- **La aleatorización garantiza que en promedio acertamos** (insesgadez)
- Hay variabilidad muestral (no todas las simulaciones dan exactamente ATE), ¡pero en promedio es correcto!

---

**🔬 Comparación lado a lado**

Para comparar ambos escenarios, puedes correr ambos códigos y guardar los resultados con nombres diferentes, luego combinarlos:

```stata
* Nota: Este código requiere haber corrido ambos escenarios y guardado los datos
* En el Escenario 1, antes del último 'graph export', agregar:
* save "resultados_seleccion.dta", replace

* En el Escenario 2, antes del último 'graph export', agregar:
* save "resultados_aleatorizacion.dta", replace

* Luego puedes combinar y comparar:
use "resultados_seleccion.dta", clear
gen escenario = 1
append using "resultados_aleatorizacion.dta"
replace escenario = 2 if missing(escenario)

label define esc 1 "Con selección" 2 "Con aleatorización"
label values escenario esc

* Gráfico comparativo
twoway ///
    (histogram naive_sesgo if escenario==1, color(red%30)) ///
    (histogram naive_sesgo if escenario==2, color(green%30)), ///
    legend(order(1 "Con selección" 2 "Con aleatorización")) ///
    xline(0, lcolor(black) lwidth(thick)) ///
    title("Comparación: Sesgo con vs sin aleatorización") ///
    subtitle("Usando datos de clase - 1000 simulaciones Monte Carlo") ///
    note("Con los datos de clase vemos la misma lección: solo la aleatorización elimina el sesgo")
graph export "comparacion_escenarios.png", replace
```

---

**💡 Lecciones clave del Monte Carlo**

1. **El sesgo de selección NO desaparece con más datos**
   - En el Escenario 1, incluso con 1000 observaciones por simulación, el sesgo persiste

2. **La aleatorización elimina el sesgo en expectativa**
   - En el Escenario 2, el sesgo promedio es ~0 (aunque hay variabilidad muestral)

3. **Podemos CUANTIFICAR el sesgo**
   - Las simulaciones nos muestran qué tan grande es el problema

4. **La inferencia causal requiere identificación, no solo más datos**
   - Big Data con sesgo sigue teniendo sesgo
   - Small Data bien identificado es más confiable

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
