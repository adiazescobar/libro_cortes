* Clase 4 - Estimadores Causales en Secciones Transversales
* Profesora: Ana Díaz

* --------------------------
* Cargar datos y generar y
* --------------------------

use "04_data.dta", clear

gen y = D*yd1 + (1-D)*yd0
label var y "Salarios en millones de pesos"
label define D 0 "Control" 1 "Tratados"
label value D D
numlabel, add

* --------------------------
* Estadísticas descriptivas
* --------------------------

tab D
sum y
bysort D: sum y
sum y if D == 0
sum y if D == 1

* --------------------------
* Diferencia de medias y regresión
* --------------------------

ttest y, by(D)
reg y D, robust

* --------------------------
* Generar efecto individual (tau)
* --------------------------

gen tau = yd1 - yd0

* --------------------------
* Definir programa estimadores
* --------------------------

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

* --------------------------
* Ejecutar programa estimadores
* --------------------------

estimadores tau y D

* --------------------------
* Experimento 1: Aumentar tamaño muestral
* --------------------------

drop y tau
expand 10000

gen y = D*yd1 + (1-D)*yd0
gen tau = yd1 - yd0

estimadores tau y D

* --------------------------
* Experimento 2: Asignación aleatoria
* --------------------------

drop y D tau
set seed 87634
gen D = (uniform() > 0.5)
gen y = D*yd1 + (1-D)*yd0
gen tau = yd1 - yd0

estimadores tau y D

* ==================================================
* Experimento 3: Simulación Monte Carlo
* ==================================================

* ¿Por qué eliminamos el D original en cada simulación?
* Si NO eliminamos el D original, todas las 1000 simulaciones usarían
* exactamente la misma asignación de tratamiento y darían el mismo resultado.
* El Monte Carlo no tendría sentido.
*
* Lo que queremos es simular "¿qué pasaría si repetimos el estudio 1000 veces?":
* - En cada simulación mantenemos los mismos resultados potenciales (yd0, yd1)
* - Pero re-asignamos el tratamiento de forma diferente

* --------------------------
* Escenario 1: Con SELECCIÓN (viola independencia)
* --------------------------

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

* --------------------------
* Escenario 2: Con ALEATORIZACIÓN (cumple independencia)
* --------------------------

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

di _n "=== FIN DE LA SIMULACIÓN MONTE CARLO ==="
