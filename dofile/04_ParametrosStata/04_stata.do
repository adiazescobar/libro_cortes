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

* --------------------------
* Escenario 1: Con SELECCIÓN (viola independencia)
* --------------------------

clear all
set seed 12345

* Crear base para almacenar resultados de 1000 simulaciones
set obs 1000
gen sim_id = _n
gen naive_sesgo = .
gen ate_sesgo = .

* Loop de Monte Carlo
forvalues i = 1/1000 {

    quietly {
        preserve
        clear

        * Generar población de 1000 individuos
        set obs 1000

        * Generar resultados potenciales
        gen yd0 = rnormal(10, 3)     // Sin tratamiento
        gen yd1 = yd0 + rnormal(2, 1) // Con tratamiento (efecto heterogéneo)

        * SELECCIÓN: Los que tienen mejor yd0 se tratan más
        gen prob_D = invlogit((yd0 - 10)/2)  // Mayor yd0 → mayor prob de D=1
        gen D = (uniform() < prob_D)

        * Generar resultado observado
        gen y = D*yd1 + (1-D)*yd0
        gen tau = yd1 - yd0

        * Calcular estimadores
        sum tau
        scalar ate_sim = r(mean)

        sum tau if D==1
        scalar att_sim = r(mean)

        sum y if D==1
        scalar y1_sim = r(mean)
        sum y if D==0
        scalar y0_sim = r(mean)
        scalar naive_sim = y1_sim - y0_sim

        restore

        * Guardar resultados de esta simulación
        replace naive_sesgo = naive_sim - ate_sim in `i'
        replace ate_sesgo = att_sim - ate_sim in `i'
    }

    * Mostrar progreso cada 100 simulaciones
    if mod(`i', 100) == 0 {
        di "Simulación `i' de 1000 completada"
    }
}

* Resultados de la simulación CON SELECCIÓN
di _n "=== RESULTADOS CON SELECCIÓN (viola independencia) ==="
sum naive_sesgo
di "Sesgo promedio del estimador Naive: " r(mean)
di "El sesgo persiste incluso con muchas observaciones!"

* Gráfico
histogram naive_sesgo, ///
    xline(0, lcolor(red) lwidth(thick)) ///
    title("Distribución del Sesgo del Estimador Naive") ///
    subtitle("1000 simulaciones con SELECCIÓN") ///
    xtitle("Sesgo = Naive - ATE verdadero") ///
    note("Línea roja = sesgo cero (lo ideal)")
graph export "sesgo_con_seleccion.png", replace

* --------------------------
* Escenario 2: Con ALEATORIZACIÓN (cumple independencia)
* --------------------------

clear all
set seed 12345

* Crear base para almacenar resultados
set obs 1000
gen sim_id = _n
gen naive_sesgo = .
gen ate_sesgo = .

* Loop de Monte Carlo
forvalues i = 1/1000 {

    quietly {
        preserve
        clear

        * Generar población de 1000 individuos
        set obs 1000

        * Generar resultados potenciales (exactamente igual que antes)
        gen yd0 = rnormal(10, 3)
        gen yd1 = yd0 + rnormal(2, 1)

        * ALEATORIZACIÓN: D es independiente de yd0 y yd1
        gen D = (uniform() < 0.5)   // 50% tratamiento, 50% control

        * Generar resultado observado
        gen y = D*yd1 + (1-D)*yd0
        gen tau = yd1 - yd0

        * Calcular estimadores
        sum tau
        scalar ate_sim = r(mean)

        sum tau if D==1
        scalar att_sim = r(mean)

        sum y if D==1
        scalar y1_sim = r(mean)
        sum y if D==0
        scalar y0_sim = r(mean)
        scalar naive_sim = y1_sim - y0_sim

        restore

        * Guardar resultados
        replace naive_sesgo = naive_sim - ate_sim in `i'
        replace ate_sesgo = att_sim - ate_sim in `i'
    }

    if mod(`i', 100) == 0 {
        di "Simulación `i' de 1000 completada"
    }
}

* Resultados de la simulación CON ALEATORIZACIÓN
di _n "=== RESULTADOS CON ALEATORIZACIÓN (cumple independencia) ==="
sum naive_sesgo
di "Sesgo promedio del estimador Naive: " r(mean)
di "El sesgo es aproximadamente CERO!"

* Gráfico
histogram naive_sesgo, ///
    xline(0, lcolor(green) lwidth(thick)) ///
    title("Distribución del Sesgo del Estimador Naive") ///
    subtitle("1000 simulaciones con ALEATORIZACIÓN") ///
    xtitle("Sesgo = Naive - ATE verdadero") ///
    note("Línea verde = sesgo cero. ¡La distribución está centrada en cero!")
graph export "sesgo_con_aleatorizacion.png", replace

di _n "=== FIN DE LA SIMULACIÓN MONTE CARLO ==="
