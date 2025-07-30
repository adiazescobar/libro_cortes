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
