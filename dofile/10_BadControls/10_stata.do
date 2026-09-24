********************************************************************************
* Malos controles — clase empirica
* Tres DGP: mediador, colisionador y proxy postratamiento contaminado
*
* Corre desde cualquier carpeta. Dentro del repositorio del libro escribe los
* resultados canonicos en dofile/10_BadControls/results; fuera de el, en una
* subcarpeta results de la carpeta de trabajo actual.
********************************************************************************

version 16
clear all
set more off

local root "."
capture confirm file "dofile/10_BadControls/10_stata.do"
if _rc == 0 local root "dofile/10_BadControls"
capture mkdir "`root'/results"

tempname estimates
tempfile estimates_dta
postfile `estimates' str24 case str28 specification str36 estimand ///
    double estimate se truth using `estimates_dta', replace

********************************************************************************
* CASO 1: D -> M -> Y. El estimando es el efecto total (= 2).
********************************************************************************
set seed 2468
set obs 10000
gen byte D = runiform() < .5
gen double M = 2*D + rnormal()
gen double Y = M + rnormal()

di as text _n "=== CASO 1: sin mediador (efecto total = 2) ==="
regress Y D, vce(robust)
post `estimates' ("mediator") ("without_mediator") ("total effect") ///
    (_b[D]) (_se[D]) (2)
di as text _n "=== CASO 1: con mediador ==="
regress Y D M, vce(robust)
post `estimates' ("mediator") ("with_mediator") ("not the total effect") ///
    (_b[D]) (_se[D]) (2)

********************************************************************************
* CASO 2: D -> C <- U -> Y. Controlar C abre el camino no causal.
********************************************************************************
clear
set seed 12345
set obs 10000
gen double D = rnormal()
gen double U = rnormal()
gen double C = 2*D - .5*U + rnormal()
gen double Y = U + rnormal()

di as text _n "=== CASO 2: sin colisionador (efecto verdadero = 0) ==="
regress Y D, vce(robust)
post `estimates' ("collider") ("without_collider") ("total effect") ///
    (_b[D]) (_se[D]) (0)
di as text _n "=== CASO 2: con colisionador ==="
regress Y D C, vce(robust)
post `estimates' ("collider") ("with_collider") ("noncausal coefficient") ///
    (_b[D]) (_se[D]) (0)

********************************************************************************
* CASO 3: D -> L <- U -> Y. L es un proxy postratamiento contaminado.
* D es aleatorio: sin L recuperamos el efecto total (= 2).
********************************************************************************
clear
set seed 99999
set obs 10000
gen double D = rnormal()
gen double U = rnormal()
gen double L = .8*D + 1.2*U + rnormal()
gen double Y = 2*D + 1.5*U + rnormal()

di as text _n "=== CASO 3: sin proxy (efecto total = 2) ==="
regress Y D, vce(robust)
post `estimates' ("contaminated_proxy") ("without_proxy") ("total effect") ///
    (_b[D]) (_se[D]) (2)
di as text _n "=== CASO 3: con proxy postratamiento ==="
regress Y D L, vce(robust)
post `estimates' ("contaminated_proxy") ("with_post_proxy") ("noncausal coefficient") ///
    (_b[D]) (_se[D]) (2)
di as text _n "=== CASO 3: con U (referencia, no observable) ==="
regress Y D U, vce(robust)
post `estimates' ("contaminated_proxy") ("with_true_U") ("total effect conditional on U") ///
    (_b[D]) (_se[D]) (2)

postclose `estimates'
use `estimates_dta', clear
export delimited using "`root'/results/bad_controls_estimates.csv", replace
list case specification estimate se truth, noobs sepby(case) abbreviate(16)

********************************************************************************
* MONTE CARLO
********************************************************************************
capture program drop mc_bad_controls
program define mc_bad_controls, rclass
    version 16
    syntax, Case(integer)
    clear
    set obs 1500

    if `case' == 1 {
        gen byte D = runiform() < .5
        gen double M = 2*D + rnormal()
        gen double Y = M + rnormal()
        quietly regress Y D
        return scalar correct = _b[D]
        quietly regress Y D M
        return scalar bad = _b[D]
    }
    else if `case' == 2 {
        gen double D = rnormal()
        gen double U = rnormal()
        gen double C = 2*D - .5*U + rnormal()
        gen double Y = U + rnormal()
        quietly regress Y D
        return scalar correct = _b[D]
        quietly regress Y D C
        return scalar bad = _b[D]
    }
    else {
        gen double D = rnormal()
        gen double U = rnormal()
        gen double L = .8*D + 1.2*U + rnormal()
        gen double Y = 2*D + 1.5*U + rnormal()
        quietly regress Y D
        return scalar correct = _b[D]
        quietly regress Y D L
        return scalar bad = _b[D]
    }
end

tempname mc
tempfile mc_dta
postfile `mc' str24 case str28 specification double mean_estimate truth ///
    int repetitions using `mc_dta', replace

forvalues c = 1/3 {
    quietly simulate correct=r(correct) bad=r(bad), reps(300) seed(`=7000+`c''): ///
        mc_bad_controls, case(`c')
    di as text _n "=== MONTE CARLO, CASO `c': correct = sin mal control; bad = con mal control ==="
    summarize correct bad
    quietly summarize correct
    local good_mean = r(mean)
    quietly summarize bad
    local bad_mean = r(mean)
    if `c' == 1 {
        post `mc' ("mediator") ("without_mediator") (`good_mean') (2) (300)
        post `mc' ("mediator") ("with_mediator") (`bad_mean') (2) (300)
    }
    else if `c' == 2 {
        post `mc' ("collider") ("without_collider") (`good_mean') (0) (300)
        post `mc' ("collider") ("with_collider") (`bad_mean') (0) (300)
    }
    else {
        post `mc' ("contaminated_proxy") ("without_proxy") (`good_mean') (2) (300)
        post `mc' ("contaminated_proxy") ("with_post_proxy") (`bad_mean') (2) (300)
    }
}

postclose `mc'
use `mc_dta', clear
export delimited using "`root'/results/bad_controls_montecarlo.csv", replace
list case specification mean_estimate truth, noobs sepby(case) abbreviate(16)

di as result "Resultados exportados en `root'/results/"
