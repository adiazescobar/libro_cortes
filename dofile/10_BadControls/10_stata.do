********************************************************************************
* Malos Controles en Stata
* Econometria Avanzada - Ana Maria Diaz
********************************************************************************

clear all
set more off

********************************************************************************
* PARTE 1: MEDIADOR PURO (sobrecontrol del efecto total)
* DGP: D -> M -> Y, sin otras causas de M o Y (salvo ruido)
********************************************************************************

set seed 2468
set obs 10000

* Parametros (efecto total = a*b)
scalar a = 2     // D -> M
scalar b = 1     // M -> Y

gen double D = (runiform()<0.5)
gen double M = a*D + rnormal()
gen double Y = b*M + rnormal()

* (Total) Y ~ D  -> coef(D) ≈ a*b = 2
di "=== Regresion sin mediador (efecto TOTAL) ==="
reg Y D, vce(robust)

* (Directo) Y ~ D M  -> bloquea D->M->Y, coef(D) ≈ 0  => mal control para TOTAL
di "=== Regresion con mediador (efecto DIRECTO - MAL CONTROL) ==="
reg Y D M, vce(robust)

********************************************************************************
* PARTE 2: COLISIONADOR CLASICO
* D y Y independientes; C es efecto de D y Y
********************************************************************************

clear
set seed 2468
set obs 1000

* D y Y independientes
gen double D = rnormal()
gen double Y = rnormal()

* C es colisionador (causado por D y Y)
gen double C = 2*D - 0.5*Y + rnormal()

* (Correcto) Y ~ D  -> ~0
di "=== Regresion sin colisionador (CORRECTO) ==="
reg Y D, vce(robust)

* (MAL) Y ~ D C  -> sesgo por abrir camino espurio
di "=== Regresion con colisionador (MAL CONTROL) ==="
reg Y D C, vce(robust)

********************************************************************************
* PARTE 3: MONTE CARLO - MEDIADOR
********************************************************************************

capture program drop mc_mediador_puro
program define mc_mediador_puro, rclass
    clear
    set obs 3000
    gen double D = (runiform()<0.5)
    gen double M = 2*D + rnormal()
    gen double Y = 1*M + rnormal()
    qui reg Y D
    return scalar b_total = _b[D]
    qui reg Y D M
    return scalar b_directo = _b[D]
end

di "=== Monte Carlo: Mediador Puro (300 reps) ==="
simulate b_total=r(b_total) b_directo=r(b_directo), reps(300): mc_mediador_puro
summ b_total b_directo

********************************************************************************
* PARTE 4: MONTE CARLO - COLISIONADOR
********************************************************************************

capture program drop mc_colisionador
program define mc_colisionador, rclass
    clear
    set obs 1000
    gen double D = rnormal()
    gen double Y = rnormal()
    gen double C = 2*D - 0.5*Y + rnormal()
    qui reg Y D
    return scalar b_noC = _b[D]
    qui reg Y D C
    return scalar b_conC = _b[D]
end

di "=== Monte Carlo: Colisionador (300 reps) ==="
simulate b_noC=r(b_noC) b_conC=r(b_conC), reps(300): mc_colisionador
summ b_noC b_conC

********************************************************************************
* PARTE 5: F DESCENDIENTE DE COLISIONADOR
* D -> Y <- U  y  Y -> F
********************************************************************************

clear
set seed 12345
set obs 1000

gen double D  = rnormal()
gen double eY = rnormal()
gen double Y  = 2*D + eY
gen double F  = -0.5*Y + rnormal()

di "=== F descendiente de colisionador: Sin F (CORRECTO) ==="
reg Y D, vce(robust)

di "=== F descendiente de colisionador: Con F (MAL CONTROL) ==="
reg Y D F, vce(robust)

********************************************************************************
* PARTE 6: F POST-TRATAMIENTO (MEDIADOR)
* D -> F -> Y
********************************************************************************

clear
set seed 12345
set obs 1000

scalar a = 2
scalar b = 3

gen double D = rnormal()
gen double F = a*D + rnormal()
gen double Y = b*F + rnormal()

di "=== F post-tratamiento: Sin F (efecto TOTAL) ==="
reg Y D, vce(robust)

di "=== F post-tratamiento: Con F (sobrecontrol) ==="
reg Y D F, vce(robust)

di "=== FIN DEL DO FILE ==="
