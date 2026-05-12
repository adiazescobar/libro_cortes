*===============================================================
* RDD_simulacion.do
* Econometría Avanzada — Javeriana
* Acompañamiento al Cap. Regresión Discontinua (libro_cortes)
*
* Cinco partes:
*   A. RDN con DGP lineal
*   B. RDN con DGP curvo (Gelman & Imbens 2019)
*   C. Sensibilidad al bandwidth
*   D. Tests de validez (rddensity, covariables, placebos)
*   E. RDB (rdrobust fuzzy + 2SLS local)
*   F. Variable de asignación discreta (Kolesár & Rothe 2018)
*===============================================================

clear all
set more off
capture log close
log using "RDD_simulacion.log", replace text

* Paquetes requeridos
* ssc install rdrobust
* ssc install rddensity
* ssc install lpdensity

*===============================================================
* PARTE A — RDN con DGP lineal
*===============================================================

clear
set seed 20260511
set obs 2000

gen Z    = runiform()*24 + 50          // edad entre 50 y 74
gen D    = (Z >= 62)
gen Zt   = Z - 62                       // Z centrada en cutoff
gen y    = 1 + 0.10*Zt + 2.0*D + rnormal()

* Visualización primero
rdplot y Z, c(62) graph_options( ///
    title("RDN — DGP lineal") ///
    xtitle("Edad") ytitle("Resultado"))
graph export "rdplot_A.png", replace

* --- Estimación I: polinomio global ---
gen DZt  = D*Zt
gen Zt2  = Zt^2
gen DZt2 = D*Zt2

di _newline "*** Polinomio lineal global ***"
reg y D Zt DZt

di _newline "*** Polinomio cuadrático global ***"
reg y D Zt DZt Zt2 DZt2

* --- Estimación II: local lineal con kernel triangular ---
di _newline "*** Local lineal con rdrobust (kernel triangular, MSE-óptimo) ***"
rdrobust y Z, c(62) p(1) kernel(triangular)

*===============================================================
* PARTE B — RDN con DGP curvo
*===============================================================

clear
set seed 20260511
set obs 2000

gen Z   = runiform()*24 + 50
gen D   = (Z >= 62)
gen Zt  = Z - 62
gen Zt2 = Zt^2
gen y   = 1 + 0.5*Zt - 0.02*Zt2 + 2.0*D + rnormal()

rdplot y Z, c(62) graph_options( ///
    title("RDN — DGP curvo") ///
    xtitle("Z") ytitle("y"))
graph export "rdplot_B.png", replace

gen DZt  = D*Zt
gen DZt2 = D*Zt2

di _newline "*** [DGP curvo] Polinomio lineal — sesgo esperado ***"
reg y D Zt DZt

di _newline "*** [DGP curvo] Polinomio cuadrático — corrige (si conocemos el DGP) ***"
reg y D Zt DZt Zt2 DZt2

di _newline "*** [DGP curvo] Local lineal — no requiere conocer la curvatura ***"
rdrobust y Z, c(62) p(1) kernel(triangular)

*===============================================================
* PARTE C — Sensibilidad al bandwidth
*===============================================================

* Recargamos DGP lineal sencillo
clear
set seed 20260511
set obs 2000
gen Z  = runiform()*24 + 50
gen D  = (Z >= 62)
gen Zt = Z - 62
gen y  = 1 + 0.10*Zt + 2.0*D + rnormal()

di _newline "*** Barrido manual de bandwidth ***"
foreach h in 1 2 3 5 8 12 {
    qui rdrobust y Z, c(62) h(`h') p(1) kernel(triangular)
    di "h = " %-3.0f `h' "   coef = " %6.3f e(tau_cl) ///
       "   se = " %5.3f e(se_tau_cl)
}

di _newline "*** Bandwidth automático MSE-óptimo (CCT, default) ***"
rdrobust y Z, c(62) bwselect(mserd)

di _newline "*** Bandwidth Coverage-error-rate óptimo ***"
rdrobust y Z, c(62) bwselect(cerrd)

*===============================================================
* PARTE D — Tests de validez
*===============================================================

* D.1 Test de manipulación: rddensity
di _newline "*** rddensity — no manipulación (H0 esperado: no rechazo) ***"
rddensity Z, c(62)

* Caso patológico: forzamos manipulación
preserve
    replace Z = 62.5 if Z >= 61.5 & Z < 62 & runiform() < 0.5
    di _newline "*** rddensity — con manipulación artificial (debería rechazar) ***"
    rddensity Z, c(62)
restore

* D.2 Continuidad de covariables predeterminadas
clear
set seed 20260511
set obs 2000
gen Z     = runiform()*24 + 50
gen D     = (Z >= 62)
gen Zt    = Z - 62
gen sexo  = runiform() < 0.5             // covariable balanceada
gen ingre = 100 + 5*D + rnormal()*10     // contaminada por el tratamiento
gen y     = 1 + 0.10*Zt + 2.0*D + rnormal()

di _newline "*** Covariable predeterminada (sexo) — no debe saltar ***"
rdrobust sexo Z, c(62)

di _newline "*** Covariable contaminada (ingre) — sí salta: NO usar como control ***"
rdrobust ingre Z, c(62)

* D.3 Placebos en cutoffs falsos
di _newline "*** Placebos en cutoffs falsos — ninguno debe ser significativo ***"
foreach c_fake in 58 60 64 66 {
    qui rdrobust y Z, c(`c_fake')
    di "Cutoff falso " %-3.0f `c_fake' "   coef = " %6.3f e(tau_cl) ///
       "   robust p = " %5.3f e(pv_rb)
}

*===============================================================
* PARTE E — RDB
*===============================================================

clear
set seed 20260511
set obs 5000

gen Z  = runiform()*24 + 50
gen W  = (Z >= 62)                       // elegibilidad
gen Zt = Z - 62
gen p  = 0.20 + 0.60*W                   // toma del tratamiento
gen D  = runiform() < p
gen y  = 1 + 0.10*Zt + 2.0*D + rnormal()

* Salto en el resultado (forma reducida)
di _newline "*** Forma reducida: salto en y ***"
rdrobust y Z, c(62)

* Salto en el tratamiento (primera etapa)
di _newline "*** Primera etapa: salto en D ***"
rdrobust D Z, c(62)

* RDB: ratio de los dos saltos
di _newline "*** RDB con rdrobust ***"
rdrobust y Z, c(62) fuzzy(D)

* Equivalencia con 2SLS local
local h = 5
gen WZt = W*Zt
di _newline "*** 2SLS local con bandwidth fijo h = `h' (kernel uniforme) ***"
ivregress 2sls y Zt WZt (D = W) if abs(Zt) < `h', vce(robust)

*===============================================================
* PARTE F — Variable de asignación discreta (Kolesár & Rothe 2018)
*===============================================================

clear
set seed 20260511
set obs 3000

gen Z  = floor(runiform()*101)           // puntaje entero 0-100
gen D  = (Z >= 50)
gen Zt = Z - 50
gen y  = 1 + 0.05*Zt + 2.0*D + rnormal()

di _newline "*** SE convencionales (subcubren cuando Z es discreta) ***"
rdrobust y Z, c(50)

di _newline "*** SE clusterizados en Z (Lee & Card 2008) — más conservadores ***"
rdrobust y Z, c(50) cluster(Z)

* Nota: honest CIs (Kolesár & Rothe 2018) requieren el paquete R RDHonest;
* no hay implementación oficial en Stata.

log close
display "RDD_simulacion.do — terminado."
