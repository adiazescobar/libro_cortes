/*
Propensity score matching — Clase empírica
Ana María Díaz Escobar, Econometría Avanzada

Objetivo: psmatch2 como flujo principal; teffects psmatch como verificación;
telasso como extensión de alta dimensión con confusores obligatorios.
Datos: base6.dta
Requiere: psmatch2 (SSC) y Stata 17 o superior para telasso.
*/

version 19
clear all
set more off
set linesize 100
set seed 1298

* Resolver la ruta desde la raíz del libro o desde la carpeta del do-file
capture confirm file "base6.dta"
if _rc {
    capture confirm file "dofile/16_PSM_IPW_Sinteticos/base6.dta"
    if !_rc cd "dofile/16_PSM_IPW_Sinteticos"
}
capture confirm file "base6.dta"
if _rc {
    di as error "No se encontró base6.dta. Ejecute desde la raíz del libro o la carpeta del do-file."
    exit 601
}

capture which psmatch2
if _rc {
    di as error "Falta psmatch2. Instálelo una vez con: ssc install psmatch2"
    exit 199
}

capture log close
log using "psm_classroom.log", replace text
use "base6.dta", clear

global Xmust "personas orden_n ocupado_jefe educa_jefe ingresos_hogar_jefe hombre"
global Xflex "c.personas#c.personas c.educa_jefe#c.educa_jefe c.ingresos_hogar_jefe#c.ingresos_hogar_jefe i.ocupado_jefe#i.hombre"

di as text "PASO 1. Definir estimando: ATT = E[Y(D=1)-Y(D=0)|D=1]"
tab D
table D, statistic(count D) statistic(mean y2 personas ocupado_jefe educa_jefe)
mean y2, over(D)
lincom _b[c.y2@1.D] - _b[c.y2@0.D]
scalar diff_raw = r(estimate)

di as text "PASO 2. Estimar propensity score con confusores pretratamiento"
logit D $Xmust
predict double pscore, pr
summarize pscore, detail

twoway (kdensity pscore if D==1, lcolor(navy) lwidth(medthick)) ///
       (kdensity pscore if D==0, lcolor(maroon) lwidth(medthick)), ///
       legend(order(1 "Tratados" 2 "Controles")) ///
       title("Soporte común") xtitle("Propensity score") ytitle("Densidad")
graph export "pscore_distribution.png", replace width(1800)

di as text "PASO 3. psmatch2 principal: logit, ATT, NN(1), reemplazo, common, ties"
psmatch2 D $Xmust, outcome(y2) logit neighbor(1) common ties
scalar att_psm = r(att)
scalar se_psm  = r(seatt)
count if D==1 & _support==0
scalar offsupport = r(N)
psgraph
graph export "psm_support.png", replace width(1800)
pstest $Xmust, treated(D) both graph
graph export "psm_balance.png", replace width(1800)

di as text "PASO 4. Sensibilidad a decisiones de matching"
psmatch2 D $Xmust, outcome(y2) logit neighbor(5) common ties
scalar att_nn5 = r(att)
psmatch2 D $Xmust, outcome(y2) logit kernel kerneltype(epan) bwidth(0.06) common
scalar att_kernel = r(att)
psmatch2 D $Xmust, outcome(y2) logit neighbor(1) caliper(0.02) common ties
scalar att_caliper = r(att)

di as text "PASO 5. Verificar ATET con teffects psmatch"
teffects psmatch (y2) (D $Xmust, logit), atet nneighbor(1)
matrix b_te = e(b)
matrix V_te = e(V)
scalar att_teffects = b_te[1,1]
scalar se_teffects  = sqrt(V_te[1,1])
estimates store te_psm

di as text "PASO 6. telasso: confusores obligatorios y candidatos flexibles"
telasso (y2 $Xflex, ainclude($Xmust)) ///
        (D  $Xflex, ainclude($Xmust)), ///
        atet selection(plugin) xfolds(5) resample(3) rseed(1298)
matrix b_lasso = e(b)
matrix V_lasso = e(V)
scalar att_telasso = b_lasso[1,1]
scalar se_telasso  = sqrt(V_lasso[1,1])
estimates store te_lasso

di as result "COMPARACIÓN PEDAGÓGICA"
di as text   "Diferencia cruda         = " as result %7.3f diff_raw
di as text   "psmatch2 NN(1), ATT      = " as result %7.3f att_psm ///
    as text "  ES reportado = " as result %7.3f se_psm
di as text   "psmatch2 NN(5), ATT      = " as result %7.3f att_nn5
di as text   "psmatch2 kernel, ATT     = " as result %7.3f att_kernel
di as text   "psmatch2 caliper, ATT    = " as result %7.3f att_caliper
di as text   "teffects psmatch, ATET   = " as result %7.3f att_teffects ///
    as text "  ES ajustado = " as result %7.3f se_teffects
di as text   "telasso, ATET            = " as result %7.3f att_telasso ///
    as text "  ES robusto  = " as result %7.3f se_telasso
di as text   "Tratados fuera soporte  = " as result %7.0f offsupport

di as text "Interpretar cercanía, no exigir igualdad mecánica."
di as text "Balance observado no demuestra CIA."

log close
exit
