/*
Título: PSM en Stata: Propensity Score Matching
Autor: Ana María Díaz Escobar
Fecha: 2026-04-23

Objetivos:
  1. Estimar el propensity score con covariables observadas.
  2. Comparar algoritmos de matching en Stata.
  3. Verificar soporte común y balance post-matching.

Datos requeridos: base6.dta en esta carpeta o en dofile/16_PSM_IPW_Sinteticos/
Paquetes requeridos: psmatch2 (incluye pstest y psgraph)
*/

clear all
set seed 1298
set more off
set linesize 100

* Resolver ruta de datos sin depender de un computador específico
capture confirm file "base6.dta"
if _rc {
    capture confirm file "dofile/16_PSM_IPW_Sinteticos/base6.dta"
    if !_rc cd "dofile/16_PSM_IPW_Sinteticos"
}
capture confirm file "base6.dta"
if _rc {
    di as error "No se encontró base6.dta. Corre este do-file desde su carpeta o desde la raíz del libro."
    exit 601
}

* Cargar datos
use base6.dta, clear

* Instalar comandos necesarios (ejecutar solo la primera vez)
* ssc install psmatch2
* ssc install pstest

* Definir variables
global X "personas orden_n ocupado_jefe educa_jefe ingresos_hogar_jefe hombre"

log using psm_demo.log, replace

***============================================***
* 1. ESTADÍSTICAS DESCRIPTIVAS
***============================================***

tab D
table D, stat(mean y2)

***============================================***
* 2. ESTIMAR EL PROPENSITY SCORE
***============================================***

logit D $X
predict double pscore1, pr

summ pscore1, detail

***============================================***
* 3. VERIFICAR SOPORTE COMÚN
***============================================***

twoway (kdensity pscore1 if D==1, lcolor(blue) lwidth(medium)) ///
       (kdensity pscore1 if D==0, lcolor(red) lwidth(medium)), ///
       legend(label(1 "Tratados (D=1)") label(2 "Controles (D=0)")) ///
       title("Distribución del Propensity Score") ///
       xtitle("Propensity Score") ytitle("Densidad")

graph export pscore_distribution.png, replace

***============================================***
* 4. NEAREST NEIGHBOR (1) - SIN REEMPLAZO
***============================================***

drawnorm orden
sort orden

psmatch2 D, outcome(y2) n(1) pscore(pscore1) noreplacement
psgraph
pstest $X, treated(D) both graph

***============================================***
* 5. NEAREST NEIGHBOR (1) - CON SOPORTE COMÚN
***============================================***

psmatch2 D $X, outcome(y2) n(1) noreplacement common
psgraph
pstest $X, treated(D) both graph

***============================================***
* 6. NEAREST NEIGHBOR (1) - CON REEMPLAZO Y ES ANALÍTICOS
***============================================***

psmatch2 D $X, outcome(y2) n(1) common ai(1)
psgraph
pstest $X, treated(D) both graph

***============================================***
* 7. KERNEL MATCHING - EPANECHNIKOV
***============================================***

psmatch2 D $X, outcome(y2) kernel kerneltype(epan) bwidth(0.06) common
psgraph
pstest $X, treated(D) both graph

***============================================***
* 8. KERNEL MATCHING - GAUSSIANO
***============================================***

psmatch2 D $X, outcome(y2) kernel kerneltype(normal) bwidth(0.06) common
psgraph
pstest $X, treated(D) both graph

***============================================***
* 9. RADIO MATCHING CON CALIPER
***============================================***

psmatch2 D $X, outcome(y2) radius caliper(0.001) common ai(1)
psgraph
pstest $X, treated(D) both graph

***============================================***
* 10. ALTERNATIVA: teffects psmatch (Stata nativo 13+)
***============================================***

* ATT con NN(1)
teffects psmatch (y2) (D $X, probit), atet
estimates store att_nn1

* ATE con NN(1)
teffects psmatch (y2) (D $X, probit), ate nn(1)
estimates store ate_nn1

* Comparación
estimates table att_nn1 ate_nn1, b se

log close
