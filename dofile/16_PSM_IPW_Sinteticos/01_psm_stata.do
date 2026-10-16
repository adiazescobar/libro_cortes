/*
Título: PSM en Stata: Propensity Score Matching
Autor: Ana María Díaz Escobar
Fecha: 2026-04-23 (actualizado 2026-10-16, clase del 15 de octubre de 2026)

Objetivos:
  1. Estimar el propensity score con covariables observadas.
  2. Calcular efectos marginales del probit con margins (bloque 2B).
  3. Comparar algoritmos de matching en Stata.
  4. Verificar soporte común y balance post-matching.
  5. Seleccionar términos del propensity score con LASSO sin soltar
     los confusores obligatorios (bloque 11).

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
* 2B. PROBIT Y EFECTOS MARGINALES (margins)
***============================================***

* Los coeficientes del probit no son efectos sobre la probabilidad.
* El efecto de una x continua es g(xb)*b, distinto para cada hogar.
* Con i. Stata sabe cuáles variables son binarias y para ellas
* calcula G(xb | x=1) - G(xb | x=0) en lugar de una derivada.

probit D personas orden_n i.ocupado_jefe educa_jefe ingresos_hogar_jefe i.hombre
predict double pscore_probit, pr
predict double xb_probit, xb

* Efecto marginal promedio (AME): promedio sobre i de g(x_i b)*b
margins, dydx(*)

* Efecto marginal en las medias de X (MEM)
margins, dydx(*) atmeans

* Verificación a mano para educa_jefe, que es continua.
* normalden() es la densidad normal g(.)
gen double em_educa = normalden(xb_probit) * _b[educa_jefe]
summ em_educa
di "AME de educa_jefe calculado a mano = " r(mean)

* Verificación a mano para hombre, que es binaria.
* Para cada hogar se predice con hombre=1 y con hombre=0 y se resta.
gen double xb_h1 = xb_probit + _b[1.hombre]*(1 - hombre)
gen double xb_h0 = xb_probit - _b[1.hombre]*hombre
gen double em_hombre = normal(xb_h1) - normal(xb_h0)
summ em_hombre
di "AME de hombre calculado a mano = " r(mean)

* El logit da coeficientes distintos pero efectos marginales casi iguales
logit D personas orden_n i.ocupado_jefe educa_jefe ingresos_hogar_jefe i.hombre
margins, dydx(*)

* Y el propensity score de los dos modelos es casi el mismo
corr pscore1 pscore_probit

drop xb_probit em_educa xb_h1 xb_h0 em_hombre

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

***============================================***
* 11. SELECCIÓN DE VARIABLES CON LASSO
***============================================***

* Regla de la clase: los confusores obligatorios se escogen con teoría
* y LASSO no puede sacarlos. LASSO solo decide qué términos adicionales
* (cuadrados e interacciones) entran al propensity score.
* psmatch2 no acepta notación de factores, así que creamos los términos.

gen double personas2  = personas^2
gen double educa2     = educa_jefe^2
gen double ingresos2  = ingresos_hogar_jefe^2
gen double ocup_hom   = ocupado_jefe*hombre
gen double educa_ingr = educa_jefe*ingresos_hogar_jefe

global Xflex "personas2 educa2 ingresos2 ocup_hom educa_ingr"

* (a) Lo que NO hacemos: dejar que LASSO escoja todo, incluidos
* los confusores. Sirve para ver qué sacaría si lo dejáramos.
lasso probit D $X $Xflex, selection(cv) rseed(1298)
lassocoef

* (b) Lo que sí hacemos: los paréntesis obligan a mantener $X
* y LASSO solo elige entre los términos de $Xflex.
lasso probit D ($X) $Xflex, selection(cv) rseed(1298)
lassocoef
global Xsel "`e(allvars_sel)'"
di "Variables en el propensity score: $Xsel"

* Matching con el propensity score que eligió LASSO
psmatch2 D $Xsel, outcome(y2) n(1) common ai(1)
pstest $X, treated(D) both

* (c) Benchmark doblemente robusto: telasso usa LASSO en el modelo
* del resultado y en el del tratamiento. ainclude() deja fijo $X.
telasso (y2 $X $Xflex, ainclude($X)) (D $X $Xflex, ainclude($X)), atet rseed(1298)
lassocoef (., for(D)) (., for(y2) tlevel(0)) (., for(y2) tlevel(1))

log close
