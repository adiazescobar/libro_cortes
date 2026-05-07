*===============================================================
* IV y LATE — Simulación pedagógica
* Curso: Econometría Avanzada (Javeriana)
*
* Parte A: Muestras finitas — OLS sesgado vs IV consistente
* Parte B: LATE paso a paso (Wald = LATE en compliers)
*===============================================================

clear all
set more off
capture ssc install ivreg2
capture ssc install ranktest

*===============================================================
* PARTE A — MUESTRAS FINITAS: OLS SESGADO vs IV CONSISTENTE
*===============================================================
*
* DGP (instrumento DEBIL: pi=0.2):
*   z, w, eD, u  ~ iid N(0,1)
*   D = 0.2*z + eD + w           (z apenas mueve a D)
*   y = 0.5*D + w + u            (tau verdadero = 0.5; w es confounder)
*
* Sesgo OLS:  cov(D,w)/var(D) ~ 0.49 -> plim(OLS) ~ 0.99
* IV consistente (plim = 0.5) PERO muy sesgado en muestra finita
* cuando F de la primera etapa es bajo (Bound, Jaeger & Baker 1995).
*
* En N=30 esperamos:
*   - OLS estable cerca de 0.99 (sesgo grande, pero bajo error muestral)
*   - IV con sesgo enorme HACIA OLS, varianza explosiva, F<2 (instr. debil)
* En N=10000:
*   - OLS sigue clavado en 0.99 (inconsistente)
*   - IV converge a 0.5, F~200 (consistencia visible)
*---------------------------------------------------------------

* (1) Una sola realizacion para mostrar la mecanica
clear
set seed 20260506
set obs 1000
gen z  = rnormal()
gen w  = rnormal()
gen eD = rnormal()
gen u  = rnormal()
gen D  = 0.2*z + eD + w
gen y  = 0.5*D + w + u

reg y D
ivreg2 y (D = z), first    // notese el F de primera etapa

*---------------------------------------------------------------
* (2) Monte Carlo: variar N para ver el sesgo en muestra finita
*     y la consistencia asintotica del IV
*---------------------------------------------------------------
cap program drop monteiv
program monteiv, rclass
    syntax, n(integer)
    drop _all
    set obs `n'
    gen z  = rnormal()
    gen w  = rnormal()
    gen eD = rnormal()
    gen u  = rnormal()
    gen D  = 0.2*z + eD + w
    gen y  = 0.5*D + w + u
    qui reg y D
    return scalar b_ols = _b[D]
    qui ivreg2 y (D = z)
    return scalar b_iv  = _b[D]
    * F de la primera etapa
    qui reg D z
    return scalar F1 = e(F)
end

* Para cada N: media, mediana, p25, p75 y F medio de primera etapa
* (la mediana y el IQR informan mejor que la media porque con
*  instrumento debil hay outliers fuertes en el IV)

* N = 30 (instrumento debilisimo: F~1.7, sesgo enorme del IV hacia OLS)
simulate b_ols=r(b_ols) b_iv=r(b_iv) F1=r(F1), reps(2000) seed(1): monteiv, n(30)
tabstat b_ols b_iv F1, stat(mean p50 p25 p75) col(stat)
hist b_iv, width(0.1) xline(0.5 1) ///
    title("IV en N=30 (verdadero=0.5, OLS~1)") name(h30, replace)

* N = 100
simulate b_ols=r(b_ols) b_iv=r(b_iv) F1=r(F1), reps(2000) seed(2): monteiv, n(100)
tabstat b_ols b_iv F1, stat(mean p50 p25 p75) col(stat)

* N = 300
simulate b_ols=r(b_ols) b_iv=r(b_iv) F1=r(F1), reps(2000) seed(3): monteiv, n(300)
tabstat b_ols b_iv F1, stat(mean p50 p25 p75) col(stat)

* N = 1000
simulate b_ols=r(b_ols) b_iv=r(b_iv) F1=r(F1), reps(2000) seed(4): monteiv, n(1000)
tabstat b_ols b_iv F1, stat(mean p50 p25 p75) col(stat)

* N = 10000 (F~200, IV converge limpio; OLS sigue sesgado)
simulate b_ols=r(b_ols) b_iv=r(b_iv) F1=r(F1), reps(2000) seed(5): monteiv, n(10000)
tabstat b_ols b_iv F1, stat(mean p50 p25 p75) col(stat)

* Histograma comparativo en N=10000 (consistencia visual)
twoway (hist b_ols, color(red%40) width(0.005)) ///
       (hist b_iv,  color(blue%40) width(0.005)), ///
       xline(0.5, lcolor(black) lpattern(dash)) ///
       xline(0.99, lcolor(red) lpattern(dash)) ///
       legend(order(1 "OLS" 2 "IV") position(2)) ///
       title("Distribucion de los estimadores, N=10,000") ///
       xtitle("Estimador") name(g_n10000, replace)


*===============================================================
* PARTE B — LATE PASO A PASO (replica de Clase19.pdf)
*===============================================================
*
* Construccion explicita de los 3 tipos:
*   never-takers (d00=1):     5,000 individuos -> D=0 siempre
*   always-takers (d11=1):    5,000 individuos -> D=1 siempre
*   compliers (d01=1):       10,000 individuos -> D=Z
*
* Efectos heterogeneos:
*   never-takers:  LATE = -0.5
*   always-takers: LATE =  0
*   compliers:     LATE = +1   <- lo que IV debe recuperar
*
* ATE = 0.25*(-0.5) + 0.25*(0) + 0.50*(1) = 0.375
*---------------------------------------------------------------

clear
set seed 54687
set obs 20000

* Instrumento aleatorio mitad-mitad
gen Z = uniform() > 0.5
tab Z

* Tipos
gen d00 = (_n <= 5000)                       // never-takers
gen d11 = (_n >  5000 & _n <= 10000)         // always-takers
gen d01 = (_n > 10000)                       // compliers
tab d00
tab d11
tab d01

* Efecto heterogeneo (LATE individual)
gen late = -0.5 if d00 == 1
replace late = 0  if d11 == 1
replace late = 1  if d01 == 1
tab late

* Resultados potenciales
gen y0 = 0.25 * invnormal(uniform())
gen y1 = y0 + late
sum y0 y1

* Tratamiento observado: D = d11 + Z*d01
gen D = d11 + Z*d01
tab D

* Verificacion: D consistente con tipos
tab D d00
tab D d11
tab D d01

* Resultado observado
gen y = D*y1 + (1-D)*y0

* ATE poblacional (no observable en la realidad)
sum late

* OLS: NO recupera ni el ATE (0.375) ni el LATE (1.0)
reg y D

* IV: recupera el LATE de compliers (= 1.0)
ivreg2 y (D = Z)

* Wald manual
sum y if Z==1
local EyZ1 = r(mean)
sum y if Z==0
local EyZ0 = r(mean)
sum D if Z==1
local EDZ1 = r(mean)
sum D if Z==0
local EDZ0 = r(mean)
di "Wald = " (`EyZ1' - `EyZ0') / (`EDZ1' - `EDZ0')

*===============================================================
* RESUMEN ESPERADO
*  Parte A:  OLS plim ~ 0.99 (sesgado, inconsistente, estable)
*           IV  plim = 0.50 (consistente PERO sesgado en muestra finita)
*           - N=30:    F~1.7; mediana(IV)~0.80 (sesgo enorme hacia OLS)
*           - N=10000: F~200; mediana(IV)~0.50 (consistencia visible)
*  Parte B:  OLS ~ 0.50 (no es ATE ni LATE)
*           IV  ~ 1.00 (= LATE de compliers)
*===============================================================
