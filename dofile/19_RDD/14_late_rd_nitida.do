/*================================================================
  Econometría Avanzada 2026-2 · Pontificia Universidad Javeriana
  Clase: LATE en Stata y regresión discontinua nítida
  Ana María Díaz Escobar

  PARTE A  LATE paso a paso (simulación con tres tipos y con defiers)
  PARTE B  RD nítida: beca por puntaje Saber 11 (datos simulados)
  PARTE C  RD nítida: ancho de banda y pruebas de validez
  PARTE D  RD nítida con manipulación del puntaje

  Paquetes (instalar una sola vez):
    ssc install ivreg2
    ssc install ranktest
    net install rdrobust,  from(https://raw.githubusercontent.com/rdpackages/rdrobust/master/stata)  replace
    net install rddensity, from(https://raw.githubusercontent.com/rdpackages/rddensity/master/stata) replace
    net install lpdensity, from(https://raw.githubusercontent.com/nppackages/lpdensity/master/stata) replace

  Las gráficas se guardan como png en la carpeta de trabajo actual.
================================================================*/

clear all
set more off
set linesize 100
capture log close
log using 14_late_rd_nitida.log, replace text


*================================================================
* PARTE A — LATE PASO A PASO (simulación de Clase19)
*
* Construimos los tres tipos a mano:
*   never-takers  (d00 = 1):  5.000 personas -> D = 0 siempre
*   always-takers (d11 = 1):  5.000 personas -> D = 1 siempre
*   compliers     (d01 = 1): 10.000 personas -> D = Z
*
* Efectos heterogéneos:
*   never-takers  -0,5
*   always-takers  0
*   compliers     +1   <- lo que VI debe recuperar
*
* ATE = 0,25(-0,5) + 0,25(0) + 0,5(1) = 0,375
*================================================================

clear
set seed 54687
set obs 20000

* Instrumento aleatorio mitad y mitad
gen Z = uniform() > 0.5
tab Z

* Tipos
gen d00 = (_n <= 5000)                       // never-takers
gen d11 = (_n >  5000 & _n <= 10000)         // always-takers
gen d01 = (_n > 10000)                       // compliers
tab d00
tab d11
tab d01

* Tratamiento observado
gen D = d11 + Z*d01
tab D

* ¿Estamos seguros de que está bien?
tab D d00
tab D d11
tab D d01

* Efecto de cada grupo
gen late = -0.5 if d00 == 1
replace late = 0  if d11 == 1
replace late = 1  if d01 == 1
tab late

* Resultados potenciales
gen y0 = 0.25*invnormal(uniform())
gen y1 = y0 + late
sum y0 y1

* Resultado observado
gen y = D*y1 + (1 - D)*y0

* ATE: el promedio de los efectos (no se observa en la realidad)
sum late

* MCO no estima ni el ATE ni el LATE
reg y D

* VI recupera el LATE de los compliers
ivregress 2sls y (D = Z)

*----------------------------------------------------------------
* A.2 ¿Y SI HAY DEFIERS?
*
* Ahora 2.000 de los compliers son defiers (d10): van al
* tratamiento solo si Z = 0. Su efecto es +3.
*   never-takers  25 %  efecto -0,5
*   always-takers 25 %  efecto  0
*   compliers     40 %  efecto +1
*   defiers       10 %  efecto +3
* Wald = [0,4(1) - 0,1(3)] / (0,4 - 0,1) = 0,33
*----------------------------------------------------------------

clear
set seed 54687
set obs 20000
gen Z = uniform() > 0.5

gen d00 = (_n <= 5000)                       // never-takers
gen d11 = (_n >  5000 & _n <= 10000)         // always-takers
gen d01 = (_n > 10000 & _n <= 18000)         // compliers
gen d10 = (_n > 18000)                       // defiers

* Los defiers hacen lo contrario de Z
gen D = d11 + Z*d01 + (1 - Z)*d10
tab D d10

gen late = -0.5 if d00 == 1
replace late = 0  if d11 == 1
replace late = 1  if d01 == 1
replace late = 3  if d10 == 1

gen y0 = 0.25*invnormal(uniform())
gen y1 = y0 + late
gen y = D*y1 + (1 - D)*y0

* Todos los que cambian de tratamiento ganan 1 o más, pero VI da menos
ivregress 2sls y (D = Z)


*================================================================
* PARTE B — RD NÍTIDA: BECA POR PUNTAJE
*
* Ejemplo simulado. Quien saca 310 o más en Saber 11 recibe una
* beca para la universidad (D = 1). Resultado y = puntaje Saber Pro.
*   Z = puntaje Saber 11 (variable de asignación)
*   c = 310
*   D = 1{Z >= 310}           (regla nítida: nadie se salta la regla)
*   efecto verdadero de la beca en el umbral = 8 puntos
*================================================================

clear
set seed 1105
set obs 5000

generate habilidad  = rnormal()
generate motivacion = rnormal()
generate Z  = 300 + 40*habilidad + rnormal(0, 20)
replace  Z  = round(Z, 0.1)
generate mujer   = runiform() < 0.52
generate oficial = runiform() < invlogit(1 - 0.02*(Z - 300))

generate Zt = Z - 310                 // Z centrada en el umbral
generate D  = Zt >= 0

* Resultado: depende de Z de forma curva, de la habilidad y de la beca
generate y = 150 + 0.45*Zt - 0.0025*Zt^2 + 0.00002*Zt^3 ///
           + 4*habilidad + 4*motivacion + 3*mujer - 2*oficial + 8*D + rnormal(0, 6)

* La base solo trae estudiantes con Saber 11 entre 200 y 420
keep if inrange(Z, 200, 420)

summarize Z, detail
tabulate D

*--- B.1 La regla de asignación se ve en un gráfico ------------------
twoway (scatter D Z, msize(tiny) mcolor(navy%30) jitter(3)),         ///
       xline(310, lcolor(maroon) lpattern(dash))                     ///
       ytitle("Recibe la beca (D)") xtitle("Puntaje Saber 11 (Z)")   ///
       ylabel(0 1) title("RD nítida: D salta de 0 a 1 en 310")       ///
       name(g_regla, replace)
graph export rd_regla.png, width(1600) replace

*--- B.2 Comparar becados con no becados NO sirve -------------------
regress y D, vce(robust)
* Los becados tienen mejor Saber 11 y más habilidad. El 'efecto'
* mezcla la beca con todo lo que se mueve con Z.

*--- B.3 Primero el gráfico: rdplot ----------------------------------
rdplot y Z if abs(Zt) <= 40, c(310) p(1) h(40 40) kernel(triangular) nbins(20 20) ///
    graph_options(title("Saber Pro alrededor del umbral")            ///
                  ytitle("Puntaje Saber Pro (promedio por bin)")     ///
                  xtitle("Puntaje Saber 11") legend(off)             ///
                  name(g_rdplot, replace))
graph export rd_rdplot.png, width(1600) replace

rdplot y Z, c(310) graph_options(title("rdplot con polinomio global")  ///
                  ytitle("Puntaje Saber Pro") xtitle("Puntaje Saber 11") ///
                  legend(off) name(g_rdplot_global, replace))
graph export rd_rdplot_global.png, width(1600) replace

*--- B.4 Regresión con Z centrada e interacción ----------------------
* Modelo: y = a + tau*D + b0*Zt + b1*(D x Zt) + e
* Con Z centrada, el coeficiente de D es el salto en Z = 310.

* (1) Global lineal, misma pendiente
regress y D Zt, vce(robust)
estimates store g_lin1

* (2) Global lineal, pendientes distintas
regress y i.D##c.Zt, vce(robust)
estimates store g_lin2

* (3) Global cuadrático, pendientes y curvaturas distintas
regress y i.D##c.Zt##c.Zt, vce(robust)
estimates store g_cuad

* (4) Local lineal: solo 20 puntos a cada lado
regress y i.D##c.Zt if abs(Zt) <= 20, vce(robust)
estimates store l_lin20

* (5) Local lineal con kernel triangular y ancho de banda óptimo
rdrobust y Z, c(310) p(1) kernel(triangular) bwselect(mserd)
scalar tau_rd = e(tau_cl)
scalar h_opt  = e(h_l)
display as text "Ancho de banda MSE-óptimo = " as result %5.2f h_opt

* Ojo: los polinomios globales usan datos lejanos al umbral.
estimates table g_lin1 g_lin2 g_cuad l_lin20, keep(1.D D) b(%6.2f) se(%6.2f) stats(N)
display as text "rdrobust (local lineal, h óptimo): " as result %6.2f tau_rd
display as text "Efecto verdadero: 8"


*================================================================
* PARTE C — ANCHO DE BANDA Y PRUEBAS DE VALIDEZ
*================================================================

*--- C.1 Sensibilidad al ancho de banda ------------------------------
tempname resultados
tempfile anchos
postfile `resultados' h b li ls n using `anchos'
foreach h in 5 7.5 10 15 20 25 30 40 50 60 80 {
    quietly rdrobust y Z, c(310) p(1) kernel(triangular) h(`h')
    post `resultados' (`h') (e(tau_cl)) (e(tau_cl) - 1.96*e(se_tau_cl)) ///
                      (e(tau_cl) + 1.96*e(se_tau_cl)) (e(N_h_l) + e(N_h_r))
}
postclose `resultados'

preserve
use `anchos', clear
list, noobs clean
twoway (rcap li ls h, lcolor(navy)) (scatter b h, mcolor(navy)),          ///
       yline(8, lcolor(maroon) lpattern(dash))                             ///
       xtitle("Ancho de banda h (puntos de Saber 11)")                     ///
       ytitle("Salto estimado e IC 95 %") legend(off)                      ///
       title("Más h: menos varianza, más riesgo de sesgo")                 ///
       name(g_anchos, replace)
graph export rd_anchos.png, width(1600) replace
restore

*--- C.2 ¿Hay manipulación del puntaje? Densidad en el umbral -------
rddensity Z, c(310)
* H0: la densidad de Z es continua en 310. No rechazar es buena señal.

*--- C.3 Balance: las características no deben saltar ----------------
foreach x in mujer oficial {
    quietly rdrobust `x' Z, c(310)
    display as text "Salto en `x' = " as result %6.3f e(tau_cl) ///
            as text "   p robusto = " as result %5.3f e(pv_rb)
}

*--- C.4 Umbrales falsos: no debería haber saltos --------------------
* Usamos solo un lado del umbral verdadero para no mezclar el salto real.
foreach cf in 280 290 330 340 {
    if `cf' < 310 quietly rdrobust y Z if Z < 310,  c(`cf')
    else          quietly rdrobust y Z if Z >= 310, c(`cf')
    display as text "Umbral falso `cf': salto = " as result %6.2f e(tau_cl) ///
            as text "   p robusto = " as result %5.3f e(pv_rb)
}


*================================================================
* PARTE D — ¿Y SI HAY MANIPULACIÓN?
*
* Quienes quedaron entre 300 y 310 y están motivados piden
* recalificación (80 % de ellos) y logran subir justo por encima
* de 310. La motivación también sube el puntaje Saber Pro.
*================================================================

generate reclama = inrange(Z, 300, 309.9) & runiform() < 0.8 & motivacion > 0
generate Zm  = cond(reclama, 310 + runiform()*4, Z)
generate Dm  = Zm >= 310
generate ym  = 150 + 0.45*(Zm - 310) - 0.0025*(Zm - 310)^2 + 0.00002*(Zm - 310)^3 ///
             + 4*habilidad + 4*motivacion + 3*mujer - 2*oficial + 8*Dm + rnormal(0, 6)

count if reclama

histogram Zm if inrange(Zm, 260, 360), width(2) start(260)                 ///
          xline(310, lcolor(maroon)) fcolor(navy%50) lcolor(white) ytitle("Densidad") ///
          xtitle("Puntaje Saber 11 reportado") title("Amontonamiento justo arriba de 310") ///
          name(g_manip, replace)
graph export rd_manipulacion.png, width(1600) replace

rddensity Zm, c(310)
rdrobust ym Zm, c(310)
* La densidad salta y el efecto estimado ya no es 8.

log close
display "14_late_rd_nitida.do terminado."
