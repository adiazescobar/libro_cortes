/*================================================================
RDD en Stata: Regresión Discontinua Nítida y Borrosa - CLASSROOM
Clase — EconometriaAV 2026-1
MOSTRANDO COMANDOS Y RESULTADOS PASO A PASO

USO: Corre esto durante la clase. Integra:
  (1) Simulación pedagógica clásica (DGP con drawnorm: q, v, epsilon)
      — Sharp RDD: ATE conocido = 4
      — Fuzzy RDD: ATE conocido = 5
  (2) Estimación con OLS, ventanas locales, rdrobust e ivreg2
  (3) Comparación con DGP curvo + tests de validez (Parte D)

Capítulo del libro complementario:
  libro_cortes/dofile/19_RDD/RDD_simulacion.do
================================================================*/

clear all
set more off
set linesize 100
capture log close
cd ~/Library/CloudStorage/Dropbox/ClasesR/EconometriaAV/EjerciciosClase
log using rdd_classroom.log, replace text

* Paquetes necesarios (instalar una vez):
*   ssc install rdrobust
*   ssc install rddensity
*   ssc install lpdensity
*   ssc install ivreg2
*   ssc install ranktest


*================================================================
* PARTE 1 — SHARP RDD (RDN)
* DGP: y = 1 + 2*z + 4*D + 5*q + v
* Variable de asignación: z = 5 + 8*q + epsilon
* Cutoff: c = 5     →     ATE verdadero = 4
*================================================================

di _newline(2) "============================================================"
di ">>> PARTE 1 — SHARP RDD"
di ">>> El tratamiento se asigna DETERMINÍSTICAMENTE en z >= c"
di "============================================================"

*--- 1.1 Crear datos artificiales ---
matrix C  = (1, 0, 0 \ 0, 1, 0 \ 0, 0, 1)
matrix sd = (0.1, 0.2, 0.3)
matrix m  = (0, 0, 0)
set seed 124569
drawnorm q v epsilon, n(1000) means(m) corr(C) sds(sd) cstorage(full) clear

* Variable de asignación (running variable)
generate z         = 5 + 8*q + epsilon
generate c         = 5
generate z_centrado = z - c
summarize z, detail

* Variable de tratamiento (regla nítida)
generate D = z >= c
tabulate D

* Variable dependiente (con ATE = 4)
generate y = 1 + 2*z + 4*D + 5*q + v

di _newline ">>> RECORDAR: el ATE verdadero por construcción es 4."

*--- 1.2 Gráficas: visualización ANTES de estimar ---
twoway (scatter y z_centrado, msize(vsmall) mcolor(gs10))      ///
       (lfit y z_centrado if z_centrado <  0, lcolor(navy))    ///
       (lfit y z_centrado if z_centrado >= 0, lcolor(maroon))  ///
       , ytitle("y") xtitle("z centrado (z - c)")              ///
         xline(0, lcolor(black))                               ///
         title("Sharp RDD — salto en y en el cutoff")          ///
         legend(off) name(sharp_scatter, replace)

twoway (scatter D z, msize(vsmall) mcolor(gs10))               ///
       (lfit D z if z <  c, lcolor(navy))                      ///
       (lfit D z if z >= c, lcolor(maroon))                    ///
       , ytitle("D") xtitle("z")                               ///
         xline(5, lcolor(black))                               ///
         title("Sharp RDD — D salta de 0 a 1 en c = 5")        ///
         legend(off) name(sharp_D, replace)

rdplot y z, c(5) graph_options(ytitle("y promedio") xtitle("z") ///
       title("rdplot — bins por defecto") name(rdplot_sharp1, replace))
rdplot y z, c(5) binselect(es) graph_options(ytitle("y promedio") xtitle("z") ///
       title("rdplot — bins evenly-spaced") name(rdplot_sharp2, replace))
graph export rdplot_sharp1.pdf, replace

*--- 1.3 Estimación con OLS ---
di _newline ">>> 1.3 OLS con TODOS los controles (incl. inobservable q): consistente"
regress y z D q

di _newline ">>> 1.3.b OLS sin controles: SESGADO (omite q correlacionado con D)"
regress y D

*--- 1.4 Comparación de medias en torno al cutoff (intercepto a cada lado) ---
di _newline ">>> 1.4 Estimación tipo 'salto': diferencia de interceptos"
regress y z_centrado if z_centrado >= 0
scalar a_der = _b[_cons]

regress y z_centrado if z_centrado < 0
scalar a_izq = _b[_cons]

di "Salto estimado (a_der - a_izq) = " a_der - a_izq

*--- 1.5 Tres especificaciones paramétricas equivalentes ---
di _newline ">>> 1.5.a Lineal — misma pendiente a ambos lados"
regress y D z, robust

di _newline ">>> 1.5.b Lineal — cambio de pendiente (interacción D × z_centrado)"
regress y c.z_centrado##D, robust

di _newline ">>> 1.5.c Cuadrático — cambio de pendiente y curvatura"
regress y c.z_centrado##c.z_centrado##D, robust

*--- 1.6 rdrobust: estimación local lineal moderna ---
di _newline ">>> 1.6.a rdrobust con kernel uniforme y h = 10 (manual)"
rdrobust y z_centrado, kernel(uniform) h(10)

di _newline ">>> 1.6.b rdrobust con kernel uniforme, h = 10, opción ALL (3 SE)"
rdrobust y z_centrado, kernel(uniform) h(10) all

di _newline ">>> 1.6.c rdrobust con bandwidth MSE-óptimo (default, CCT 2014)"
rdrobust y z_centrado, kernel(uniform) all

di _newline ">>> 1.6.d rdrobust con kernel triangular y bw MSE-óptimo (recomendado)"
rdrobust y z_centrado, kernel(triangular) all

*--- 1.7 Sensibilidad al bandwidth ---
di _newline ">>> 1.7 Barrido manual de bandwidth (h)"
foreach h in 0.5 1 2 3 5 10 {
    qui rdrobust y z_centrado, h(`h') p(1) kernel(triangular)
    di "  h = " %5.2f `h' "   coef = " %6.3f e(tau_cl) ///
       "   se = " %5.3f e(se_tau_cl) "   N efectivo = " e(N_h_l)+e(N_h_r)
}


*================================================================
* PARTE 2 — FUZZY RDD (RDB)
* DGP: y = 1 + 2*z + 5*D + 3*q + v
* z = 5 + epsilon  ;   W = 1{z >= 5}  (instrumento)
* D = 1{w >= w0}   donde w = 1 + 8*W - 5*q   →    ATE = 5
*================================================================

di _newline(2) "============================================================"
di ">>> PARTE 2 — FUZZY RDD"
di ">>> El cruce del umbral CAMBIA la PROBABILIDAD de tratamiento"
di ">>> W es el instrumento; D es la verdadera variable de tratamiento"
di "============================================================"

*--- 2.1 Crear datos ---
matrix C  = (1, 0, 0 \ 0, 1, 0 \ 0, 0, 1)
matrix sd = (1, 0.2, 0.3)
matrix m  = (0, 0, 0)
set seed 1243497
drawnorm q v epsilon, n(10000) means(m) corr(C) sds(sd) cstorage(full) clear

generate z          = 5 + epsilon
generate c          = 5
generate z_centrado = z - c

* Asignación (instrumento — el "W" de la clase del martes)
generate W = z >= c
tabulate W

* Tratamiento efectivo: probabilidad salta en c pero el cumplimiento NO es perfecto
generate w  = 1 + 8*W - 5*q
generate w0 = 5
generate D  = w >= w0

di _newline ">>> Tabla de cumplimiento (compliance): D × W"
tabulate D W, row

* Variable dependiente (ATE = 5)
generate y = 1 + 2*z + 5*D + 3*q + v

*--- 2.2 Gráficas ---
rdplot y z_centrado, graph_options(ytitle("y promedio") ///
       xtitle("z centrado") title("Fuzzy RDD — salto en y") ///
       name(rdplot_fuzzy1, replace))
rdplot y z_centrado, binselect(es) graph_options(ytitle("y promedio") ///
       xtitle("z centrado") title("Fuzzy RDD — bins evenly-spaced") ///
       name(rdplot_fuzzy2, replace))

rdplot D z_centrado, graph_options(ytitle("Pr(D = 1)") ///
       xtitle("z centrado") title("Fuzzy RDD — PRIMERA ETAPA: salto en D") ///
       name(rdplot_fuzzy_first, replace))

*--- 2.3 OLS (para mostrar el sesgo) ---
di _newline ">>> 2.3.a OLS con todos los controles incl. q: consistente"
regress y z D q

di _newline ">>> 2.3.b OLS sin controles: SESGADO"
regress y D

di _newline ">>> 2.3.c OLS solo con z centrado: aún sesgado"
regress y z_centrado D, robust

*--- 2.4 Variables instrumentales (2SLS) — el corazón del Fuzzy RDD ---
di _newline ">>> 2.4.a IV lineal SIN cambio de pendiente (W instrumenta D)"
ivreg2 y z_centrado (D = W)

di _newline ">>> 2.4.b IV lineal CON cambio de pendiente (interacción)"
gen z_centradoD = z_centrado*D
gen z_centradoW = z_centrado*W
ivreg2 y z_centrado (D z_centradoD = W z_centradoW), first

*--- 2.5 rdrobust con fuzzy() ---
di _newline ">>> 2.5.a rdrobust fuzzy con bw MSE-óptimo"
rdrobust y z, c(5) fuzzy(D) all

di _newline ">>> 2.5.b rdrobust fuzzy con kernel uniforme y h = 7"
rdrobust y z, c(5) fuzzy(D) kernel(uniform) h(7)

*--- 2.6 'A pie': cocientes de interceptos (Wald estimator) ---
di _newline ">>> 2.6 Wald a la izquierda y derecha del cutoff:"
di ">>>     coef = [E(y|W=1) - E(y|W=0)] / [E(D|W=1) - E(D|W=0)]"

regress y z_centrado if W == 1
local num_der = _b[_cons]

regress y z_centrado if W == 0
local num_izq = _b[_cons]

regress D z_centrado if W == 1
local den_der = _b[_cons]

regress D z_centrado if W == 0
local den_izq = _b[_cons]

di _newline "  Salto en y    = " `num_der' - `num_izq'
di         "  Salto en D    = " `den_der' - `den_izq'
di         "  Cociente Wald = " (`num_der' - `num_izq')/(`den_der' - `den_izq')


*================================================================
* PARTE 3 — TESTS DE VALIDEZ (recordatorio mínimo)
* Se trabajan más en el capítulo del libro (PARTE D de RDD_simulacion.do)
*================================================================

di _newline(2) "============================================================"
di ">>> PARTE 3 — TESTS DE VALIDEZ (esquemático)"
di "============================================================"

di _newline ">>> 3.1 Test de manipulación (McCrary / Cattaneo): rddensity"
rddensity z_centrado, c(0)

di _newline ">>> 3.2 Placebos en cutoffs FALSOS — ninguno debe ser significativo"
foreach cf in -0.5 -0.25 0.25 0.5 {
    capture noisily qui rdrobust y z_centrado, c(`cf') fuzzy(D)
    if _rc == 0 {
        local b  = e(tau_cl)
        local pv = e(pv_rb)
        di "  cutoff falso " %5.2f `cf' "   coef = " %6.3f `b' "   p robusto = " %5.3f `pv'
    }
    else di "  cutoff falso " %5.2f `cf' "   (sin obs suficientes)"
}

log close
display "rdd_classroom.do — terminado."
