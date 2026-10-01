********************************************************************************
* Adopción escalonada: divorcio unilateral y suicidio femenino
* Stevenson y Wolfers (2006, QJE), reanalizado por Goodman-Bacon (2021, JoE)
* Econometría Avanzada — Ana María Díaz Escobar — Javeriana 2026-2
*
* Do-file auxiliar para la clase. Trae solo el código largo (etiquetas y
* gráficas). El resto lo escribimos juntos en clase.
********************************************************************************

clear all
set more off
set linesize 120

********************************************************************************
* BLOQUE 0. PAQUETES (correr una sola vez)
********************************************************************************
* ssc install ftools, replace
* ssc install reghdfe, replace
* ssc install drdid, replace
* ssc install csdid, replace

********************************************************************************
* BLOQUE 1. LOS DATOS Y EL CALENDARIO DE LA REFORMA
********************************************************************************
use "http://pped.org/bacon_example.dta", clear




* Nombre de cada estado (abreviatura) a partir de su código FIPS
label define estado 1 "AL" 4 "AZ" 5 "AR" 6 "CA" 8 "CO" 9 "CT" 10 "DE" 11 "DC" ///
    12 "FL" 13 "GA" 16 "ID" 17 "IL" 18 "IN" 19 "IA" 20 "KS" 21 "KY" 22 "LA"    ///
    23 "ME" 24 "MD" 25 "MA" 26 "MI" 27 "MN" 28 "MS" 29 "MO" 30 "MT" 31 "NE"    ///
    32 "NV" 33 "NH" 34 "NJ" 35 "NM" 36 "NY" 37 "NC" 38 "ND" 39 "OH" 40 "OK"    ///
    41 "OR" 42 "PA" 44 "RI" 45 "SC" 46 "SD" 47 "TN" 48 "TX" 49 "UT" 50 "VT"    ///
    51 "VA" 53 "WA" 54 "WV" 55 "WI" 56 "WY"
label values stfips estado




* Calendario de adopción, una fila por estado (necesita la variable tipo)
preserve
egen orden = group(tipo _nfd stfips), missing
levelsof orden, local(filas)
foreach o of local filas {
    quietly levelsof stfips if orden == `o', local(f)
    label define orden `o' "`: label estado `f''", add
}
label values orden orden
twoway (scatter orden year if post == 0, msymbol(S) msize(small) mcolor(gs13)) ///
       (scatter orden year if post == 1, msymbol(S) msize(small) mcolor(navy)), ///
       ylabel(1(1)49, valuelabel angle(0) labsize(vsmall) nogrid) yscale(reverse) ///
       xlabel(1965(5)1995) xtitle("Año") ytitle("")                             ///
       legend(order(1 "Sin divorcio unilateral" 2 "Con divorcio unilateral") rows(1) position(6)) ///
       title("Cuándo adoptó cada estado") ysize(7) xsize(6)
graph export "divorcio_calendario.png", replace width(1400)
restore

* Suicidio promedio por tipo de estado (necesita la variable tipo)
preserve
collapse (mean) asmrs, by(tipo year)
twoway (line asmrs year if tipo == 1, lpattern(solid))  ///
       (line asmrs year if tipo == 2, lpattern(dash))   ///
       (line asmrs year if tipo == 3, lpattern(shortdash)), ///
       legend(order(1 "Siempre tratados" 2 "Adoptan 1969-85" 3 "Nunca adoptan") rows(1) position(6)) ///
       xline(1969 1985, lcolor(gs12)) ///
       ytitle("Suicidios de mujeres por millón") xtitle("Año") ///
       title("Suicidio femenino por grupo de estados")
graph export "divorcio_tendencias.png", replace width(1600)
restore

********************************************************************************
* BLOQUE 2. UNA SOLA COHORTE: 1973 CONTRA NUNCA
********************************************************************************




********************************************************************************
* BLOQUE 3. TODAS LAS COHORTES: TWFE
********************************************************************************




********************************************************************************
* BLOQUE 3B. EL MISMO TWFE CON reghdfe
********************************************************************************




********************************************************************************
* BLOQUE 4. DESCOMPOSICIÓN DE BACON
********************************************************************************




********************************************************************************
* BLOQUE 5. SIN LOS ESTADOS QUE YA ESTABAN TRATADOS
********************************************************************************




********************************************************************************
* BLOQUE 6. CALLAWAY Y SANT'ANNA
********************************************************************************




********************************************************************************
* BLOQUE 6B. ¿QUÉ PASA CON LOS SIEMPRE TRATADOS?
********************************************************************************




********************************************************************************
* BLOQUE 7. EFECTOS DINÁMICOS
********************************************************************************




* Gráfico del event study (después de estat event)
csdid_plot, title("Divorcio unilateral y suicidio femenino (Callaway-Sant'Anna)") ///
            xtitle("Años desde la adopción") ytitle("Efecto en suicidios por millón")
graph export "divorcio_eventstudy_cs.png", replace width(1600)

********************************************************************************
* BLOQUE 8. PRUEBAS DE TENDENCIAS PARALELAS CON TODAS LAS COHORTES
********************************************************************************



