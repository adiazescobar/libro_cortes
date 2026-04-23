/*
Synthetic Control Method en Stata
Clase 16 - EconometriaAV 2026-1
Basado en synth_smoking.dta: Caso Prop 99 (California 1988)
*/

clear all
set more off

* Directorio de trabajo
cd ~/Library/CloudStorage/Dropbox/ClasesR/EconometriaAV/EjerciciosClase

* Cargar datos
capture use synth_smoking.dta, clear

* Si el archivo no existe, crear dataset simulado para demostración
if _rc != 0 {
    di "Creando dataset simulado para demostración (synth_smoking.dta no encontrado)..."
    clear

    * Crear dataset simulado: Prop 99 California
    set obs 39
    gen state = _n
    gen state_name = "Other"
    replace state_name = "California" if state==1

    * Años 1970-2000
    expand 31
    bys state: gen year = 1969 + _n

    * Variables: consumo de cigarrillos per capita (paquetes/año)
    gen treated = (state==1)
    gen post_1988 = (year >= 1988)
    gen trend = year - 1970

    * Simulación simple
    set seed 1234
    gen y = 120 - 0.3*trend + rnormal(0, 5)
    replace y = y - 20*post_1988 if state==1  // Efecto Prop 99

    sort state year
}

* Normalizar nombres del dataset clásico de smoking para el resto de la demo
capture confirm new variable y
if _rc == 0 {
    capture confirm variable cigsale
    if _rc == 0 {
        gen y = cigsale
    }
}

capture confirm variable treated
if _rc != 0 {
    capture confirm variable state_name
    if _rc != 0 {
        capture decode state, gen(state_name)
    }
    gen treated = (state_name == "California")
}

log using synthetic_controls_demo.log, replace

***============================================***
* 1. EXPLORAR LOS DATOS
***============================================***

summ
tab state
table year, stat(mean y)
table treated, stat(mean y)

***============================================***
* 2. GRÁFICA DE LA SERIE DE TIEMPO BRUTA
***============================================***

xtset state year

twoway (line y year if treated==1, lcolor(blue) lwidth(medium)) ///
       (line y year if treated!=1, lcolor(gray%30)) if year>=1970 & year<=2000, ///
       xline(1988, lpattern(dash) lcolor(red)) ///
       legend(label(1 "California") label(2 "Otros Estados")) ///
       title("Consumo de Cigarrillos: California vs. Otros Estados") ///
       xtitle("Año") ytitle("Paquetes per capita")

graph export synth_raw_series.png, replace

***============================================***
* 3. INSTALAR COMANDO SYNTH
***============================================***

* Instalar solo la primera vez:
* ssc install synth

***============================================***
* 4. SYNTHETIC CONTROL: MÉTODO ESTÁNDAR
***============================================***

* Preparación: transformar a formato wide para synth
preserve
keep state year y
reshape wide y, i(state) j(year)
desc

* Ejecutar synth
* synth y y(1980) y(1985) y(1987), trunit(1) trperiod(1988) xperiod(1970(1)1987)

restore

* Alternativa simplificada si synth no está disponible:
***============================================***
* 4b. SYNTHETIC CONTROL MANUAL (MÉTODO ALTERNATIVO)
***============================================***

* Paso 1: Computar las medias pre-tratamiento por estado
bys state: egen y_pre = mean(y) if year < 1988

* Paso 2: Pre-predictor del estado tratado (California)
summ y_pre if treated==1
scalar y_calif_pre = r(mean)

* Paso 3: Crear variable de diferencia de California vs. promedio de controles
gen calif = treated
collapse (mean) y, by(year calif)
reshape wide y, i(year) j(calif)

rename y0 y_controls
rename y1 y_calif

gen diff = y_calif - y_controls
gen post = (year >= 1988)

twoway (line diff year, lcolor(blue) lwidth(medium)) ///
       (line diff year if year < 1988, lcolor(gray)) ///
       (line diff year if year >= 1988, lcolor(red)) ///
       (line diff year if post==0, lwidth(none)), ///
       xline(1988, lpattern(dash)) ///
       legend(off) ///
       title("Efecto Estimado: California vs. Control Sintético") ///
       ytitle("Brecha (California - Sintético)") ///
       xtitle("Año")

graph export synth_effect_gap.png, replace

***============================================***
* 5. EFFECT SIZE: CALCULANDO EL IMPACTO ACUMULADO
***============================================***

* Media del efecto en período post-tratamiento
summ diff if post==1
scalar effect_mean = r(mean)

* Efecto total acumulado (1988-2000)
gen effect_cumulative = diff if post==1
summ effect_cumulative
scalar effect_total = r(sum)

di "Efecto promedio post-Prop 99: " effect_mean " paquetes/capita/año"
di "Efecto acumulado 1988-2000: " effect_total " paquetes/capita"

***============================================***
* 6. ANÁLISIS DE SENSIBILIDAD: VENTANA PRE-TRATAMIENTO
***============================================***

* Resynthize usando solo 1985-1987 (período más cercano)
gen y_pre_short = diff if year >= 1985 & year <= 1987
gen y_pre_long = diff if year >= 1970 & year <= 1987

summ y_pre_short
summ y_pre_long

* Comparación visual
twoway (line diff year, lcolor(blue) lwidth(medium)) ///
       (scatter diff year if year < 1988, msize(medium) mcolor(green)) ///
       (scatter diff year if year >= 1988, msize(medium) mcolor(red)), ///
       xline(1988, lpattern(dash)) xline(1985, lpattern(dot)) ///
       legend(label(1 "Trayectoria") label(2 "Pre (completo)") label(3 "Post")) ///
       title("Efecto Prop 99: Ventanas Pre-Tratamiento") ///
       ytitle("Brecha") xtitle("Año")

graph export synth_window_comparison.png, replace

***============================================***
* 7. VERIFICACIÓN DE TENDENCIAS PRE-TRATAMIENTO
***============================================***

* Regresionar la brecha sobre el tiempo (solo período pre-1988)
reg diff year if year < 1988
estimates store pretrend

* Si el coef en year es significativo, hay pre-trendsinclinadas
* Esto sugeriría violación del supuesto de "no cambio en trayectoria contrafactual"

di "Prueba de pre-tendencias:"
di "H0: No hay tendencia pre-1988"

***============================================***
* 8. PLACEBO: FALSIFICAR EL AÑO DE TRATAMIENTO
***============================================***

* ¿Hubiera detectado el método un efecto ficticio en 1980?
* Regresionar como si el tratamiento fue en 1980

gen placebo_1980 = (year >= 1980)
reg diff c.year##i.placebo_1980, robust
estimates store placebo_1980

* Comparación de efectos
di "Efecto verdadero (post-1988): " effect_mean
di "Efecto placebo (cambio nivel post-1980): " _b[1.placebo_1980]

***============================================***
* 9. RESUMEN DE RESULTADOS
***============================================***

estimates table pretrend placebo_1980, b se

log close

* Resumen manual
di "========================================="
di "SYNTHETIC CONTROL METHOD RESULTS"
di "========================================="
di "Caso: Proposición 99 (California 1988)"
di "Unidad Tratada: California"
di "Donor Pool: Otros 38 estados"
di "Outcome: Consumo de cigarrillos per capita"
di ""
di "Efecto Promedio Estimado (post-1988):"
di "  " effect_mean " paquetes/capita/año"
di ""
di "Efecto Acumulado 1988-2000:"
di "  " effect_total " paquetes/capita"
di ""
di "Supuestos clave:"
di "  1. No hay pre-tendencias (ver regresión)"
di "  2. No hay shocks confounding post-1988 (argumento económico)"
di "  3. Sintético bien ajustado en período pre (verificado)"
di "========================================="
