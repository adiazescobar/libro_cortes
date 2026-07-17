********************************************************************************
* Datos de panel y TWFE — clase empírica
* Resultados canónicos para el libro
********************************************************************************

version 19
clear all
set more off

local root "dofile/11_TWFE"
capture mkdir "`root'/results"
capture mkdir "`root'/figures"

********************************************************************************
* INSTALACIÓN (ejecutar una vez si hace falta)
********************************************************************************
* ssc install reghdfe, replace
* ssc install bacondecomp, replace
* ssc install twowayfeweights, replace
* ssc install csdid, replace
* ssc install did_imputation, replace
* ssc install eventstudyinteract, replace
* ssc install did_multiplegt_dyn, replace
* ssc install did2s, replace

********************************************************************************
* PANEL: POOLED, FE, FD Y RE
********************************************************************************
set seed 1117
local N = 300
local T = 6
set obs `=`N'*`T''
egen id = seq(), block(`T')
bysort id: gen t = _n
xtset id t

bysort id: gen double alpha_i = rnormal() if _n == 1
bysort id: replace alpha_i = alpha_i[1]
gen double X = .7*alpha_i + .3*t + rnormal()
gen double Y = alpha_i + .4*t + 3*X + rnormal()

tempname panel
tempfile panel_dta
postfile `panel' str20 dgp str20 method str34 parameter ///
    double estimate se truth using `panel_dta', replace

quietly regress Y X i.t, vce(cluster id)
post `panel' ("panel") ("Pooled OLS") ("beta within/between") (_b[X]) (_se[X]) (3)

quietly xtreg Y X i.t, fe vce(cluster id)
post `panel' ("panel") ("FE") ("beta within") (_b[X]) (_se[X]) (3)

quietly regress D.Y D.X ibn.t, noconstant vce(cluster id)
post `panel' ("panel") ("FD") ("beta first difference") (_b[D.X]) (_se[D.X]) (3)

quietly xtreg Y X i.t, re vce(cluster id)
post `panel' ("panel") ("RE") ("beta quasi-within") (_b[X]) (_se[X]) (3)

postclose `panel'
preserve
use `panel_dta', clear
export delimited using "`root'/results/panel_estimators.csv", replace
restore

********************************************************************************
* EQUIVALENCIA 2x2
********************************************************************************
clear
set seed 2468
set obs 800
gen id = ceil(_n/2)
gen t = mod(_n-1,2)
gen byte treated = id > 200
gen byte D = treated*t
bysort id: gen double alpha_i = rnormal() if _n == 1
bysort id: replace alpha_i = alpha_i[1]
gen double Y = alpha_i + .5*t + 3*D + rnormal()
xtset id t

tempname eq
tempfile eq_dta
postfile `eq' str20 dgp str24 method str24 parameter ///
    double estimate se truth using `eq_dta', replace

quietly summarize Y if treated==1 & t==0
scalar yt0 = r(mean)
quietly summarize Y if treated==1 & t==1
scalar yt1 = r(mean)
quietly summarize Y if treated==0 & t==0
scalar yc0 = r(mean)
quietly summarize Y if treated==0 & t==1
scalar yc1 = r(mean)
scalar did_manual = (yt1-yt0)-(yc1-yc0)
post `eq' ("2x2") ("DiD manual") ("ATT") (did_manual) (0) (3)

quietly regress Y treated t D, vce(cluster id)
post `eq' ("2x2") ("Regression DiD") ("ATT") (_b[D]) (_se[D]) (3)

quietly regress D.Y D.D, vce(cluster id)
post `eq' ("2x2") ("First differences") ("ATT") (_b[D.D]) (_se[D.D]) (3)

quietly reghdfe Y D, absorb(id t) vce(cluster id)
post `eq' ("2x2") ("TWFE") ("ATT") (_b[D]) (_se[D]) (3)

postclose `eq'
use `eq_dta', clear
export delimited using "`root'/results/twfe_2x2.csv", replace

********************************************************************************
* ADOPCIÓN ESCALONADA CON EFECTOS DINÁMICOS HETEROGÉNEOS
********************************************************************************
clear
set seed 717
local N = 900
local T = 12
set obs `=`N'*`T''
egen id = seq(), block(`T')
bysort id: gen t = _n
gen cohort = cond(id<=300,5,cond(id<=600,8,0))
gen byte D = cohort>0 & t>=cohort
gen event_time = t-cohort if cohort>0
gen double tau = 0
replace tau = 1 + .45*event_time if cohort==5 & D
replace tau = 2 + .25*event_time if cohort==8 & D
bysort id: gen double alpha_i = rnormal() if _n==1
bysort id: replace alpha_i = alpha_i[1]
gen double Y0 = alpha_i + .25*t + rnormal()
gen double Y = Y0 + tau
xtset id t

quietly summarize tau if D
scalar true_att = r(mean)
quietly reghdfe Y D, absorb(id t) vce(cluster id)
scalar twfe_b = _b[D]
scalar twfe_se = _se[D]

tempname staggered
tempfile staggered_dta
postfile `staggered' str20 dgp str24 method str34 parameter ///
    str32 comparison_sample double estimate se using `staggered_dta', replace
post `staggered' ("staggered") ("True ATT") ("average treated-cell effect") ///
    ("all treated cells") (true_att) (0)
post `staggered' ("staggered") ("TWFE") ("implicit weighted average") ///
    ("all cohorts and periods") (twfe_b) (twfe_se)

* Goodman-Bacon: muestra las comparaciones 2x2.
capture noisily bacondecomp Y D, ddetail

* de Chaisemartin-D'Haultfoeuille: pesos sobre efectos grupo-periodo.
capture noisily twowayfeweights Y id t D, type(feTR) summary_measures

postclose `staggered'
preserve
use `staggered_dta', clear
export delimited using "`root'/results/twfe_staggered.csv", replace
restore

********************************************************************************
* EVENT STUDY TWFE TRADICIONAL
********************************************************************************
forvalues h = 4(-1)2 {
    gen byte lead`h' = event_time == -`h'
}
forvalues h = 0/4 {
    gen byte lag`h' = event_time == `h'
}

quietly reghdfe Y lead4 lead3 lead2 lag0 lag1 lag2 lag3 lag4, ///
    absorb(id t) vce(cluster id)

tempname event
tempfile event_dta
postfile `event' str20 dgp str24 method str34 parameter ///
    int horizon double estimate se using `event_dta', replace
forvalues h = 4(-1)2 {
    post `event' ("staggered") ("TWFE event study") ("relative-time coefficient") ///
        (-`h') (_b[lead`h']) (_se[lead`h'])
}
forvalues h = 0/4 {
    post `event' ("staggered") ("TWFE event study") ("relative-time coefficient") ///
        (`h') (_b[lag`h']) (_se[lag`h'])
}
postclose `event'
use `event_dta', clear
export delimited using "`root'/results/twfe_eventstudy.csv", replace

gen ci_low=estimate-1.96*se
gen ci_high=estimate+1.96*se
twoway (rcap ci_low ci_high horizon, lcolor(navy)) ///
       (scatter estimate horizon, mcolor(navy) msymbol(O)), ///
    xline(-1, lpattern(dash) lcolor(gs8)) yline(0, lcolor(gs8)) ///
    xlabel(-4(1)4) legend(off) ///
    title("Event study TWFE: pretrends aparentes") ///
    xtitle("Tiempo relativo") ytitle("Coeficiente")
graph export "`root'/figures/twfe_eventstudy.png", replace width(1800)

********************************************************************************
* GRÁFICAS PEDAGÓGICAS
********************************************************************************

* Adopción simultánea y violación deliberada de tendencias
clear
set seed 818
set obs 2400
egen id=seq(), block(12)
bysort id: gen t=_n
gen byte treated=id>100
gen byte D=treated & t>=7
bysort id: gen double alpha_i=rnormal() if _n==1
bysort id: replace alpha_i=alpha_i[1]
gen double Y_parallel=alpha_i+.25*t+2*D+rnormal(0,.7)
gen double Y_violation=Y_parallel+.18*treated*t
collapse (mean) Y_parallel Y_violation, by(t treated)

twoway (connected Y_parallel t if treated==0, lcolor(navy) mcolor(navy)) ///
       (connected Y_parallel t if treated==1, lcolor(maroon) mcolor(maroon)), ///
    xline(6.5, lpattern(dash) lcolor(gs8)) ///
    legend(order(1 "Control" 2 "Tratado") rows(1)) ///
    title("Adopción simultánea: tendencias paralelas") ///
    xtitle("Periodo") ytitle("Media de Y")
graph export "`root'/figures/panel_simultaneous.png", replace width(1800)

twoway (connected Y_violation t if treated==0, lcolor(navy) mcolor(navy)) ///
       (connected Y_violation t if treated==1, lcolor(maroon) mcolor(maroon)), ///
    xline(6.5, lpattern(dash) lcolor(gs8)) ///
    legend(order(1 "Control" 2 "Tratado") rows(1)) ///
    title("Violación deliberada de tendencias paralelas") ///
    note("El grupo tratado ya crece más rápido antes de t=7") ///
    xtitle("Periodo") ytitle("Media de Y")
graph export "`root'/figures/panel_parallel_violation.png", replace width(1800)

* Mismo timing, efectos heterogéneos
clear
set seed 919
set obs 3600
egen id=seq(), block(12)
bysort id: gen t=_n
gen cohort=cond(id<=100,0,cond(id<=200,1,2))
gen byte D=cohort>0 & t>=7
gen tau=cond(cohort==1,2,cond(cohort==2,4,0))
gen Y=.25*t+tau*D+rnormal(0,.6)
collapse (mean) Y, by(t cohort)
twoway (connected Y t if cohort==0, lcolor(navy) mcolor(navy)) ///
       (connected Y t if cohort==1, lcolor(maroon) mcolor(maroon)) ///
       (connected Y t if cohort==2, lcolor(forest_green) mcolor(forest_green)), ///
    xline(6.5, lpattern(dash) lcolor(gs8)) ///
    legend(order(1 "Nunca" 2 "Tratada: efecto 2" 3 "Tratada: efecto 4") rows(1)) ///
    title("Mismo timing, efectos heterogéneos") ///
    xtitle("Periodo") ytitle("Media de Y")
graph export "`root'/figures/panel_same_timing_heterogeneity.png", replace width(1800)

* Adopción escalonada dinámica y pesos causales
clear
set seed 717
local N = 900
local T = 12
set obs `=`N'*`T''
egen id=seq(), block(`T')
bysort id: gen t=_n
gen cohort=cond(id<=300,5,cond(id<=600,8,0))
gen byte D=cohort>0 & t>=cohort
gen event_time=t-cohort if cohort>0
gen double tau=0
replace tau=1+.45*event_time if cohort==5 & D
replace tau=2+.25*event_time if cohort==8 & D
bysort id: gen double alpha_i=rnormal() if _n==1
bysort id: replace alpha_i=alpha_i[1]
gen double Y=alpha_i+.25*t+tau+rnormal()

preserve
collapse (mean) Y, by(t cohort)
twoway (connected Y t if cohort==0, lcolor(navy) mcolor(navy)) ///
       (connected Y t if cohort==5, lcolor(maroon) mcolor(maroon)) ///
       (connected Y t if cohort==8, lcolor(forest_green) mcolor(forest_green)), ///
    xline(4.5 7.5, lpattern(dash) lcolor(gs8)) ///
    legend(order(1 "Nunca tratada" 2 "Cohorte 5" 3 "Cohorte 8") rows(1)) ///
    title("Adopción escalonada y efectos dinámicos") ///
    xtitle("Periodo") ytitle("Media de Y")
graph export "`root'/figures/panel_staggered_dynamic.png", replace width(1800)
restore

bysort id: egen D_bar_i=mean(D)
bysort t: egen D_bar_t=mean(D)
quietly summarize D
scalar D_bar=r(mean)
gen double D_tilde=D-D_bar_i-D_bar_t+D_bar
egen denom=total(D_tilde^2)
gen double peso_causal=D_tilde/denom if D==1
collapse (sum) peso_causal, by(cohort t D)
keep if D==1

* Para visualizar un peso negativo usamos el ejemplo mínimo 2x4 de la teoría.
clear
input str10 cohort t peso_causal
"Temprana" 2  .5
"Temprana" 3  .5
"Temprana" 4 -.5
"Tardía"   4  .5
end
encode cohort, gen(cohort_id)
twoway (connected peso_causal t if cohort_id==2, lcolor(maroon) mcolor(maroon)) ///
       (scatter peso_causal t if cohort_id==1, mcolor(forest_green) msymbol(D)), ///
    yline(0, lcolor(gs8)) ///
    xlabel(2(1)4) ylabel(-.5(.25).5) ///
    legend(order(1 "Cohorte temprana" 2 "Cohorte tardía") rows(1)) ///
    title("Peso negativo en una celda ya tratada") ///
    note("Ejemplo mínimo 2 unidades × 4 periodos; los pesos tratados suman 1") ///
    xtitle("Periodo") ytitle("Peso agregado de la celda")
graph export "`root'/figures/twfe_causal_weights.png", replace width(1800)

********************************************************************************
* MAPA MÉTODO–PARÁMETRO
********************************************************************************
clear
input str28 method str52 parameter str42 comparison_sample str22 horizon
"csdid" "ATT(g,t) and explicit aggregations" "never or not-yet treated" "calendar/group/event"
"eventstudyinteract" "interaction-weighted relative-time average" "chosen control cohort" "relative time"
"did_imputation" "event effects by imputation" "untreated observations" "requested horizons"
"did_multiplegt_dyn" "dynamic current-vs-status quo effects" "switchers and valid controls" "dynamic/cumulative"
"did2s" "parameter defined by second stage" "untreated first-stage sample" "second-stage variables"
end
export delimited using "`root'/results/method_parameter_map.csv", replace

********************************************************************************
* SINTAXIS DE REFERENCIA: ejecutar según el parámetro y el diseño
********************************************************************************
* csdid Y, ivar(id) time(t) gvar(cohort) notyet
* did_imputation Y id t cohort, horizons(0/4) pretrend(4)
* eventstudyinteract Y lead4 lead3 lead2 lag0-lag4, ///
*     absorb(id t) cohort(cohort) control_cohort(never_treat) vce(cluster id)
* did_multiplegt_dyn Y id t D, effects(4) placebo(4) cluster(id)
* did2s Y, first_stage(i.id i.t) second_stage(lag0-lag4) ///
*     treatment(D) cluster(id)
*
* Si se usa event_plot, cada par debe existir antes de llamarlo:
* matrix method_b = e(b)
* matrix method_v = e(V)
* event_plot method_b#method_v

di as result "Resultados canónicos exportados en `root'/results/"
