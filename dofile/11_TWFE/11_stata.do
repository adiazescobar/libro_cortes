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
