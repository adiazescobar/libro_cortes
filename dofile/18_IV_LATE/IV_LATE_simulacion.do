*===============================================================================
* Variables instrumentales y LATE - clase empirica
* Datos completamente ficticios para fines pedagogicos
* Curso: Econometria Avanzada, Pontificia Universidad Javeriana
*
* Este archivo es la fuente canonica de las bases, tablas y figuras publicas.
* Los datos NO son los microdatos originales de PACES ni de ningun articulo.
*===============================================================================

version 19
clear all
set more off
set seed 54687

local base "dofile/18_IV_LATE"
local results "`base'/results"
local figures "`base'/figures"
local data "`base'/data"

capture mkdir "`results'"
capture mkdir "`figures'"
capture mkdir "`data'"

capture which ivreg2
if _rc {
    display as error "Falta ivreg2. Instale una vez con: ssc install ivreg2"
    exit 499
}

*===============================================================================
* PARTE A Y B - UNA MISMA SIMULACION TIPO PACES
*===============================================================================

set obs 20000
gen long id = _n

* Instrumento: resultado ficticio de una loteria de becas.
gen byte Z = runiform() < 0.50
label variable Z "Gano la loteria ficticia"

* Covariables predeterminadas.
gen byte female = runiform() < 0.52
gen byte low_income = runiform() < 0.50
gen double baseline_score = rnormal(50 - 4*low_income + 1.5*female, 10)
gen double u_type = runiform()
gen double u_family = rnormal()

* Estratos principales. Las probabilidades dependen de X para que el perfil
* de los compliers sea informativo, pero Z sigue siendo aleatorio.
gen double p_always = 0.15 + 0.10*(1-low_income)
gen double p_complier = 0.45 + 0.15*low_income
gen byte compliance_type = 2 if u_type < p_always
replace compliance_type = 3 if missing(compliance_type) & u_type < p_always + p_complier
replace compliance_type = 1 if missing(compliance_type)

label define compliance_lbl 1 "Never-taker" 2 "Always-taker" 3 "Complier"
label values compliance_type compliance_lbl
label variable compliance_type "Tipo verdadero, observable solo en la simulacion"

* Tratamientos potenciales. Monotonicidad se impone por construccion.
gen byte D0 = compliance_type == 2
gen byte D1 = inlist(compliance_type, 2, 3)
assert D1 >= D0
gen byte D = D0*(1-Z) + D1*Z
label variable D "Uso la beca ficticia"

* Efectos heterogeneos: LATE, ATE y ATT son deliberadamente distintos.
gen double tau_i = -0.20 if compliance_type == 1
replace tau_i = 0.20 if compliance_type == 2
replace tau_i = 1.20 + 0.20*low_income if compliance_type == 3

* Resultados potenciales y observado.
gen double Y0 = 55 + 0.15*baseline_score - 1.5*low_income + 0.5*female + u_family + rnormal(0,1.5)
gen double Y1 = Y0 + tau_i
gen double Y = D*Y1 + (1-D)*Y0
label variable Y "Puntaje final ficticio"
label variable Y0 "Y(D=0), visible solo en la simulacion"
label variable Y1 "Y(D=1), visible solo en la simulacion"

label data "PACES simulado - datos ficticios, no son los datos originales"

* Guardar la verdad para la profesora y una base publica solo con observables.
save "`data'/paces_simulada_con_verdad.dta", replace
preserve
keep id Z D Y female baseline_score low_income
label data "PACES simulado - base ficticia para estudiantes"
save "`data'/paces_simulada_estudiantes.dta", replace
export delimited using "`data'/paces_simulada_estudiantes.csv", replace
restore

*-------------------------------------------------------------------------------
* Verdad poblacional conocida gracias a la simulacion
*-------------------------------------------------------------------------------

quietly summarize tau_i, meanonly
local ate_true = r(mean)
quietly summarize tau_i if D == 1, meanonly
local att_true = r(mean)
quietly summarize tau_i if compliance_type == 3, meanonly
local late_true = r(mean)

quietly count if compliance_type == 1
local share_never = r(N)/_N
quietly count if compliance_type == 2
local share_always = r(N)/_N
quietly count if compliance_type == 3
local share_complier = r(N)/_N
local share_defier = 0

tempname truth
postfile `truth' str32 metric double value using "`results'/paces_truth.dta", replace
post `truth' ("ate_true") (`ate_true')
post `truth' ("att_true") (`att_true')
post `truth' ("late_true") (`late_true')
post `truth' ("share_complier") (`share_complier')
post `truth' ("share_always") (`share_always')
post `truth' ("share_never") (`share_never')
post `truth' ("share_defier") (`share_defier')
postclose `truth'

preserve
use "`results'/paces_truth.dta", clear
export delimited using "`results'/paces_truth.csv", replace
restore

*-------------------------------------------------------------------------------
* Lo que puede estimar el investigador con las variables observadas
*-------------------------------------------------------------------------------

quietly regress Y D, vce(robust)
local ols = _b[D]

quietly regress Y Z, vce(robust)
local itt = _b[Z]
local reduced_form = _b[Z]

quietly regress D Z, vce(robust)
local first_stage = _b[Z]
local share_complier_estimated = _b[Z]

local wald = `reduced_form'/`first_stage'

quietly ivregress 2sls Y (D = Z), vce(robust)
local iv_2sls = _b[D]

* La igualdad Wald=2SLS debe cumplirse con un instrumento binario y sin X.
assert abs(`wald' - `iv_2sls') < 1e-8

tempname estimates
postfile `estimates' str32 metric double value using "`results'/paces_estimators.dta", replace
post `estimates' ("ate_true") (`ate_true')
post `estimates' ("att_true") (`att_true')
post `estimates' ("late_true") (`late_true')
post `estimates' ("ols") (`ols')
post `estimates' ("itt") (`itt')
post `estimates' ("first_stage") (`first_stage')
post `estimates' ("reduced_form") (`reduced_form')
post `estimates' ("wald") (`wald')
post `estimates' ("iv_2sls") (`iv_2sls')
post `estimates' ("share_complier_estimated") (`share_complier_estimated')
postclose `estimates'

preserve
use "`results'/paces_estimators.dta", clear
export delimited using "`results'/paces_estimators.csv", replace
restore

* Salida completa de diagnosticos para uso en clase.
ivreg2 Y (D = Z), robust first

*-------------------------------------------------------------------------------
* Perfil de compliers: verdad y estimacion mediante kappa de Abadie
*-------------------------------------------------------------------------------

quietly summarize Z, meanonly
local pz = r(mean)
gen double kappa_manual = 1 - D*(1-Z)/(1-`pz') - (1-D)*Z/`pz'
quietly summarize kappa_manual, meanonly
local sum_kappa = r(sum)

tempname profile
postfile `profile' str24 group str24 variable double mean using "`results'/paces_complier_profile.dta", replace

foreach x in female baseline_score low_income {
    quietly summarize `x', meanonly
    local pop_`x' = r(mean)
    quietly summarize `x' if compliance_type == 3, meanonly
    local true_`x' = r(mean)
    gen double kappa_`x' = kappa_manual*`x'
    quietly summarize kappa_`x', meanonly
    local estimated_`x' = r(sum)/`sum_kappa'
    drop kappa_`x'
}

post `profile' ("Population") ("Female") (`pop_female')
post `profile' ("True compliers") ("Female") (`true_female')
post `profile' ("Estimated compliers") ("Female") (`estimated_female')
post `profile' ("Population") ("Baseline score") (`pop_baseline_score')
post `profile' ("True compliers") ("Baseline score") (`true_baseline_score')
post `profile' ("Estimated compliers") ("Baseline score") (`estimated_baseline_score')
post `profile' ("Population") ("Low income") (`pop_low_income')
post `profile' ("True compliers") ("Low income") (`true_low_income')
post `profile' ("Estimated compliers") ("Low income") (`estimated_low_income')
postclose `profile'

preserve
use "`results'/paces_complier_profile.dta", clear
export delimited using "`results'/paces_complier_profile.csv", replace
restore

* StataNow: estimador LATE y perfil oficial. El calculo manual anterior queda
* siempre disponible y es la fuente de la tabla pedagogica.
capture which lateffects
if _rc == 0 {
    capture noisily lateffects kappa (Y) (D) (Z female baseline_score low_income)
    if _rc == 0 {
        capture noisily estat compliers female baseline_score low_income, genkappa(kappa_statanow)
    }
}

*-------------------------------------------------------------------------------
* Figuras canonicas
*-------------------------------------------------------------------------------

graph bar (percent), over(compliance_type) ///
    ytitle("Porcentaje de la poblacion") ///
    title("Tipos verdaderos de cumplimiento") ///
    subtitle("Simulacion ficticia tipo PACES") ///
    note("Los tipos son observables porque conocemos D(0) y D(1).") ///
    bar(1, color(navy)) graphregion(color(white))
graph export "`figures'/compliance_types.png", width(1800) replace

preserve
use "`results'/paces_complier_profile.dta", clear
encode group, gen(group_id)
label define group_short 1 "Estimados" 2 "Poblacion" 3 "Verdaderos", replace
label values group_id group_short
gen double display_mean = mean
replace display_mean = 100*mean if inlist(variable, "Female", "Low income")
graph bar display_mean if variable == "Baseline score", over(group_id, label(angle(20))) ///
    ytitle("Puntos") title("Puntaje inicial") ///
    bar(1, color(navy)) graphregion(color(white)) name(profile_score, replace)
graph bar display_mean if variable == "Female", over(group_id, label(angle(20))) ///
    ytitle("Porcentaje") title("Mujeres") ///
    bar(1, color(eltblue)) graphregion(color(white)) name(profile_female, replace)
graph bar display_mean if variable == "Low income", over(group_id, label(angle(20))) ///
    ytitle("Porcentaje") title("Ingreso bajo") ///
    bar(1, color(orange)) graphregion(color(white)) name(profile_income, replace)
graph combine profile_score profile_female profile_income, rows(1) ///
    title("Perfil de los compliers") ///
    subtitle("Poblacion, verdad simulada y estimacion kappa") ///
    note("Los pesos kappa describen promedios; no identifican personas.") ///
    graphregion(color(white))
graph export "`figures'/complier_profile.png", width(2000) replace
restore

display as result "PACES simulado: archivos canonicos creados correctamente."

*===============================================================================
* PARTE C - INSTRUMENTOS FUERTES Y DEBILES CON EL MISMO TAMANO MUESTRAL
*===============================================================================

preserve

tempname weakpost
postfile `weakpost' str8 scenario int n double pi first_stage_F kp_F ols iv ///
    conventional_ci_low conventional_ci_high str20 robust_ci_low ///
    str20 robust_ci_high str20 robust_ci_type ///
    using "`results'/weak_iv_comparison.dta", replace

foreach scenario in weak strong {
    clear
    set obs 1000
    if "`scenario'" == "weak" {
        local pi = 0.05
        set seed 71001
    }
    else {
        local pi = 0.70
        set seed 71002
    }

    gen double z = rnormal()
    gen double w = rnormal()
    gen double eD = rnormal()
    gen double u = rnormal()
    gen double D = `pi'*z + w + eD
    gen double y = 0.50*D + w + u

    quietly regress D z
    local first_stage_F = e(F)

    quietly regress y D, vce(robust)
    local ols = _b[D]

    quietly ivreg2 y (D = z), robust first
    local kp_F = e(widstat)

    quietly ivregress 2sls y (D = z), vce(robust)
    local iv = _b[D]
    local iv_se = _se[D]
    local conventional_ci_low = `iv' - invnormal(0.975)*`iv_se'
    local conventional_ci_high = `iv' + invnormal(0.975)*`iv_se'

    local robust_ci_low "not available"
    local robust_ci_high "not available"
    capture noisily estat weakrobust, ci ar
    if _rc == 0 {
        capture matrix ar_ci = r(ar_ci)
        if _rc == 0 & rowsof(ar_ci) == 1 {
            if ar_ci[1,1] == .l local robust_ci_low "-inf"
            else local robust_ci_low = string(ar_ci[1,1], "%12.6f")
            if ar_ci[1,2] == .u local robust_ci_high "+inf"
            else local robust_ci_high = string(ar_ci[1,2], "%12.6f")
        }
        else if _rc == 0 {
            local robust_ci_low "disjoint/unbounded"
            local robust_ci_high "disjoint/unbounded"
        }
    }

    post `weakpost' ("`scenario'") (1000) (`pi') (`first_stage_F') ///
        (`kp_F') (`ols') (`iv') (`conventional_ci_low') ///
        (`conventional_ci_high') ("`robust_ci_low'") ///
        ("`robust_ci_high'") ("Anderson-Rubin")
}
postclose `weakpost'

use "`results'/weak_iv_comparison.dta", clear
export delimited using "`results'/weak_iv_comparison.csv", replace

* Monte Carlo pequeno: suficiente para visualizar, sin confundir N con pi.
capture program drop iv_montecarlo
program define iv_montecarlo, rclass
    syntax, PI(real) N(integer)
    drop _all
    set obs `n'
    gen double z = rnormal()
    gen double w = rnormal()
    gen double eD = rnormal()
    gen double u = rnormal()
    gen double D = `pi'*z + w + eD
    gen double y = 0.50*D + w + u
    quietly regress y D
    return scalar ols = _b[D]
    quietly ivregress 2sls y (D = z)
    return scalar iv = _b[D]
end

tempfile weak_mc strong_mc
simulate ols=r(ols) iv=r(iv), reps(500) seed(72001): iv_montecarlo, pi(0.05) n(1000)
gen str8 scenario = "Debil"
save `weak_mc', replace

simulate ols=r(ols) iv=r(iv), reps(500) seed(72002): iv_montecarlo, pi(0.70) n(1000)
gen str8 scenario = "Fuerte"
save `strong_mc', replace

use `weak_mc', clear
append using `strong_mc'

twoway (kdensity iv if scenario == "Debil" & inrange(iv,-3,3), ///
        lcolor(orange) lwidth(medthick)), ///
       xline(0.50, lcolor(black) lpattern(dash)) ///
       title("Instrumento debil") xtitle("Estimacion IV") ytitle("Densidad") ///
       xscale(range(-3 3)) xlabel(-3(1)3) legend(off) ///
       graphregion(color(white)) name(weak_density, replace)
twoway (kdensity iv if scenario == "Fuerte" & inrange(iv,-3,3), ///
        lcolor(navy) lwidth(medthick)), ///
       xline(0.50, lcolor(black) lpattern(dash)) ///
       title("Instrumento fuerte") xtitle("Estimacion IV") ytitle("Densidad") ///
       xscale(range(-3 3)) xlabel(-3(1)3) legend(off) ///
       graphregion(color(white)) name(strong_density, replace)
graph combine weak_density strong_density, rows(1) ///
       title("Distribucion del estimador IV") ///
       subtitle("Mismo N=1,000; cambia solo la relevancia") ///
       note("500 replicas por escenario; eje recortado a [-3,3]. Efecto verdadero = 0.50.") ///
       graphregion(color(white))
graph export "`figures'/weak_iv_distributions.png", width(1800) replace

restore

display as result "Comparacion de instrumentos fuertes y debiles creada."

*===============================================================================
* PARTE D - CASO CRITICO DE DIVORCIO CON INSTRUMENTO CONTINUO
*===============================================================================
*
* Inspirado solamente en la estructura conceptual de Frimmel, Halla y
* Winter-Ebmer (2024). Los datos y los parametros son completamente ficticios.
* Se introduce deliberadamente un canal directo hipotetico para mostrar que una
* primera etapa fuerte no demuestra la restriccion de exclusion.
*===============================================================================

clear
set seed 81024
set obs 12000

gen long child_id = _n
gen double father_age = rnormal(40,6)
gen double father_educ = min(max(round(rnormal(12,2.5)),6),20)
gen double firm_size = round(exp(rnormal(4.5,0.7)))
gen double industry_female_share = min(max(rnormal(0.45,0.12),0.08),0.85)
gen double workplace_gender_balance = min(max(industry_female_share + rnormal(0,0.14),0.02),0.98)
gen double family_conflict = rnormal()

* Tratamiento endogeno: divorcio antes de que el hijo cumpla 18 anos.
gen double divorce_latent = -1.15 + 2.20*workplace_gender_balance ///
    + 0.45*family_conflict - 0.035*(father_educ-12) ///
    + 0.010*(father_age-40) + rnormal()
gen byte parental_divorce = divorce_latent > 0

* Efecto causal verdadero y canal directo hipotetico de exclusion.
local divorce_effect = -3.00
local direct_channel = 1.20
gen double child_outcome = 70 + `divorce_effect'*parental_divorce ///
    - 2.00*family_conflict + 0.30*father_educ ///
    + `direct_channel'*workplace_gender_balance + rnormal(0,3)

label variable workplace_gender_balance "Balance de genero en el trabajo del padre"
label variable parental_divorce "Divorcio parental"
label variable child_outcome "Resultado posterior del hijo"
label data "Divorcio e IV - datos ficticios, no son datos del articulo"

save "`data'/divorcio_iv_simulado_con_verdad.dta", replace
preserve
keep child_id workplace_gender_balance parental_divorce child_outcome ///
    father_age father_educ firm_size industry_female_share
label data "Divorcio e IV - base ficticia para estudiantes"
save "`data'/divorcio_iv_simulado_estudiantes.dta", replace
export delimited using "`data'/divorcio_iv_simulado_estudiantes.csv", replace
restore

local controls "father_age father_educ firm_size industry_female_share"

quietly regress parental_divorce workplace_gender_balance `controls', vce(robust)
local first_stage_slope = _b[workplace_gender_balance]
local first_stage_p = 2*normal(-abs(_b[workplace_gender_balance]/_se[workplace_gender_balance]))

quietly regress child_outcome parental_divorce `controls', vce(robust)
local divorce_ols = _b[parental_divorce]

quietly ivreg2 child_outcome `controls' ///
    (parental_divorce = workplace_gender_balance), robust first
local divorce_iv = _b[parental_divorce]
local divorce_kp = e(widstat)

* Contraste oficial y CI robusto a debilidad. No soluciona exclusion invalida.
quietly ivregress 2sls child_outcome `controls' ///
    (parental_divorce = workplace_gender_balance), vce(robust)
assert abs(_b[parental_divorce] - `divorce_iv') < 1e-8
capture noisily estat weakrobust, ci ar

tempname divorcepost
postfile `divorcepost' str40 metric double value ///
    using "`results'/divorce_iv_estimators.dta", replace
post `divorcepost' ("true_causal_effect") (`divorce_effect')
post `divorcepost' ("hypothetical_direct_channel") (`direct_channel')
post `divorcepost' ("ols") (`divorce_ols')
post `divorcepost' ("iv_2sls") (`divorce_iv')
post `divorcepost' ("first_stage_slope") (`first_stage_slope')
post `divorcepost' ("first_stage_p") (`first_stage_p')
post `divorcepost' ("kp_F") (`divorce_kp')
postclose `divorcepost'

preserve
use "`results'/divorce_iv_estimators.dta", clear
export delimited using "`results'/divorce_iv_estimators.csv", replace
restore

* Primera etapa visual en veinte grupos del instrumento continuo.
xtile instrument_bin = workplace_gender_balance, nq(20)
collapse (mean) parental_divorce workplace_gender_balance, by(instrument_bin)
twoway (connected parental_divorce workplace_gender_balance, ///
        mcolor(navy) lcolor(navy) msymbol(circle)), ///
       title("Primera etapa: balance de genero y divorcio") ///
       subtitle("Datos ficticios inspirados en la estructura del articulo") ///
       xtitle("Balance de genero en el lugar de trabajo del padre") ///
       ytitle("Proporcion con divorcio parental") ///
       note("Promedios en 20 grupos. No son los datos originales.") ///
       graphregion(color(white))
graph export "`figures'/divorce_first_stage.png", width(1800) replace

display as result "Caso ficticio de divorcio e IV creado."
