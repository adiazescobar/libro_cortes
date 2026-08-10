version 19.0
clear all
set more off
capture log close
log using "04_stata.log", replace text

capture mkdir "results"

capture program drop post_estimands
program define post_estimands
    syntax, POSTname(name) SCENario(string)
    local n = _N

    quietly count if D == 0
    post `postname' ("`scenario'") ("N_D0") (r(N)) (`n')
    quietly count if D == 1
    post `postname' ("`scenario'") ("N_D1") (r(N)) (`n')

    quietly summarize y if D == 0
    local y0 = r(mean)
    post `postname' ("`scenario'") ("MEDIA_Y_D0") (`y0') (`n')
    quietly summarize y if D == 1
    local y1 = r(mean)
    post `postname' ("`scenario'") ("MEDIA_Y_D1") (`y1') (`n')

    quietly summarize tau
    local ate = r(mean)
    post `postname' ("`scenario'") ("ATE") (`ate') (`n')
    quietly summarize tau if D == 1
    local att = r(mean)
    post `postname' ("`scenario'") ("ATT") (`att') (`n')
    quietly summarize tau if D == 0
    post `postname' ("`scenario'") ("ATU") (r(mean)) (`n')
    quietly summarize tau if X == 0
    post `postname' ("`scenario'") ("CATE_X0") (r(mean)) (`n')
    quietly summarize tau if X == 1
    post `postname' ("`scenario'") ("CATE_X1") (r(mean)) (`n')

    local naive = `y1' - `y0'
    post `postname' ("`scenario'") ("NAIVE") (`naive') (`n')
    post `postname' ("`scenario'") ("SESGO_ATT") (`naive' - `att') (`n')
    post `postname' ("`scenario'") ("DESV_NAIVE_ATE") (`naive' - `ate') (`n')

    quietly regress y D, vce(robust)
    local tcrit = invttail(e(df_r), 0.025)
    post `postname' ("`scenario'") ("COEF_REG_D") (_b[D]) (`n')
    post `postname' ("`scenario'") ("SE_ROBUST_REG_D") (_se[D]) (`n')
    post `postname' ("`scenario'") ("IC95_INF_REG_D") (_b[D] - `tcrit'*_se[D]) (`n')
    post `postname' ("`scenario'") ("IC95_SUP_REG_D") (_b[D] + `tcrit'*_se[D]) (`n')
    post `postname' ("`scenario'") ("COEF_REG_CONSTANTE") (_b[_cons]) (`n')
    post `postname' ("`scenario'") ("SE_ROBUST_REG_CONSTANTE") (_se[_cons]) (`n')
    post `postname' ("`scenario'") ("IC95_INF_REG_CONSTANTE") (_b[_cons] - `tcrit'*_se[_cons]) (`n')
    post `postname' ("`scenario'") ("IC95_SUP_REG_CONSTANTE") (_b[_cons] + `tcrit'*_se[_cons]) (`n')
end

* ================================================================
* 1. EJERCICIO MANUAL: los mismos ocho perfiles del capítulo teórico
* ================================================================
use "04_data.dta", clear
generate byte X = (_n > 4)
generate double tau = yd1 - yd0
generate double y = D*yd1 + (1-D)*yd0
label define tratamiento 0 "Control" 1 "Tratados"
label values D tratamiento
list X D yd0 yd1 y tau, clean noobs

ttest y, by(D)
regress y D, robust
summarize tau
scalar ATE_original = r(mean)
summarize tau if D == 1
scalar ATT_original = r(mean)
summarize tau if D == 0
scalar ATU_original = r(mean)
summarize tau if X == 0
scalar CATE_X0_original = r(mean)
summarize tau if X == 1
scalar CATE_X1_original = r(mean)
summarize y if D == 1
scalar media_y1_original = r(mean)
summarize y if D == 0
scalar NAIVE_original = media_y1_original-r(mean)
scalar SESGO_original = NAIVE_original-ATT_original

display "ATE = " ATE_original
display "ATT = " ATT_original
display "ATU = " ATU_original
display "CATE(0) = " CATE_X0_original
display "CATE(1) = " CATE_X1_original
display "NAIVE = " NAIVE_original
display "NAIVE - ATT = " SESGO_original

tempfile original population
save `original', replace

tempname pointpost
postfile `pointpost' str24 escenario str32 estimando double valor long N using "results/parameters_results.dta", replace
post_estimands, postname(`pointpost') scenario("datos_originales")

* ================================================================
* 2. MISMA SELECCIÓN CON N = 10.000: más N no elimina el sesgo
* ================================================================
expand 1250
assert _N == 10000
post_estimands, postname(`pointpost') scenario("seleccion_n10000")

quietly summarize y if D == 1
scalar media_y1_n10000 = r(mean)
quietly summarize y if D == 0
scalar NAIVE_n10000 = media_y1_n10000-r(mean)
quietly summarize tau if D == 1
scalar SESGO_n10000 = NAIVE_n10000-r(mean)
assert abs(NAIVE_n10000-NAIVE_original) < 1e-10
assert abs(SESGO_n10000-SESGO_original) < 1e-10

drop y D
save `population', replace

* ================================================================
* 3. UNA ASIGNACIÓN ALEATORIA: solo cambia D
* ================================================================
set seed 87634
generate byte D = (runiform() < .5)
generate double y = D*yd1 + (1-D)*yd0
post_estimands, postname(`pointpost') scenario("aleatorizacion_unica")

postclose `pointpost'
preserve
use "results/parameters_results.dta", clear
sort escenario estimando
export delimited using "results/parameters_results.csv", replace
restore

* ================================================================
* 4. MONTE CARLO: un D nuevo en cada repetición
* ================================================================
capture program drop one_random_assignment
program define one_random_assignment, rclass
    syntax, POPulation(string)
    use "`population'", clear
    generate byte D = (runiform() < .5)
    generate double y = D*yd1 + (1-D)*yd0
    quietly summarize y if D == 1
    local y1 = r(mean)
    quietly summarize y if D == 0
    return scalar estimador = `y1'-r(mean)
end

simulate estimador=r(estimador), reps(1000) seed(87634) nodots: one_random_assignment, population("`population'")
generate str16 escenario = "aleatorizacion"
generate long rep = _n
order escenario rep estimador
sort escenario rep
save "results/monte_carlo_draws.dta", replace

quietly summarize estimador, detail
scalar mc_media = r(mean)
scalar mc_sd = r(sd)
scalar mc_se_media = r(sd)/sqrt(r(N))
assert abs(mc_media-ATE_original) < 3*mc_se_media

tempname summarypost
postfile `summarypost' str16 escenario long N double media desv_est p5 mediana p95 using "results/monte_carlo_summary.dta", replace
post `summarypost' ("aleatorizacion") (r(N)) (r(mean)) (r(sd)) (r(p5)) (r(p50)) (r(p95))
postclose `summarypost'

preserve
use "results/monte_carlo_summary.dta", clear
export delimited using "results/monte_carlo_summary.csv", replace
restore

histogram estimador, fraction color(forest_green%70) xline(`=ATE_original', lcolor(navy) lwidth(medthick)) title("Estimador bajo asignación aleatoria") subtitle("1.000 asignaciones nuevas; N = 10.000") xtitle("Diferencia de medias") ytitle("Fracción") note("Línea azul: ATE = `=string(ATE_original, "%5.2f")'") name(g_aleatorizacion, replace)
graph export "sesgo_con_aleatorizacion.png", replace width(1800)

use `original', clear
display "Pipeline canónico completado"
log close
