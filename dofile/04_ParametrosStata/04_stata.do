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

    quietly summarize tau
    local ate = r(mean)
    post `postname' ("`scenario'") ("ATE") (`ate') (`n')

    quietly summarize tau if D == 1
    local att = r(mean)
    post `postname' ("`scenario'") ("ATT") (`att') (`n')

    quietly summarize tau if D == 0
    local atu = r(mean)
    post `postname' ("`scenario'") ("ATU") (`atu') (`n')

    quietly summarize tau if X == 0
    post `postname' ("`scenario'") ("CATE_X0") (r(mean)) (`n')

    quietly summarize tau if X == 1
    post `postname' ("`scenario'") ("CATE_X1") (r(mean)) (`n')

    quietly summarize y if D == 1
    local y1 = r(mean)
    quietly summarize y if D == 0
    local naive = `y1' - r(mean)
    post `postname' ("`scenario'") ("NAIVE") (`naive') (`n')
    post `postname' ("`scenario'") ("SESGO_ATT") (`naive' - `att') (`n')
    post `postname' ("`scenario'") ("DESV_NAIVE_ATE") (`naive' - `ate') (`n')

    quietly regress y D, vce(robust)
    post `postname' ("`scenario'") ("COEF_REG_D") (_b[D]) (`n')
end

use "04_data.dta", clear
generate byte X = (_n > 4)
label define grupo_pre 0 "Grupo A" 1 "Grupo B"
label values X grupo_pre
generate double y = D*yd1 + (1-D)*yd0
generate double tau = yd1 - yd0

tempfile original population
save `original', replace

tempname pointpost
postfile `pointpost' str24 escenario str20 estimando double valor long N using "results/parameters_results.dta", replace
post_estimands, postname(`pointpost') scenario("datos_originales")

* Se replican perfiles idénticos. El N nominal aumenta, pero no la información
* independiente; por construcción, los estimandos y el sesgo no cambian.
expand 10000
post_estimands, postname(`pointpost') scenario("datos_duplicados")
drop y tau
save `population', replace

drop D
set seed 87634
generate byte D = (runiform() < 0.5)
generate double y = D*yd1 + (1-D)*yd0
generate double tau = yd1 - yd0
post_estimands, postname(`pointpost') scenario("aleatorizacion_unica")

postclose `pointpost'
use "results/parameters_results.dta", clear
sort escenario estimando
export delimited using "results/parameters_results.csv", replace

capture program drop one_rep
program define one_rep, rclass
    syntax, POPulation(string) SCENario(string)
    use "`population'", clear
    drop D
    if "`scenario'" == "seleccion" {
        quietly summarize yd0
        generate double p = invlogit((yd0-r(mean))/2)
        generate byte D = (runiform() < p)
    }
    else {
        generate byte D = (runiform() < 0.5)
    }
    generate double y = D*yd1 + (1-D)*yd0
    generate double tau = yd1-yd0
    quietly summarize tau if D == 1
    local att = r(mean)
    quietly summarize y if D == 1
    local y1 = r(mean)
    quietly summarize y if D == 0
    return scalar sesgo = `y1' - r(mean) - `att'
end

simulate sesgo=r(sesgo), reps(1000) seed(12345) nodots: one_rep, population("`population'") scenario("seleccion")
generate str16 escenario = "seleccion"
generate long rep = _n
tempfile seleccion
save `seleccion', replace

simulate sesgo=r(sesgo), reps(1000) seed(87634) nodots: one_rep, population("`population'") scenario("aleatorizacion")
generate str16 escenario = "aleatorizacion"
generate long rep = _n
append using `seleccion'
order escenario rep sesgo
sort escenario rep
save "results/monte_carlo_draws.dta", replace

tempname summarypost
postfile `summarypost' str16 escenario long N double media desv_est p5 mediana p95 using "results/monte_carlo_summary.dta", replace
foreach s in seleccion aleatorizacion {
    quietly summarize sesgo if escenario == "`s'", detail
    post `summarypost' ("`s'") (r(N)) (r(mean)) (r(sd)) (r(p5)) (r(p50)) (r(p95))
}
postclose `summarypost'

preserve
use "results/monte_carlo_summary.dta", clear
sort escenario
export delimited using "results/monte_carlo_summary.csv", replace
restore

quietly summarize sesgo
local xmin = floor(r(min)*10)/10
local xmax = ceil(r(max)*10)/10

histogram sesgo if escenario == "seleccion", width(.01) start(`xmin') fraction color(navy%70) xline(0, lcolor(maroon) lwidth(medthick)) xscale(range(`xmin' `xmax')) xlabel(0(1)4) title("Sesgo con selección") subtitle("1.000 repeticiones; N = 80.000") xtitle("NAIVE - ATT") ytitle("Fracción") name(g_seleccion, replace)
graph export "sesgo_con_seleccion.png", replace width(1800)

histogram sesgo if escenario == "aleatorizacion", width(.01) start(`xmin') fraction color(forest_green%70) xline(0, lcolor(maroon) lwidth(medthick)) xscale(range(`xmin' `xmax')) xlabel(0(1)4) title("Sesgo con aleatorización") subtitle("1.000 repeticiones; N = 80.000") xtitle("NAIVE - ATT") ytitle("Fracción") name(g_aleatorizacion, replace)
graph export "sesgo_con_aleatorizacion.png", replace width(1800)

twoway (kdensity sesgo if escenario == "seleccion", lcolor(navy) lwidth(medthick)) (kdensity sesgo if escenario == "aleatorizacion", lcolor(forest_green) lwidth(medthick)), xline(0, lcolor(maroon) lwidth(medthick)) xscale(range(`xmin' `xmax')) xlabel(0(1)4) title("Comparación de escenarios") subtitle("Distribución del sesgo en 1.000 repeticiones") xtitle("NAIVE - ATT") ytitle("Densidad") legend(order(1 "Selección" 2 "Aleatorización")) name(g_comparacion, replace)
graph export "comparacion_escenarios.png", replace width(1800)

use `original', clear
display "Pipeline canónico completado"
log close
