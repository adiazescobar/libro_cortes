/*
IPW en Stata: resultados canónicos para la clase empírica
Datos: base6.dta
Semilla: 1298
*/

version 19
clear all
set more off
set seed 1298

capture confirm file "base6.dta"
if _rc {
    capture confirm file "dofile/16_PSM_IPW_Sinteticos/base6.dta"
    if !_rc cd "dofile/16_PSM_IPW_Sinteticos"
}
confirm file "base6.dta"
capture mkdir "results"
capture log close
log using "ipw_demo.log", text replace

use "base6.dta", clear
global Xmust personas orden_n ocupado_jefe educa_jefe ingresos_hogar_jefe hombre
drop if missing(D, y2, personas, orden_n, ocupado_jefe, educa_jefe, ingresos_hogar_jefe, hombre)

quietly count
scalar N = r(N)
quietly count if D == 1
scalar NT = r(N)
quietly summarize D, meanonly
scalar pD = r(mean)

quietly summarize y2 if D == 1, meanonly
scalar raw1 = r(mean)
quietly summarize y2 if D == 0, meanonly
scalar raw0 = r(mean)
scalar rawdiff = raw1 - raw0

logit D $Xmust
predict double ps, pr
assert ps > 0 & ps < 1

gen double w_ate = D/ps + (1-D)/(1-ps)
gen double w_att = D + (1-D)*ps/(1-ps)
gen double w_ate_stab = D*pD/ps + (1-D)*(1-pD)/(1-ps)

gen double ht1_ate_i = D*y2/ps
gen double ht0_ate_i = (1-D)*y2/(1-ps)
quietly summarize ht1_ate_i, meanonly
scalar ht1_ate = r(mean)
quietly summarize ht0_ate_i, meanonly
scalar ht0_ate = r(mean)
scalar ht_ate = scalar(ht1_ate) - scalar(ht0_ate)

quietly summarize y2 [aw=1/ps] if D == 1, meanonly
scalar hajek1_ate = r(mean)
quietly summarize y2 [aw=1/(1-ps)] if D == 0, meanonly
scalar hajek0_ate = r(mean)
scalar hajek_ate = hajek1_ate - hajek0_ate

gen double ht1_att_i = D*y2
gen double ht0_att_i = (1-D)*ps*y2/(1-ps)
quietly summarize ht1_att_i, meanonly
scalar ht1_att = r(sum)/NT
quietly summarize ht0_att_i, meanonly
scalar ht0_att = r(sum)/NT
scalar ht_att = scalar(ht1_att) - scalar(ht0_att)

quietly summarize y2 if D == 1, meanonly
scalar hajek1_att = r(mean)
quietly summarize y2 [aw=ps/(1-ps)] if D == 0, meanonly
scalar hajek0_att = r(mean)
scalar hajek_att = hajek1_att - hajek0_att

tempname estimates
postfile `estimates' str28 estimator str4 estimand double estimate se using "results/ipw_estimates.dta", replace
post `estimates' ("Diferencia cruda") ("ATE") (scalar(rawdiff)) (.)
post `estimates' ("HT manual") ("ATE") (scalar(ht_ate)) (.)
post `estimates' ("Hajek manual") ("ATE") (scalar(hajek_ate)) (.)
post `estimates' ("HT manual") ("ATT") (scalar(ht_att)) (.)
post `estimates' ("Hajek manual") ("ATT") (scalar(hajek_att)) (.)

foreach target in ate atet {
    local label = cond("`target'" == "ate", "ATE", "ATT")
    quietly teffects ipw (y2) (D $Xmust, logit), `target'
    matrix B = e(b)
    matrix V = e(V)
    post `estimates' ("teffects ipw") ("`label'") (B[1,1]) (sqrt(V[1,1]))

    quietly teffects aipw (y2 $Xmust) (D $Xmust, logit), `target'
    matrix B = e(b)
    matrix V = e(V)
    post `estimates' ("teffects aipw") ("`label'") (B[1,1]) (sqrt(V[1,1]))

    quietly teffects ipwra (y2 $Xmust) (D $Xmust, logit), `target'
    matrix B = e(b)
    matrix V = e(V)
    post `estimates' ("teffects ipwra") ("`label'") (B[1,1]) (sqrt(V[1,1]))
}
postclose `estimates'
preserve
use "results/ipw_estimates.dta", clear
export delimited using "results/ipw_estimates.csv", replace
restore

tempname diagnostics
postfile `diagnostics' str16 weight str10 statistic double value using "results/ipw_weight_diagnostics.dta", replace
foreach w in w_ate w_att w_ate_stab {
    quietly summarize `w', detail
    post `diagnostics' ("`w'") ("p1") (r(p1))
    post `diagnostics' ("`w'") ("p50") (r(p50))
    post `diagnostics' ("`w'") ("p99") (r(p99))
    post `diagnostics' ("`w'") ("max") (r(max))
    quietly summarize `w', meanonly
    scalar sumw = r(sum)
    gen double `w'_sq = `w'^2
    quietly summarize `w'_sq, meanonly
    scalar ess = sumw^2/r(sum)
    post `diagnostics' ("`w'") ("sum") (sumw)
    post `diagnostics' ("`w'") ("ESS") (ess)
    drop `w'_sq
}
postclose `diagnostics'
preserve
use "results/ipw_weight_diagnostics.dta", clear
export delimited using "results/ipw_weight_diagnostics.csv", replace
restore

tempname balance
postfile `balance' str28 covariate double smd_raw smd_weighted using "results/ipw_balance.dta", replace
foreach x of global Xmust {
    quietly summarize `x' if D == 1
    scalar m1 = r(mean)
    scalar v1 = r(Var)
    quietly summarize `x' if D == 0
    scalar m0 = r(mean)
    scalar v0 = r(Var)
    scalar denom = sqrt((v1+v0)/2)
    scalar smdraw = cond(denom > 0, (m1-m0)/denom, 0)
    quietly summarize `x' [aw=w_ate] if D == 1
    scalar wm1 = r(mean)
    quietly summarize `x' [aw=w_ate] if D == 0
    scalar wm0 = r(mean)
    scalar smdw = cond(denom > 0, (wm1-wm0)/denom, 0)
    post `balance' ("`x'") (smdraw) (smdw)
}
postclose `balance'
preserve
use "results/ipw_balance.dta", clear
export delimited using "results/ipw_balance.csv", replace
restore

twoway (kdensity ps if D == 1, lcolor(navy) lwidth(medthick)) (kdensity ps if D == 0, lcolor(maroon) lwidth(medthick)), legend(order(1 "Tratados" 2 "Controles")) title("Soporte del propensity score") xtitle("P(D=1|X)") ytitle("Densidad")
graph export "ipw_support.png", width(1800) replace

histogram w_ate, fraction color(navy%55) title("Distribucion de pesos ATE") xtitle("Peso IPW") ytitle("Fraccion")
graph export "ipw_weights_dist.png", width(1800) replace

preserve
clear
set obs 4000
gen double x = rnormal()
gen double ps_true = invlogit(-0.2 + 3*x)
gen byte D = runiform() < ps_true
gen double tau = 2
gen double y0 = 1 + x + rnormal()
gen double y = y0 + tau*D
logit D x
predict double ps_hat, pr
gen double w = D/ps_hat + (1-D)/(1-ps_hat)
gen double h1 = D*y/ps_hat
gen double h0 = (1-D)*y/(1-ps_hat)
gen double ht_score = h1-h0
quietly summarize h1, meanonly
scalar sim_ht = r(mean)
quietly summarize h0, meanonly
scalar sim_ht = scalar(sim_ht)-r(mean)
quietly summarize ht_score
scalar sim_ht_se = r(sd)/sqrt(r(N))
quietly regress y D [pw=w], vce(robust)
scalar sim_hajek = _b[D]
scalar sim_hajek_se = _se[D]
quietly summarize w, detail
scalar sim_wmax = r(max)
scalar sim_sumw = r(sum)
gen double w_sq = w^2
quietly summarize w_sq, meanonly
scalar sim_ess = scalar(sim_sumw)^2/r(sum)
quietly count if ps_hat >= .05 & ps_hat <= .95
scalar n_overlap = r(N)
quietly regress y D [pw=w] if inrange(ps_hat,.05,.95), vce(robust)
scalar sim_trim = _b[D]
scalar sim_trim_se = _se[D]
quietly summarize w if inrange(ps_hat,.05,.95), detail
scalar sim_trim_wmax = r(max)
scalar sim_trim_sumw = r(sum)
quietly summarize w_sq if inrange(ps_hat,.05,.95), meanonly
scalar sim_trim_ess = scalar(sim_trim_sumw)^2/r(sum)

twoway (scatter w ps_hat if w < 100, msize(tiny) mcolor(navy%25)) (function y=20, range(0 1) lcolor(maroon) lpattern(dash)), legend(off) title("Positividad debil: pesos y propensity score") xtitle("Propensity score estimado") ytitle("Peso ATE (vista hasta 100)")
graph export "ipw_positivity_weak.png", width(1800) replace

clear
set obs 3
gen str30 estimator = ""
gen double estimate = .
gen double se = .
gen double true_effect = 2
gen double max_weight = .
gen double ess = .
gen double n_used = .
replace estimator = "HT, muestra completa" in 1
replace estimate = sim_ht in 1
replace se = sim_ht_se in 1
replace max_weight = sim_wmax in 1
replace ess = sim_ess in 1
replace n_used = 4000 in 1
replace estimator = "Hajek, muestra completa" in 2
replace estimate = sim_hajek in 2
replace se = sim_hajek_se in 2
replace max_weight = sim_wmax in 2
replace ess = sim_ess in 2
replace n_used = 4000 in 2
replace estimator = "Hajek, soporte 0.05-0.95" in 3
replace estimate = sim_trim in 3
replace se = sim_trim_se in 3
replace max_weight = sim_trim_wmax in 3
replace ess = sim_trim_ess in 3
replace n_used = n_overlap in 3
export delimited using "results/ipw_positivity_simulation.csv", replace
restore

teffects ipw (y2) (D $Xmust, logit), ate
tebalance summarize
tebalance density personas

log close
