version 19.0
clear all
set more off
set seed 1298

capture confirm file "synth_smoking.dta"
if _rc {
    capture confirm file "dofile/17_SyntheticControls/synth_smoking.dta"
    if !_rc cd "dofile/17_SyntheticControls"
}
confirm file "synth_smoking.dta"

capture which synth
if _rc {
    di as error "Falta synth. Instálelo una vez con: ssc install synth"
    exit 499
}

capture mkdir "results"

use "synth_smoking.dta", clear
isid state year
assert inrange(year, 1970, 2000)
capture confirm string variable state
if !_rc {
    encode state, gen(state_id)
    clonevar state_name = state
}
else {
    clonevar state_id = state
    decode state, gen(state_name)
}
assert state_id == 3 if state_name == "California"
xtset state_id year

tempfile panel state_map weights_data manual_path native_path paths_data
save `panel'

preserve
    keep state_id state_name
    duplicates drop
    isid state_id
    rename state_name state
    save `state_map'
restore

xtline cigsale, overlay legend(off) ///
    xline(1989, lpattern(shortdash) lcolor(gs7)) ///
    title("Ventas de cigarrillos por estado") ///
    subtitle("Serie bruta, 1970-2000") ///
    xtitle("Año") ytitle("Paquetes per cápita")
graph export "synth_raw_series.png", replace width(1800)

synth cigsale beer(1984(1)1988) lnincome retprice age15to24 ///
    cigsale(1988) cigsale(1980) cigsale(1975), ///
    trunit(3) trperiod(1989) xperiod(1980(1)1988) nested ///
    keep(results/california_synth_native.dta) replace

tempname weights_mat balance_mat
matrix `weights_mat' = e(W_weights)
matrix `balance_mat' = e(X_balance)

preserve
    clear
    svmat double `weights_mat', names(col)
    rename _Co_Number state_id
    rename _W_Weight weight
    assert state_id == floor(state_id)
    merge m:1 state_id using `state_map'
    assert _merge != 1
    keep if _merge == 3
    drop _merge
    assert weight >= -1e-10
    egen double sum_weight = total(weight)
    assert abs(sum_weight - 1) < 1e-6
    drop sum_weight
    order state_id state weight
    sort state_id
    save `weights_data'
    export delimited state_id state weight using "results/synth_weights.csv", replace
restore

preserve
    clear
    svmat double `balance_mat', names(col)
    rename Treated treated
    rename Synthetic synthetic
    gen str80 predictor = ""
    local predictor_names : rownames `balance_mat'
    local predictor_count : word count `predictor_names'
    assert _N == `predictor_count'
    forvalues row = 1/`predictor_count' {
        local predictor : word `row' of `predictor_names'
        replace predictor = "`predictor'" in `row'
    }
    order predictor treated synthetic
    export delimited predictor treated synthetic using "results/synth_predictor_balance.csv", replace
restore

use `panel', clear
drop if state_id == 3
merge m:1 state_id using `weights_data', keepusing(weight)
assert _merge == 3
drop _merge
gen double weighted_cigsale = weight * cigsale
collapse (sum) manual_synthetic=weighted_cigsale, by(year)
isid year
save `manual_path'

use "results/california_synth_native.dta", clear
keep if !missing(_time)
rename _time year
rename _Y_synthetic synthetic
keep year synthetic
isid year
save `native_path'

use `panel', clear
keep if state_id == 3
rename cigsale treated
keep year treated
isid year
merge 1:1 year using `manual_path'
assert _merge == 3
drop _merge
merge 1:1 year using `native_path'
assert _merge == 3
drop _merge

gen double reconstruction_error = abs(synthetic - manual_synthetic)
summ reconstruction_error, meanonly
di as result "Maximum reconstruction error: " %21.15g r(max)
assert r(max) < 1e-8

gen double gap = treated - synthetic
gen byte post = year >= 1989
order year treated synthetic manual_synthetic gap post
sort year
export delimited year treated synthetic manual_synthetic gap post using "results/synth_paths.csv", replace
save `paths_data'

gen double gap_sq = gap^2
summ gap_sq if year < 1989, meanonly
scalar pre_rmspe = sqrt(r(mean))
summ gap_sq if year >= 1989, meanonly
scalar post_rmspe = sqrt(r(mean))
scalar rmspe_ratio = post_rmspe / pre_rmspe

di as result "California pre-treatment RMSPE: " %21.15g pre_rmspe
di as result "California post-treatment RMSPE: " %21.15g post_rmspe
di as result "California post/pre RMSPE ratio: " %21.15g rmspe_ratio

preserve
    clear
    set obs 1
    gen str20 unit = "California"
    gen double pre_rmspe = scalar(pre_rmspe)
    gen double post_rmspe = scalar(post_rmspe)
    gen double ratio = scalar(rmspe_ratio)
    export delimited unit pre_rmspe post_rmspe ratio using "results/synth_rmspe.csv", replace
restore

use `paths_data', clear
twoway ///
    (line treated year, lcolor(navy) lwidth(medthick)) ///
    (line synthetic year, lcolor(maroon) lpattern(dash) lwidth(medthick)), ///
    xline(1989, lpattern(shortdash) lcolor(gs7)) ///
    title("California y su control sintético") ///
    subtitle("Intervención en 1989") ///
    xtitle("Año") ytitle("Paquetes per cápita") ///
    legend(order(1 "California" 2 "California sintética") rows(1) position(6))
graph export "synth_actual_vs_synthetic.png", replace width(1800)

twoway ///
    (line gap year, lcolor(navy) lwidth(medthick)), ///
    xline(1989, lpattern(shortdash) lcolor(gs7)) ///
    yline(0, lcolor(gs10)) ///
    title("Brecha: California menos control sintético") ///
    subtitle("Intervención en 1989") ///
    xtitle("Año") ytitle("Brecha en paquetes per cápita") ///
    legend(off)
graph export "synth_gap.png", replace width(1800)

di as result "Estimación canónica de Prop 99 completada."
