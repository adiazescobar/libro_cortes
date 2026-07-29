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

* Placebos espaciales: las 39 asignaciones usan la especificación canónica.
tempfile placebo_native placebo_stats placebo_gaps positive_donors loo_native loo_data time_native
use `panel', clear
levelsof state_id, local(all_units)
local expected_placebos : word count `all_units'
assert `expected_placebos' == 39

tempname placebo_stats_handle placebo_gaps_handle
postfile `placebo_stats_handle' int unit_id str30 unit double pre_rmspe post_rmspe ratio str32 optimization using `placebo_stats', replace
postfile `placebo_gaps_handle' int unit_id int year double gap using `placebo_gaps', replace

local failed_placebos ""
local completed_placebos = 0
foreach treated of local all_units {
    local optimization "nested"
    local donors ""
    foreach candidate of local all_units {
        if `candidate' != `treated' local donors "`donors' `candidate'"
    }
    quietly levelsof state_name if state_id == `treated', local(unit_name) clean
    capture quietly synth cigsale beer(1984(1)1988) lnincome retprice age15to24 ///
        cigsale(1988) cigsale(1980) cigsale(1975), ///
        trunit(`treated') trperiod(1989) xperiod(1980(1)1988) nested ///
        counit(`donors') keep(`placebo_native') replace
    if _rc {
        local first_rc = _rc
        di as error "Falló el intento nested para state_id=`treated' (`unit_name'), rc=`first_rc'."
        if `treated' != 34 | `first_rc' != 430 {
            local failed_placebos "`failed_placebos' `treated'"
            di as error "No hay fallback autorizado para esta unidad o código de error."
            continue
        }
        di as text "Fallback autorizado para Utah tras rc=430: misma especificación y donor pool con optimización default sin nested."
        use `panel', clear
        capture quietly synth cigsale beer(1984(1)1988) lnincome retprice age15to24 ///
            cigsale(1988) cigsale(1980) cigsale(1975), ///
            trunit(`treated') trperiod(1989) xperiod(1980(1)1988) ///
            counit(`donors') keep(`placebo_native') replace
        if _rc {
            local failed_placebos "`failed_placebos' `treated'"
            di as error "Falló el fallback autorizado para Utah, rc=" _rc
            continue
        }
        local optimization "default_fallback_after_rc430"
        di as result "Fallback default completado para Utah después del rc=430 nested."
    }

    preserve
        use `placebo_native', clear
        keep if !missing(_time)
        gen double placebo_gap = _Y_treated - _Y_synthetic
        gen double placebo_gap_sq = placebo_gap^2
        quietly summarize placebo_gap_sq if _time < 1989, meanonly
        local placebo_pre = sqrt(r(mean))
        quietly summarize placebo_gap_sq if _time >= 1989, meanonly
        local placebo_post = sqrt(r(mean))
        local placebo_ratio = `placebo_post' / `placebo_pre'
        assert `placebo_pre' < .
        assert `placebo_post' < .
        assert `placebo_ratio' < .
        post `placebo_stats_handle' (`treated') ("`unit_name'") (`placebo_pre') (`placebo_post') (`placebo_ratio') ("`optimization'")
        forvalues row = 1/`=_N' {
            post `placebo_gaps_handle' (`treated') (_time[`row']) (placebo_gap[`row'])
        }
    restore
    local completed_placebos = `completed_placebos' + 1
    di as text "Placebo espacial completado: `completed_placebos'/`expected_placebos' (`unit_name')."
}
postclose `placebo_stats_handle'
postclose `placebo_gaps_handle'

if `completed_placebos' != `expected_placebos' {
    di as error "Placebos espaciales incompletos. state_id con falla:`failed_placebos'"
    exit 498
}

use `placebo_stats', clear
isid unit_id
assert _N == 39
count if unit == "California"
assert r(N) == 1
count if optimization == "nested"
assert r(N) == 38
count if unit == "Utah" & optimization == "default_fallback_after_rc430"
assert r(N) == 1
assert optimization == "nested" if unit != "Utah"
quietly summarize pre_rmspe if unit == "California", meanonly
scalar pre_rmspe_california = r(mean)
quietly summarize ratio if unit == "California", meanonly
scalar ratio_california = r(mean)
scalar placebo_cutoff = 5*pre_rmspe_california
gen byte eligible = pre_rmspe <= placebo_cutoff
assert eligible == (pre_rmspe <= 5*pre_rmspe_california)
gen byte ratio_at_least_california = ratio >= ratio_california if eligible
quietly summarize ratio_at_least_california if eligible, meanonly
scalar eligible_share_ge_california = r(mean)
gen double eligible_share_ge_california = scalar(eligible_share_ge_california)
label variable eligible_share_ge_california "Proporción de placebos elegibles con razón al menos tan grande"
sort unit_id
export delimited unit_id unit pre_rmspe post_rmspe ratio eligible optimization ratio_at_least_california eligible_share_ge_california using "results/synth_placebos.csv", replace
di as result "Umbral de elegibilidad (5 x RMSPE pre de California): " %21.15g placebo_cutoff
di as result "Proporción de placebos elegibles con razón al menos tan grande: " %9.4f scalar(eligible_share_ge_california)

local ca_ratio_plot = scalar(ratio_california)
histogram ratio if eligible, fraction ///
    xline(`ca_ratio_plot', lcolor(navy) lwidth(medthick)) ///
    title("Razones RMSPE de asignaciones elegibles") ///
    subtitle("Línea: California; elegible si RMSPE pre <= 5 x California") ///
    xtitle("Razón RMSPE post/pre") ytitle("Proporción")
graph export "synth_rmspe_ratios.png", replace width(1800)

use `placebo_gaps', clear
isid unit_id year
reshape wide gap, i(year) j(unit_id)
local placebo_lines ""
foreach treated of local all_units {
    if `treated' != 3 local placebo_lines `"`placebo_lines' (line gap`treated' year, lcolor(gs12) lwidth(vthin))"'
}
twoway `placebo_lines' ///
    (line gap3 year, lcolor(navy) lwidth(thick)), ///
    xline(1989, lpattern(shortdash) lcolor(gs7)) ///
    yline(0, lcolor(gs10)) ///
    title("Brechas de las 39 asignaciones placebo") ///
    subtitle("California destacada; intervención en 1989") ///
    xtitle("Año") ytitle("Brecha en paquetes per cápita") legend(off)
graph export "synth_placebo_gaps.png", replace width(1800)

* Placebo temporal: beer solo existe desde 1984 y se excluye para evitar fuga.
* Los demás predictores y resultados usados en el ajuste terminan en 1979.
use `panel', clear
local time_donors ""
foreach candidate of local all_units {
    if `candidate' != 3 local time_donors "`time_donors' `candidate'"
}
quietly synth cigsale lnincome retprice age15to24 ///
    cigsale(1979) cigsale(1975) cigsale(1970), ///
    trunit(3) trperiod(1980) xperiod(1972(1)1979) nested ///
    counit(`time_donors') keep(`time_native') replace

use `time_native', clear
keep if inrange(_time, 1970, 1988)
gen double gap = _Y_treated - _Y_synthetic
assert gap < .
rename _time year
keep year gap
isid year
assert _N == 19
export delimited year gap using "results/synth_time_placebo.csv", replace
twoway ///
    (line gap year, lcolor(navy) lwidth(medthick)), ///
    xline(1980, lpattern(shortdash) lcolor(gs7)) ///
    yline(0, lcolor(gs10)) ///
    title("Placebo temporal para California") ///
    subtitle("Tratamiento ficticio en 1980; ajuste usa solo información hasta 1979") ///
    xtitle("Año") ytitle("Brecha en paquetes per cápita") legend(off)
graph export "synth_time_placebo.png", replace width(1800)

* Leave-one-out: excluir cada donante con peso estrictamente positivo.
use `weights_data', clear
keep if weight > 1e-8
keep state_id state weight
isid state_id
count
local positive_count = r(N)
levelsof state_id, local(positive_ids)
save `positive_donors'

tempname loo_handle
postfile `loo_handle' str30 omitted_state int year double gap using `loo_data', replace
use `panel', clear
foreach omitted of local positive_ids {
    local loo_donors ""
    foreach candidate of local all_units {
        if `candidate' != 3 & `candidate' != `omitted' local loo_donors "`loo_donors' `candidate'"
    }
    quietly levelsof state_name if state_id == `omitted', local(omitted_name) clean
    quietly synth cigsale beer(1984(1)1988) lnincome retprice age15to24 ///
        cigsale(1988) cigsale(1980) cigsale(1975), ///
        trunit(3) trperiod(1989) xperiod(1980(1)1988) nested ///
        counit(`loo_donors') keep(`loo_native') replace
    preserve
        use `loo_native', clear
        keep if !missing(_time)
        gen double loo_gap = _Y_treated - _Y_synthetic
        assert loo_gap < .
        forvalues row = 1/`=_N' {
            post `loo_handle' ("`omitted_name'") (_time[`row']) (loo_gap[`row'])
        }
    restore
    di as text "Leave-one-out completado: `omitted_name'."
}
postclose `loo_handle'

use `loo_data', clear
isid omitted_state year
bysort omitted_state: assert _N == 31
preserve
    keep omitted_state
    duplicates drop
    rename omitted_state state
    merge 1:1 state using `positive_donors', keepusing(state_id weight)
    assert _merge == 3
    assert _N == `positive_count'
restore
sort omitted_state year
export delimited omitted_state year gap using "results/synth_leave_one_out.csv", replace

encode omitted_state, gen(omitted_id)
drop omitted_state
levelsof omitted_id, local(loo_plot_ids)
reshape wide gap, i(year) j(omitted_id)
merge 1:1 year using `paths_data', keepusing(gap)
assert _merge == 3
drop _merge
rename gap main_gap
local loo_lines ""
foreach omitted_id of local loo_plot_ids {
    local loo_lines `"`loo_lines' (line gap`omitted_id' year, lcolor(gs10) lwidth(thin))"'
}
twoway `loo_lines' ///
    (line main_gap year, lcolor(navy) lwidth(thick)), ///
    xline(1989, lpattern(shortdash) lcolor(gs7)) ///
    yline(0, lcolor(gs10)) ///
    title("Sensibilidad leave-one-out") ///
    subtitle("Especificación principal destacada") ///
    xtitle("Año") ytitle("Brecha en paquetes per cápita") legend(off)
graph export "synth_leave_one_out.png", replace width(1800)

di as result "Estimación canónica, placebos y sensibilidad de Prop 99 completados."
