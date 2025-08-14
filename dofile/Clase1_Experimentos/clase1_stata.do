****************************************************
* CLASE: Experimentos controlados, controles y heterogeneidad
* Usando datos del experimento (data.dta)
*******************************************************
clear all 
capture log close
log using "clase_experimentos.log", text replace

*------------------------------------------------------
* 0. Paquetes sugeridos
*------------------------------------------------------
cap which esttab
if _rc ssc install estout, replace
cap which iebaltab
if _rc ssc install ietoolkit, replace

*------------------------------------------------------
* 1. Cargar base REAL
*------------------------------------------------------
use "data.dta", clear

*------------------------------------------------------
* 2. Variables clave 
*------------------------------------------------------
gen y = resultado

gen D = (grupo == "B")
label define Dlbl 0 "Control" 1 "Tratado"
label values D Dlbl

gen mujer     = (genero   == "Mujer")
gen pregrado  = (programa == "Pregrado")
gen maestria  = (programa == "Maestría")

global X edad mujer libros pregrado maestria

* Descriptivas rápidas
tab D
table D, statistic(mean y) nformat(%9.3f)

*------------------------------------------------------
* 3. Programa de balance con postfile (incluye nombre de variable)
*    - Guarda: variable mean_T mean_C diff tstat pval se sd_T sd_C N_T N_C
*    - Imprime tabla alineada en consola
*------------------------------------------------------
cap program drop difmedias
program define difmedias, rclass
    version 18
    syntax varlist(min=1) [, BY(varname) SAVEPOST(string asis)]
    if ("`by'"=="") {
        di as err "Necesitas especificar , by(varname)."
        exit 198
    }
    local vars `varlist'

    local do_post = 0
    if ("`savepost'"!="") local do_post = 1
    tempname posth
    if `do_post' {
        postfile `posth' str32 variable ///
            double mean_T mean_C diff tstat pval se sd_T sd_C N_T N_C ///
            using "`savepost'", replace
    }

    di as txt "Var{col 22}Mean_T{col 33}Mean_C{col 44}Diff{col 55}t{col 66}p"
    foreach v of local vars {
        quietly ttest `v', by(`by')

        local mu1 = r(mu_1)
        local mu2 = r(mu_2)
        local sd1 = r(sd_1)
        local sd2 = r(sd_2)
        local N1  = r(N_1)
        local N2  = r(N_2)
        local t   = r(t)
        local se  = r(se)
        local p   = r(p)

        local mean_T = `mu2'
        local mean_C = `mu1'
        local diff   = `mu2' - `mu1'

        di as res "`v'" ///
            "{col 22}" %9.3f `mean_T' ///
            "{col 33}" %9.3f `mean_C' ///
            "{col 44}" %9.3f `diff'   ///
            "{col 55}" %9.3f `t'      ///
            "{col 66}" %9.3f `p'

        if `do_post' {
            post `posth' ("`v'") (`mean_T') (`mean_C') (`diff') (`t') (`p') ///
                           (`se') (`sd2') (`sd1') (`N2') (`N1')
        }
    }
    if `do_post' postclose `posth'
end

*------------------------------------------------------
* 4. Balance de covariables (dos salidas: propia y iebaltab)
*------------------------------------------------------
difmedias $X, by(D) savepost("Table_Balance_raw.dta")

iebaltab $X, grpvar(D) control(0) rowvarlabels ftest ///
    savetex("Table_Balance.tex") save("Table_Balance.xlsx") replace format(%9.3f)

* (Opcional) versión Excel de nuestra tabla
preserve
use "Table_Balance_raw.dta", clear
order variable mean_T mean_C diff tstat pval se sd_T sd_C N_T N_C
export excel using "Table_Balance_raw.xlsx", firstrow(variables) replace
restore

*------------------------------------------------------
* 5. ¿Quedó bien asignado? (Pruebas multivariadas)
*------------------------------------------------------
eststo clear
eststo: reg   D $X, vce(robust)
test $X
eststo: logit D $X, vce(robust)
eststo: probit D $X, vce(robust)

esttab, se star(* 0.10 ** 0.05 *** 0.01) ///
    stats(N r2, fmt(%9.0g %9.3f) labels("N" "R2")) ///
    nomtitle compress

*------------------------------------------------------
* 6. Efecto del tratamiento (con y sin controles)
*    Observa el cambio en coef y SE(D) al incluir X
*------------------------------------------------------
eststo clear
eststo m1: reg y D, vce(robust)
eststo m2: reg y D $X, vce(robust)

esttab m1 m2, se star(* 0.10 ** 0.05 *** 0.01) ///
    stats(N r2, fmt(%9.0g %9.3f) labels("N" "R2")) ///
    b(%9.4f) compress nonote ///
    title("Efecto del tratamiento con y sin controles")

*------------------------------------------------------
* 7. Guardar medias T/C y diff para y y X en un solo archivo
*------------------------------------------------------
local cols y $X
difmedias `cols', by(D) savepost("means_yX.dta")

preserve
use "means_yX.dta", clear
order variable mean_T mean_C diff tstat pval se sd_T sd_C N_T N_C
export excel using "means_yX.xlsx", firstrow(variables) replace
restore

*------------------------------------------------------
* 8. Efectos heterogéneos
*------------------------------------------------------
* 8.1 Por mujer (binaria)
eststo clear
eststo: reg y D##i.mujer, vce(robust)
margins, dydx(D)
margins D, at(mujer=(0 1))

* 8.2 Por libros (continua) + gráfico limpio
eststo: reg y D##c.libros, vce(robust)
margins, dydx(D) at(libros=(0(1)8))
marginsplot, title("Efecto marginal de D según libros")
graph export "margins_libros.pdf", replace

* 8.3 Por cuartiles de edad
xtile q_edad = edad, nq(4)
eststo: reg y D##i.q_edad, vce(robust)
margins D#q_edad
marginsplot, title("Heterogeneidad por cuartiles de edad")
graph export "margins_qedad.pdf", replace

*------------------------------------------------------
* 9. (Opcional) Visual comparativa T vs C por libros con predicción al promedio
*------------------------------------------------------
preserve
summ edad mujer pregrado maestria, meanonly
local medad     = r(mean1)
local mmujer    = r(mean2)
local mpregrado = r(mean3)
local mmaestria = r(mean4)

* Modelo con controles para fijar al promedio y trazar curvas limpias
qui reg y i.D##c.libros edad mujer pregrado maestria, vce(robust)

range libros_g 0 8 60
expand 4 if _n==1
gen D_g = 0 in 1/2
replace D_g = 1 in 3/4
gen libros_g2 = libros_g
replace libros_g2 = libros_g in 3/4
gen edad_g     = `medad'
gen mujer_g    = `mmujer'
gen pregrado_g = `mpregrado'
gen maestria_g = `mmaestria'

predict yhat_g if e(sample), xb
drop if missing(D_g)

twoway ///
 (line yhat_g libros_g if D_g==0, sort) ///
 (line yhat_g libros_g if D_g==1, sort), ///
 legend(order(1 "Control" 2 "Tratado")) ///
 title("Predicción y vs. libros por grupo (controles al promedio)")
graph export "scatter_line_libros.pdf", replace
restore

*------------------------------------------------------
* 10. Versión alternativa (opcional): matriz en r(table)
*     Útil si no quieres escribir a disco; puedes svmat luego.
*------------------------------------------------------
cap program drop difmedias_mx
program define difmedias_mx, rclass
    version 18
    syntax varlist(min=1) [, BY(varname)]
    if ("`by'"=="") {
        di as err "Necesitas especificar , by(varname)."
        exit 198
    }
    tempname M
    matrix `M' = J(0,10,.)
    local rn ""

    foreach v of local varlist {
        quietly ttest `v', by(`by')
        local mu1 = r(mu_1)
        local mu2 = r(mu_2)
        local sd1 = r(sd_1)
        local sd2 = r(sd_2)
        local N1  = r(N_1)
        local N2  = r(N_2)
        local t   = r(t)
        local se  = r(se)
        local p   = r(p)
        matrix row = (`mu2',`mu1',`mu2'-`mu1',`t',`p',`se',`sd2',`sd1',`N2',`N1')
        matrix `M' = (`M' \ row)
        local rn `rn' `v'
    }
    matrix colnames `M' = mean_T mean_C diff t p se sd_T sd_C N_T N_C
    matrix rownames `M' = `rn'
    return matrix table = `M'
end

* Ejemplo de uso en memoria (sin escribir a disco):
* quietly difmedias_mx $X, by(D)
* mat list r(table)

*------------------------------------------------------
* 11. Cierre
*------------------------------------------------------
log close
di as res "Listo. Archivos clave: Table_Balance.{tex,xlsx}, Table_Balance_raw.{dta,xlsx}, means_yX.{dta,xlsx}, gráficos PDF."

