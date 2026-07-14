***************************************************************
* Cap 6: Experimentos Aleatorizados en Stata
* Base: data.dta (experimento de aula, 4 semestres, N=70)
*   Grupo B = definiciones de palabras (tratamiento)
*   Grupo A = texto crítico de economía (control)
*   semestre = estrato (1-4)
***************************************************************
clear all
set seed 1298
set more off
set linesize 100
capture log close
log using "clase6_experimentos.log", text replace

*--------------------------------------------------------------
* 0. Paquetes
*--------------------------------------------------------------
cap which esttab
if _rc ssc install estout, replace
cap which iebaltab
if _rc ssc install ietoolkit, replace

*--------------------------------------------------------------
* PASO 1: Cargar base y crear variables
*--------------------------------------------------------------
capture confirm file "data.dta"
if _rc {
    capture confirm file "dofile/06_RCT_Stata/data.dta"
    if !_rc cd "dofile/06_RCT_Stata"
}
capture confirm file "data.dta"
if _rc {
    di as error "No se encontró data.dta. Corre este do-file desde su carpeta o desde la raíz del libro."
    exit 601
}
use "data.dta", clear
capture mkdir "results"

* Validación explícita del contrato de datos compartido con Python
foreach v in resultado grupo genero programa semestre edad libros {
    capture confirm variable `v'
    if _rc {
        di as error "Falta la variable requerida `v' en data.dta."
        exit 111
    }
}

* Tratamiento: B = definiciones (1), A = texto crítico (0)
gen D = (grupo == "B")
label define Dlbl 0 "Control" 1 "Tratado"
label values D Dlbl

* Outcome
gen y = resultado

* Controles pre-tratamiento
gen mujer    = (genero   == "Mujer")
gen pregrado = (programa == "Pregrado")
gen maestria = (programa == "Maestría")

global X edad mujer libros pregrado maestria

* Descriptivas rápidas
tab D
tab semestre D
table D, statistic(mean y) nformat(%9.3f)

*--------------------------------------------------------------
* PASO 2: Aleatorización
* (La asignación ya fue hecha en cada semestre. Aquí solo
* mostramos cómo se haría si quisiéramos re-aleatorizar.)
*--------------------------------------------------------------

* --- Bernoulli simple (50/50) ---
set seed 20250813
gen double u = runiform()
gen byte D_sim = (u < 0.5)
tab D_sim
drop u D_sim

* --- Estratificada por semestre (50/50 exacto dentro de cada estrato) ---
set seed 20250813
bys semestre: gen double u = runiform()
bys semestre (u): gen byte D_strat = (_n <= round(_N*0.50))
tab D_strat
by semestre: tab D_strat
drop u D_strat

*--------------------------------------------------------------
* PASO 3: Verificar balance de covariables
*--------------------------------------------------------------

* 3.1 Programa difmedias con postfile
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
            post `posth' ("`v'") (`mean_T') (`mean_C') (`diff') ///
                (`t') (`p') (`se') (`sd2') (`sd1') (`N2') (`N1')
        }
    }
    if `do_post' postclose `posth'
end

* 3.2 Balance univariado
tempfile balance_raw
capture noisily difmedias $X, by(D) savepost("`balance_raw'")
if _rc {
    di as text "La rutina difmedias no corrió; se omite Table_Balance_raw.xlsx en esta ejecución."
}
else {
    preserve
    use "`balance_raw'", clear
    order variable mean_T mean_C diff tstat pval se sd_T sd_C N_T N_C
    export excel using "Table_Balance_raw.xlsx", firstrow(variables) replace
    rename mean_T media_tratado
    rename mean_C media_control
    rename diff diferencia
    rename pval p_value
    keep variable media_tratado media_control diferencia p_value N_T N_C
    export delimited using "results/balance_stata.csv", replace
    restore
}

* Exportación canónica independiente para las tablas del libro.
* No depende de la rutina docente difmedias, que se conserva arriba.
tempname balanceh
tempfile canonical_balance
postfile `balanceh' str32 variable double media_tratado media_control ///
    diferencia p_value N_T N_C using "`canonical_balance'", replace
foreach v of global X {
    quietly ttest `v', by(D)
    post `balanceh' ("`v'") (r(mu_2)) (r(mu_1)) ///
        (r(mu_2) - r(mu_1)) (r(p)) (r(N_2)) (r(N_1))
}
postclose `balanceh'
preserve
use "`canonical_balance'", clear
export delimited using "results/balance_stata.csv", replace
restore

* 3.3 Balance con iebaltab
capture noisily iebaltab $X, grpvar(D) control(0) rowvarlabels ftest ///
    savexlsx("Table_Balance.xlsx") replace format(%9.3f)
if _rc {
    di as text "iebaltab no generó Table_Balance.xlsx; se conserva Table_Balance_raw.xlsx."
}

* 3.4 Pruebas multivariadas: D ~ X
eststo clear
eststo: reg   D $X, vce(robust)
test $X
eststo: logit D $X, vce(robust)
eststo: probit D $X, vce(robust)

esttab, se star(* 0.10 ** 0.05 *** 0.01) ///
    stats(N r2, fmt(%9.0g %9.3f) labels("N" "R2")) ///
    nomtitle compress

*--------------------------------------------------------------
* PASO 4: Los cuatro escenarios de estimación del ATE
*--------------------------------------------------------------
* Escenario 1: Y = α + τD + u              (simple)
* Escenario 2: Y = α + τD + β'X + u        (+ controles)
* Escenario 3: Y = α + τD + δ'S + u        (+ estratos)
* Escenario 4: Y = α + τD + δ'S + β'X + u  (completo)
*--------------------------------------------------------------

eststo clear

* Escenario 1: RCT simple (diferencia de medias)
eststo m1: reg y D, vce(robust)

* Escenario 2: RCT simple + controles baseline
eststo m2: reg y D $X, vce(robust)

* Escenario 3: Estratificado por semestre, sin controles adicionales
eststo m3: reg y D i.semestre, vce(robust)

* Escenario 4: Estratificado + controles (especificación completa)
eststo m4: reg y D i.semestre $X, vce(robust)

* Exportar contrato canónico para la verificación Stata vs. Python
tempname resultsh
tempfile canonical_results
postfile `resultsh' str24 modelo str12 termino double coeficiente ///
    error_estandar N R2 str24 prueba double estadistico p_value ///
    using "`canonical_results'", replace

foreach spec in m1_simple m2_controles m3_estratos m4_completo {
    if "`spec'" == "m1_simple"     local stored m1
    if "`spec'" == "m2_controles" local stored m2
    if "`spec'" == "m3_estratos"  local stored m3
    if "`spec'" == "m4_completo"  local stored m4
    estimates restore `stored'
    local p_D = 2*ttail(e(df_r), abs(_b[D]/_se[D]))
    post `resultsh' ("`spec'") ("D") (_b[D]) (_se[D]) (e(N)) (e(r2)) ///
        ("") (.) (`p_D')
}
postclose `resultsh'

preserve
use "`canonical_results'", clear
export delimited using "results/resultados_stata.csv", replace
restore

* Comparar los cuatro modelos
esttab m1 m2 m3 m4, se star(* 0.10 ** 0.05 *** 0.01) ///
    stats(N r2, fmt(%9.0g %9.3f) labels("N" "R2")) ///
    b(%9.4f) compress nonote ///
    mtitle("Simple" "+Controles" "+Estratos" "Completo") ///
    title("Cuatro escenarios de estimación del ATE")

* Exportar a Excel
esttab m1 m2 m3 m4 using "Tabla_4escenarios.xlsx", replace ///
    se star(* 0.10 ** 0.05 *** 0.01) ///
    stats(N r2, fmt(%9.0g %9.3f) labels("N" "R2")) ///
    b(%9.4f) mtitle("Simple" "+Controles" "+Estratos" "Completo")

*--------------------------------------------------------------
* PASO 5: Efectos heterogéneos
*--------------------------------------------------------------

* 5.1 Por mujer (binaria)
eststo clear
eststo het_mujer: reg y D##i.mujer, vce(robust)
margins, dydx(D)
margins D, at(mujer=(0 1))

* Resultado canónico principal de heterogeneidad para el libro
estimates restore het_mujer
local het_b = _b[1.D#1.mujer]
local het_se = _se[1.D#1.mujer]
local het_N = e(N)
preserve
clear
set obs 1
gen str20 moderador = "genero"
gen str40 termino = "D#Mujer"
gen double coeficiente = `het_b'
gen double error_estandar = `het_se'
gen double N = `het_N'
export delimited using "results/heterogeneidad_stata.csv", replace
restore

* 5.2 Por libros (continua)
eststo het_libros: reg y D##c.libros, vce(robust)
margins, dydx(D) at(libros=(0(1)8))
marginsplot, title("Efecto marginal de D según libros")
graph export "margins_libros.pdf", replace

* 5.3 Por cuartiles de edad
xtile q_edad = edad, nq(4)
eststo het_edad: reg y D##i.q_edad, vce(robust)
margins D#q_edad
marginsplot, title("Heterogeneidad por cuartiles de edad")
graph export "margins_qedad.pdf", replace

*--------------------------------------------------------------
* PASO 6: El truco de centrar (Wooldridge)
*--------------------------------------------------------------
* Sin centrar: τ = efecto solo para libros=0
di as txt "--- Sin centrar ---"
reg y c.D##c.libros, vce(robust)
* El coef de D es el efecto cuando libros=0

* Con centrado: τ = ATE promedio
di as txt "--- Con centrado (Wooldridge) ---"
sum libros
gen libros_c = libros - r(mean)
reg y c.D##c.libros_c, vce(robust)
* Ahora el coef de D es directamente el ATE

* También para mujer (ya es binaria, pero el centrado cambia interpretación)
sum mujer
gen mujer_c = mujer - r(mean)
reg y c.D##c.mujer_c, vce(robust)

*--------------------------------------------------------------
* 7. Cierre
*--------------------------------------------------------------
log close
di as res "Listo. Revisa: Table_Balance.xlsx, Tabla_4escenarios.xlsx, gráficos PDF."
