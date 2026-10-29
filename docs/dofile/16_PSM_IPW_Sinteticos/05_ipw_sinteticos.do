/*
Título: IPW y controles sintéticos en Stata
Autor: Ana María Díaz Escobar
Fecha: 2026-10-29

Contenido:
  Parte 1. IPW con el ejemplo de clase (8 personas, curso de formación).
  Parte 2. Control sintético: Proposición 99 en California.

Datos: synth_smoking.dta, en esta carpeta o en dofile/16_PSM_IPW_Sinteticos/
Paquete: synth (ssc install synth)
*/

clear all
set more off
set seed 1298
set linesize 100

capture confirm file "synth_smoking.dta"
if _rc {
    capture confirm file "dofile/16_PSM_IPW_Sinteticos/synth_smoking.dta"
    if !_rc cd "dofile/16_PSM_IPW_Sinteticos"
}
confirm file "synth_smoking.dta"
capture which synth
if _rc ssc install synth, replace

capture log close
log using ipw_sinteticos.log, replace



***============================================***
* PARTE 1. IPW CON EL EJEMPLO DE CLASE
***============================================***

* QUÉ HACE: crea las 8 personas de las notas. D = 1 si hizo el curso,
*   motivacion = 1 si tenía motivación alta en la línea de base,
*   y = salario mensual en millones.
* POR QUÉ: con 8 personas cada número se puede verificar a mano.
* QUÉ DEBEN VER: los tratados son sobre todo motivados y los controles no.

clear
input id D motivacion y
1 1 0 2.0
2 1 1 2.5
3 1 1 2.6
4 1 1 2.7
5 0 0 1.7
6 0 0 1.8
7 0 0 1.9
8 0 1 2.4
end
label variable D "Hizo el curso"
label variable motivacion "Motivación alta en la línea de base"
label variable y "Salario mensual (millones)"

tab motivacion D
table motivacion D, stat(mean y)

* 1.1 Diferencia cruda
* QUÉ DEBEN VER: 0,5 millones. Mezcla el curso con la motivación.
reg y D

* 1.2 El propensity score
* QUÉ HACE: estima p(X) = P(D = 1 | motivación) con un logit.
* QUÉ DEBEN VER: 0,25 para motivación baja y 0,75 para alta, que son
*   las proporciones de tratados en cada grupo de la tabla anterior.
logit D motivacion
predict double p, pr
list id D motivacion y p, sep(0)

* 1.3 Los pesos del ATE
* QUÉ HACE: tratados pesan 1/p(X) y controles 1/(1 - p(X)).
* QUÉ DEBEN VER: el tratado poco motivado pesa 4 y cada tratado motivado
*   1,33. En cada grupo los pesos suman 8, el tamaño de la población.
gen double w_ate = D/p + (1-D)/(1-p)
list id D motivacion y p w_ate, sep(0)
tabstat w_ate, by(D) stat(sum)

* 1.4 Promedios ponderados
* QUÉ DEBEN VER: 2,3 para los tratados y 2,1 para los controles.
mean y [aw=w_ate], over(D)

* 1.5 La fórmula de las notas, persona por persona
* QUÉ HACE: D*Y/p(X) - (1-D)*Y/(1-p(X)) para cada persona y promedia.
* QUÉ DEBEN VER: la media de ht es 0,2, el ATE.
gen double ht = D*y/p - (1-D)*y/(1-p)
list id D y p ht, sep(0)
summ ht

* 1.6 La misma cuenta como regresión ponderada
* QUÉ DEBEN VER: el coeficiente de D es 0,2.
reg y D [pw=w_ate]

* 1.7 El ATT
* QUÉ HACE: los tratados pesan 1 y los controles p(X)/(1 - p(X)).
* POR QUÉ: ahora queremos que los controles se parezcan a los tratados,
*   que son sobre todo motivados.
* QUÉ DEBEN VER: el control poco motivado pesa 1/3 y el motivado 3.
*   El ATT también es 0,2, porque el efecto es 0,2 en los dos grupos.
gen double w_att = D + (1-D)*p/(1-p)
list id D motivacion y w_att, sep(0)
reg y D [pw=w_att]

* 1.8 Con el comando de Stata
* QUÉ DEBEN VER: los mismos 0,2 del ATE y del ATT.
teffects ipw (y) (D motivacion, logit)
teffects ipw (y) (D motivacion, logit), atet


***============================================***
* PARTE 2. CONTROL SINTÉTICO: PROPOSICIÓN 99
***============================================***

* QUÉ HACE: California subió el impuesto al cigarrillo en 1988 (vigente
*   desde 1989). Hay un solo estado tratado y 38 donantes.
* POR QUÉ: con un solo tratado no hay con qué promediar. Fabricamos una
*   California con pedazos de otros estados, cuyos pesos son W.
* Variables: cigsale = paquetes per cápita (el resultado), retprice =
*   precio, lnincome = log del ingreso, age15to24 = % de 15 a 24 años,
*   beer = cerveza per cápita.

use synth_smoking.dta, clear
capture confirm string variable state
if !_rc {
    encode state, gen(state_id)
    drop state
    rename state_id state
}
tsset state year
label list
tab state if state==3

* 2.1 Los datos
* QUÉ DEBEN VER: California (gruesa) ya fumaba menos que casi todos y
*   caía más rápido antes de 1989.
xtline cigsale, overlay legend(off) addplot(line cigsale year if state==3, lwidth(thick) lcolor(navy)) ///
    xline(1989, lpattern(dash)) ytitle("Paquetes per cápita") xtitle("Año")

* 2.2 Por qué no sirve el promedio de los otros 38
* QUÉ DEBEN VER: el promedio de los donantes está por encima de California
*   desde 1970. Compararlos después de 1989 mezclaría la ley con esa brecha
*   que ya existía.
preserve
    gen byte california = state==3
    collapse (mean) cigsale, by(california year)
    reshape wide cigsale, i(year) j(california)
    twoway (line cigsale1 year, lcolor(navy) lwidth(thick)) (line cigsale0 year, lcolor(gs8) lpattern(dash)), ///
        xline(1989, lpattern(dash)) legend(order(1 "California" 2 "Promedio de los 38 donantes")) ///
        ytitle("Paquetes per cápita") xtitle("Año")
restore

* 2.3 El control sintético
* QUÉ HACE: busca los pesos W de los 38 estados para que la mezcla se
*   parezca a California en siete predictores medidos antes de 1989.
*   trunit(3) es California y trperiod(1989) el primer año con la ley.
*   beer(1984(1)1988) es la cerveza promediada entre 1984 y 1988.
*   lnincome, retprice y age15to24 se promedian en xperiod, 1980-1988.
*   cigsale(1975), cigsale(1980) y cigsale(1988) son el consumo en esos años.
*   mspeperiod(1970(1)1988) son los años que la sintética debe copiar.
*   nested busca también V, el criterio de cuánto pesa cada predictor.
* QUÉ DEBEN VER en la salida:
*   - RMSPE: el error de ajuste antes de 1989, cerca de 1,8 paquetes.
*   - Unit weights: Utah, Nevada, Montana, Colorado y Connecticut tienen
*     peso positivo; los otros 33 pesan cero.
*   - Predictor balance: California y la sintética casi iguales, salvo el
*     ingreso, porque California es más rica que casi todos los donantes.
*   - La gráfica: las dos líneas juntas hasta 1988 y separadas después.
synth cigsale beer(1984(1)1988) lnincome retprice age15to24 ///
    cigsale(1988) cigsale(1980) cigsale(1975), ///
    trunit(3) trperiod(1989) xperiod(1980(1)1988) ///
    mspeperiod(1970(1)1988) nested fig keep(synth_ca) replace

* W (la receta), V (el criterio) y el balance
matrix W = e(W_weights)
matrix V = e(V_matrix)
matrix list W
matrix list V
matrix list e(X_balance)

* 2.4 La sintética a mano
* QUÉ HACE: multiplica el consumo de cada donante por su peso y suma.
* POR QUÉ: para ver que la sintética no es más que un promedio ponderado
*   de estados reales, con pesos positivos que suman 1.
* QUÉ DEBEN VER: sintetica_mano igual a _Y_synthetic del comando.
preserve
    clear
    svmat double W, names(col)
    rename _Co_Number state
    rename _W_Weight w
    tempfile pesos
    save `pesos'
restore
preserve
    drop if state==3
    merge m:1 state using `pesos', nogen
    list state w if w > 0.001 & year==1988, sep(0)
    gen double aporte = w*cigsale
    collapse (sum) sintetica_mano=aporte, by(year)
    tempfile mano
    save `mano'
    use synth_ca, clear
    rename _time year
    keep if !missing(year)
    merge 1:1 year using `mano', nogen
    list year _Y_treated _Y_synthetic sintetica_mano if inlist(year, 1975, 1980, 1988, 1995, 2000), sep(0)
restore

* 2.5 El efecto
* QUÉ HACE: la brecha de cada año es California menos su sintética.
* QUÉ DEBEN VER: brecha cercana a cero antes de 1989 y negativa después,
*   de unos -26 paquetes hacia el 2000.
preserve
    use synth_ca, clear
    rename _time year
    keep if !missing(year)
    gen double brecha = _Y_treated - _Y_synthetic
    list year brecha if year >= 1985, sep(0)
    summ brecha if year >= 1989
    gen double brecha2 = brecha^2
    summ brecha2 if year < 1989
    scalar rmspe_pre = sqrt(r(mean))
    summ brecha2 if year >= 1989
    scalar rmspe_post = sqrt(r(mean))
    scalar list rmspe_pre rmspe_post
    twoway line brecha year, lcolor(navy) lwidth(thick) xline(1989, lpattern(dash)) yline(0) ///
        ytitle("California menos sintética (paquetes per cápita)") xtitle("Año")
restore

* 2.6 Qué pasa si todos los predictores pesan lo mismo
* QUÉ HACE: fija V con todos los predictores iguales, en vez de buscarlo.
* QUÉ DEBEN VER: otra receta (Colorado, Connecticut, Texas y Utah) y un
*   RMSPE de unos 5,9, más de tres veces el anterior. Que todos pesen lo
*   mismo suena neutral, pero esa mezcla no copia la caída de los ochenta.
synth cigsale beer(1984(1)1988) lnincome retprice age15to24 ///
    cigsale(1988) cigsale(1980) cigsale(1975), ///
    trunit(3) trperiod(1989) xperiod(1980(1)1988) ///
    mspeperiod(1970(1)1988) customV(1 1 1 1 1 1 1)

* 2.7 Placebos en el espacio
* QUÉ HACE: finge que cada uno de los 39 estados fue el tratado, le fabrica
*   su sintética con los otros 38 y guarda su RMSPE antes y después.
* POR QUÉ: con un solo tratado no hay errores estándar. La pregunta es si
*   la brecha de California es rara frente a la de estados sin la ley.
* QUÉ DEBEN VER: California tiene la razón RMSPE después/antes más alta o
*   casi la más alta. El valor p es su puesto dividido por 39.
* Nota: sin nested para que corra rápido; los pesos cambian un poco.
tempname ph
tempfile placebos brechas
postfile `ph' int estado double rmspe_pre rmspe_post using `placebos', replace
forvalues s = 1/39 {
    quietly synth cigsale beer(1984(1)1988) lnincome retprice age15to24 ///
        cigsale(1988) cigsale(1980) cigsale(1975), ///
        trunit(`s') trperiod(1989) xperiod(1980(1)1988) ///
        mspeperiod(1970(1)1988) keep(placebo_`s') replace
    preserve
        use placebo_`s', clear
        keep if !missing(_time)
        gen double g2 = (_Y_treated - _Y_synthetic)^2
        quietly summ g2 if _time < 1989
        local pre = sqrt(r(mean))
        quietly summ g2 if _time >= 1989
        local post = sqrt(r(mean))
    restore
    post `ph' (`s') (`pre') (`post')
    di as text "Placebo `s' de 39 listo"
}
postclose `ph'

preserve
    use `placebos', clear
    gen double razon = rmspe_post/rmspe_pre
    gsort -razon
    gen puesto = _n
    list estado rmspe_pre rmspe_post razon puesto, sep(0)
    list puesto razon if estado==3
    summ puesto if estado==3
    di "Valor p de permutación = " r(mean)/39
restore

* Las brechas de los 39 en una sola gráfica
* QUÉ DEBEN VER: California (gruesa) se separa del resto después de 1989.
*   Se excluyen los estados que antes de 1989 ajustaban 5 veces peor que
*   California, porque su brecha después no dice nada.
preserve
    use `placebos', clear
    quietly summ rmspe_pre if estado==3
    keep if rmspe_pre <= 5*r(mean)
    levelsof estado, local(buenos)
restore
preserve
    clear
    tempfile todas
    save `todas', emptyok
    foreach s of local buenos {
        use placebo_`s', clear
        keep if !missing(_time)
        gen int estado = `s'
        gen double brecha = _Y_treated - _Y_synthetic
        keep estado _time brecha
        append using `todas'
        save `todas', replace
    }
    use `todas', clear
    local lineas ""
    foreach s of local buenos {
        if `s' != 3 local lineas `"`lineas' (line brecha _time if estado==`s', lcolor(gs12) lwidth(vthin))"'
    }
    twoway `lineas' (line brecha _time if estado==3, lcolor(navy) lwidth(thick)), ///
        xline(1989, lpattern(dash)) yline(0) legend(off) ///
        ytitle("Brecha (paquetes per cápita)") xtitle("Año")
restore

* 2.8 Placebo en el tiempo
* QUÉ HACE: finge que la ley fue en 1980 y usa solo información hasta 1979.
*   La cerveza se deja por fuera porque solo existe desde 1984.
* QUÉ DEBEN VER: ninguna brecha entre 1980 y 1988. Si apareciera, la
*   sintética estaría capturando algo distinto a la ley.
synth cigsale lnincome retprice age15to24 ///
    cigsale(1979) cigsale(1975) cigsale(1970), ///
    trunit(3) trperiod(1980) xperiod(1972(1)1979) ///
    mspeperiod(1970(1)1979) nested keep(synth_1980) replace
preserve
    use synth_1980, clear
    rename _time year
    keep if inrange(year, 1970, 1988)
    gen double brecha = _Y_treated - _Y_synthetic
    twoway line brecha year, lcolor(navy) lwidth(thick) xline(1980, lpattern(dash)) yline(0) ///
        ytitle("Brecha con ley ficticia en 1980") xtitle("Año")
restore

forvalues s = 1/39 {
    capture erase placebo_`s'.dta
}
capture erase synth_ca.dta
capture erase synth_1980.dta

log close
