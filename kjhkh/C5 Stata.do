***************************************************************
*Experimentos Controlados
***************************************************************


*Paso 1. Leer la base de datos


gen y = /* Nota */
gen D = /* Grupo B */
gen mujer = [genero == "Mujer"]
gen pregrado 
edad genero programa

global X  /* Todas las variables de Control libros mujer maestria edad*/
global X edad

/*************************************************************/
*Paso 2. Hacer estadísticas descriptivas para tratados y controles

tab D
table D, contents(mean y)


/*************************************************************/
/*************************************************************/

*Paso 3. Verificar que la asignación aleatoria fue correcta

/*Diferencia de Medias*/







/*************************************************************
      r(N_1)         sample size n_1
      r(N_2)         sample size n_2
      r(p_l)         lower one-sided p-value
      r(p_u)         upper one-sided p-value
      r(p)           two-sided p-value
      r(se)          estimate of standard error
      r(t)           t statistic
      r(sd_1)        standard deviation for first variable
      r(sd_2)        standard deviation for second variable
      r(mu_1)        x_1 bar, mean for population 1
      r(mu_2)        x_2 bar, mean for population 2
      r(df_t)        degrees of freedom
**************************************************************/

*Diferencias de Medias en una Tabla para todas las variables
cap program drop difmedias
program define difmedias
syntax varlist [, by(varname)]
 di "Var" " {col 20} " "Media T Media C Diff t Valor-p"
  foreach v of local varlist{
  qui ttest `v', by(`by')
  di "`v'" "{col 20}" %6.2f r(mu_2) " " %6.2f r(mu_1) " " %6.2f r(mu_2)-r(mu_1) " " %6.2f r(t) " " %6.2f r(p)
 }
end

difmedias $X, by(D)

eststo  mco: reg D $X, r
test $X
eststo dprobit: dprobit D $X
eststo logit: logit D $X
eststo logitmfx: mfx
estout mco dprobit logitmfx, cells(b(star fmt(%9.2f)) se(par)) starlevels(* 0.10 ** 0.05 *** 0.01)stats(N F chi2 p) margin legend

**************************************************************/
*Paso 4: Vamos a explorar el efecto causal


difmedias y, by(D)

*Regresiones

eststo mco1: reg y D, r
eststo mco2: reg y D $X,r 

estout mco1 mco2 , cells(b(star fmt(%9.4f)) se(par)) stats(F chi2 p) margin legend


iebaltab $X , grpvar(D) control(1) savetex("Table1") replace rowvarlabels fnoobs  texvspace(1pt)  format(%9.2f)

iebaltab $X , grpvar(D) control(1) save("Table1.xlsx") replace rowvarlabels fnoobs  texvspace(1pt)  format(%9.2f)

*************************************************************************************************************
/*Efectos heterogéneos*/
*************************************************************************************************************
/*Variables de Factor
Son extensiones de las varaibles existentes que se emplean para codificar las variables categÛricas.
Crean:
- indicadores (dummy) a partir de variables categÛricas
- interacciones de las variables categÛricas
- interacciones de las variables categÛricas y continuas
- interacciones de las variables continuas (polinomios)
Se pueden emplear con la mayorÌa de los comandos de estimaciÛn y post-estimaciÛn y con algunos otros comandos
(no se puede emplear con tab)
La siguiente tabla proporciona una lista de los operadores de variables de factor.

    i.    factor variable (variable dummy o categÛrica)
    b.    grupo de referencia (base)
			EspecificaciÛn DescripciÛn
			--------------------------------------------------------
			b#.            use # como base, #= valor de la varible
			b(first).      use el menor valor de la variable como base (default)
			b(last).       use el mayor valor de la variables como base
			b(freq).       use el valor m·s frecuente como base
			bn.            no emplee base 


    #     interacciÛn
    ##    interacciÛn factorial

    c.    variable continua
    o.    omitted variable or levels
*/
************************************************************************************************************* 


sum edad
sum i.edad
sum b(freq).edad

*Emplee el operador \#\# para especificar interacciones factoriales entre las variables
*A\#\#B es una forma compacta de escribir i.A i.B A\#B
reg score D i.edad, r
reg score D##i.mujer, r
reg score D##i.maestria, r
reg score D##c.libros, r

reg score D##i.mujer, r
margins , dydx(D)

reg score D##i.mujer, r
margins D, at( mujer ==(0 1))

reg score D libros, r
predict participantes if D== 1
label var participantes "Tratados"
predict control if D==0
label var control "Control"
twoway (connected participantes libros) (connected control libros)
graph export c2f.pdf, replace


reg score D##c.libros, r
predict participantes2 if D== 1
label var participantes2 "Tratados"
predict control2 if D==0
label var control2 "Control"
twoway (connected participantes2 libros) (connected control2 libros)
graph export c3f.pdf, replace



sum libros, d
reg score D##c.libros, r 
margins , dydx(D)
margins , dydx(D) at( libros =(0(1)6))
qui margins D,  at(libros =(0(1)6))
marginsplot
graph export c3f2.pdf, replace

qui margins r.D,  at(libros =(0(1)6))
marginsplot
graph export c3f3.pdf, replace


