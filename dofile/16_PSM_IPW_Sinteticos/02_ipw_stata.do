/*
IPW (Inverse Probability Weighting) en Stata
Clase 16 - EconometriaAV 2026-1
Basado en base6.dta
*/

clear all
set seed 1298
set more off

* Directorio de trabajo
cd ~/Library/CloudStorage/Dropbox/ClasesR/EconometriaAV/EjerciciosClase

* Cargar datos
use base6.dta, clear

* Instalar comandos necesarios
* ssc install teffects (ya vienen con Stata 13+)

* Definir variables
global X "personas orden_n ocupado_jefe educa_jefe ingresos_hogar_jefe hombre"

log using ipw_demo.log, replace

***============================================***
* 1. ESTADÍSTICAS DESCRIPTIVAS
***============================================***

summ D y2 $X
tab D

***============================================***
* 2. ESTIMAR EL PROPENSITY SCORE
***============================================***

* Método 1: Probit
probit D $X
predict double ps_probit, pr

* Método 2: Logit (alternativa)
logit D $X
predict double ps_logit, pr

summ ps_probit ps_logit

***============================================***
* 3. VERIFICAR SOPORTE COMÚN
***============================================***

* Los pesos IPW pueden explotar si hay poco soporte común
* Regla: descartar obs donde P(X) < 0.1 o P(X) > 0.9

gen soporte = (ps_probit > 0.1 & ps_probit < 0.9)
summ soporte

twoway (kdensity ps_probit if D==1, lcolor(blue) lwidth(medium)) ///
       (kdensity ps_probit if D==0, lcolor(red) lwidth(medium)), ///
       legend(label(1 "Tratados") label(2 "Controles")) ///
       title("Propensity Score: Verificar Soporte Común") ///
       xtitle("P(D=1|X)") ytitle("Densidad") ///
       xline(0.1, lpattern(dash) lcolor(gray)) ///
       xline(0.9, lpattern(dash) lcolor(gray))

graph export ipw_support.png, replace

***============================================***
* 4. CONSTRUIR PESOS IPW (Horvitz-Thompson)
***============================================***

* IPW para ATT (Average Treatment Effect on the Treated)
gen double w_att = cond(D==1, 1, ps_probit/(1-ps_probit))

* IPW para ATE (Average Treatment Effect)
gen double w_ate = cond(D==1, 1/ps_probit, 1/(1-ps_probit))

* IPW normalizado
bys D: gen double w_att_norm = w_att / sum(w_att)
bys D: gen double w_ate_norm = w_ate / sum(w_ate)

summ w_att w_ate
summ w_att if D==0
summ w_ate if D==0

***============================================***
* 5. MÉTODOS PARA USAR LOS PESOS IPW EN REGRESIÓN
***============================================***

***--- 5a. Ponderación en OLS (estimación manual) ---***

* ATT simple: comparación de medias ponderadas
mean y2 if D==1
mean y2 if D==0 [pw=w_att]

* Diferencia
quietly summ y2 if D==1, meanonly
scalar mu1_att = r(mean)
quietly summ y2 [aw=w_att] if D==0, meanonly
scalar mu0_att = r(mean)
scalar diff_att = mu1_att - mu0_att
di "ATT manual IPW: " diff_att

***--- 5b. Ponderación en Regresión OLS ---***

* Especificación simple
reg y2 D [pw=w_att], robust
estimates store ipw_att_simple

reg y2 D [pw=w_ate], robust
estimates store ipw_ate_simple

* Con controles adicionales
reg y2 D $X [pw=w_att], robust
estimates store ipw_att_controls

reg y2 D $X [pw=w_ate], robust
estimates store ipw_ate_controls

***--- 5c. Comando nativo: teffects ipw ---***

* teffects ipw es un one-stop-shop para IPW con SE correctos
* Sintaxis: teffects ipw (outcome) (treatment vars, probit/logit), atet/ate vce(robust)

teffects ipw (y2) (D $X, probit), atet vce(robust)
estimates store teff_ipw_att

teffects ipw (y2) (D $X, probit), ate vce(robust)
estimates store teff_ipw_ate

* Guardar pesos generados por teffects
capture predict double ps_teff, ps
if _rc != 0 {
    gen double ps_teff = ps_probit
}

***============================================***
* 6. COMPARAR ESTIMADORES
***============================================***

estimates table ipw_att_simple ipw_ate_simple ipw_att_controls ipw_ate_controls ///
               teff_ipw_att teff_ipw_ate, b se

***============================================***
* 7. VERIFICAR BALANCE CON PESOS IPW
***============================================***

* Balance antes de ponderar
table D, stat(mean $X)

* Balance después de ponderar (muestra el balance "esperado")
table D, stat(sum w_att)

* Calcular pesos standardizados para análisis
summ w_att, meanonly
scalar mean_w_att = r(mean)
gen double w_att_std = w_att / mean_w_att if D==0
replace w_att_std = 1 if D==1

***============================================***
* 8. ANÁLISIS DE SENSIBILIDAD: VARIACIÓN DE BANDWIDTH
***============================================***

* Los pesos IPW pueden ser muy grandes si PS es muy cercano a 0 o 1
* Opción 1: Trim (descartar observaciones extremas)

gen w_att_trim = w_att if soporte==1

* Opción 2: Capping (limitar el peso máximo)
gen w_att_cap = min(w_att, 5)

* Opción 3: Stabilized IPW (dividir por P(D=1))
summ D, meanonly
scalar p_treated = r(mean)
gen double w_att_stab = cond(D==1, 1, ps_probit/(1-ps_probit)) / p_treated

summ w_att w_att_trim w_att_cap w_att_stab

***============================================***
* 9. GRÁFICAS DE LOS PESOS
***============================================***

histogram w_att if D==0, title("Distribución de Pesos IPW (ATT, D=0)") ///
          xtitle("Peso") ytitle("Frecuencia")
graph export ipw_weights_dist.png, replace

***============================================***
* 10. ESTIMACIONES FINALES CON DIFERENTES ESPECIFICACIONES
***============================================***

eststo clear

* Naive (sin controlar selección)
eststo: reg y2 D, robust

* OLS con controles
eststo: reg y2 D $X, robust

* IPW con pesos ATT
eststo: reg y2 D $X [pw=w_att], robust

* IPW con pesos ATE
eststo: reg y2 D $X [pw=w_ate], robust

* Comando integrado teffects
eststo: teffects ipw (y2) (D $X, probit), atet vce(robust)

esttab, b(%9.4f) se(%9.4f) star(* 0.10 ** 0.05 *** 0.01) ///
        title("Comparación de Estimadores")

log close
