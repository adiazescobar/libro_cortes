********************************************************************************
** Ejercicio: Discriminación Racial en el Mercado Laboral
** Bertrand & Mullainathan (2004) — "Are Emily and Greg More Employable
** than Lakisha and Jamal? A Field Experiment on Labor Market Discrimination"
** American Economic Review, 94(4): 991-1013.
**
** Econometría Avanzada — Pontificia Universidad Javeriana 2026-1
** Instrucciones: descargue bm.dta y ajuste la ruta en la línea "use"
********************************************************************************

clear all
set more off

* ============================================================
* CARGA DE DATOS
* ============================================================
* Descargue bm.dta desde el enlace del curso y ajuste la ruta:
use "https://www.dropbox.com/scl/fi/ephx1kl4opc0q3oxe5ckp/bm.dta?rlkey=zwp0hwtec5z25a4ll9qn8biz7&dl=1", clear

* Variables disponibles en la base:
*   black          = 1 si el nombre suena afroamericano, 0 si suena blanco  (TRATAMIENTO)
*   call           = 1 si recibió llamada de regreso                         (OUTCOME)
*   female         = 1 si el nombre es femenino
*   yearsexp       = años de experiencia en la hoja de vida
*   education      = nivel educativo
*   ofjobs         = número de empleos anteriores
*   computerskills = 1 si menciona habilidades informáticas

label variable black          "Nombre afroamericano (tratamiento)"
label variable call           "Recibió llamada de regreso"
label variable female         "Nombre femenino"
label variable yearsexp       "Años de experiencia"
label variable education      "Nivel educativo"
label variable ofjobs         "Número de empleos anteriores"
label variable computerskills "Habilidades informáticas"

* Confirmar variables cargadas
describe
sum

********************************************************************************
** PREGUNTA 1: ¿Por qué un experimento y no comparar contratados negros vs blancos?
********************************************************************************
/*
Esta pregunta es conceptual — la respuesta que esperamos es:

La comparación de individuos CONTRATADOS negros vs blancos sufre de sesgo de
selección grave: los empleadores escogen a quién contratan, y si discriminan,
el pool de negros contratados estará positivamente seleccionado (solo los más
calificados superan el sesgo). Una regresión simple estimaría β sesgado,
probablemente hacia cero o incluso positivo, ocultando la discriminación.

El experimento aleatorio envía hojas de vida IDÉNTICAS (excepto el nombre),
eliminando la selección: cualquier diferencia en callback se debe solo al nombre.
*/

di as result "=" * 70
di as result "PREGUNTA 1 — RESPUESTA CONCEPTUAL (ver comentarios en el do-file)"
di as result "=" * 70

********************************************************************************
** PREGUNTA 2: ¿La asignación aleatoria fue exitosa? (BALANCE)
********************************************************************************

di as result "=" * 70
di as result "PREGUNTA 2 — TABLA DE BALANCE"
di as result "=" * 70

* Variables de control disponibles en esta versión de la base
local controls yearsexp education ofjobs computerskills female

* Estadísticas descriptivas por grupo
tabstat `controls', by(black) stat(mean sd) format(%6.4f) nototal

* Pruebas t por variable — debe no haber diferencias significativas
di as result _newline "Tests t de diferencia de medias por característica de la HV:"
foreach var of local controls {
    quietly ttest `var', by(black)
    local pval = round(r(p), 0.001)
    di as text "  `var': p-value = `pval'"
}

* Regresión de las covariables sobre tratamiento: si la asignación fue exitosa,
* ningún coeficiente debería ser significativo
di as result _newline "F-test conjunto: ¿predicen las covariables el tratamiento?"
reg black `controls', robust
test `controls'   // si p > 0.05 → balance exitoso

********************************************************************************
** PREGUNTA 3: Efecto del tratamiento sobre la probabilidad de recibir una llamada
********************************************************************************

di as result "=" * 70
di as result "PREGUNTA 3 — EFECTO PROMEDIO DEL TRATAMIENTO"
di as result "=" * 70

* --- Tasas crudas por grupo ---
di as result _newline "Tasa de callback por grupo:"
tabulate black call, row nofreq

* --- Modelo SIN controles ---
di as result _newline "Modelo sin controles (MCO):"
reg call black, robust
estimates store m_sin_controles

* --- Modelo CON controles ---
di as result _newline "Modelo con controles (MCO):"
reg call black female yearsexp education ofjobs computerskills, robust
estimates store m_con_controles

* --- Tabla comparativa ---
di as result _newline "Comparación de modelos:"
estimates table m_sin_controles m_con_controles, ///
    keep(black) b(%8.5f) se star stats(N r2) ///
    title("Efecto de tener nombre afroamericano sobre P(callback)")

/*
Interpretación esperada:
- Sin controles: β ≈ -0.032 (los negros tienen ~3.2pp menos probabilidad de callback)
- Con controles: β ≈ -0.032 (casi no cambia — ¡gracias a la aleatorización!)
- La diferencia ≈ 0 porque la aleatorización garantiza que las covariables
  son independientes del tratamiento → incluir controles solo reduce varianza,
  no cambia el estimador.
*/

* --- Probit con efectos marginales ---
di as result _newline "Probit (efectos marginales promedio en muestra):"
probit call black female yearsexp education ofjobs computerskills, robust
margins, dydx(black)

********************************************************************************
** PREGUNTA 4: EFECTOS HETEROGÉNEOS
********************************************************************************

di as result "=" * 70
di as result "PREGUNTA 4 — EFECTOS HETEROGÉNEOS"
di as result "=" * 70

* --- 4a. Variable BINARIA: género del nombre (female) ---
di as result _newline "Heterogeneidad por género (variable binaria):"

reg call i.black##i.female, robust
/*
Interpretación:
  Coeficiente de black:             efecto para hombres (female==0)
  Coeficiente de 1.black#1.female:  diferencia del efecto mujer vs hombre
  Si es significativo → la discriminación difiere por género
*/

* Efectos marginales condicionales
margins female, dydx(black)
marginsplot, title("Efecto del nombre afroamericano por género")

* --- 4b. Variable CONTINUA: años de experiencia (yearsexp) ---
di as result _newline "Heterogeneidad por años de experiencia (variable continua):"

reg call c.black##c.yearsexp, robust
/*
Interpretación:
  black:              efecto con 0 años de experiencia
  black#c.yearsexp:   ¿cómo cambia el efecto a medida que la experiencia aumenta?
  Si es negativo → la brecha se amplía con más experiencia (retorno a calidad ≠ para negros)
  Este es el resultado más impactante de B&M: más calidad en la HV ayuda a los
  blancos pero casi no ayuda a los negros.
*/

* Visualizar el efecto para distintos niveles de experiencia
margins, dydx(black) at(yearsexp=(0(2)20))
marginsplot, ///
    title("Efecto del nombre sobre P(callback) por años de experiencia") ///
    xtitle("Años de experiencia en la hoja de vida") ///
    ytitle("Efecto marginal de nombre afroamericano")

********************************************************************************
** PREGUNTA 5: PODER ESTADÍSTICO
********************************************************************************

di as result "=" * 70
di as result "PREGUNTA 5 — CÁLCULO DE PODER ESTADÍSTICO"
di as result "=" * 70

* --- Estadísticas base ---
quietly sum call if black == 0
local p_white = r(mean)
local n_white = r(N)

quietly sum call if black == 1
local p_black = r(mean)
local n_black = r(N)

di as result _newline "Tasa de callback blancos:        " %6.4f `p_white'
di as result "Tasa de callback afroamericanos: " %6.4f `p_black'
di as result "Diferencia observada:            " %6.4f (`p_white' - `p_black')
di as result "N total:                         " _N

* --- 5a. ¿Qué poder tiene la muestra de 4870 para detectar el efecto observado? ---
di as result _newline "5a. Poder dado N=4870 y las tasas observadas:"
power twoproportions `p_white' `p_black', n(4870) alpha(0.05)

* --- 5b. ¿Cuál es el efecto mínimo detectable con N=4870 y poder=0.80? ---
di as result _newline "5b. Efecto mínimo detectable (MDE) dado N=4870:"
power twoproportions `p_white', n(4870) power(0.80) alpha(0.05)

local mde = r(delta)
di as result "MDE = " %6.4f `mde' " pp (diferencia mínima detectable en tasas de callback)"

* --- 5c. ¿Cuántas observaciones se necesitarían para detectar la mitad del efecto? ---
local efecto_mitad  = (`p_white' - `p_black') / 2
local p_black_mitad = `p_white' - `efecto_mitad'

di as result _newline "5c. Muestra necesaria para detectar la mitad del efecto observado:"
power twoproportions `p_white' `p_black_mitad', power(0.80) alpha(0.05)
local N_mitad = r(N)
di as result "Se necesitarían aproximadamente " %6.0f `N_mitad' " observaciones"

* --- 5d. Gráfico: poder vs diferencia en proporción ---
di as result _newline "5d. Gráfico: curvas de poder:"
power twoproportions `p_white' (`p_black'(0.005)`p_white'), n(4870) alpha(0.05) graph ///
    title("Poder estadístico dado N=4870") ///
    xtitle("Tasa de callback en grupo afroamericano (p2)") ///
    ytitle("Poder") ///
    xline(`p_black', lcolor(red) lpattern(dash))

/*
Interpretación de la Pregunta 5:
Con N=4870 y las tasas observadas (~6.5% vs ~9.7%):
- El estudio tiene poder muy alto (>99%) para detectar la diferencia de 3.2pp
- El MDE es considerablemente menor que el efecto observado
- Para detectar un efecto de la mitad (~1.6pp), se necesitarían ~15,000-20,000 obs

Conclusión: la muestra de 4870 es suficiente para el efecto que encontraron.
Si el efecto real fuera más pequeño (ej. 1pp), serían insuficientes.
*/

di as result _newline "=" * 70
di as result "FIN DEL EJERCICIO — Bertrand & Mullainathan (2004)"
di as result "=" * 70
