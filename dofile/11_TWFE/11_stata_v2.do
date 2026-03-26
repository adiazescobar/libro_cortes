********************************************************************************
* Datos de Panel, DiD, TWFE y Estimadores Modernos
* Econometría Avanzada — Ana María Díaz Escobar — Javeriana 2026-I
*
* VERSIÓN 2 (2026-03-26) — mejorada para clase
*
* SECCIONES:
*  0.  Instalar paquetes (ejecutar UNA sola vez; luego dejar comentado)
*  1.  Datos de panel: xtset, within, FE / FD / RE / Hausman
*  2.  2×2 exacto: DiD = FD = TWFE (equivalencia algebraica)
*  3.  Panel largo, adopción simultánea: DiD = TWFE ≠ FD; tendencias paralelas
*  4.  Adopción escalonada, misma fecha, efectos heterogéneos
*  5.  NUEVO: Ejemplo mínimo de pesos negativos (2 unidades, 4 períodos)
*  6.  Staggered adoption + Descomposición de Bacon
*  7.  Estimadores modernos (simulación grande) + event-study comparativo
*
* CAMBIOS VS v1:
*  - Corregido cluster(i) → cluster(id) en todo el código
*  - Corregido: matrix names en event_plot alineados con los que se guardan
*  - Eliminado 'ref' en stackedev (variable que no se creaba)
*  - Agregada Sección 5: ejemplo numérico de pesos negativos (nuevo)
*  - Agregadas: opciones de postestimación de csdid (estat event/simple/calendar)
*  - Agregado: csdid_plot
*  - Agregado: jwdid
*  - Paquetes comentados en bloque propio (Sección 0)
*  - Comentarios mejorados en todo el archivo
*
* NOTA: Todo el do-file es replicable (set seed en cada sección).
*       Cada sección limpia los datos con 'clear' antes de empezar.
********************************************************************************

clear all
set more off
set linesize 120

********************************************************************************
* SECCIÓN 0: INSTALAR PAQUETES
* Ejecutar UNA sola vez. Luego comentar este bloque.
********************************************************************************

/*
* ── Paquetes de utilidad ────────────────────────────────────────────────────
ssc install schemepack,         replace   // esquemas de gráficos
ssc install avar,               replace   // estimadores de varianza robustos
ssc install reghdfe,            replace   // FE de alta dimensión
ssc install event_plot,         replace   // gráfico comparativo de event studies
ssc install palettes,           replace   // paleta de colores
ssc install colrspace,          replace   // espacio de color para palettes
ssc install bacondecomp,        replace   // descomposición de Goodman-Bacon

* ── Paquetes de DiD modernos ────────────────────────────────────────────────
ssc install drdid,              replace   // doubly-robust DiD (necesario para csdid)
ssc install csdid,              replace   // Callaway & Sant'Anna (2021)
ssc install did_imputation,     replace   // Borusyak, Jaravel & Spiess (2022)
ssc install eventstudyinteract, replace   // Sun & Abraham (2021)
ssc install did_multiplegt_dyn, replace   // de Chaisemartin & D'Haultfœuille (2024)
ssc install stackedev,          replace   // Cengiz et al. (2019), apilamiento
ssc install did2s,              replace   // Gardner (2022), dos etapas
ssc install jwdid,              replace   // Wooldridge (2021), pooled OLS interaccionado
*/

********************************************************************************
* SECCIÓN 1: INTRODUCCIÓN A DATOS DE PANEL
*
* DGP: 100 individuos, 6 periodos
*   Y_it = alpha_i + lambda_t + beta*X_it + eps_it
*   alpha_i correlacionado con X_it → FE necesario
********************************************************************************

set seed 9999
local N = 100
local T = 6
set obs `= `N' * `T''

* ── Estructura de panel ────────────────────────────────────────────────────
egen id = seq(), b(`T')
egen t  = seq(), f(1) t(`T')
sort id t
xtset id t
label variable id "Individuo"
label variable t  "Periodo (1–6)"

* ── DGP ────────────────────────────────────────────────────────────────────
gen alpha_i = 2 * id / `N' + rnormal(0, 0.5)   // EF individual, correlacionado con X
gen X = 0.5 * alpha_i + 0.3 * t + rnormal(0, 1)
label variable X "Variable de interés"
scalar beta_true = 3
gen Y = alpha_i + 0.8 * t + beta_true * X + rnormal(0, 1)
label variable Y "Variable resultado"

* ── Descripción del panel ──────────────────────────────────────────────────
di _n "=== DESCRIPCIÓN DEL PANEL ==="
xtdes
xtsum Y X
* LECTURA: between = diferencias entre medias individuales
*          within  = variación de cada individuo alrededor de su propia media
*          FE/FD explotan solo la variación WITHIN

* ── Spaghetti plot ─────────────────────────────────────────────────────────
xtline Y if id <= 10, overlay ///
    title("Evolución de Y: primeros 10 individuos") ///
    xtitle("Periodo") ytitle("Y") legend(off) ///
    name(g0_xtline, replace)

* ── OLS pooled (incorrecto porque Cov(X, alpha_i) ≠ 0) ───────────────────
di _n "=== OLS Pooled (INCORRECTO) ==="
reg Y X, vce(cluster id)
di "Beta verdadero = `= scalar(beta_true)'. OLS estará sesgado hacia arriba."

* ── Efectos fijos: xtreg fe ────────────────────────────────────────────────
di _n "=== EFECTOS FIJOS ==="
xtreg Y X i.t, fe vce(cluster id)
di "Beta verdadero = `= scalar(beta_true)'. FE lo recupera. ✓"

* ── FE a mano: transformación within ──────────────────────────────────────
di _n "=== FE A MANO (transformación within) ==="
bysort id: egen mean_Y = mean(Y)
bysort id: egen mean_X = mean(X)
gen Y_within = Y - mean_Y
gen X_within = X - mean_X
reg Y_within X_within, nocons
di "Debe coincidir con xtreg ,fe (sin i.t para simplificar)"
drop mean_Y mean_X Y_within X_within

* ── Efectos aleatorios ─────────────────────────────────────────────────────
di _n "=== EFECTOS ALEATORIOS (incorrecto aquí) ==="
xtreg Y X i.t, re vce(cluster id)

* ── Test de Hausman ────────────────────────────────────────────────────────
di _n "=== HAUSMAN: ¿FE o RE? ==="
xtreg Y X i.t, fe
estimates store fe_hausman
xtreg Y X i.t, re
hausman fe_hausman ., sigmamore
* H0: RE consistente (Cov(X,alpha_i)=0). Si p<0.05 → usar FE.

* ── Primeras diferencias ───────────────────────────────────────────────────
di _n "=== PRIMERAS DIFERENCIAS ==="
reg D.Y D.X ibn.t, noconstant vce(cluster id)
di "FD elimina alpha_i por diferencia (usa T-1 periodos por individuo)"

di _n "=" * 65
di "RESUMEN ESTIMADORES"
di "=" * 65
di "  Cov(X, alpha_i)=0  →  RE eficiente    (Hausman no rechaza)"
di "  Cov(X, alpha_i)≠0  →  FE consistente  (Hausman rechaza)"
di "  X endógeno en eps  →  TODOS sesgan     (necesitas IV)"
di "  T=2               →  FD = FE = DiD   (identidad algebraica)"
di "  T>2               →  FE más eficiente que FD"
di "=" * 65


********************************************************************************
* SECCIÓN 2: 2×2 — DiD = FD = TWFE (EQUIVALENCIA ALGEBRAICA EXACTA)
*
* DGP: 200 individuos, 2 periodos, 2 grupos. Efecto causal τ = 3.
* OBJETIVO: mostrar que los 4 estimadores dan EXACTAMENTE el mismo número.
********************************************************************************

clear
set seed 1234
set obs 400

gen id   = ceil(_n / 2)
gen t    = mod(_n - 1, 2)         // t=0 antes, t=1 después
gen trat = (id > 100)             // grupo tratado: id 101–200
gen D    = trat * (t == 1)        // tratamiento efectivo: tratado × post
gen alpha_i = 2 * id / 200 + rnormal(0, 0.5)
gen eps     = rnormal(0, 1)
gen Y = alpha_i + 1.5 * t + 3 * D + eps   // τ = 3 (verdadero)
xtset id t

* ── DiD manual (4 medias) ─────────────────────────────────────────────────
quietly sum Y if trat==1 & t==0
scalar y_t0 = r(mean)
quietly sum Y if trat==1 & t==1
scalar y_t1 = r(mean)
quietly sum Y if trat==0 & t==0
scalar y_c0 = r(mean)
quietly sum Y if trat==0 & t==1
scalar y_c1 = r(mean)
scalar DiD_manual = (y_t1 - y_t0) - (y_c1 - y_c0)

* ── Regresión DiD clásica: Y = α + βD_i + γt + τ(D_i×t) ─────────────────
reg Y trat t D, robust
scalar DiD_reg = _b[D]

* ── Primeras Diferencias (T=2) ────────────────────────────────────────────
reg D.Y D.D, robust
scalar FD_2x2 = _b[D.D]

* ── TWFE ──────────────────────────────────────────────────────────────────
reghdfe Y D, absorb(id t) vce(robust)
scalar TWFE_2x2 = _b[D]

di _n "=" * 60
di "  SECCIÓN 2: 2×2 — EQUIVALENCIA ALGEBRAICA EXACTA"
di "=" * 60
di "  Valor verdadero de τ            = 3"
di "  DiD manual (4 medias)           = " %7.4f DiD_manual
di "  Regresión DiD (Y~trat+t+D)      = " %7.4f DiD_reg
di "  Primeras diferencias (ΔY~ΔD)    = " %7.4f FD_2x2
di "  TWFE (reghdfe absorb id t)      = " %7.4f TWFE_2x2
di "-" * 60
di "  Los cuatro deben ser IDÉNTICOS — equivalencia algebraica ✓"
di "  Con T=2 grupos, 2 períodos, no hay escogencia posible."
di "=" * 60


********************************************************************************
* SECCIÓN 3: PANEL LARGO, ADOPCIÓN SIMULTÁNEA — TENDENCIAS PARALELAS
*
* DGP: 3 unidades, T=11. Adopción en t=1985. Dos casos:
*   A: tendencias paralelas ✓     B: tendencias NO paralelas ✗
********************************************************************************

* ─────────────────────────────────────────────────────────────────────────────
* CASO 3A: Tendencias paralelas — DiD = TWFE ✓
* ─────────────────────────────────────────────────────────────────────────────

clear
set seed 5678
local inicio = 1980
local fin    = 1990
local tiempo = `fin' - `inicio' + 1
set obs `= 3 * `tiempo''

gen id = ceil(_n / `tiempo')
gen t  = `inicio' + mod(_n - 1, `tiempo')
sort id t
xtset id t

gen D = (id >= 2) * (t >= 1985)
gen Y = id + 3 * (t - 1980) + 5 * D + rnormal(0, 0.5)
label variable Y "Y (tendencias paralelas ✓)"

twoway ///
    (connected Y t if id==1, msymbol(circle)  lcolor(blue)   lwidth(medium)) ///
    (connected Y t if id==2, msymbol(triangle) lcolor(red)    lwidth(medium)) ///
    (connected Y t if id==3, msymbol(square)   lcolor(orange) lwidth(medium)) ///
    , xline(1984.5, lpattern(dash) lcolor(gray)) ///
      xlabel(`inicio'(1)`fin') ///
      legend(order(1 "Control (id=1)" 2 "Tratado id=2" 3 "Tratado id=3") pos(6) row(1)) ///
      title("Caso 3A: tendencias paralelas ✓") ///
      xtitle("Año") ytitle("Y") name(g3a_trends, replace)

* DiD manual
quietly sum Y if id >= 2 & t >= 1985
scalar y_t_post = r(mean)
quietly sum Y if id >= 2 & t < 1985
scalar y_t_pre  = r(mean)
quietly sum Y if id == 1 & t >= 1985
scalar y_c_post = r(mean)
quietly sum Y if id == 1 & t < 1985
scalar y_c_pre  = r(mean)
scalar DiD3a = (y_t_post - y_t_pre) - (y_c_post - y_c_pre)

reghdfe Y D, absorb(id t) vce(robust)
scalar TWFE3a = _b[D]

* Test formal de tendencias paralelas (Stata 17+)
gen trat_grupo = (id >= 2)
xtdidregress (Y) (D), group(id) time(t)
estat trendplots, title("Tendencias pre — Caso 3A") name(g3a_trendplots, replace)
estat ptrends
* H0: pendientes pre-tratamiento son iguales. p>0.05 → no rechazamos → OK ✓

di _n "=== CASO 3A: Tendencias paralelas ✓ ==="
di "DiD manual = " %6.3f DiD3a "   TWFE = " %6.3f TWFE3a "   (τ = 5)"
di "  → DiD = TWFE con adopción simultánea y T.P. ✓"

* ─────────────────────────────────────────────────────────────────────────────
* CASO 3B: Tendencias NO paralelas ✗ — TWFE está sesgado
* ─────────────────────────────────────────────────────────────────────────────

clear
set seed 5678
local inicio = 1980
local fin    = 1990
local tiempo = `fin' - `inicio' + 1
set obs `= 3 * `tiempo''

gen id = ceil(_n / `tiempo')
gen t  = `inicio' + mod(_n - 1, `tiempo')
sort id t
xtset id t

gen D = (id >= 2) * (t >= 1985)
* DIFERENCIA: tratados tienen tendencia pre más pronunciada (+2/año)
gen tendencia_extra = (id >= 2) * (t - `inicio') * 2
gen Y = id + 3 * (t - 1980) + tendencia_extra + 5 * D + rnormal(0, 0.5)
label variable Y "Y (tendencias NO paralelas ✗)"

twoway ///
    (connected Y t if id==1, msymbol(circle)  lcolor(blue)   lwidth(medium)) ///
    (connected Y t if id==2, msymbol(triangle) lcolor(red)    lwidth(medium)) ///
    (connected Y t if id==3, msymbol(square)   lcolor(orange) lwidth(medium)) ///
    , xline(1984.5, lpattern(dash) lcolor(gray)) ///
      xlabel(`inicio'(1)`fin') ///
      legend(order(1 "Control (id=1)" 2 "Tratado id=2" 3 "Tratado id=3") pos(6) row(1)) ///
      title("Caso 3B: tendencias NO paralelas ✗") ///
      note("Tratados crecen más rápido antes del tratamiento") ///
      xtitle("Año") ytitle("Y") name(g3b_trends, replace)

reghdfe Y D, absorb(id t) vce(robust)
scalar TWFE3b = _b[D]

xtdidregress (Y) (D), group(id) time(t)
estat trendplots, title("Tendencias pre — Caso 3B (violación)") ///
    name(g3b_trendplots, replace)
estat ptrends
* p<0.05 → rechazamos H0 → tendencias NO son paralelas → TWFE sesgado

di _n "=== CASO 3B: Tendencias NO paralelas ✗ ==="
di "TWFE = " %6.3f TWFE3b "   (τ = 5 → sesgo ≈ " %6.3f TWFE3b - 5 ")"
di "  → TWFE captura τ + diferencia de tendencias = SESGO positivo"

di _n "  Mecánica del sesgo (primeras diferencias):"
di "  ΔY_control  (1984→1985) = tendencia común = 3"
di "  ΔY_tratado  (1984→1985) = tendencia común + tendencia_extra + τ"
di "                           = 3 + 2 + 5 = 10"
di "  DiD 1-paso = 10 - 3 = 7  ≠  τ = 5 (sesgo = 2)"


********************************************************************************
* SECCIÓN 4: ADOPCIÓN SIMULTÁNEA, EFECTOS HETEROGÉNEOS
*
* DGP: id=2 τ=2, id=3 τ=4, ambos tratados desde 1985.
* OBJETIVO: mostrar que TWFE promedia bien cuando los efectos son constantes
*           (sin heterogeneidad dinámica). ATT verdadero = 3, TWFE ≈ 3.
********************************************************************************

clear
local unidades = 3
local inicio   = 1980
local fin      = 1989
local tiempo   = `fin' - `inicio' + 1
local obs      = `unidades' * `tiempo'
set obs `obs'

gen id = .
gen t  = .
forvalues i = 1/`unidades' {
    forvalues j = 0/`=`tiempo'-1' {
        local obsnum = (`i' - 1)*`tiempo' + `j' + 1
        replace id = `i' in `obsnum'
        replace t  = `inicio' + `j' in `obsnum'
    }
}
sort id t
xtset id t
label variable id "Unidad"
label variable t  "Año"

gen D = 0
replace D = 1 if id >= 2 & t >= 1985
label variable D "Tratamiento desde 1985 (id≥2)"

gen Y = id + t
replace Y = Y + 2 if id == 2 & D == 1   // τ=2 para id=2
replace Y = Y + 4 if id == 3 & D == 1   // τ=4 para id=3
label variable Y "Variable dependiente"

twoway ///
    (connected Y t if id==1, msymbol(circle)  lcolor(blue)   lwidth(medium)) ///
    (connected Y t if id==2, msymbol(triangle) lcolor(red)    lwidth(medium)) ///
    (connected Y t if id==3, msymbol(square)   lcolor(orange) lwidth(medium)) ///
    , xline(1984.5, lpattern(dash) lcolor(gray)) ///
      xlabel(`inicio'(1)`fin') ///
      legend(order(1 "id=1 (Control)" 2 "id=2 (τ=2)" 3 "id=3 (τ=4)") pos(6) row(1)) ///
      title("Sección 4: mismo timing, efectos heterogéneos") ///
      xtitle("Año") ytitle("Y") name(g4_hetero, replace)

xtreg Y D i.t, fe
reghdfe Y D, absorb(id t) vce(robust)

* ATT verdadero = (5×2 + 5×4) / 10 = 3
di _n "  ATT verdadero (ponderado por obs tratadas) = (5×2 + 5×4)/10 = 3"
di "  TWFE debe dar ≈ 3 ✓ (con efectos constantes en el tiempo, funciona)"
di "  CUIDADO: si los efectos crecieran con el tiempo, TWFE ya fallaría aquí."


********************************************************************************
* SECCIÓN 5: STAGGERED ADOPTION + DESCOMPOSICIÓN DE BACON
*
* DGP: id=1 control, id=2 trata 1985 (τ=2), id=3 trata 1988 (τ=4)
* OBJETIVO: ver los tres tipos de comparaciones 2×2 y el β̂_TWFE compuesto
* Referencia: Goodman-Bacon (2021), Journal of Econometrics
********************************************************************************

clear
local unidades = 3
local inicio   = 1980
local fin      = 1989
local tiempo   = `fin' - `inicio' + 1
local obs      = `unidades' * `tiempo'
set obs `obs'

gen id = .
gen t  = .
forvalues i = 1/`unidades' {
    forvalues j = 0/`=`tiempo'-1' {
        local obsnum = (`i' - 1)*`tiempo' + `j' + 1
        replace id = `i' in `obsnum'
        replace t  = `inicio' + `j' in `obsnum'
    }
}
sort id t
xtset id t
label variable id "Unidad"
label variable t  "Año"

gen D = 0
replace D = 1 if id == 2 & t >= 1985   // cohorte temprana, τ=2
replace D = 1 if id == 3 & t >= 1988   // cohorte tardía, τ=4
label variable D "Tratamiento escalonado"

gen Y = 0
replace Y = D * 2 if id==2 & D==1   // τ=2 cuando id=2 está tratado
replace Y = D * 4 if id==3 & D==1   // τ=4 cuando id=3 está tratado
label variable Y "Variable dependiente (Y=0 sin tratar; Y=τ con tratar)"

twoway ///
    (connected Y t if id==1, msymbol(circle)  lcolor(blue)   lwidth(medium)) ///
    (connected Y t if id==2, msymbol(triangle) lcolor(red)    lwidth(medium)) ///
    (connected Y t if id==3, msymbol(square)   lcolor(orange) lwidth(medium)) ///
    , xline(1984.5 1987.5, lpattern(dash) lcolor(gray)) ///
      xlabel(`inicio'(1)`fin') ///
      legend(order(1 "id=1 (Control)" 2 "id=2 (trata 1985, τ=2)" ///
             3 "id=3 (trata 1988, τ=4)") pos(6) row(1)) ///
      title("Sección 5: adopción escalonada") ///
      xtitle("Año") ytitle("Y") name(g5_staggered, replace)

* ── TWFE completo ─────────────────────────────────────────────────────────
di _n "  TWFE (todo el panel):"
reghdfe Y D, absorb(id t) vce(robust)
* TWFE combina las 3 familias de comparaciones

* ── Pares 2×2 "limpios" (con control puro = id=1) ─────────────────────────
di _n "  Par id=1 vs id=2 (control puro — solo compara limpiamente):"
xtreg Y D i.t if inlist(id, 1, 2), fe robust
di _n "  Par id=1 vs id=3 (control puro):"
xtreg Y D i.t if inlist(id, 1, 3), fe robust

* ── Descomposición de Bacon ────────────────────────────────────────────────
di _n "  Descomposición de Goodman-Bacon:"
bacondecomp Y D, ddetail
* LECTURA:
*   Early_v_Late: id=2 (early) vs id=3 (late) en periodos PRE de id=3
*     → β ≈ 2 (efecto de id=2). Todos los pesos POSITIVOS.
*   Late_v_Early: id=3 (late) vs id=2 (early) DESPUÉS de que id=2 ya está tratado
*     → β ≈ 4 (efecto de id=3, pero usando control ya tratado)
*   Never_v_timing: cohortes tratadas vs id=1 (control puro)
*     → promedio ponderado de ambas cohortes
*
* CLAVE: los pesos de Bacon suman a 1 y son todos POSITIVOS.
* El problema NO es que haya pesos negativos en la descomposición de Bacon.
* El problema es que Late_v_Early usa id=2 (ya tratado) como "control" para id=3.
* Con efectos heterogéneos, esa comparación no identifica ATT(id=3) correctamente.

di ""
di "  ATT_overall verdadero = (5×2 + 2×4) / 7 ≈ " (5*2 + 2*4)/7
di "  β̂_TWFE (arriba) difiere de ATT_overall cuando los efectos varían"


********************************************************************************
* SECCIÓN 6: PESOS NEGATIVOS — EJEMPLO NUMÉRICO MÍNIMO
*
* DGP exacto: 2 unidades, 4 periodos, sin ruido.
*   Unidad 1: tratada desde t=2, τ=2 (constante)
*   Unidad 2: tratada desde t=4, τ=4 (constante)
*   Sin nunca-tratados (escenario donde el problema es más claro)
*
* OBJETIVO:
*   1. Mostrar que D̃_it puede ser NEGATIVO cuando D_it=1
*   2. Mostrar que el peso implícito sobre ATT(1,4) es NEGATIVO
*   3. Mostrar que TWFE ≠ ATT_overall cuando los efectos son heterogéneos
*   4. Mostrar que TWFE = ATT_overall si los efectos fueran homogéneos
********************************************************************************

clear

* ── 2 unidades, 4 periodos ────────────────────────────────────────────────
set obs 8
gen id = ceil(_n / 4)   // unidad 1 u 2
gen t  = mod(_n - 1, 4) + 1   // t = 1, 2, 3, 4
sort id t
xtset id t

* Tratamiento: id=1 desde t=2; id=2 desde t=4
gen D = 0
replace D = 1 if id == 1 & t >= 2
replace D = 1 if id == 2 & t >= 4

* Resultados (sin ruido): Y_it = id + t + tau_i * D_it
gen Y = id + t + 2 * D * (id == 1) + 4 * D * (id == 2)
label variable Y "Y (sin ruido)"
label variable D "Tratamiento"

di _n "=" * 65
di "  SECCIÓN 6: PESOS NEGATIVOS — EJEMPLO NUMÉRICO MÍNIMO"
di "=" * 65
di ""
di "  Datos del panel:"
list id t D Y, sep(4)

* ── Medias individuales, temporales y globales ────────────────────────────
bysort id: egen D_bar_i  = mean(D)   // D̄_i
bysort t:   egen D_bar_t  = mean(D)   // D̄_t
summ D
scalar D_bar = r(mean)               // D̄

* ── D demeaned: D̃_it = D_it - D̄_i - D̄_t + D̄ ───────────────────────────
gen D_tilde = D - D_bar_i - D_bar_t + D_bar
label variable D_tilde "D demeaned (D̃_it)"

* ── Y demeaned ────────────────────────────────────────────────────────────
bysort id: egen Y_bar_i  = mean(Y)
bysort t:   egen Y_bar_t  = mean(Y)
summ Y
scalar Y_bar = r(mean)
gen Y_tilde = Y - Y_bar_i - Y_bar_t + Y_bar
label variable Y_tilde "Y demeaned (Ỹ_it)"

di _n "  D demeaned (D̃_it):"
list id t D D_tilde, sep(4)

di _n "  OBSERVACIÓN CLAVE:"
di "  Unidad 1, t=4: D=1 (tratada) pero D̃ = " ///
    %6.4f D_tilde[4] " < 0 → PESO NEGATIVO"

* ── Verificar: pesos implícitos sobre ATT(g,t) ───────────────────────────
summ D_tilde
local denom = 0
quietly {
    forvalues obs = 1/8 {
        local d_til = D_tilde[`obs']
        local denom = `denom' + `d_til'^2
    }
}

di _n "  Denominador Σ D̃² = " `denom'
di ""
di "  Pesos implícitos π_it = D̃_it / Σ D̃² para celdas TRATADAS (D=1):"
di ""
di "  ┌─────────────────────────────────────────────────────────────────┐"
di "  │ (id,t) │  D  │   D̃    │  π = D̃/ΣD̃²  │  ATT(g,t)  │ Signo  │"
di "  ├─────────────────────────────────────────────────────────────────┤"
di "  │ (1, 2) │  1  │  +1/4  │    +1/2      │     2      │  ✓ pos │"
di "  │ (1, 3) │  1  │  +1/4  │    +1/2      │     2      │  ✓ pos │"
di "  │ (1, 4) │  1  │  -1/4  │    -1/2      │     2      │  ✗ NEG │"
di "  │ (2, 4) │  1  │  +1/4  │    +1/2      │     4      │  ✓ pos │"
di "  └─────────────────────────────────────────────────────────────────┘"
di ""
di "  β̂_TWFE = (+1/2)×2 + (+1/2)×2 + (-1/2)×2 + (+1/2)×4 = 1+1-1+2 = 3"
di "  ATT_overall verdadero = (2+2+2+4)/4 = 2.5"
di "  → TWFE = 3 ≠ ATT_overall = 2.5  (sesgo = +0.5)"

* ── Verificación con regresión ────────────────────────────────────────────
di _n "  Verificación numérica con reghdfe:"
reghdfe Y D, absorb(id t)
di "  β̂_TWFE = " %6.4f _b[D] "  (debe ser ≈ 3) ✓"

* ── También directamente de D̃ Ỹ ─────────────────────────────────────────
gen numerator = D_tilde * Y_tilde
summ numerator
scalar num = r(sum)
summ D_tilde
* lo hacemos manual
scalar den2 = 0
quietly {
    forvalues obs = 1/8 {
        local d_til = D_tilde[`obs']
        scalar den2 = den2 + `d_til'^2
    }
}
di "  TWFE a mano (ΣD̃Ỹ / ΣD̃²) = " %6.4f num / den2

* ── ¿Por qué el peso negativo en (1,4)? Intuición ─────────────────────────
di _n "=" * 65
di "  ¿POR QUÉ ES NEGATIVO EL PESO EN (Unidad 1, t=4)?"
di "=" * 65
di ""
di "  En t=4, AMBAS unidades están tratadas."
di "  El EF de tiempo (λ_t=4) captura el promedio de Y en t=4."
di "  La Unidad 1 lleva 3 períodos tratada → su Y en t=4 ya refleja"
di "  3 períodos de τ=2."
di "  La Unidad 2 entra recién en t=4 → su Y sube bruscamente."
di ""
di "  TWFE 've' que U1 en t=4 no aporta nueva variación en D:"
di "  D̃_{1,4} = 1 - 3/4 - 1 + 1/2 = -1/4 < 0"
di ""
di "  Interpretación: TWFE usa a U1 como 'control' para U2 en t=4,"
di "  comparando (Y_{2,4}-Y_{2,3}) vs (Y_{1,4}-Y_{1,3})."
di "  Si el efecto de U1 es constante, Y_{1,4}-Y_{1,3} = 0 → no contamina."
di "  Si el efecto crece, Y_{1,4}-Y_{1,3} > 0 → contamina → sesgo."

* ── Caso extremo: efectos que crecen (para mostrar severidad del problema) ─
di _n "  CASO EXTREMO: ATT(1,4)=100, resto=2"
di "  β̂_TWFE = (+1/2)×2 + (+1/2)×2 + (-1/2)×100 + (+1/2)×4"
di "           = 1 + 1 - 50 + 2 = -46"
di "  ATT_overall = (2+2+100+4)/4 = 27"
di "  → TWFE = -46 !! (signo incorrecto, ATT = +27)"

* ── Caso homogéneo: no hay sesgo aunque haya pesos negativos ──────────────
di _n "  CON EFECTOS HOMOGÉNEOS (τ=3 para todos):"
di "  β̂_TWFE = (+1/2)×3 + (+1/2)×3 + (-1/2)×3 + (+1/2)×3"
di "           = 1.5 + 1.5 - 1.5 + 1.5 = 3 = ATT_overall ✓"
di "  → Los pesos negativos por sí solos NO causan sesgo."
di "  → El sesgo requiere pesos negativos + heterogeneidad de efectos."

drop D_bar_i D_bar_t D_bar Y_bar_i Y_bar_t Y_bar D_tilde Y_tilde numerator


********************************************************************************
* SECCIÓN 7 (PAUSA): EJERCICIO — ADOPCIÓN ESCALONADA, 3 GRUPOS
*
* INSTRUCCIONES PARA ESTUDIANTES (ver pausa_twfe_instrucciones.md en TEAMS):
*   A. Calcular el ATT verdadero a mano
*   B. Identificar las 9 comparaciones 2×2 y calcular β̂ para cada una
*   C. Predecir β̂_TWFE y correr bacondecomp
*
* El generador de datos está dado. El resto lo hacen ustedes.
********************************************************************************

clear
set obs 48
gen id = ceil(_n/6)          // id = 1 a 8
gen t  = mod(_n-1, 6) + 1   // t  = 1 a 6
sort id t
xtset id t

* id=1,2: NUNCA TRATADOS
* id=3,4: EARLY  — tratan en t=2, τ = 2
* id=5,6: MEDIUM — tratan en t=4, τ = 3
* id=7,8: LATE   — tratan en t=6, τ = 5
gen D = 0
replace D = 1 if inlist(id,3,4) & t >= 2
replace D = 1 if inlist(id,5,6) & t >= 4
replace D = 1 if inlist(id,7,8) & t >= 6

gen Y = 0
replace Y = 2 if inlist(id,3,4) & D == 1   // τ=2 para EARLY
replace Y = 3 if inlist(id,5,6) & D == 1   // τ=3 para MEDIUM
replace Y = 5 if inlist(id,7,8) & D == 1   // τ=5 para LATE

list id t D Y, sep(6)

* ── PARTE A: ATT verdadero ─────────────────────────────────────────────────
* Completa tú:
* di "ATT verdadero = " _______________   // → esperado: 2.67

* ── PARTE B: Las 9 comparaciones 2×2 ──────────────────────────────────────
* --- Limpias: control = nunca tratado ----------------------------------
* DiD 1: Early vs Nunca
* xtreg Y D i.t if _______________, fe robust

* DiD 2: Medium vs Nunca
* xtreg Y D i.t if _______________, fe robust

* DiD 3: Late vs Nunca
* xtreg Y D i.t if _______________, fe robust

* --- Limpias: control = aún no tratado --------------------------------
* DiD 4: Early vs Medium  (ventana t=1–3)
* xtreg Y D i.t if _______________ & t <= ___, fe robust

* DiD 5: Early vs Late    (ventana t=1–5)
* xtreg Y D i.t if _______________ & t <= ___, fe robust

* DiD 6: Medium vs Late   (ventana t=1–5)
* xtreg Y D i.t if _______________ & t <= ___, fe robust

* --- Sucias: control = ya tratado ⚠️ -----------------------------------
* DiD 7: Medium vs Early  (Early ya tratado desde t=2)
* gen D_med = D * inlist(id,5,6)
* xtreg Y D_med i.t if _______________ & t >= ___, fe robust

* DiD 8: Late vs Early    (Early ya tratado desde t=2)
* gen D_lat = D * inlist(id,7,8)
* xtreg Y D_lat i.t if _______________ & t >= ___, fe robust

* DiD 9: Late vs Medium   (Medium ya tratado desde t=4)
* xtreg Y D_lat i.t if _______________ & t >= ___, fe robust

* ── PARTE C: TWFE y Bacon ──────────────────────────────────────────────────
* reghdfe Y D, absorb(id t) vce(robust)
* bacondecomp Y D, ddetail


********************************************************************************
* SECCIÓN 8: ESTIMADORES MODERNOS — SIMULACIÓN GRANDE
*
* DGP: 30 unidades × 60 períodos, timing y efectos aleatorios por cohorte,
*       efectos que CRECEN con el tiempo (heterogeneidad dinámica)
*
* OBJETIVO: comparar TWFE vs csdid, did_imputation, eventstudyinteract,
*            did2s, did_multiplegt_dyn, stackedev y jwdid
*            en un event-study gráfico comparativo.
*
* NOTA: Algunos paquetes deben estar instalados (ver Sección 0).
********************************************************************************

clear
local units = 30
local start = 1
local end   = 60
local time  = `end' - `start' + 1
local obsv  = `units' * `time'
set obs `obsv'

egen id = seq(), b(`time')
egen t  = seq(), f(`start') t(`end')
sort id t
xtset id t
label variable id "Panel variable"
label variable t  "Time variable"

set seed 20211222

gen Y           = 0
gen D           = 0
gen cohort      = .
gen effect      = .
gen first_treat = .
gen rel_time    = .

* Asignar cohorte, efecto y timing a cada unidad
levelsof id, local(lvls)
foreach x of local lvls {
    local chrt = runiformint(0, 5)
    replace cohort = `chrt' if id == `x'
}

levelsof cohort, local(lvls)
foreach x of local lvls {
    local eff    = runiformint(2, 10)
    replace effect = `eff' if cohort == `x'
    * timing aleatorio: algunos quedarán como never-treated (timing > end)
    local timing = runiformint(`start', `end' + 20)
    replace first_treat = `timing' if cohort == `x'
    replace first_treat = . if first_treat > `end'
    replace D = 1 if cohort == `x' & t >= `timing'
}

* Efecto crece con el tiempo desde el tratamiento (heterogeneidad dinámica)
replace rel_time = t - first_treat
replace Y = id + t + cond(D==1, effect * rel_time, 0) + rnormal()

* ── Generar leads y lags para event study ─────────────────────────────────
* Convenio: F_k = 1 si rel_time == -k (k períodos ANTES del tratamiento)
*            L_k = 1 si rel_time ==  k (k períodos DESPUÉS del tratamiento)
* Se omite F_1 (período inmediatamente antes) como categoría de referencia

summ rel_time
local relmin = abs(r(min))
local relmax = abs(r(max))

cap drop F_* L_*
forval x = 2/`relmin' {
    gen F_`x' = (rel_time == -`x')
}
forval x = 0/`relmax' {
    gen L_`x' = (rel_time == `x')
}

* ── Variables auxiliares para estimadores modernos ─────────────────────────
gen never_treat = (first_treat == .)           // = 1 si nunca tratado
summ first_treat
gen last_cohort = (first_treat == r(max))      // = 1 si adoptó más tarde

gen gvar = first_treat
recode gvar (. = 0)    // csdid: gvar=0 → nunca tratado

* ── TWFE con event study ────────────────────────────────────────────────────
* CUIDADO: con staggered adoption, los coeficientes de este event study
* son promedios ponderados que pueden tener pesos negativos (ver Sun-Abraham 2021)
di _n "=== TWFE EVENT STUDY (puede estar sesgado) ==="
reghdfe Y L_* F_*, absorb(id t) cluster(id)
estimates store twfe
* Observar: los pre-trends F_k pueden NO ser cero incluso bajo tendencias paralelas

* ── csdid — Callaway & Sant'Anna (2021) ────────────────────────────────────
* Estima ATT(g,t) cohorte por cohorte, usando "not-yet-treated" como control
di _n "=== CSDID — Callaway & Sant'Anna (2021) ==="
csdid Y, ivar(id) time(t) gvar(gvar) notyet
* notyet: usa unidades "no-aún-tratadas" como control (más eficiente que never-treated)
* Alternativa: omitir 'notyet' para usar solo never-treated

* Postestimación: diferentes formas de agregar ATT(g,t)
estat simple           // ATT overall (promedio sobre todas las celdas tratadas)
estat calendar         // ATT por período calendario
estat group            // ATT por cohorte (grupo de adopción)
estat event, window(-10 10) estore(csdd)  // event study: ATT por tiempo relativo
* window(-10 10): muestra 10 períodos antes y después del tratamiento

* Gráfico del event study de csdid
csdid_plot, title("Event study: Callaway & Sant'Anna") ///
    name(g7_csdid_es, replace) ///
    xtitle("Períodos desde el tratamiento") ytitle("ATT estimado")

* ── did_imputation — Borusyak, Jaravel & Spiess (2022) ────────────────────
di _n "=== DID_IMPUTATION — Borusyak, Jaravel & Spiess (2022) ==="
did_imputation Y id t first_treat, horizons(0/10) pretrend(10) minn(0)
estimates store didimp
* Idea: estima los counterfactuales para no-tratados usando EF,
*        luego "imputa" el counterfactual para tratados y calcula ATT.

* ── did_multiplegt_dyn — de Chaisemartin & D'Haultfœuille (2024) ──────────
di _n "=== DID_MULTIPLEGT_DYN — de Chaisemartin & D'Haultfœuille (2024) ==="
did_multiplegt_dyn Y id t D, effects(10) placebo(10) cluster(id)
matrix didmgt_b = e(estimates)
matrix didmgt_v = e(variances)

* ── eventstudyinteract — Sun & Abraham (2021) ─────────────────────────────
di _n "=== EVENTSTUDYINTERACT — Sun & Abraham (2021) ==="
eventstudyinteract Y L_* F_*, vce(cluster id) absorb(id t) ///
    cohort(first_treat) control_cohort(never_treat)
* Sun-Abraham descompone β̂_TWFE en pesos por cohorte y los repondera correctamente
matrix evtstint_b = e(b_iw)
matrix evtstint_v = e(V_iw)

* ── did2s — Gardner (2022) ────────────────────────────────────────────────
di _n "=== DID2S — Gardner (2022) ==="
did2s Y, first_stage(id t) second_stage(F_* L_*) treatment(D) cluster(id)
* Primera etapa: estima EF usando solo untreated (nunca + no-aún)
* Segunda etapa: regresa residuales de 1a etapa sobre leads/lags
matrix did2s_b = e(b)
matrix did2s_v = e(V)

* ── stackedev — Cengiz et al. (2019) ─────────────────────────────────────
di _n "=== STACKEDEV — Cengiz et al. (2019) ==="
stackedev Y F_* L_*, cohort(first_treat) time(t) ///
    never_treat(never_treat) unit_fe(id) clust_unit(id)
* "Apilamiento": construye una muestra separada por cohorte, la apila,
*  y estima un event study limpio (sin comparaciones cross-cohorte)
matrix stackedev_b = e(b)
matrix stackedev_v = e(V)

* ── jwdid — Wooldridge (2021) ─────────────────────────────────────────────
di _n "=== JWDID — Wooldridge (2021) ==="
jwdid Y, ivar(id) tvar(t) gvar(gvar)
* Enfoque: OLS con todas las interacciones grupo×tiempo para los tratados.
* Equivalente a CS bajo supuestos de regresión lineal.
estat simple   // ATT overall
estat event    // event study

* ── Event study plot comparativo ─────────────────────────────────────────
* Este gráfico compara los 7 estimadores en una sola figura.
* TWFE (con staggered) → tipicamente más plano o sesgado pre-trend
* Los modernos → pre-trends cercanos a 0 y post-trends consistentes

colorpalette tableau, nograph

event_plot    twfe             csdd               didimp           ///
              didmgt_b#didmgt_v   evtstint_b#evtstint_v            ///
              stackedev_b#stackedev_v  did2s_b#did2s_v  ,          ///
    stub_lag( L_#   Tp#   tau#   Effect_#   L_#    L_#   L_#)      ///
    stub_lead(F_#   Tm#   pre#   Placebo_#  F_#    F_#   F_#)      ///
    together perturb(-0.30(0.10)0.30) trimlead(20) trimlag(20)     ///
    noautolegend plottype(scatter) ciplottype(rspike)               ///
        lag_opt1(msymbol(+)   msize(1.2) mlwidth(0.3) color(black))         ///
        lag_ci_opt1(color(black) lw(0.15))                                   ///
        lag_opt2(msymbol(lgx) msize(1.2) mlwidth(0.3) color("`r(p1)'"))     ///
        lag_ci_opt2(color("`r(p1)'") lw(0.15))                              ///
        lag_opt3(msymbol(Dh)  msize(1.2) mlwidth(0.3) color("`r(p2)'"))     ///
        lag_ci_opt3(color("`r(p2)'") lw(0.15))                              ///
        lag_opt4(msymbol(Th)  msize(1.2) mlwidth(0.3) color("`r(p3)'"))     ///
        lag_ci_opt4(color("`r(p3)'") lw(0.15))                              ///
        lag_opt5(msymbol(Sh)  msize(1.2) mlwidth(0.3) color("`r(p4)'"))     ///
        lag_ci_opt5(color("`r(p4)'") lw(0.15))                              ///
        lag_opt6(msymbol(Oh)  msize(1.2) mlwidth(0.3) color("`r(p5)'"))     ///
        lag_ci_opt6(color("`r(p5)'") lw(0.15))                              ///
        lag_opt7(msymbol(V)   msize(1.2) mlwidth(0.3) color("`r(p6)'"))     ///
        lag_ci_opt7(color("`r(p6)'") lw(0.15))                              ///
    graph_opt(                                                               ///
        title("Event study: TWFE vs estimadores modernos")                  ///
        xtitle("Períodos desde el tratamiento") ytitle("Efecto promedio")   ///
        xlabel(-20(2)20)                                                     ///
        legend(order(1 "TWFE" 3 "csdid (CS 2021)"                          ///
               5 "did_imputation (BJS 2022)"                                ///
               7 "did_multiplegt_dyn (dCdH 2024)"                          ///
               9 "eventstudyinteract (SA 2021)"                             ///
               11 "stackedev (Cengiz 2019)"                                 ///
               13 "did2s (Gardner 2022)")                                   ///
               pos(6) rows(4) region(style(none)))                           ///
        xline(-0.5, lc(gs8) lp(dash)) yline(0, lc(gs8) lp(dash))          ///
    )

di _n "=" * 70
di "  TABLA DE COMANDOS DE STATA PARA TWFE Y DiD MODERNO"
di "=" * 70
di ""
di "  A. DIAGNÓSTICO Y EQUIVALENCIA BÁSICA"
di "  ─────────────────────────────────────────────────────────────"
di "  reghdfe Y D, absorb(id t) vce(cluster id)      TWFE básico"
di "  xtreg Y D i.t, fe vce(cluster id)              TWFE (más lento)"
di "  xtdidregress (Y)(D), group(id) time(t)          Gráfico y test TP"
di "  estat ptrends                                   Test formal TP"
di "  estat trendplots                                Gráfico tendencias pre"
di "  bacondecomp Y D, ddetail                       Descomposición Bacon"
di ""
di "  B. EVENT STUDY TWFE (con staggered: puede estar sesgado)"
di "  ─────────────────────────────────────────────────────────────"
di "  reghdfe Y L_* F_*, absorb(id t) cluster(id)    Event study TWFE"
di "  [Generar F_k y L_k manualmente con rel_time]"
di ""
di "  C. ESTIMADORES ROBUSTOS A HETEROGENEIDAD"
di "  ─────────────────────────────────────────────────────────────"
di "  csdid Y, ivar(id) time(t) gvar(gvar) notyet    Callaway-Sant'Anna"
di "  estat simple / estat calendar / estat group     Agregaciones csdid"
di "  estat event, window(-k k) estore(csdd)          Event study csdid"
di "  csdid_plot                                      Gráfico event study CS"
di ""
di "  eventstudyinteract Y L* F*, absorb(id t) ...   Sun-Abraham"
di "    cohort(first_treat) control_cohort(never_treat)"
di ""
di "  did_imputation Y id t first_treat, ...         Borusyak-Jaravel-Spiess"
di "    horizons(0/10) pretrend(10)"
di ""
di "  did_multiplegt_dyn Y id t D, effects(10) ...   de Chaisemartin-D'H"
di "    placebo(10) cluster(id)"
di ""
di "  did2s Y, first_stage(id t) ...                 Gardner (2022)"
di "    second_stage(F_* L_*) treatment(D) cluster(id)"
di ""
di "  stackedev Y F_* L_*, cohort(first_treat) ...   Cengiz et al."
di "    time(t) never_treat(never_treat)"
di "    unit_fe(id) clust_unit(id)"
di ""
di "  jwdid Y, ivar(id) tvar(t) gvar(gvar)          Wooldridge (2021)"
di ""
di "  D. GRÁFICO COMPARATIVO (una vez que tienes los estimates)"
di "  ─────────────────────────────────────────────────────────────"
di "  event_plot twfe csdd didimp ..., stub_lag(L_# Tp# tau# ...)"
di "             stub_lead(F_# Tm# pre# ...) together"
di "=" * 70
