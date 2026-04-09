********************************************************************************
* Datos de Panel, DiD, TWFE y Estimadores Modernos
* Econometría Avanzada — Ana María Díaz Escobar — Javeriana 2026-I
*
* VERSIÓN 3.0 (2026-04-09) — reorganización pedagógica Sección 5:
*   Secciones 6 y 7 eliminadas; Sección 5 reescrita como bloque central
*   de la clase sobre Bacon → Callaway-Sant'Anna → event study
*
* SECCIONES:
*  0.  Instalar paquetes (ejecutar UNA sola vez; luego dejar comentado)
*  1.  Datos de panel: xtset, within, FE / FD / RE / Hausman
*  2.  2×2 exacto: DiD = FD = TWFE (equivalencia algebraica)
*  3.  Panel largo, adopción simultánea: DiD = TWFE ≠ FD; tendencias paralelas
*  4.  Adopción simultánea con efectos heterogéneos
*  5.  Adopción escalonada: Bacon → Callaway-Sant'Anna → event study (CLASE)
*        5.A  DGP mínimo (3 unidades) + TWFE: el problema
*        5.B  Descomposición de Bacon: diagnóstico
*        5.C  DGP expandido (30 unidades): base para csdid
*        5.D  Callaway & Sant'Anna: ATT(g,t), agregaciones, csdid_plot
*        5.E  Event study: leads, lags, confusión de nombres, TWFE vs. CS
*  8.  [REVISIÓN AUTÓNOMA] Comparativo de estimadores modernos (sim. grande)
*
* CAMBIOS v3.0 vs v2.1:
*  - Sec.5 completamente reescrita (era solo Bacon + 3 unidades)
*  - Secciones 6 (pesos negativos) y 7 (pausa) eliminadas
*  - Sec.8 conservada como revisión autónoma
*  - DGP expandido (30 unidades) con efectos dinámicos para csdid
*  - Explicación detallada de leads/lags y la confusión de nombres
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
*   Y_it = alpha_i + beta*D_it + eps_it
*   alpha_i correlacionado con D_it → FE necesario
* NOTA: sin tendencia de tiempo en el DGP para que la transformación within
*       (one-way: Y - Ȳ_i) sea exactamente igual a xtreg Y D, fe
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
gen alpha_i = 2 * id / `N' + rnormal(0, 0.5)   // EF individual, correlacionado con D
gen D = 0.5 * alpha_i + rnormal(0, 1)            // D correlacionado con alpha_i
label variable D "Variable de interés"
scalar beta_true = 3
gen Y = alpha_i + beta_true * D + rnormal(0, 1)  // sin tendencia de tiempo
label variable Y "Variable resultado"

* ── Descripción del panel ──────────────────────────────────────────────────
di _n "=== DESCRIPCIÓN DEL PANEL ==="
xtdes
xtsum Y D
* LECTURA: between = diferencias entre medias individuales
*          within  = variación de cada individuo alrededor de su propia media
*          FE/FD explotan solo la variación WITHIN

* ── Spaghetti plot ─────────────────────────────────────────────────────────
xtline Y if id <= 10, overlay ///
    title("Evolución de Y: primeros 10 individuos") ///
    xtitle("Periodo") ytitle("Y") legend(off) ///
    name(g0_xtline, replace)

* ── OLS pooled (incorrecto porque Cov(D, alpha_i) ≠ 0) ───────────────────
di _n "=== OLS Pooled (INCORRECTO) ==="
reg Y D, vce(cluster id)
di "Beta verdadero = `= scalar(beta_true)'. OLS sesgado (Cov(D,alpha_i) ≠ 0)."

* ── Efectos fijos: xtreg fe ────────────────────────────────────────────────
di _n "=== EFECTOS FIJOS ==="
xtreg Y D, fe vce(cluster id)
di "  → One-way FE (sin tiempo). Beta verdadero = `= scalar(beta_true)' ✓"
xtreg Y D i.t, fe vce(cluster id)
di "  → Two-way FE (TWFE con tiempo). Beta verdadero = `= scalar(beta_true)' ✓"

* ── FE a mano: transformación within ──────────────────────────────────────
di _n "=== FE A MANO (transformación within) ==="
bysort id: egen mean_Y = mean(Y)
bysort id: egen mean_D = mean(D)
gen Y_within = Y - mean_Y   // desviación de la media individual de Y
gen D_within = D - mean_D   // desviación de la media individual de D
reg Y_within D_within, nocons
di "  → Idéntico a 'xtreg Y D, fe' (one-way). Beta = `= scalar(beta_true)' ✓"
* NOTA: one-way (Y-Ȳ_i) coincide con xtreg sin i.t.
*       Con tendencia de tiempo en el DGP habría que hacer two-way demeaning.
drop mean_Y mean_D Y_within D_within

* ── Efectos aleatorios ─────────────────────────────────────────────────────
di _n "=== EFECTOS ALEATORIOS (incorrecto aquí) ==="
xtreg Y D i.t, re vce(cluster id)

* ── Test de Hausman ────────────────────────────────────────────────────────
di _n "=== HAUSMAN: ¿FE o RE? ==="
xtreg Y D i.t, fe
estimates store fe_hausman
xtreg Y D i.t, re
hausman fe_hausman ., sigmamore
* H0: RE consistente (Cov(D,alpha_i)=0). Si p<0.05 → usar FE.

* ── Primeras diferencias ───────────────────────────────────────────────────
di _n "=== PRIMERAS DIFERENCIAS ==="
reg D.Y D.D ibn.t, noconstant vce(cluster id)
* NOTA: D. es el operador de primera diferencia de Stata. D.D = ΔD (cambio en D).
di "FD elimina alpha_i por diferencia (usa T-1 periodos por individuo)"

di _n "=" * 65
di "RESUMEN ESTIMADORES"
di "=" * 65
di "  Cov(D, alpha_i)=0  →  RE eficiente    (Hausman no rechaza)"
di "  Cov(D, alpha_i)≠0  →  FE consistente  (Hausman rechaza)"
di "  D endógeno en eps  →  TODOS sesgan     (necesitas IV)"
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
* DGP: 40 unidades (20 control + 20 tratadas), T=11 (1980–1990)
*      Adopción en t=1985. Efecto τ=5.
*   A: tendencias paralelas ✓     B: tendencias NO paralelas ✗
* NOTA: se necesitan al menos ~20 unidades por grupo para que
*       xtdidregress / estat ptrends tenga suficientes grados de libertad.
********************************************************************************

* ─────────────────────────────────────────────────────────────────────────────
* CASO 3A: Tendencias paralelas — DiD = TWFE ✓
* ─────────────────────────────────────────────────────────────────────────────

clear
set seed 5678
local N_grp  = 20       // 20 control + 20 tratados
local inicio = 1980
local fin    = 1990
local tiempo = `fin' - `inicio' + 1
set obs `= 2 * `N_grp' * `tiempo''

gen id = ceil(_n / `tiempo')
gen t  = `inicio' + mod(_n - 1, `tiempo')
sort id t
xtset id t

gen trat = (id > `N_grp')
gen D    = trat * (t >= 1985)
label variable D "Tratamiento (adopción en 1985)"

* DGP: EF individuales aleatorios, tendencia común, τ=5
gen alpha_raw = rnormal(0, 2)
bysort id (t): replace alpha_raw = alpha_raw[1]   // constante dentro del individuo
gen Y = alpha_raw + 3 * (t - `inicio') + 5 * D + rnormal(0, 0.5)
label variable Y "Y (tendencias paralelas ✓)"

twoway ///
    (connected Y t if trat==0, lcolor(blue%30) lwidth(thin) msymbol(none)) ///
    (connected Y t if trat==1, lcolor(red%30)  lwidth(thin) msymbol(none)) ///
    , xline(1984.5, lpattern(dash) lcolor(gray)) ///
      xlabel(`inicio'(1)`fin') ///
      legend(order(1 "Control" 2 "Tratado") pos(6) row(1)) ///
      title("Caso 3A: tendencias paralelas ✓") ///
      xtitle("Año") ytitle("Y") name(g3a_trends, replace)

* DiD manual
quietly sum Y if trat==1 & t >= 1985
scalar y_t_post = r(mean)
quietly sum Y if trat==1 & t < 1985
scalar y_t_pre  = r(mean)
quietly sum Y if trat==0 & t >= 1985
scalar y_c_post = r(mean)
quietly sum Y if trat==0 & t < 1985
scalar y_c_pre  = r(mean)
scalar DiD3a = (y_t_post - y_t_pre) - (y_c_post - y_c_pre)

reghdfe Y D, absorb(id t) vce(robust)
scalar TWFE3a = _b[D]

* Test formal de tendencias paralelas (Stata 17+)
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
local N_grp  = 20
local inicio = 1980
local fin    = 1990
local tiempo = `fin' - `inicio' + 1
set obs `= 2 * `N_grp' * `tiempo''

gen id = ceil(_n / `tiempo')
gen t  = `inicio' + mod(_n - 1, `tiempo')
sort id t
xtset id t

gen trat = (id > `N_grp')
gen D    = trat * (t >= 1985)
label variable D "Tratamiento (adopción en 1985)"

gen alpha_raw = rnormal(0, 2)
bysort id (t): replace alpha_raw = alpha_raw[1]

* DIFERENCIA: tratados tienen tendencia pre más pronunciada (+2/año)
gen Y = alpha_raw + (3 + 2*trat) * (t - `inicio') + 5 * D + rnormal(0, 0.5)
* Control: pendiente = 3.  Tratado: pendiente = 5.  → NO paralelas.
label variable Y "Y (tendencias NO paralelas ✗)"

twoway ///
    (connected Y t if trat==0, lcolor(blue%30) lwidth(thin) msymbol(none)) ///
    (connected Y t if trat==1, lcolor(red%30)  lwidth(thin) msymbol(none)) ///
    , xline(1984.5, lpattern(dash) lcolor(gray)) ///
      xlabel(`inicio'(1)`fin') ///
      legend(order(1 "Control" 2 "Tratado") pos(6) row(1)) ///
      title("Caso 3B: tendencias NO paralelas ✗") ///
      note("Tratados crecen más rápido antes del tratamiento (+2/año)") ///
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

di _n "  Mecánica del sesgo (diferencia de pendientes pre-tratamiento):"
di "  Control: pendiente = 3/año"
di "  Tratado: pendiente = 5/año antes de 1985"
di "  TWFE estima τ + componente de tendencia diferencial ≠ τ = 5"


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
* SECCIÓN 5: ADOPCIÓN ESCALONADA — DE BACON A CALLAWAY & SANT'ANNA
*
* PROBLEMA CENTRAL:
*   Con adopción escalonada, unidades distintas reciben el tratamiento en
*   distintos momentos. TWFE combina automáticamente dos tipos de comparaciones:
*   (a) LIMPIAS:  tratado vs. nunca-tratado (o aún-no-tratado)
*   (b) SUCIAS:   tratado-tarde vs. tratado-temprano (que ya tiene τ en su Y)
*
*   Las comparaciones sucias son problemáticas cuando los efectos varían en
*   el tiempo: el "control" ya-tratado lleva su propio τ incorporado en Y,
*   lo que contamina la estimación del grupo que acaba de adoptar.
*
* ESTRUCTURA:
*   5.A  DGP mínimo (3 unidades) + TWFE: mostrar el problema
*   5.B  Descomposición de Bacon: diagnosticar qué mezcla TWFE
*   5.C  DGP expandido (30 unidades): necesario para csdid y event study
*   5.D  Callaway & Sant'Anna (2021): estimar ATT(g,t) limpiamente
*   5.E  Event study — leads y lags: dinámica del efecto
*
* DGP:
*   Cohorte g=1985 (adopción temprana): τ = 2 × (rel_time + 1)  [creciente]
*   Cohorte g=1988 (adopción tardía):  τ = 3 × (rel_time + 1)  [creciente]
*   Grupo de control: nunca tratado
*   Efectos CRECIENTES → garantizan que TWFE esté sesgado y que el event study
*   de TWFE muestre "pre-trends falsos" (contaminación de staggered).
*
* REFERENCIAS:
*   Goodman-Bacon (2021) J. of Econometrics 225(2):254-277
*   Callaway & Sant'Anna (2021) J. of Econometrics 225(2):200-230
********************************************************************************


* ─────────────────────────────────────────────────────────────────────────────
* 5.A — DGP MÍNIMO (3 UNIDADES) + TWFE: EL PROBLEMA
*
* Usamos datos determinísticos (sin ruido, sin EF individuales) para ver
* exactamente qué pesos asigna TWFE a cada comparación 2×2.
* DGP: Y = τ_i × D_it  →  Y=0 antes del tratamiento; Y=τ_i después.
* ─────────────────────────────────────────────────────────────────────────────

clear
local unidades = 3
local inicio   = 1980
local fin      = 1989
local tiempo   = `fin' - `inicio' + 1
set obs `= `unidades' * `tiempo''

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
replace D = 1 if id == 2 & t >= 1985   // cohorte temprana g=1985, τ=2
replace D = 1 if id == 3 & t >= 1988   // cohorte tardía   g=1988, τ=4
label variable D "Tratamiento escalonado"

gen Y = 0
replace Y = 2 if id == 2 & D == 1      // τ=2 CONSTANTE para id=2
replace Y = 4 if id == 3 & D == 1      // τ=4 CONSTANTE para id=3
label variable Y "Variable dependiente (DGP determinístico)"

* ── Visualizar las trayectorias ────────────────────────────────────────────
twoway ///
    (connected Y t if id==1, msymbol(circle)   lcolor(blue)   lwidth(medium)) ///
    (connected Y t if id==2, msymbol(triangle)  lcolor(red)    lwidth(medium)) ///
    (connected Y t if id==3, msymbol(square)    lcolor(orange) lwidth(medium)) ///
    , xline(1984.5, lpattern(dash) lcolor(gray)) ///
      xline(1987.5, lpattern(dash) lcolor(gray)) ///
      xlabel(`inicio'(1)`fin') ///
      legend(order(1 "id=1 (nunca tratado)" 2 "id=2 (g=1985, τ=2)" ///
             3 "id=3 (g=1988, τ=4)") pos(6) row(1)) ///
      title("5.A: Adopción escalonada — trayectorias") ///
      xtitle("Año") ytitle("Y") name(g5a_stag, replace)

* ── ATT verdadero ──────────────────────────────────────────────────────────
* Celdas tratadas: id=2 en 1985-1989 (5 obs) + id=3 en 1988-1989 (2 obs) = 7
* ATT_overall = promedio ponderado de τ sobre las 7 celdas
di _n "=" * 65
di "  5.A — TWFE CON ADOPCIÓN ESCALONADA"
di "=" * 65
di "  Celdas tratadas: 5 × τ=2 (id=2) + 2 × τ=4 (id=3) = 7 en total"
di "  ATT_overall verdadero = (5×2 + 2×4)/7 = " %6.4f (5*2 + 2*4)/7
di ""

reghdfe Y D, absorb(id t) vce(robust)
di ""
di "  β̂_TWFE = " %6.4f _b[D]
di "  ATT_overall = " %6.4f (5*2 + 2*4)/7
di "  Sesgo = " %6.4f _b[D] - (5*2 + 2*4)/7
di ""
di "  ¿Por qué TWFE ≠ ATT_overall?"
di "  → TWFE usa id=2 como 'control' de id=3 en el período 1988-1989."
di "    Pero id=2 ya está tratada desde 1985 y tiene τ=2 en su Y."
di "    Si τ(id=2) ≠ τ(id=3), esa comparación es 'sucia'."
di "    (ver Sección 5.B para el diagnóstico detallado)"


* ─────────────────────────────────────────────────────────────────────────────
* 5.B — DESCOMPOSICIÓN DE BACON: DIAGNÓSTICO DEL PROBLEMA
*
* Goodman-Bacon (2021) demuestra:
*   β̂_TWFE = Σ_{k,l} w_{kl} × β̂_{2×2,kl}
*
* donde la suma recorre todos los pares de grupos (k,l), β̂_{2×2,kl} es el
* estimador DiD en la submuestra {k,l}, y los pesos w_{kl} suman a 1.
*
* CON 3 UNIDADES (id=1,2,3) Y 2 COHORTES (g=1985, g=1988):
*
* TIPO 1: Cohorte vs. Nunca Tratado  →  LIMPIAS ✓
*   id=2 vs id=1 (todo el período): id=1 nunca se trata → control puro ✓
*   id=3 vs id=1 (todo el período): ídem ✓
*
* TIPO 2: Early vs. Late en ventana PRE-late  →  LIMPIA ✓
*   id=2 (early) como tratada, id=3 (late) como control, VENTANA: 1980–1987
*   id=3 aún no se ha tratado → sirve como control limpio en esa ventana ✓
*
* TIPO 3: Late vs. Early en ventana POST-early  →  SUCIA ✗
*   id=3 (late) como nueva tratada, id=2 (early) como "control", VENTANA: 1985–1989
*   PROBLEMA: id=2 lleva tratada desde 1985 y tiene τ=2 incorporado en Y.
*   Si τ(id=2) varía con el tiempo, id=2 no sirve como control para id=3.
*   Esta comparación dice: "el efecto de tratar a id=3 en 1988 es la
*   diferencia id=3 menos id=2 después de 1985, descontando la tendencia."
*   Pero esa diferencia ya incluye el τ de id=2 → CONTAMINADA ✗
* ─────────────────────────────────────────────────────────────────────────────

di _n "=" * 65
di "  5.B — DESCOMPOSICIÓN DE BACON"
di "=" * 65
di ""
di "  β̂_TWFE = suma ponderada de los β̂ 2×2 en el panel"
di ""
di "  ┌───────────────────────────────────────────────────────────┐"
di "  │ Comparación        │ Control usado      │ ¿Limpia? │"
di "  ├───────────────────────────────────────────────────────────┤"
di "  │ id=2 vs id=1       │ nunca tratado      │    ✓     │"
di "  │ id=3 vs id=1       │ nunca tratado      │    ✓     │"
di "  │ id=2 vs id=3 *     │ aún-no-tratado     │    ✓     │"
di "  │ id=3 vs id=2 †     │ YA TRATADO         │    ✗     │"
di "  └───────────────────────────────────────────────────────────┘"
di "  * ventana 1980-1987  (antes que id=3 se trate)"
di "  † ventana 1985-1989  (id=2 lleva tratada desde 1985 = SUCIA)"
di ""

* ── Verificar cada 2×2 manualmente ────────────────────────────────────────
di "  [Limpia 1] id=2 vs id=1 — todo el período:"
xtreg Y D i.t if inlist(id, 1, 2), fe robust
di "    β̂ = " %6.4f _b[D] "   (debe ser ≈ 2 = τ de id=2)"

di ""
di "  [Limpia 2] id=3 vs id=1 — todo el período:"
xtreg Y D i.t if inlist(id, 1, 3), fe robust
di "    β̂ = " %6.4f _b[D] "   (debe ser ≈ 4 = τ de id=3)"

di ""
di "  [Limpia 3] id=2 (early) vs id=3 (late, aún no tratado) — ventana 1980-1987:"
preserve
    keep if inlist(id, 2, 3) & t <= 1987
    xtreg Y D i.t, fe robust
    di "    β̂ = " %6.4f _b[D] "   (debe ser ≈ 2 = τ de id=2)"
restore

di ""
di "  [Sucia] id=3 (late) vs id=2 (early, YA TRATADO) — ventana 1985-1989:"
preserve
    keep if inlist(id, 2, 3) & t >= 1985
    gen D_late = (id == 3 & t >= 1988)
    xtreg Y D_late i.t, fe robust
    di "    β̂ = " %6.4f _b[D_late]
    di "    Con τ CONSTANTE → da ≈ τ(id=3)=4 ✓  [el problema no se ve aquí]"
    di "    Con τ CRECIENTE → β̂ distorsionado ✗  [ver DGP expandido 5.C-5.D]"
restore

di ""
di "  BACONDECOMP — descomposición automática:"
bacondecomp Y D, ddetail
* LEER LA TABLA:
*   never_v_timing:     cohortes vs id=1 (nunca tratado) — LIMPIAS ✓
*   timing_v_timing:    Early vs Late y Late vs Early
*                       La fila "Late vs Early" es la SUCIA
*
* NOTA: los pesos de Bacon son SIEMPRE POSITIVOS.
* El problema del TWFE NO son pesos negativos en la descomposición de Bacon.
* Los pesos negativos aparecen en la regresión D demeanada (D̃_it), no en Bacon.
* El problema aquí es que la comparación "Late vs Early" usa un ya-tratado
* como control. Con τ constante, no importa; con τ variable, sí importa.

di ""
di "  CONCLUSIÓN DE BACON:"
di "  β̂_TWFE = promedio ponderado de 4 comparaciones 2×2."
di "  Bacon ayuda a DIAGNOSTICAR qué mezcla TWFE y cuánto pesa cada parte."
di "  La comparación sucia (Late vs Early) no contamina con τ CONSTANTE."
di "  Con τ CRECIENTE (como en el DGP expandido), TWFE se aleja del ATT."
di "  → SOLUCIÓN: Callaway & Sant'Anna (Sección 5.D)."


* ─────────────────────────────────────────────────────────────────────────────
* 5.C — DGP EXPANDIDO (30 UNIDADES)
*
* La misma estructura conceptual (dos cohortes + control puro) pero con
* 10 unidades por grupo → csdid necesita suficientes obs por cohorte.
*
* CAMBIO CLAVE: efectos CRECIENTES con tiempo desde tratamiento (rel_time).
*   τ(g=1985, k) = 2 × (k+1):  τ=2 en k=0, τ=4 en k=1, τ=6 en k=2, ...
*   τ(g=1988, k) = 3 × (k+1):  τ=3 en k=0, τ=6 en k=1
*
* CONSECUENCIAS:
*   (a) TWFE está sesgado (heterogeneidad dinámica + comparaciones sucias)
*   (b) Event study de TWFE muestra "pre-trends falsos" (contaminación)
*   (c) csdid estima ATT(g,t) correctamente
*   (d) Pre-trends de csdid son ≈ 0 (validación del supuesto)
* ─────────────────────────────────────────────────────────────────────────────

clear
set seed 2025
local N_grp = 10       // 10 unidades por grupo
local inicio = 1980
local fin    = 1989
local tiempo = `fin' - `inicio' + 1
set obs `= 3 * `N_grp' * `tiempo''

gen id = ceil(_n / `tiempo')
gen t  = `inicio' + mod(_n-1, `tiempo')
sort id t
xtset id t
label variable id "Unidad"
label variable t  "Año"

* Grupos:  id=1-10 → nunca tratados
*          id=11-20 → cohorte g=1985
*          id=21-30 → cohorte g=1988
gen first_treat = .
replace first_treat = 1985 if id > 10 & id <= 20
replace first_treat = 1988 if id > 20

gen D = 0
replace D = 1 if id > 10 & id <= 20 & t >= 1985
replace D = 1 if id > 20              & t >= 1988
label variable D "Tratamiento escalonado"

* Tiempo relativo al tratamiento (missing para nunca-tratados)
gen rel_time = t - first_treat

* Efectos crecientes con el tiempo desde tratamiento
gen tau_it = 0
replace tau_it = 2 * (rel_time + 1) if id > 10 & id <= 20 & D == 1
replace tau_it = 3 * (rel_time + 1) if id > 20              & D == 1
* Ejemplos:
*   id=11, t=1985 → k=0 → τ=2×1=2
*   id=11, t=1986 → k=1 → τ=2×2=4
*   id=11, t=1987 → k=2 → τ=2×3=6
*   id=21, t=1988 → k=0 → τ=3×1=3
*   id=21, t=1989 → k=1 → τ=3×2=6

* Efectos fijos individuales + tendencia temporal + ruido
gen alpha_raw = rnormal(0, 1)
bysort id (t): replace alpha_raw = alpha_raw[1]
gen Y = alpha_raw + 0.5*(t - `inicio') + tau_it + rnormal(0, 0.5)
label variable Y "Variable resultado"

* ── Trayectorias medias por cohorte ────────────────────────────────────────
preserve
    collapse (mean) Y, by(t first_treat)
    twoway ///
        (line Y t if first_treat == .,    lcolor(blue)   lwidth(medthick)) ///
        (line Y t if first_treat == 1985, lcolor(red)    lwidth(medthick)) ///
        (line Y t if first_treat == 1988, lcolor(orange) lwidth(medthick)) ///
        , xline(1984.5, lpattern(dash) lcolor(gray)) ///
          xline(1987.5, lpattern(dash) lcolor(gray)) ///
          xlabel(`inicio'(1)`fin') ///
          legend(order(1 "Nunca tratado" 2 "g=1985 (τ creciente)" ///
                 3 "g=1988 (τ creciente)") pos(6) row(1)) ///
          title("5.C: DGP expandido — medias por cohorte") ///
          xtitle("Año") ytitle("Media de Y") name(g5c_dgp, replace)
    * Leer: las pendientes post-tratamiento suben cada año (τ crece)
restore

* ── ATT verdadero del DGP expandido ───────────────────────────────────────
* g=1985: t=1985(k=0)→τ=2, t=1986(k=1)→τ=4, t=1987(k=2)→τ=6,
*         t=1988(k=3)→τ=8, t=1989(k=4)→τ=10.  Suma=30.  Obs=10×5=50.
* g=1988: t=1988(k=0)→τ=3, t=1989(k=1)→τ=6.  Suma=9.   Obs=10×2=20.
* ATT_overall = (50×promedio_g1985 + 20×promedio_g1988) / 70
*             = (50×6 + 20×4.5) / 70 = (300 + 90) / 70 ≈ 5.57

di _n "=" * 65
di "  5.C — DGP EXPANDIDO: TWFE SESGADO CON EFECTOS CRECIENTES"
di "=" * 65
di "  ATT_overall verdadero ≈ " %6.4f (50*6 + 20*4.5)/70
di "  (promedio ponderado de todas las celdas tratadas)"
di ""
reghdfe Y D, absorb(id t) vce(cluster id)
di "  β̂_TWFE = " %6.4f _b[D]
di "  Diferencia (sesgo) = " %6.4f _b[D] - (50*6 + 20*4.5)/70
di ""
di "  Con τ creciente, TWFE mezcla comparaciones de distintos rel_time."
di "  La comparación sucia (g=1988 vs g=1985 ya-tratado) contamina β̂."

* Variable gvar para csdid: 0=nunca tratado, o el año de primer tratamiento
gen gvar = first_treat
recode gvar (. = 0)
label variable gvar "Grupo de adopción (0=nunca tratado)"


* ─────────────────────────────────────────────────────────────────────────────
* 5.D — CALLAWAY & SANT'ANNA (2021): LA SOLUCIÓN
*
* IDEAS FUNDAMENTALES:
*
* 1. PARÁMETRO: ATT(g, t)
*    CS estima UN CUADRO COMPLETO de efectos — no un único β̂.
*    ATT(g, t) = E[Y_t(1) - Y_t(0) | G = g]
*    donde g = año de primer tratamiento (cohorte) y Y_t(0) es el
*    resultado contrafactual en el período t sin tratamiento.
*    Solo se estima para t ≥ g (la unidad ya recibió el tratamiento).
*
* 2. CONTROLES LIMPIOS: not-yet-treated vs. never-treated
*    CS identifica ATT(g,t) comparando la cohorte g contra un grupo
*    que en el período t NO ha recibido el tratamiento:
*
*    (a) Never-treated (sin la opción 'notyet'):
*        solo unidades que nunca se tratan en todo el panel.
*        Ventaja: siempre limpios. Desventaja: pueden ser pocos.
*
*    (b) Not-yet-treated (opción 'notyet'):
*        incluye unidades que en el período t aún no se han tratado,
*        aunque puedan hacerlo en el futuro.
*        Ventaja: más observaciones → estimaciones más precisas.
*        Supuesto extra: los no-aún-tratados no anticipan el tratamiento
*        (su Y no se mueve antes de que traten).
*
*    REGLA: usar 'notyet' cuando los never-treated son pocos.
*
* 3. ESTIMACIÓN COHORTE POR COHORTE (DiD 2×2 por celda)
*    Para cada (g, t) con t ≥ g, CS estima:
*    ATT(g, t) = E[Y_t - Y_{g-1} | G=g] - E[Y_t - Y_{g-1} | control limpio]
*    donde g-1 es el período justo anterior al tratamiento.
*    → Es un DiD 2×2 puro: solo esa cohorte vs. ese grupo de control.
*    → No hay mezcla entre cohortes → no hay comparaciones sucias.
*
* 4. AGREGACIONES DEL CUADRO ATT(g, t)
*    Una vez estimados los ATT(g,t), CS puede agregarlos:
*
*    estat simple   → ATT_overall: promedio sobre todos los (g,t) tratados
*                     (comparable a β̂_TWFE pero sin sesgo)
*
*    estat group    → ATT por cohorte: promedio dentro de cada g
*                     (¿qué cohorte se beneficia más del tratamiento?)
*
*    estat calendar → ATT por período calendario: promedio en cada año t
*                     (¿cómo evoluciona el efecto en el tiempo del calendario?)
*
*    estat event    → ATT por tiempo relativo k=t-g
*                     (dinámica del tratamiento — ver Sección 5.E)
*                     También sirve como test de pre-trends
* ─────────────────────────────────────────────────────────────────────────────

di _n "=" * 65
di "  5.D — CALLAWAY & SANT'ANNA (2021)"
di "=" * 65
di ""
di "  Sintaxis del comando csdid:"
di "  csdid depvar, ivar(panel_id) time(time_var) gvar(group_var) [notyet]"
di "    ivar(id)  : identificador del individuo/panel"
di "    time(t)   : variable de tiempo"
di "    gvar(gvar): primer período de tratamiento (0 = nunca tratado)"
di "    notyet    : usar no-aún-tratados como controles (más eficiente)"
di ""

* ── Estimación principal ────────────────────────────────────────────────────
* La salida muestra ATT(g,t) para cada celda tratada.
* Cada fila = un par (cohorte g, período t).
* Pre-períodos (t < g): son PRE-TRENDS — deben ser ≈ 0 bajo CIA.
csdid Y, ivar(id) time(t) gvar(gvar) notyet

di ""
di "  LECTURA DE LA SALIDA:"
di "  Cada fila es ATT(g,t) para una combinación (cohorte, año)."
di "  Pre-períodos (los 't < g'): son el test de pretrends — deben ser ≈ 0."
di "  Los IC son SIMULTÁNEOS (corrección de Bonferroni)."

* ── Agregaciones ────────────────────────────────────────────────────────────
di _n "  --- AGREGACIÓN 1: ATT_simple ---"
estat simple
* Interpreta: promedio de todos los ATT(g,t) ponderado por el tamaño de celda.
* Es el número comparable a β̂_TWFE (pero sin sesgo).
* Si β̂_TWFE ≠ ATT_simple: hay sesgo por comparaciones sucias + heterogeneidad.

di _n "  --- AGREGACIÓN 2: ATT por cohorte ---"
estat group
* Una fila por cohorte g.
* Promedio de los ATT(g,t) sobre todos los t ≥ g de esa cohorte.
* Responde: ¿cuánto ganó en promedio la cohorte g durante su período tratado?
* g=1985 acumula 5 períodos de τ creciente → efecto promedio más alto
* g=1988 solo tiene 2 períodos → efecto promedio menor

di _n "  --- AGREGACIÓN 3: ATT por período calendario ---"
estat calendar
* Una fila por período t.
* Para t < 1985: nadie está tratado → no aplica.
* Para 1985-1987: solo g=1985 está tratada.
* Para 1988-1989: ambas cohortes están tratadas (promedio sobre las dos).
* Responde: en el año t, ¿cuánto han ganado en promedio los ya-tratados?

di _n "  --- AGREGACIÓN 4: Event study (ATT por tiempo relativo k = t - g) ---"
estat event, window(-4 4) estore(cs_event)
* window(-4 4): muestra 4 períodos antes (k=-4,...,-1) y 4 después (k=0,...,4)
* k=-1: período de referencia (normalizado a 0 — omitido del gráfico)
* k<-1: pre-tratamiento → deben ser ≈ 0 (test de tendencias paralelas)
* k≥0:  post-tratamiento → dinámica del efecto causal
* Con τ creciente, k=1 > k=0, k=2 > k=1, etc. → gráfico ascendente

csdid_plot, title("Event study: Callaway & Sant'Anna (2021)") ///
    xtitle("Períodos desde el tratamiento") ytitle("ATT estimado") ///
    note("Barras = IC 95%. Referencia: k=-1 (período justo antes del tto.)") ///
    name(g5d_csdid, replace)
* LEER:
*   k < -1: coeficientes deben caer sobre el cero → valida tendencias paralelas
*   k ≥ 0:  la curva debe subir → τ creciente en este DGP
*   Si los pre-trends del TWFE no son cero pero los de CS sí:
*     → el problema no es de tendencias paralelas, sino de staggered


* ─────────────────────────────────────────────────────────────────────────────
* 5.E — EVENT STUDY: LEADS Y LAGS — DINÁMICA DEL EFECTO
*
* ¿QUÉ ES UN EVENT STUDY?
*   En lugar de un único β̂, estimamos un coeficiente para cada "distancia
*   al tratamiento": ¿cuánto cambia Y en el período k relativo al momento
*   en que ocurrió el tratamiento?
*
*   Permite responder dos preguntas con el mismo modelo:
*   (1) ¿Había diferencias PRE-existentes entre tratados y controles?
*       → test de tendencias paralelas (usando los coeficientes pre-tratamiento)
*   (2) ¿Cómo evoluciona el efecto del tratamiento a lo largo del tiempo?
*       → dinámica del efecto (usando los coeficientes post-tratamiento)
*
* ─────────────────────────────────────────────────────────────────────────────
* ¿QUÉ ES rel_time?
*   rel_time_it = t - first_treat_i
*   Ejemplos con este DGP:
*     id=11 (g=1985): rel_time en 1984=-1, en 1985=0, en 1986=1, en 1987=2, ...
*     id=21 (g=1988): rel_time en 1987=-1, en 1988=0, en 1989=1
*   Nunca-tratados (id=1-10): rel_time = missing (first_treat=.)
* ─────────────────────────────────────────────────────────────────────────────
* ¿QUÉ SON "LEADS" Y "LAGS" EN EVENT STUDY (Y POR QUÉ GENERA CONFUSIÓN)?
*
*   F_k  (k=2, 3, ...):  indicadores pre-tratamiento, llamados "leads"
*     F_k = 1  si  rel_time = -k  (k períodos ANTES del tratamiento)
*     NOMBRE: "leads" porque estos períodos CONDUCEN HACIA el evento —
*              preceden al tratamiento, señalan en su dirección.
*     REFERENCIA: F_1 (1 período antes) se OMITE — es la normalización.
*     INTERPRETACIÓN: bajo tendencias paralelas, todos deben ser ≈ 0.
*                     Si F_k ≠ 0 → hay diferencias pre-tratamiento.
*
*   L_k  (k=0, 1, 2, ...):  indicadores post-tratamiento, llamados "lags"
*     L_k = 1  si  rel_time = k   (k períodos DESPUÉS del tratamiento)
*     NOMBRE: "lags" porque el efecto del evento SE ARRASTRA hacia adelante —
*              tarda k períodos en manifestarse (o k períodos después).
*     INTERPRETACIÓN: L_0 = impacto instantáneo (en el período de adopción).
*                     L_1, L_2, ... = evolución del efecto en el tiempo.
*
* ─────────────────────────────────────────────────────────────────────────────
* FUENTE DE CONFUSIÓN (importante para entender a Cunningham):
*
*   En series de tiempo y en Stata:
*     L.Y  = lag (pasado):    valor de Y en t-1
*     F.Y  = forward (futuro): valor de Y en t+1
*
*   En event studies:
*     "lead" F_k  → período ANTERIOR al evento (en tiempo calendario: pasado)
*     "lag"  L_k  → período POSTERIOR al evento (en tiempo calendario: futuro)
*
*   Las etiquetas están INVERTIDAS respecto a la convención de series de tiempo:
*     El "lead" del event study = el "lag" de series de tiempo (es el pasado)
*     El "lag" del event study  = el "lead" de series de tiempo (es el futuro)
*
*   MNEMÓNICA (Cunningham, "The Mixtape"):
*     Lead = "conduce hacia el tratamiento" (viene antes, apunta hacia él)
*     Lag  = "queda rezagado del tratamiento" (viene después, lo sigue)
*
*   Reglas prácticas:
*     F_k: rel_time = -k  → pre-evento (negativo → pasado relativo)
*     L_k: rel_time = +k  → post-evento (positivo → futuro relativo)
*     F_1 se OMITE siempre (es la categoría de referencia = normalización)
*     L_0 NUNCA se omite (es el impacto en el período de adopción)
* ─────────────────────────────────────────────────────────────────────────────

di _n "=" * 65
di "  5.E — EVENT STUDY: LEADS Y LAGS"
di "=" * 65

* ── Generar leads y lags ────────────────────────────────────────────────────
summ rel_time
local rel_min = abs(r(min))    // períodos pre máximos (como positivo)
local rel_max = r(max)         // períodos post máximos

di ""
di "  Distribución de rel_time en la muestra:"
tab rel_time, missing
di ""
di "  Ventana disponible: " -`rel_min' " a +" `rel_max' " períodos"
di "  (nunca tratados: rel_time = missing → sus F_k y L_k serán 0)"

cap drop F_* L_*

* Leads (pre): F_k = 1 si rel_time = -k. F_1 omitida (referencia).
forval k = 2/`rel_min' {
    gen byte F_`k' = (rel_time == -`k')
    label variable F_`k' "`k' períodos ANTES (lead)"
}

* Lags (post): L_k = 1 si rel_time = k
forval k = 0/`rel_max' {
    gen byte L_`k' = (rel_time == `k')
    label variable L_`k' "`k' períodos DESPUÉS (lag)"
}

di ""
di "  Variables creadas:"
di "  Leads: F_2, F_3, ..., F_`rel_min'  (pre-tratamiento; F_1=ref, omitida)"
di "  Lags:  L_0, L_1, ..., L_`rel_max'  (post-tratamiento)"
di ""
di "  Interpretación:"
di "  F_2: la unidad está 2 años antes de tratar (rel_time=-2)"
di "  L_0: la unidad está en su año de adopción (rel_time=0)"
di "  L_1: la unidad lleva 1 año tratada (rel_time=1)"

* ── TWFE Event Study ────────────────────────────────────────────────────────
di _n "  TWFE EVENT STUDY"
di "  Ecuación: Y_it = α_i + λ_t + Σ_k δ_k F_k + Σ_k γ_k L_k + ε_it"
di "  Referencia: F_1 (un período antes del tratamiento)"
di ""
di "  CUIDADO con adopción escalonada:"
di "  Con staggered adoption + efectos heterogéneos entre cohortes,"
di "  los coeficientes del TWFE event study son promedios ponderados"
di "  que incluyen las comparaciones sucias (ya-tratados como control)."
di "  Los F_k pueden ser ≠ 0 AUN CON TENDENCIAS PARALELAS porque la"
di "  contaminación de staggered altera los coeficientes pre-tratamiento."
di "  → NO interpretes F_k ≠ 0 en TWFE como violación de paralelas."
di "    Interpreta como señal de heterogeneidad + comparaciones sucias."
di ""

reghdfe Y F_2 F_3 F_4 L_0 L_1 L_2 L_3 L_4, ///
    absorb(id t) cluster(id)
estimates store twfe_es

di ""
di "  Leer los coeficientes:"
di "  F_2, F_3, F_4: períodos 2, 3, 4 años ANTES del tratamiento"
di "    Bajo tendencias paralelas puras → deben ser ≈ 0"
di "    Con staggered + dirty comparisons → pueden ≠ 0 (falsos pre-trends)"
di "  L_0: impacto en el año de adopción"
di "  L_1, L_2, L_3, L_4: dinámica post-tratamiento"
di "    Con τ creciente → la secuencia L_0 < L_1 < L_2 < ... (ascendente)"

* ── Gráfico comparativo: TWFE vs. Callaway & Sant'Anna ─────────────────────
di _n "  COMPARACIÓN VISUAL: TWFE vs. Callaway & Sant'Anna"
di ""
di "  El gráfico mostrará:"
di "  TWFE (rojo): pre-trends pueden ser ≠ 0 (contaminación de staggered)"
di "  CS   (azul): pre-trends deben ser ≈ 0 (estimación limpia por cohorte)"
di "  En post-tratamiento: ambos suben, pero CS refleja el τ verdadero"
di ""

cap noisily {
    event_plot twfe_es cs_event, ///
        stub_lag(L_# Tp#) stub_lead(F_# Tm#) ///
        together perturb(-0.12 0.12) trimlag(4) trimlead(4) ///
        noautolegend plottype(scatter) ciplottype(rspike) ///
            lag_opt1( msymbol(O) msize(1.5) mcolor(red%90)  mlwidth(0.3)) ///
            lag_ci_opt1(lcolor(red%50) lwidth(0.3)) ///
            lag_opt2( msymbol(D) msize(1.5) mcolor(navy%90) mlwidth(0.3)) ///
            lag_ci_opt2(lcolor(navy%50) lwidth(0.3)) ///
        graph_opt( ///
            title("TWFE vs. Callaway & Sant'Anna — Event Study") ///
            xtitle("Períodos desde el tratamiento") ///
            ytitle("ATT estimado") ///
            xlabel(-4(1)4) ///
            xline(-0.5, lc(gs8) lp(dash)) ///
            yline(0, lc(gs8) lp(dash)) ///
            legend(order(1 "TWFE (puede tener falsos pre-trends)" ///
                         3 "Callaway-Sant'Anna (pre-trends correctos)") ///
                   pos(6) rows(2)) ///
        )
    graph rename g5e_comparison, replace
}

* ── Resumen de la sección ────────────────────────────────────────────────────
di _n "=" * 65
di "  RESUMEN SECCIÓN 5"
di "=" * 65
di ""
di "  TWFE con adopción escalonada:"
di "    β̂_TWFE ≠ ATT_overall cuando hay efectos heterogéneos"
di "    Bacon (2021) diagnostica cuánto pesa cada comparación 2×2"
di "    La comparación sucia (ya-tratado como control) contamina β̂"
di ""
di "  Callaway & Sant'Anna (2021):"
di "    Estima ATT(g,t) cohorte por cohorte — sin comparaciones sucias"
di "    Controles: not-yet-treated ('notyet') o solo never-treated"
di "    4 agregaciones: simple, group, calendar, event"
di ""
di "  Event study — leads y lags:"
di "    F_k (leads): k períodos ANTES del evento → test de pretrends"
di "    L_k (lags):  k períodos DESPUÉS del evento → dinámica del efecto"
di "    F_1 = referencia (omitida). L_0 nunca se omite."
di "    Con staggered: TWFE event study puede tener falsos pre-trends."
di "    Usar CS event study para una estimación limpia."
di "=" * 65



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
