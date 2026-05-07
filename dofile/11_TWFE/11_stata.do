********************************************************************************
* Datos de Panel, DiD, Primeras Diferencias y TWFE
* Econometría Avanzada — Ana María Díaz Escobar — Javeriana 2026-I
*
* SECCIÓN 0:  Introducción a datos de panel (xtset, within, FE/FD/RE/Hausman)
* SECCIÓN 1:  2×2 — DiD = FD = TWFE (equivalencia algebraica exacta)
* SECCIÓN 2:  Panel largo, adopción simultánea — DiD = TWFE; FD ≠ TWFE
*             + Tendencias paralelas: visualización y test formal
* SECCIÓN 3:  ¿Qué pasa cuando se viola el supuesto de tendencias paralelas?
********************************************************************************

clear all
set more off


********************************************************************************
* SECCIÓN 0: INTRODUCCIÓN A DATOS DE PANEL
*
* DGP sintético: 100 individuos, 6 periodos
*   - efecto fijo individual alpha_i (heterogéneo, correlacionado con X → FE)
*   - tendencia temporal común lambda_t
*   - variable de interés: X (tiempo-variable)
*   - resultado: Y = alpha_i + lambda_t + beta*X + eps
********************************************************************************

set seed 9999
local N = 100
local T = 6
set obs `= `N' * `T''

* ── Panel structure ───────────────────────────────────────────────────────────
egen id = seq(), b(`T')
egen t  = seq(), f(1) t(`T')
sort id t
xtset id t

label variable id "Individuo"
label variable t  "Periodo (1–6)"

* ── Efectos fijos individuales (heterogéneos, correlacionados con X) ──────────
gen alpha_i = 2 * id / `N' + rnormal(0, 0.5)   // varía entre individuos

* ── Variable de tratamiento/interés (correlacionada con alpha_i) ──────────────
gen X = 0.5 * alpha_i + 0.3 * t + rnormal(0, 1)
label variable X "Variable de interés (tiempo-variable)"

* ── Variable resultado ────────────────────────────────────────────────────────
scalar beta_true = 3
gen Y = alpha_i + 0.8 * t + beta_true * X + rnormal(0, 1)
label variable Y "Variable resultado"

* ── Describir el panel ────────────────────────────────────────────────────────
di _n "=== DESCRIPCIÓN DEL PANEL ==="
xtdes                                  // estructura: balanceado, T=6 por individuo
xtsum Y X                              // variación within vs between

* ── Gráfico: spaghetti de Y en el tiempo (primeros 10 individuos) ─────────────
xtline Y if id <= 10, overlay ///
    title("Evolución de Y: primeros 10 individuos") ///
    xtitle("Periodo") ytitle("Y") ///
    legend(off) name(g0_xtline, replace)

* ── Variación within vs between — intuitivo ───────────────────────────────────
di _n "=== VARIACIÓN WITHIN vs BETWEEN ==="
di "xtsum descompone la variación de Y en:"
di "  between: diferencias ENTRE individuos (promedio de c/uno)"
di "  within:  variaciones de c/individuo alrededor de SU PROPIA media"
di "Los EF usan solo la variación within (eliminan la between)."

* ── Estimación 1: OLS Pooled (incorrecto si Cov(X, alpha_i) ≠ 0) ─────────────
di _n "=== MODELO 1: OLS Pooled (incorrecto) ==="
reg Y X, vce(cluster id)
di "Sesgo: OLS absorbe la heterogeneidad individual en el error → endogeneidad"

* ── Estimación 2: EFECTOS FIJOS (within estimator) ──────────────────────────
di _n "=== MODELO 2: Efectos Fijos (xtreg fe) ==="
xtreg Y X i.t, fe vce(cluster id)
di "Valor verdadero de beta = `= scalar(beta_true)' — FE debe recuperarlo ✓"

* ── EF a mano: transformación within ─────────────────────────────────────────
di _n "=== EF A MANO: transformación within ==="
* Demean: restar la media individual de cada variable
bysort id: egen mean_Y = mean(Y)
bysort id: egen mean_X = mean(X)
gen Y_within = Y - mean_Y
gen X_within = X - mean_X
* La regresión de Y_within sobre X_within = xtreg fe (sin i.t para simplificar)
reg Y_within X_within, nocons
di "xtreg ,fe y la transformación within a mano deben dar el mismo coeficiente."

* ── Estimación 3: EFECTOS ALEATORIOS ─────────────────────────────────────────
di _n "=== MODELO 3: Efectos Aleatorios (xtreg re) ==="
di "(Incorrecto aquí porque Cov(X, alpha_i) ≠ 0 por construcción)"
xtreg Y X i.t, re vce(cluster id)

* ── Test de Hausman: ¿FE o RE? ───────────────────────────────────────────────
di _n "=== TEST DE HAUSMAN: ¿FE o RE? ==="
xtreg Y X i.t, fe
estimates store fe_hausman
xtreg Y X i.t, re
hausman fe_hausman ., sigmamore
di "H0 de Hausman: los RE son consistentes (Cov(X,alpha_i) = 0)"
di "Si p < 0.05: rechazamos H0 → usar FE. Si p > 0.05: RE es eficiente."

* ── Estimación 4: PRIMERAS DIFERENCIAS ───────────────────────────────────────
di _n "=== MODELO 4: Primeras Diferencias ==="
reg D.Y D.X ibn.t, noconstant vce(cluster id)
di "FD elimina los EF individuales por diferencia (solo usa ΔY, ΔX consecutivos)."
di "Con T=6: usa 5 periodos de diferencias por individuo (pierde t=1)."

drop mean_Y mean_X Y_within X_within

* ── Resumen comparativo ───────────────────────────────────────────────────────
di _n "{hline 65}"
di "  RESUMEN: ¿qué estimador usar?"
di "{hline 65}"
di "  TODOS los estimadores requieren exogeneidad de X_it en eps_it"
di "  La ventaja de FE/FD es específica: son consistentes aunque"
di "  Cov(X_it, alpha_i) ≠ 0  (OLS y RE no lo son en ese caso)"
di ""
di "  Cov(X, alpha_i) = 0  →  RE eficiente   (Hausman: no rechaza)"
di "  Cov(X, alpha_i) ≠ 0  →  FE consistente (Hausman: rechaza)"
di "  X_it endógeno en eps_it → TODOS sesgan (necesitas IV)"
di "  Panel dinámico (Y_t-1) → FE inconsistente → Arellano-Bond"
di "  T=2, errores i.i.d.    → FD = FE (algebraicamente idénticos)"
di "  T>2, errores i.i.d.    → FE más eficiente que FD"
di "  DiD 2×2                → FD = FE = DiD (equivalencia exacta)"
di "{hline 65}"


********************************************************************************
* SECCIÓN 1: 2×2 — DiD = FD = TWFE (EQUIVALENCIA ALGEBRAICA EXACTA)
*
* DGP: 2 grupos (tratado/control), 2 periodos (antes/después)
*      Efectos fijos individuales heterogéneos, efecto causal τ = 3
********************************************************************************

clear
set seed 1234
set obs 400

gen id   = ceil(_n / 2)           // 200 individuos
gen t    = mod(_n - 1, 2)         // t=0 (antes), t=1 (después)
gen trat = (id > 100)             // grupo tratado: id 101–200
gen D    = trat * (t == 1)        // indicador de tratamiento efectivo

gen alpha_i = 2 * id / 200 + rnormal(0, 0.5)
gen eps     = rnormal(0, 1)
gen Y = alpha_i + 1.5 * t + 3 * D + eps    // τ = 3
xtset id t

* ── DiD manual (4 medias) ─────────────────────────────────────────────────────
quietly sum Y if trat==1 & t==0
scalar y_t0 = r(mean)
quietly sum Y if trat==1 & t==1
scalar y_t1 = r(mean)
quietly sum Y if trat==0 & t==0
scalar y_c0 = r(mean)
quietly sum Y if trat==0 & t==1
scalar y_c1 = r(mean)
scalar DiD_manual = (y_t1 - y_t0) - (y_c1 - y_c0)

* ── Regresión DiD clásica: Y = α + βD_i + γt + τ(D_i×t) ─────────────────────
reg Y trat t D, robust
scalar DiD_reg = _b[D]

* ── Primeras Diferencias (T=2): ΔY_i = a + τ·ΔD_i + Δε_i ────────────────────
reg D.Y D.D, robust
scalar FD_2x2 = _b[D.D]

* ── TWFE: absorbe efectos fijos individual + temporal ─────────────────────────
reghdfe Y D, absorb(id t) vce(robust)
scalar TWFE_2x2 = _b[D]

di _n "{hline 60}"
di "  SECCIÓN 1: 2×2 — EQUIVALENCIA ALGEBRAICA EXACTA"
di "{hline 60}"
di "  Valor verdadero de τ            = 3"
di "  DiD manual (4 medias)           = " %7.4f DiD_manual
di "  Regresión DiD (Y~trat×t)        = " %7.4f DiD_reg
di "  Primeras diferencias (ΔY~ΔD)    = " %7.4f FD_2x2
di "  TWFE (reghdfe absorb id t)      = " %7.4f TWFE_2x2
di "{hline 60}"
di "  Los cuatro deben ser IDÉNTICOS — equivalencia algebraica ✓"
di "{hline 60}"


********************************************************************************
* SECCIÓN 2: PANEL LARGO, ADOPCIÓN SIMULTÁNEA — TENDENCIAS PARALELAS
*
* DGP: 3 unidades (1 control, 2 tratados), T=11 periodos (1980–1990)
*      Adopción simultánea en t=1985, efecto constante τ = 5
*
* CASO A: tendencias paralelas CORRECTAS (DGP limpio)
* CASO B: tendencias paralelas VIOLADAS (tendencias pre distintas)
********************************************************************************

* ─────────────────────────────────────────────────────────────────────────────
* CASO 2A: Tendencias paralelas ✓
* ─────────────────────────────────────────────────────────────────────────────

clear
set seed 5678
local N = 2        // 1 control + 2 tratados
local inicio = 1980
local fin    = 1990
local tiempo = `fin' - `inicio' + 1
local obs    = (`N' + 1) * `tiempo'    // 3 unidades × 11 periodos
set obs `obs'

* Panel: 3 unidades
gen id = ceil(_n / `tiempo')
gen t  = `inicio' + mod(_n - 1, `tiempo')
sort id t
xtset id t

gen D = (id >= 2) * (t >= 1985)   // tratado desde 1985 para id 2 y 3
gen Y = id + 3 * (t - 1980) + 5 * D + rnormal(0, 0.5)
label variable Y "Resultado (tendencias paralelas correctas)"

* Visualización
twoway ///
    (connected Y t if id==1, msymbol(circle)   lcolor(blue)   lwidth(medium)) ///
    (connected Y t if id==2, msymbol(triangle)  lcolor(red)    lwidth(medium)) ///
    (connected Y t if id==3, msymbol(square)    lcolor(orange) lwidth(medium)) ///
    , xline(1984.5, lpattern(dash) lcolor(gray)) ///
      xlabel(`inicio'(1)`fin') ///
      legend(order(1 "Control (id=1)" 2 "Tratado id=2" 3 "Tratado id=3") pos(6) row(1)) ///
      title("Caso 2A: tendencias paralelas ✓") ///
      xtitle("Año") ytitle("Y") ///
      name(g2a_trends, replace)

* DiD manual + TWFE
quietly sum Y if id >= 2 & t >= 1985
scalar y_t_post = r(mean)
quietly sum Y if id >= 2 & t < 1985
scalar y_t_pre  = r(mean)
quietly sum Y if id == 1 & t >= 1985
scalar y_c_post = r(mean)
quietly sum Y if id == 1 & t < 1985
scalar y_c_pre  = r(mean)
scalar DiD2a = (y_t_post - y_t_pre) - (y_c_post - y_c_pre)

reghdfe Y D, absorb(id t) vce(robust)
scalar TWFE2a = _b[D]

* ── Test formal de tendencias paralelas: xtdidregress ─────────────────────────
* Requiere la variable de grupo tratado (no el indicador D×t)
gen trat_grupo = (id >= 2)

xtdidregress (Y) (D), group(id) time(t)
estat trendplots, ///
    title("Tendencias pre-tratamiento — Caso 2A") ///
    name(g2a_trendplots, replace)
estat ptrends
* H0: tendencias son paralelas. p > 0.05 → no rechazamos → supuesto OK

di _n "=== CASO 2A: Tendencias paralelas ✓ ==="
di "DiD manual = " %6.3f DiD2a "   TWFE = " %6.3f TWFE2a "   (τ verdadero = 5)"

* ─────────────────────────────────────────────────────────────────────────────
* CASO 2B: Tendencias NO paralelas ✗
* Mismo diseño pero el tratado tiene una tendencia pre-tratamiento más alta
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

* DIFERENCIA vs 2A: el tratado tiene un EXTRA de tendencia pre-tratamiento (+2/año)
gen tendencia_extra = (id >= 2) * (t - `inicio') * 2    // tendencia más pronunciada
gen Y = id + 3 * (t - 1980) + tendencia_extra + 5 * D + rnormal(0, 0.5)
label variable Y "Resultado (tendencias NO paralelas)"

twoway ///
    (connected Y t if id==1, msymbol(circle)   lcolor(blue)   lwidth(medium)) ///
    (connected Y t if id==2, msymbol(triangle)  lcolor(red)    lwidth(medium)) ///
    (connected Y t if id==3, msymbol(square)    lcolor(orange) lwidth(medium)) ///
    , xline(1984.5, lpattern(dash) lcolor(gray)) ///
      xlabel(`inicio'(1)`fin') ///
      legend(order(1 "Control (id=1)" 2 "Tratado id=2" 3 "Tratado id=3") pos(6) row(1)) ///
      title("Caso 2B: tendencias NO paralelas ✗") ///
      note("El tratado crece más rápido incluso antes del tratamiento") ///
      xtitle("Año") ytitle("Y") ///
      name(g2b_trends, replace)

reghdfe Y D, absorb(id t) vce(robust)
scalar TWFE2b = _b[D]

* ── Diagnóstico visual: xtdidregress ─────────────────────────────────────────
xtdidregress (Y) (D), group(id) time(t)
estat trendplots, ///
    title("Tendencias pre-tratamiento — Caso 2B (violación)") ///
    name(g2b_trendplots, replace)
estat ptrends
* H0 rechazada → tendencias pre NO son paralelas → estimador sesgado

di _n "=== CASO 2B: Tendencias NO paralelas ✗ ==="
di "TWFE = " %6.3f TWFE2b "   (τ verdadero = 5 — sesgo ≈ " %6.3f TWFE2b - 5 ")"


********************************************************************************
* SECCIÓN 3: ¿QUÉ PASA CUANDO SE VIOLA EL SUPUESTO DE TENDENCIAS PARALELAS?
*
* Mostramos el sesgo del estimador DiD/TWFE cuando las tendencias pre son distintas
* usando primeras diferencias para ver por qué falla
********************************************************************************

di _n "{hline 65}"
di "  SECCIÓN 3: MECANISMO DEL SESGO POR VIOLACIÓN DE T. PARALELAS"
di "{hline 65}"
di ""
di "  DiD identifica τ comparando: Δ(tratado) - Δ(control)"
di "  Si la tendencia pre del tratado > tendencia pre del control:"
di "    → Δ(tratado) ya era más grande ANTES del tratamiento"
di "    → DiD atribuye esa diferencia de tendencia al tratamiento"
di "    → sesgo positivo = diferencia de tendencias × períodos post"
di ""
di "  En el Caso 2B:"
di "    - Tendencia extra del tratado: +2 por año"
di "    - Periodos post: 6 (1985–1990)"
di "    - Sesgo esperado ≈ 2 × (media de períodos post) ≈ grande"
di ""
di "  Verificación algebraica (FD revela el problema):"
di "  En el PERÍODO DE ADOPCIÓN (1984→1985):"
di "    ΔY_control  = tendencia_común = 3"
di "    ΔY_tratado  = tendencia_común + extra + τ = 3 + 2 + 5 = 10"
di "    'DiD 1-paso' = 10 - 3 = 7 ≠ τ = 5"
di ""
di "  CONCLUSIÓN: si hay diferencia de tendencias pre-tratamiento,"
di "  DiD = TWFE captura τ + diferencia de tendencias (SESGO)."
di "{hline 65}"

* Primera diferencia en el período de adopción (ilustración)
di _n "  Comprobación: ΔY por grupo en t=1985 (año de adopción)"
sum Y if id == 1 & t == 1984
scalar Y_c84 = r(mean)
sum Y if id == 1 & t == 1985
scalar DY_c = r(mean) - Y_c84

sum Y if id == 2 & t == 1984
scalar Y_t84 = r(mean)
sum Y if id == 2 & t == 1985
scalar DY_t = r(mean) - Y_t84

di "  ΔY control (1984→1985)  = " %6.3f DY_c
di "  ΔY tratado (1984→1985)  = " %6.3f DY_t
di "  DiD en ese año           = " %6.3f DY_t - DY_c "  (τ=5, tendencia_extra=2, esperado≈7)"


********************************************************************************
* TABLA RESUMEN FINAL
********************************************************************************

di _n "{hline 70}"
di "  RESUMEN: equivalencia DiD = FD = TWFE — ¿cuándo y cuánto importa?"
di "{hline 70}"
di ""
di "  Situación                 │ DiD=FD?  │ DiD=TWFE? │ T. paralelas?"
di "  ─────────────────────────┼──────────┼───────────┼───────────────"
di "  2×2                      │ SÍ exact.│ SÍ exact. │ (no aplica)"
di "  Panel largo, simult., T.P│ NO exact.│ SÍ exact. │ ✓ OK"
di "  Panel largo, simult., !TP│ NO       │ NO (sesgo)│ ✗ VIOLADA"
di "  Adopción escalonada, cte  │ NO       │ ~ aprox.  │ depende"
di "  Adopción escalonada, din. │ NO       │ NO (sesgo)│ depende"
di ""
di "  Regla práctica:"
di "  1. Siempre dibuja xtline y estat trendplots antes de reportar DiD"
di "  2. Usa estat ptrends (H0: tendencias paralelas) — p>0.05 es buena señal"
di "  3. Si falla: controla tendencias lineales por grupo (trend×tratado)"
di "  4. Si hay adopción escalonada + efectos dinámicos: csdid / Sun-Abraham"
di "{hline 70}"


********************************************************************************
* SECCIÓN 4: MÁS UNIDADES, MISMO AÑO DE ADOPCIÓN, EFECTOS HETEROGÉNEOS
*
* DGP: 3 unidades (id=1 control, id=2 y 3 tratados desde 1985)
*      Efectos distintos: id=2 → τ=2, id=3 → τ=4
* Objetivo: mostrar qué promedia TWFE con heterogeneidad entre tratados
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

gen Y = 0
replace Y = id + t + cond(D==1, 0, 0) if id == 1
replace Y = id + t + cond(D==1, 2, 0) if id == 2
replace Y = id + t + cond(D==1, 4, 0) if id == 3
label variable Y "Variable dependiente"

twoway ///
    (connected Y t if id==1, msymbol(circle)   lcolor(blue)   lwidth(medium)) ///
    (connected Y t if id==2, msymbol(triangle)  lcolor(red)    lwidth(medium)) ///
    (connected Y t if id==3, msymbol(square)    lcolor(orange) lwidth(medium)) ///
    , xline(1984.5, lpattern(dash) lcolor(gray)) ///
      xlabel(`inicio'(1)`fin') ///
      legend(order(1 "id=1 (Control)" 2 "id=2 (Tratado, τ=2)" 3 "id=3 (Tratado, τ=4)") pos(6) row(1)) ///
      title("Caso 4: heterogeneidad entre tratados, mismo timing") ///
      xtitle("Año") ytitle("Y") name(g4_hetero, replace)

xtreg Y D i.t, fe
reghdfe Y D, absorb(id t) vce(robust)

* ATT verdadero (promedio ponderado entre obs. tratadas):
* (5 periodos post × τ=2) + (5 periodos post × τ=4) / 10 = 3
* TWFE coincide porque los efectos son constantes en el tiempo (no hay heterogeneidad dinámica)
di _n "ATT verdadero (ponderado) = 3 — TWFE debería dar ≈ 3"


********************************************************************************
* SECCIÓN 5: ADOPCIÓN ESCALONADA (STAGGERED DiD) + DESCOMPOSICIÓN DE BACON
*
* DGP: id=2 se trata desde 1985 (τ=2), id=3 desde 1988 (τ=4)
* Objetivo: ver el sesgo de TWFE + entender los 3 tipos de comparaciones 2×2
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

* Tratamiento escalonado: id=2 desde 1985, id=3 desde 1988
gen D = 0
replace D = 1 if id==2 & t >= 1985
replace D = 1 if id==3 & t >= 1988
label variable D "Tratamiento escalonado"

gen Y = id + t + D * 2 * (id==2) + D * 4 * (id==3)
label variable Y "Variable dependiente"

twoway ///
    (connected Y t if id==1, msymbol(circle)   lcolor(blue)   lwidth(medium)) ///
    (connected Y t if id==2, msymbol(triangle)  lcolor(red)    lwidth(medium)) ///
    (connected Y t if id==3, msymbol(square)    lcolor(orange) lwidth(medium)) ///
    , xline(1984.5 1987.5, lpattern(dash) lcolor(gray)) ///
      xlabel(`inicio'(1)`fin') ///
      legend(order(1 "id=1 (Control)" 2 "id=2 (trata 1985, τ=2)" 3 "id=3 (trata 1988, τ=4)") pos(6) row(1)) ///
      title("Caso 5: adopción escalonada") ///
      xtitle("Año") ytitle("Y") name(g5_staggered, replace)

* TWFE y comparaciones por par
reghdfe Y D, absorb(id t) vce(robust)
xtreg Y D i.t if (id==1 | id==2), fe robust   // par 1-2 (limpio)
xtreg Y D i.t if (id==1 | id==3), fe robust   // par 1-3 (limpio)

* Descomposición de Bacon
* ssc install bacondecomp, replace   // instalar si no está
bacondecomp Y D, ddetail


********************************************************************************
* SECCIÓN 6: ESTIMADORES MODERNOS — SIMULACIÓN GRANDE (30 unidades × 60 períodos)
*
* Objetivo: comparar TWFE vs csdid, did_imputation, eventstudyinteract, did2s,
*           did_multiplegt_dyn y stackedev con un event-study comparativo
* Referencias: Callaway-Sant'Anna 2021, Borusyak-Jaravel-Spiess 2022,
*              Sun-Abraham 2021, Gardner 2022, Cengiz et al. 2019
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

set seed 20211222

gen Y           = 0
gen D           = 0
gen cohort      = .
gen effect      = .
gen first_treat = .
gen rel_time    = .

levelsof id, local(lvls)
foreach x of local lvls {
    local chrt = runiformint(0,5)
    replace cohort = `chrt' if id==`x'
}

levelsof cohort, local(lvls)
foreach x of local lvls {
    local eff    = runiformint(2,10)
    replace effect = `eff' if cohort==`x'
    local timing = runiformint(`start', `end' + 20)
    replace first_treat = `timing' if cohort==`x'
    replace first_treat = . if first_treat > `end'
    replace D = 1 if cohort==`x' & t >= `timing'
}

replace rel_time = t - first_treat
replace Y = id + t + cond(D==1, effect * rel_time, 0) + rnormal()

* ── Leads y lags para event study ─────────────────────────────────────────────
summ rel_time
local relmin = abs(r(min))
local relmax = abs(r(max))

cap drop F_* L_*
forval x = 2/`relmin' {
    gen F_`x' = rel_time == -`x'
}
forval x = 0/`relmax' {
    gen L_`x' = rel_time == `x'
}

gen never_treat = first_treat == .
sum first_treat
gen last_cohort = first_treat == r(max)
gen gvar        = first_treat
recode gvar (. = 0)

* ── Instalar paquetes (una sola vez — comentar después) ──────────────────────
/*
ssc install schemepack,         replace
ssc install avar,               replace
ssc install reghdfe,            replace
ssc install event_plot,         replace
ssc install palettes,           replace
ssc install colrspace,          replace
ssc install drdid,              replace
ssc install csdid,              replace
ssc install did_imputation,     replace
ssc install eventstudyinteract, replace
ssc install did_multiplegt_dyn, replace
ssc install stackedev,          replace
ssc install did2s,              replace
*/

* ── TWFE ──────────────────────────────────────────────────────────────────────
reghdfe Y L_* F_*, absorb(id t) cluster(id)
estimates store twfe

* ── csdid — Callaway & Sant'Anna (2021) ───────────────────────────────────────
csdid Y, ivar(id) time(t) gvar(gvar) notyet
estat event, window(-10 10) estore(csdd)

* ── did_imputation — Borusyak, Jaravel & Spiess (2022) ───────────────────────
did_imputation Y id t first_treat, horizons(0/10) pretrend(10) minn(0)
estimates store didimp

* ── did_multiplegt_dyn — de Chaisemartin & D'Haultfœuille ────────────────────
did_multiplegt_dyn Y id t D, effects(10) placebo(10) cluster(id)
matrix didmgt_b = e(estimates)
matrix didmgt_v = e(variances)

* ── eventstudyinteract — Sun & Abraham (2021) ────────────────────────────────
eventstudyinteract Y L_* F_*, vce(cluster id) absorb(id t) ///
    cohort(first_treat) control_cohort(never_treat)
matrix evtstint_b = e(b_iw)
matrix evtstint_v = e(V_iw)

* ── did2s — Gardner (2022) ────────────────────────────────────────────────────
did2s Y, first_stage(id t) second_stage(F_* L_*) treatment(D) cluster(id)
matrix did2s_b = e(b)
matrix did2s_v = e(V)

* ── stackedev — Cengiz et al. (2019) ─────────────────────────────────────────
stackedev Y F_* L_* ref, cohort(first_treat) time(t) ///
    never_treat(never_treat) unit_fe(id) clust_unit(id)
matrix stackedev_b = e(b)
matrix stackedev_v = e(V)

* ── Event study plot comparativo ─────────────────────────────────────────────
colorpalette tableau, nograph

event_plot    twfe          csdd           didimp                           ///
              dcdh_b#dcdh_v  sa_b#sa_v      stackedev_b#stackedev_v         ///
              did2s_b#did2s_v,                                               ///
    stub_lag( L_#   Tp#    tau#   Effect_#  L_#  L_#  L_#)                  ///
    stub_lead(F_#   Tm#    pre#   Placebo_# F_#  F_#  F_#)                  ///
    together perturb(-0.30(0.10)0.30) trimlead(20) trimlag(20) noautolegend ///
    plottype(scatter) ciplottype(rspike)                                     ///
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
    graph_opt(                                                                ///
        title("Event study: TWFE vs estimadores modernos")                   ///
        xtitle("Períodos desde el tratamiento") ytitle("Efecto promedio")    ///
        xlabel(-20(2)20)                                                     ///
        legend(order(1 "TWFE" 3 "csdid (CS 2021)"                           ///
               5 "did_imputation (BJS 2022)" 7 "did_multiplegt_dyn"          ///
               9 "eventstudyinteract (SA 2021)"                              ///
               11 "stackedev (CDLZ 2019)" 13 "did2s (G 2022)")              ///
               pos(6) rows(3) region(style(none)))                            ///
        xline(-0.5, lc(gs8) lp(dash)) yline(0, lc(gs8) lp(dash))           ///
    )
