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
di _n "=" * 65
di "  RESUMEN: ¿qué estimador usar?"
di "=" * 65
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
di "=" * 65


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

di _n "=" * 60
di "  SECCIÓN 1: 2×2 — EQUIVALENCIA ALGEBRAICA EXACTA"
di "=" * 60
di "  Valor verdadero de τ            = 3"
di "  DiD manual (4 medias)           = " %7.4f DiD_manual
di "  Regresión DiD (Y~trat×t)        = " %7.4f DiD_reg
di "  Primeras diferencias (ΔY~ΔD)    = " %7.4f FD_2x2
di "  TWFE (reghdfe absorb id t)      = " %7.4f TWFE_2x2
di "-" * 60
di "  Los cuatro deben ser IDÉNTICOS — equivalencia algebraica ✓"
di "=" * 60


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

di _n "=" * 65
di "  SECCIÓN 3: MECANISMO DEL SESGO POR VIOLACIÓN DE T. PARALELAS"
di "=" * 65
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
di "=" * 65

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

di _n "=" * 70
di "  RESUMEN: equivalencia DiD = FD = TWFE — ¿cuándo y cuánto importa?"
di "=" * 70
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
di "=" * 70
