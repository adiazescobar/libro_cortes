********************************************************************************
* Diferencias en Diferencias — Econometría Avanzada
* Ana María Díaz Escobar — Pontificia Universidad Javeriana
*
* PARTE 1: DiD básico 2×2 con base3.dta
* PARTE 2: DiD con múltiples periodos — hospdd (Stata built-in)
*          Prueba de tendencias paralelas y anticipación con xtdidregress
********************************************************************************

clear all
set more off
set linesize 100

********************************************************************************
* PARTE 1: DiD BÁSICO (2 periodos, sin ID individual)
* Base: base3.dta — niños con dos periodos de seguimiento
* Resultado: talla-para-edad (z-score)
* Tratamiento: programa de nutrición (D=1)
********************************************************************************

capture confirm file "base3.dta"
if _rc {
    capture confirm file "dofile/08_DID/base3.dta"
    if !_rc cd "dofile/08_DID"
}
capture confirm file "base3.dta"
if _rc {
    di as error "No se encontró base3.dta. Corre este do-file desde su carpeta o desde la raíz del libro."
    exit 601
}
use "base3.dta", clear

* ---- Estructura de los datos ------------------------------------------------
tab t D
tab t, sum(y)

* ---- Etiquetas para gráficos ------------------------------------------------
label define periodo  0 "Antes" 1 "Después", replace
label define grupo    0 "Control" 1 "Tratado", replace
label value t periodo
label value D grupo

* ---- Tabla 2×2: medias por grupo y periodo ----------------------------------
table D t, stat(mean y) nformat(%6.3f)

* ---- Gráfico de tendencias (visualización del DiD, NO prueba de tendencias) -
preserve
collapse (mean) y, by(t D)
twoway (connected y t if D==1, msymbol(circle)   lcolor(navy)   lwidth(medium)) ///
       (connected y t if D==0, msymbol(triangle)  lcolor(maroon) lwidth(medium)), ///
       legend(label(1 "Tratados") label(2 "Controles") pos(6) row(1)) ///
       title("Evolución de la talla-para-edad por grupo", size(medium))       ///
       note("Nota: con solo 2 periodos este gráfico visualiza el DiD." ///
            "No es una prueba de tendencias paralelas.")                       ///
       xtitle("Periodo") ytitle("Talla-para-edad (z-score)")                  ///
       xlabel(0 "Antes" 1 "Después")
restore

* ---- Test de balance en t=0 (¿eran similares antes del programa?) ----------
* Esto NO es prueba de tendencias paralelas — es prueba de balance
ttest y if t == 0, by(D)

* ---- Estimador DiD manual ---------------------------------------------------
quietly sum y if D==0 & t==0
scalar y_c0 = r(mean)

quietly sum y if D==0 & t==1
scalar y_c1 = r(mean)

quietly sum y if D==1 & t==0
scalar y_t0 = r(mean)

quietly sum y if D==1 & t==1
scalar y_t1 = r(mean)

scalar delta_trat = y_t1 - y_t0
scalar delta_ctrl = y_c1 - y_c0
scalar DD         = delta_trat - delta_ctrl

di _n "=== ESTIMADOR DiD MANUAL ==="
di "Cambio en tratados:   " %6.4f delta_trat
di "Cambio en controles:  " %6.4f delta_ctrl
di "DiD (efecto causal):  " %6.4f DD

* ---- Comando diff -----------------------------------------------------------
* diff reporta la tabla 2×2 + DiD + error estándar
ssc install diff, replace
diff y, t(D) p(t)

* La opción test() de diff hace t-tests de balance en t=0 — NO prueba tendencias
* diff y, t(D) p(t) test cov(orden_n)

* ---- Regresión DiD ----------------------------------------------------------
reg y D##t, robust
* El coeficiente de 1.D#1.t es el estimador DiD (debe coincidir con DD manual)

********************************************************************************
* PARTE 2: DiD CON MÚLTIPLES PERIODOS — base hospdd (Stata built-in)
*
* Contexto: Hospitales que adoptan en distintos momentos un nuevo procedimiento
*           de admisión (tratamiento escalonado). Resultado: satisfacción de pacientes.
*
* Comandos nuevos:
*   xtdidregress  — DiD para panel con múltiples periodos y adopción escalonada
*   estat trendplots — gráfico diagnóstico: medias observadas + tendencias lineales
*   estat ptrends    — prueba formal de TENDENCIAS PARALELAS (H0: pendientes iguales)
*   estat granger    — prueba formal de ANTICIPACIÓN     (H0: sin efectos previos)
*   estat grangerplot — event study: efectos por periodo (pre y post)
********************************************************************************

webuse hospdd, clear

* ---- Variables clave --------------------------------------------------------
* hospital:   identificador de hospital (panel)
* month:      mes (variable temporal)
* procedure:  1 si el hospital adoptó el nuevo procedimiento ese mes (tratamiento)
* satis:      puntaje de satisfacción de pacientes (resultado)

describe
tab month procedure

* Nota: hospdd tiene múltiples pacientes por hospital-mes (datos de pacientes,
* no panel hospital-mes). Se hace xtset hospital (solo panel, sin tiempo) y
* se especifica el tiempo dentro de xtdidregress con time(month).
xtset hospital

* ---- Vistazo de la estructura: ¿cuándo adopta cada hospital? ---------------
preserve
collapse (max) procedure, by(hospital month)
sort hospital month
* Mostrar solo primeros 10 hospitales para no saturar la pantalla
list if hospital <= 10, sepby(hospital) noobs abbrev(12)
restore

* ---- DiD con múltiples periodos: xtdidregress -------------------------------
* Sintaxis: xtdidregress (resultado) (tratamiento), group(id) time(mes)
xtdidregress (satis) (procedure), group(hospital) time(month)

* ---- Gráfico diagnóstico de tendencias -------------------------------------
* Muestra medias observadas por grupo (tratados vs controles) a lo largo del tiempo
* y las tendencias lineales ajustadas. Referencia vertical = inicio del tratamiento.
estat trendplots
* Opción extra: solo medias observadas
estat trendplots, omeans
* Opción extra: solo tendencias lineales
estat trendplots, ltrends

* ---- Prueba de TENDENCIAS PARALELAS: estat ptrends -------------------------
* H0: Las tendencias lineales son paralelas en el periodo pre-tratamiento
* Mecánica: regresión en periodos pre-tratamiento con interacción tratado×tiempo
*           y prueba F de ese coeficiente
* Requiere: al menos 2 periodos ANTES del tratamiento + ID de panel
estat ptrends

* Interpretación:
* p > 0.05 → no rechazamos H0 → las tendencias pre-tratamiento eran paralelas
*             (apoyo al supuesto)
* p < 0.05 → rechazamos H0 → hay evidencia de pendientes distintas antes del
*             tratamiento (el supuesto es cuestionable)

* ---- Prueba de ANTICIPACIÓN: estat granger ----------------------------------
* H0: No hubo efectos del tratamiento ANTES de que empezara (sin anticipación)
* Mecánica: crea indicadores de "leads" por cada periodo pre-tratamiento
*           (= 1 si tratado y tiempo ya superó ese umbral) y los prueba conjuntamente
* Esta prueba es DISTINTA a ptrends: pregunta si el futuro tratamiento
* predice el resultado presente — no si las tendencias eran paralelas
estat granger

* Interpretación:
* p > 0.05 → no rechazamos H0 → sin evidencia de anticipación (favorable)
* p < 0.05 → hay efectos en periodos previos al tratamiento. Dos posibilidades:
*             (a) los agentes anticiparon el programa y cambiaron su comportamiento
*             (b) las tendencias no eran paralelas
*             → requiere argumento teórico para distinguirlas

* ---- Event study: efectos por periodo (pre y post) -------------------------
* Muestra el estimador DiD para cada periodo relativo al inicio del tratamiento
* En periodos pre (k<0): deben ser ≈ 0 si no hay anticipación ni pre-tendencias
* En periodos post (k≥0): muestran la dinámica del efecto en el tiempo
estat grangerplot

* Con intervalos de confianza más amplios:
estat grangerplot, level(90)

di _n "=== NOTAS CLAVE ==="
di "1. estat ptrends  → prueba de TENDENCIAS PARALELAS (pendiente pre-tratamiento)"
di "2. estat granger  → prueba de ANTICIPACIÓN (efectos antes del tratamiento)"
di "3. Ambas requieren ≥ 2 periodos pre-tratamiento (con 2 periodos: imposible)"
di "4. Un rechazo en ptrends ≠ un rechazo en granger: preguntas distintas"
di "5. Siempre se necesita argumento teórico para separar los dos casos"


********************************************************************************
* EXPORT CANÓNICO — resultados auditables para el libro
* Genera results/did_resultados.csv (leído por 08-DIDStata.Rmd) y el insumo
* no rastreado results/hospdd_verificacion_input.csv para la verificación
* cruzada en Python. Identificadores estables por escenario, no posiciones.
********************************************************************************

capture mkdir results

tempname res
postfile `res' str32 escenario str40 comando str24 cantidad ///
    double valor_stata double ee double estadistico double p_valor double N ///
    str16 fuente using "results/did_resultados.dta", replace

* ---- Base 2×2: medias por celda ---------------------------------------------
use "base3.dta", clear

quietly sum y if D==0 & t==0
local m_c0 = r(mean)
post `res' ("media_C0") ("sum") ("media") (r(mean)) (.) (.) (.) (r(N)) ("08_DID.do")

quietly sum y if D==0 & t==1
local m_c1 = r(mean)
post `res' ("media_C1") ("sum") ("media") (r(mean)) (.) (.) (.) (r(N)) ("08_DID.do")

quietly sum y if D==1 & t==0
local m_t0 = r(mean)
post `res' ("media_T0") ("sum") ("media") (r(mean)) (.) (.) (.) (r(N)) ("08_DID.do")

quietly sum y if D==1 & t==1
local m_t1 = r(mean)
post `res' ("media_T1") ("sum") ("media") (r(mean)) (.) (.) (.) (r(N)) ("08_DID.do")

* ---- DiD manual (segunda diferencia de las cuatro medias) -------------------
local did_manual = (`m_t1' - `m_t0') - (`m_c1' - `m_c0')
quietly count
post `res' ("did_manual") ("segunda diferencia") ("DiD") (`did_manual') (.) (.) (.) (r(N)) ("08_DID.do")

* ---- Comando diff -----------------------------------------------------------
quietly diff y, t(D) p(t)
local did_diff = r(did)
local se_diff  = r(se_dd)
local N_diff   = r(N)
local p_diff   = 2*ttail(`N_diff' - 4, abs(`did_diff'/`se_diff'))
post `res' ("did_diff") ("diff y, t(D) p(t)") ("DiD") (`did_diff') (`se_diff') (.) (`p_diff') (`N_diff') ("08_DID.do")

* ---- Regresión DiD con errores robustos -------------------------------------
quietly reg y D##t, robust
local b_dd  = _b[1.D#1.t]
local se_dd = _se[1.D#1.t]
local p_dd  = 2*ttail(e(df_r), abs(`b_dd'/`se_dd'))
local N_reg = e(N)
post `res' ("did_regresion") ("reg y D##t, robust") ("DiD") (`b_dd') (`se_dd') (.) (`p_dd') (`N_reg') ("08_DID.do")

* ---- hospdd: ATET y pruebas formales ----------------------------------------
webuse hospdd, clear
xtset hospital
quietly xtdidregress (satis) (procedure), group(hospital) time(month)
local atet    = _b[r1vs0.procedure]
local se_atet = _se[r1vs0.procedure]
local p_atet  = 2*ttail(e(df_r), abs(`atet'/`se_atet'))
local N_hosp  = e(N)
post `res' ("hospdd_atet") ("xtdidregress") ("ATET") (`atet') (`se_atet') (.) (`p_atet') (`N_hosp') ("08_DID.do")

quietly estat ptrends
post `res' ("ptrends") ("estat ptrends") ("F") (r(F)) (.) (r(F)) (r(p)) (r(N)) ("08_DID.do")

quietly estat granger
post `res' ("granger") ("estat granger") ("F") (r(F)) (.) (r(F)) (r(p)) (r(N)) ("08_DID.do")

postclose `res'

* ---- Exportar CSV canónico y limpiar el dta intermedio ----------------------
preserve
use "results/did_resultados.dta", clear
export delimited using "results/did_resultados.csv", replace
restore
erase "results/did_resultados.dta"

* ---- Insumo para verificación cruzada en Python (no rastreado) --------------
webuse hospdd, clear
export delimited hospital month procedure satis ///
    using "results/hospdd_verificacion_input.csv", replace nolabel

di _n "=== EXPORT CANÓNICO COMPLETO: results/did_resultados.csv ==="
