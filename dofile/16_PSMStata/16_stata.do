********************************************************************************
* PSM en Stata — Sesión completa con base6.dta
* Ana María Díaz Escobar — Pontificia Universidad Javeriana
* Econometría Avanzada
*
* Base: base6.dta
* Tratamiento: D
* Resultado: y2
* Covariables: personas orden_n ocupado_jefe educa_jefe ingresos_hogar_jefe hombre
********************************************************************************

clear all
set more off
capture log close

* ---- Ajusta esta ruta a tu carpeta de trabajo ----------------------------
* cd "ruta_a_tu_carpeta"
use "base6.dta", clear
log using "log_psm.txt", replace

global X "personas orden_n ocupado_jefe educa_jefe ingresos_hogar_jefe hombre"

********************************************************************************
* 1. ESTADÍSTICAS DESCRIPTIVAS
********************************************************************************

tab D
table D, stat(mean y1 y2)

* Diferencia cruda (sin ajustar) entre tratados y controles
reg y2 D, robust

********************************************************************************
* 2. MODELO DE SELECCIÓN (PROPENSITY SCORE)
********************************************************************************

* dprobit: efectos marginales de cada covariable sobre P(D=1)
dprobit D $X

* Selección de variables: stepwise (nivel de significancia 10%)
sw, pe(.1): probit D $X

* Especificación final: incluir todas las covariables
global X "personas orden_n ocupado_jefe educa_jefe ingresos_hogar_jefe hombre"
dprobit D $X

********************************************************************************
* 3. PROPENSITY SCORE: ESTIMACIÓN Y DIAGNÓSTICO VISUAL
********************************************************************************

predict double ps1
label variable ps1 "Propensity Score estimado"
sum ps1

* Distribución del PS por grupo
histogram ps1, by(D) xtitle("Propensity Score")

* Gráfico de densidades superpuestas: verifica soporte común
twoway (kdensity ps1 if D==1, lcolor(navy)  lwidth(medthick)) ///
       (kdensity ps1 if D==0, lcolor(cranberry) lwidth(medthick)), ///
       legend(label(1 "Tratados (D=1)") label(2 "Controles (D=0)")) ///
       xtitle("Propensity Score estimado") ///
       title("Distribución del PS: soporte común") ///
       note("Si hay superposición amplia, el soporte común es suficiente")

* Calidad del PS: si el PS es suficiente, las covariables pierden significancia
di as text "--- PS sin incluir: covariables significativas ---"
dprobit D $X

di as text "--- PS incluido: covariables deben perder significancia ---"
dprobit D ps1 $X

********************************************************************************
* 4. PREPARAR ORDEN ALEATORIO (necesario para NN sin reemplazo)
********************************************************************************

set seed 50
drawnorm orden
sort orden

********************************************************************************
* 5. INSTALAR psmatch2 (solo si no está instalado)
********************************************************************************
* ssc install psmatch2, replace

********************************************************************************
* 5A. NN(1) SIN REEMPLAZO CON SOPORTE COMÚN
********************************************************************************

psmatch2 D $X, outcome(y2) n(1) com noreplace
psgraph
pstest $X, both graph

* Interpretar la tabla pstest:
*   %Bias < 20% : balance aceptable
*   %Bias <  5% : balance excelente
*   Pseudo R2 post-match ≈ 0 : el PS ya no discrimina entre grupos
*   LR chi2 post-match no significativo : covariables balanceadas

********************************************************************************
* 5B. NN(1) CON TRIMMING (descarta 20% de tratados con PS más alto)
********************************************************************************

psmatch2 D $X, outcome(y2) n(1) trim(20)
psgraph

********************************************************************************
* 5C. ATE (efecto sobre toda la población, no solo los tratados)
********************************************************************************

* ATE con reemplazo
psmatch2 D $X, outcome(y2) n(1) com ate
psgraph
pstest $X

* ATE sin reemplazo
psmatch2 D $X, outcome(y2) n(1) com ate noreplace
psgraph
pstest $X

********************************************************************************
* 5D. NN CON MÚLTIPLES VECINOS
********************************************************************************

psmatch2 D $X, outcome(y2) n(5)  com
psmatch2 D $X, outcome(y2) n(10) com
psmatch2 D $X, outcome(y2) n(5)  trim(20)
psmatch2 D $X, outcome(y2) n(10) trim(20)

********************************************************************************
* 5E. CALIPER (distancia máxima permitida en PS)
********************************************************************************

psmatch2 D $X, outcome(y2) caliper(0.0001) com
psgraph
pstest $X

psmatch2 D $X, outcome(y2) caliper(0.001) com
psgraph
pstest $X

********************************************************************************
* 5F. RADIO (todos los controles dentro del caliper)
********************************************************************************

psmatch2 D $X, outcome(y2) radius caliper(0.001) com
psgraph
pstest $X

psmatch2 D $X, outcome(y2) radius caliper(0.001) trim(20)
psgraph

psmatch2 D $X, outcome(y2) radius caliper(0.005) trim(20)
psgraph

********************************************************************************
* 5G. KERNEL (pesos decrecientes con la distancia al PS)
********************************************************************************

* Kernel Epanechnikov (por defecto — óptimo asintótico)
psmatch2 D $X, outcome(y2) com kernel
psgraph
pstest $X

* Kernel Gaussiano
psmatch2 D $X, outcome(y2) com kernel kerneltype(normal) bwidth(0.06)

* Kernel uniforme
psmatch2 D $X, outcome(y2) com kernel kerneltype(uniform)

* Bootstrap con kernel (1000 réplicas — tarda varios minutos)
bootstrap r(att), reps(1000) seed(2025) : psmatch2 D $X, outcome(y2) com kernel

********************************************************************************
* 5H. REGRESIÓN LOCAL LINEAL (LLR)
********************************************************************************

psmatch2 D $X, llr outcome(y2) common
bootstrap r(att), reps(1000) seed(2025) : psmatch2 D $X, llr outcome(y2) common

psmatch2 D $X, llr outcome(y2) trim(20)
bootstrap r(att), reps(1000) seed(2025) : psmatch2 D $X, llr outcome(y2) trim(20)

********************************************************************************
* 6. ERRORES ESTÁNDAR ANALÍTICOS DE ABADIE-IMBENS
*    Solo funciona con matching CON reemplazo
********************************************************************************

* NN(1) con reemplazo y ES analíticos (ai = n de vecinos para estimar varianza)
psmatch2 D $X, outcome(y2) n(1) com ai(1)
psgraph
pstest $X, both graph

* NN(5) con reemplazo y ES analíticos
psmatch2 D $X, outcome(y2) n(5) com ai(4)

********************************************************************************
* 7. ANÁLISIS DE SENSIBILIDAD: sensatt
*    ¿Cuán grande debe ser una variable no observada para cambiar los resultados?
********************************************************************************

* ssc install pscore, replace
* ssc install sensatt, replace

* Los 4 parámetros describen la correlación de la variable no observada:
*   p11 = P(u=1 | D=1, Y=1)    p10 = P(u=1 | D=1, Y=0)
*   p01 = P(u=1 | D=0, Y=1)    p00 = P(u=1 | D=0, Y=0)

sensatt des D $X, p11(0.6) p10(0.5) p01(0.5) p00(0.2)

********************************************************************************
* 8. PSM vs MCO: comparación directa
********************************************************************************

* Diferencia cruda
reg y2 D, robust

* OLS con covariables
reg y2 D $X, robust

* PSM: estimación principal (NN1, con reemplazo, SC)
psmatch2 D $X, outcome(y2) n(1) com ai(1)

* teffects psmatch (Stata nativo — SE correctos)
teffects psmatch (y2) (D $X, probit), atet
teffects psmatch (y2) (D $X, probit), ate nn(4)

log close
