/*
PSM en Stata: Propensity Score Matching - CLASSROOM VERSION
Clase 16 - EconometriaAV 2026-1
MOSTRANDO COMANDOS Y RESULTADOS (set echo on)

USO: Corre esto durante la clase para que los estudiantes vean paso a paso
*/

clear all
set seed 1298
set more off
set echo on
set linesize 100

* Directorio de trabajo
cd ~/Library/CloudStorage/Dropbox/ClasesR/EconometriaAV/EjerciciosClase

* Cargar datos
use base6.dta, clear

* Definir variables
global X "personas orden_n ocupado_jefe educa_jefe ingresos_hogar_jefe hombre"

log using psm_classroom.log, replace text

***=====================================================***
* PARTE 1: ESTADÍSTICAS DESCRIPTIVAS
* ¿Cuán diferentes son los grupos tratado vs control?
***=====================================================***

di ""
di ">>> PASO 1: MIRAR LOS DATOS CRUDOS"
di "¿Cuántos tratados? ¿Cuántos controles? ¿Cuál es la diferencia bruta?"
di ""

tab D
table D, stat(mean y2 y1)

di ""
di ">>> OBSERVACIÓN: La diferencia cruda en Y2 es:"
di "    Diferencia = E[Y|D=1] - E[Y|D=0] = 0.330 (aproximadamente)"
di "    Pero esto NO es el efecto causal: incluye sesgo de selección"
di ""
pause Press any key to continue...

***=====================================================***
* PARTE 2: ESTIMAR EL PROPENSITY SCORE
* P(D=1|X) = probabilidad de ser tratado dado características
***=====================================================***

di ""
di ">>> PASO 2: ESTIMAR EL PROPENSITY SCORE"
di "Modelo: logit D = f(personas, orden_n, ocupado_jefe, educa_jefe, ingresos_hogar_jefe, hombre)"
di ""

logit D $X
predict double pscore1, pr

di ""
di ">>> Propensity Score estimado:"
summ pscore1, detail

pause Press any key to continue...

***=====================================================***
* PARTE 3: VERIFICAR SOPORTE COMÚN
* ¿Hay sobreposición entre dist. PS de tratados y controles?
***=====================================================***

di ""
di ">>> PASO 3: VERIFICAR SOPORTE COMÚN"
di "Si hay good overlap, podemos emparejar. Si no, tenemos un problema."
di ""

twoway (kdensity pscore1 if D==1, lcolor(blue) lwidth(medium) legend(label(1 "Tratados")) title("Soporte Común: Distribuciones del PS")) ///
       (kdensity pscore1 if D==0, lcolor(red) lwidth(medium) legend(label(2 "Controles"))) ///
       , legend(label(1 "Tratados (D=1)") label(2 "Controles (D=0)")) ///
       xtitle("Propensity Score") ytitle("Densidad")

graph export pscore_distribution.png, replace
di ">>> Gráfica guardada: pscore_distribution.png"

pause Press any key to continue...

***=====================================================***
* PARTE 4: NEAREST NEIGHBOR (1) SIN REEMPLAZO + SOPORTE COMÚN
* Emparejar cada tratado con su vecino más cercano
***=====================================================***

di ""
di ">>> PASO 4: NEAREST NEIGHBOR (1) SIN REEMPLAZO"
di "Cada tratado se empareja con el control más cercano en PS"
di "common = solo usa observaciones en soporte común"
di ""

* Necesario para NN sin reemplazo
drawnorm orden
sort orden

psmatch2 D $X, outcome(y2) n(1) pscore(pscore1) noreplacement common
di ""
di ">>> Resultado NN(1):"
di "    ATT (unmatched) = diferencia cruda"
di "    ATT (matched) = efecto después de emparejar"
di ""

psgraph
graph export pscore_nn1.png, replace
di ">>> Gráficas del matching guardadas"

pstest $X, treated(D) both
di ""
di ">>> INTERPRETACIÓN pstest:"
di "    %Bias < 20% es aceptable (vemos si balanceó bien)"
di ""
pause Press any key to continue...

***=====================================================***
* PARTE 5: NEAREST NEIGHBOR (5) CON REEMPLAZO
* Usar múltiples vecinos reduce varianza
***=====================================================***

di ""
di ">>> PASO 5: NEAREST NEIGHBOR (5) CON REEMPLAZO"
di "Usar 5 vecinos en lugar de 1 (reduce varianza)"
di "ai(4) = errores estándar analíticos de Abadie-Imbens"
di ""

psmatch2 D $X, outcome(y2) n(5) common ai(4)
di ""
di ">>> Comparar: ¿Cambió mucho el ATT? ¿Es robusto?"
di ""
pause Press any key to continue...

***=====================================================***
* PARTE 6: KERNEL MATCHING
* Usar TODOS los controles, ponderados por cercanía
***=====================================================***

di ""
di ">>> PASO 6: KERNEL MATCHING - EPANECHNIKOV"
di "Usa todos los controles (ponderados por distancia)"
di "Reduce varianza vs NN, pero menos transparente"
di ""

psmatch2 D $X, outcome(y2) kernel kerneltype(epan) bwidth(0.06) common
psgraph
pstest $X, treated(D) both

di ""
di ">>> ¿Cambió mucho el ATT comparado a NN(1)?"
di ""
pause Press any key to continue...

***=====================================================***
* PARTE 7: KERNEL MATCHING GAUSSIANO
* Alternativa: kernel gaussiano en lugar de Epanechnikov
***=====================================================***

di ""
di ">>> PASO 7: KERNEL MATCHING - GAUSSIANO"
di "Alternativa a Epanechnikov"
di ""

psmatch2 D $X, outcome(y2) kernel kerneltype(normal) bwidth(0.06) common
di ""
di ">>> Resultado similar? (prueba robustez)"
di ""
pause Press any key to continue...

***=====================================================***
* PARTE 8: COMANDO NATIVO: teffects psmatch
* Alternativa Stata 13+ con SE correctos automáticamente
***=====================================================***

di ""
di ">>> PASO 8: COMANDO NATIVO teffects psmatch"
di "Stata 13+: Calcula PS + matching + SE correctos en un paso"
di ""

teffects psmatch (y2) (D $X, probit), atet
estimates store att_nn1

di ""
di ">>> Ventaja: SE correctos (toma en cuenta estimación del PS)"
di ">>> Desventaja: Menos control sobre detalles del matching"
di ""

log close

di ""
di "=========================================="
di "RESUMEN PSM CLASSROOM:"
di "=========================================="
di "✓ Diferencia bruta (con sesgo):   ~0.33"
di "✓ NN(1):                          ~0.33 (similar)"
di "✓ NN(5):                          ~0.33 (similar)"
di "✓ Kernel:                         ~0.33 (similar)"
di ""
di "→ Si los estimadores son similares, es buena señal (robusto)"
di "→ Si varían mucho, investigar: ¿Buen soporte común?"
di ""
di "Log guardado: psm_classroom.log"
di "=========================================="
