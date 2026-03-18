********************************************************************************
* EJERCICIO — Diferencias en Diferencias
* Econometría Avanzada — Pontificia Universidad Javeriana
*
* Instrucciones:
*   1. Corre este do-file completo
*   2. Anota los números que aparecen al final de cada sección
*   3. Ingresa tus respuestas en el formulario del libro
********************************************************************************

clear all
set more off

********************************************************************************
* SECCIÓN A: DiD básico con base3.dta
********************************************************************************

cd "ESCRIBE_AQUI_LA_RUTA_DE_TU_CARPETA"   // <--- cambia esto
use "base3.dta", clear

* A1. Calcula las cuatro medias de la tabla 2×2
quietly sum y if D==0 & t==0
scalar y_c0 = r(mean)
quietly sum y if D==0 & t==1
scalar y_c1 = r(mean)
quietly sum y if D==1 & t==0
scalar y_t0 = r(mean)
quietly sum y if D==1 & t==1
scalar y_t1 = r(mean)

di _n "=== SECCIÓN A: ANOTA ESTOS VALORES ==="
di "A1a. Media controles ANTES:   " %8.4f y_c0
di "A1b. Media controles DESPUÉS: " %8.4f y_c1
di "A1c. Media tratados ANTES:    " %8.4f y_t0
di "A1d. Media tratados DESPUÉS:  " %8.4f y_t1

* A2. Estimador DiD manual
scalar DD = (y_t1 - y_t0) - (y_c1 - y_c0)
di "A2.  Estimador DiD (manual):  " %8.4f DD

* A3. DiD con regresión
reg y D##t, robust
di "A3.  Estimador DiD (regresión): " %8.4f _b[1.D#1.t]
di "     (¿coincide con A2?)"

* A4. Test de balance en t=0
ttest y if t == 0, by(D)
* Anota el p-valor del test bilateral (Ha: diff != 0)

********************************************************************************
* SECCIÓN B: DiD con múltiples periodos — hospdd
********************************************************************************

webuse hospdd, clear
xtset hospital
xtdidregress (satis) (procedure), group(hospital) time(month)

di _n "=== SECCIÓN B: ANOTA ESTOS VALORES ==="
di "B1. Estimador ATET (efecto del nuevo procedimiento): " %8.4f _b[1bn.procedure]

* B2. Prueba de tendencias paralelas
estat ptrends
* Anota F-stat y p-valor

* B3. Prueba de anticipación
estat granger
* Anota F-stat y p-valor

di _n "=== FIN DEL EJERCICIO ==="
di "Ahora ingresa tus respuestas en el formulario del libro."
