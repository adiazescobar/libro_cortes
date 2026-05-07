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
set linesize 100

********************************************************************************
* SECCIÓN A: DiD básico con base3.dta
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
matrix b = e(b)
scalar atet = el(b, 1, 1)

di _n "=== SECCIÓN B: ANOTA ESTOS VALORES ==="
di "B1. Estimador ATET (efecto del nuevo procedimiento): " %8.4f atet

* B2. Prueba de tendencias paralelas
estat ptrends
* Anota F-stat y p-valor

* B3. Prueba de anticipación
estat granger
* Anota F-stat y p-valor

di _n "=== FIN DEL EJERCICIO ==="
di "Ahora ingresa tus respuestas en el formulario del libro."
