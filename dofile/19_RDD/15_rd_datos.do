/*================================================================
  Econometría Avanzada 2026-2 · Pontificia Universidad Javeriana
  Clase: RD nítida y RD difusa · Datos simulados
  Ana María Díaz Escobar

  Paquetes (instalar una sola vez):
    net install rdrobust,  from(https://raw.githubusercontent.com/rdpackages/rdrobust/master/stata)  replace
    net install rddensity, from(https://raw.githubusercontent.com/rdpackages/rddensity/master/stata) replace
    net install lpdensity, from(https://raw.githubusercontent.com/nppackages/lpdensity/master/stata) replace
================================================================*/

clear all
set more off


*================================================================
* PARTE 1. RD NÍTIDA: beca si Saber 11 >= 310
*================================================================
set seed 1105
set obs 5000

* Z = puntaje Saber 11. Beca si Z >= 310. Y = Saber Pro.
* El efecto verdadero de la beca es 8 puntos.
generate habilidad  = rnormal()
generate motivacion = rnormal()
generate Z = round(300 + 40*habilidad + rnormal(0, 20), 0.1)
generate Zc = Z - 310          // Z centrada en el corte
generate mujer   = runiform() < 0.52
generate oficial = runiform() < invlogit(1 - 0.02*(Z - 300))

generate D = Z >= 310
generate y = 150 + 0.45*Zc - 0.0025*Zc^2 ///
           + 4*habilidad + 4*motivacion + 3*mujer - 2*oficial + 8*D + rnormal(0, 6)

keep if inrange(Z, 200, 420)

*--- Para después: ¿y si hay manipulación? ---------------------------
* Los motivados que sacaron entre 300 y 310 piden recalificación
* y quedan justo por encima de 310
generate Zm = Z
replace  Zm = 310 + runiform()*4 if inrange(Z, 300, 309.9) & motivacion > 0
generate Dm = Zm >= 310
generate ym = y - 8*D + 8*Dm        // misma y, pero con la beca nueva


*================================================================
* PARTE 2. RD DIFUSA: pasar 310 da derecho a la beca, pero no todos
* la toman. Correr esta parte cuando lleguemos a la difusa
* (borra los datos de la parte 1).
*================================================================
clear
set seed 2026
set obs 20000

generate habilidad  = rnormal()
generate motivacion = rnormal()
generate Z = round(300 + 40*habilidad + rnormal(0, 20), 0.1)
generate Zc = Z - 310
generate mujer   = runiform() < 0.52
generate oficial = runiform() < invlogit(1 - 0.02*(Z - 300))

generate W = Z >= 310        // elegible: pasó el corte (el instrumento)

* Quién tiene beca de verdad (D):
generate u = runiform()
generate D = (u < 0.1) | (W == 1 & u < 0.7)
* Con menos de 310: 10 % tiene beca de todas formas
* Con 310 o más:    70 % la toma

generate y = 150 + 0.45*Zc - 0.0025*Zc^2 ///
           + 4*habilidad + 4*motivacion + 3*mujer - 2*oficial + 8*D + rnormal(0, 6)

keep if inrange(Z, 200, 420)

* Para ivregress: interacciones como variables aparte
generate WZc  = W*Zc
generate Zc2  = Zc^2
generate WZc2 = W*Zc^2
