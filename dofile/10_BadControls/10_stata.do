********************************************************************************
* Malos Controles en Stata
* Econometria Avanzada - Ana Maria Diaz
* Tres casos: mediador, colisionador, proxy contaminado
* Reproducible con set seed. Variables con nombres intuitivos.
********************************************************************************

clear all
set more off

********************************************************************************
* CASO 1: MEDIADOR / POST-TRATAMIENTO
* Estructura: tratamiento -> mediador -> resultado
* Pregunta: ¿cuál es el efecto TOTAL del tratamiento sobre el resultado?
* Valor verdadero: a × b = 2 × 1 = 2
* Con mal control (mediador): estimado ≈ 0
********************************************************************************

set seed 2468
set obs 10000

scalar a = 2         // efecto del tratamiento sobre el mediador
scalar b = 1         // efecto del mediador sobre el resultado

gen byte   tratamiento = (runiform() < 0.5)           // D: aleatorio
gen double mediador    = a * tratamiento + rnormal()   // M: post-tratamiento
gen double resultado   = b * mediador    + rnormal()   // Y: determinado por M

di as text "=== CASO 1: Efecto TOTAL (correcto) — coef ≈ 2 ==="
reg resultado tratamiento, vce(robust)

di as text "=== CASO 1: Con mediador (MAL CONTROL) — coef ≈ 0 ==="
reg resultado tratamiento mediador, vce(robust)


********************************************************************************
* MONTE CARLO — CASO 1: MEDIADOR
* Muestra que el sesgo es sistemático (no accidental)
********************************************************************************

capture program drop mc_mediador
program define mc_mediador, rclass
    clear
    set obs 3000
    gen byte   trat = (runiform() < 0.5)
    gen double med  = 2 * trat + rnormal()
    gen double res  = 1 * med  + rnormal()
    quietly reg res trat
    return scalar efecto_total   = _b[trat]
    quietly reg res trat med
    return scalar efecto_directo = _b[trat]
end

simulate efecto_total=r(efecto_total) efecto_directo=r(efecto_directo), ///
    reps(300) seed(2468): mc_mediador

di as text "=== MC Caso 1: promedio total ≈ 2; promedio directo ≈ 0 ==="
summarize efecto_total efecto_directo

twoway (hist efecto_total,   width(.05) color(navy%50))   ///
       (hist efecto_directo, width(.05) color(red%50)),   ///
       legend(order(1 "Efecto total (correcto)" 2 "Con mediador (mal control)")) ///
       xline(2, lcolor(navy) lpattern(dash))              ///
       xline(0, lcolor(red)  lpattern(dash))              ///
       title("Mediador: distribución del coeficiente de tratamiento") ///
       xtitle("Estimado de tratamiento") ytitle("Frecuencia")


********************************************************************************
* CASO 2: COLISIONADOR / SELECCIÓN ENDÓGENA
* Estructura: D -> C <- U -> Y  (simplificado: D y Y independientes, C = f(D,Y))
* Valor verdadero de D sobre Y: 0
* Sin colisionador: ≈ 0 (correcto)
* Con colisionador: ≠ 0 (sesgo inducido al controlar)
********************************************************************************

clear all
set more off
set seed 12345
set obs 1000

gen double conexiones    = rnormal()   // D: tratamiento (conexiones laborales)
gen double productividad = rnormal()   // Y: resultado — independiente de D

* C es colisionador: causado por D y por Y (o por U que afecta Y)
gen double contratacion = 2 * conexiones - 0.5 * productividad + rnormal()

di as text "=== CASO 2: Sin colisionador (correcto) — coef ≈ 0 ==="
reg productividad conexiones, vce(robust)

di as text "=== CASO 2: Con colisionador (MAL CONTROL) — coef ≠ 0 ==="
reg productividad conexiones contratacion, vce(robust)


********************************************************************************
* MONTE CARLO — CASO 2: COLISIONADOR
********************************************************************************

capture program drop mc_colisionador
program define mc_colisionador, rclass
    clear
    set obs 1000
    gen double conexiones    = rnormal()
    gen double productividad = rnormal()
    gen double contratacion  = 2 * conexiones - 0.5 * productividad + rnormal()
    quietly reg productividad conexiones
    return scalar b_sin_c = _b[conexiones]
    quietly reg productividad conexiones contratacion
    return scalar b_con_c = _b[conexiones]
end

simulate b_sin_c=r(b_sin_c) b_con_c=r(b_con_c), ///
    reps(300) seed(12345): mc_colisionador

di as text "=== MC Caso 2: b_sin_c ≈ 0; b_con_c ≠ 0 ==="
summarize b_sin_c b_con_c

twoway (hist b_sin_c, width(.03) color(navy%50))  ///
       (hist b_con_c, width(.03) color(red%50)),  ///
       legend(order(1 "Sin colisionador (correcto)" 2 "Con colisionador (sesgo)")) ///
       xline(0, lcolor(black) lpattern(dash))     ///
       title("Colisionador: distribución del coeficiente de conexiones") ///
       xtitle("Estimado de conexiones") ytitle("Frecuencia")


********************************************************************************
* CASO 3: PROXY CONTAMINADO
* Estructura: D -> L <- U -> Y  (L también afecta Y directamente)
* L parece proxy útil de U, pero también recibe el efecto de D
* Valor verdadero de D sobre Y: 2
* Sin L: sesgado por omisión de U (si D no es exógeno); correcto si D es exógeno
* Con L: sesgo adicional por contaminar el proxy con el tratamiento
* Con U directamente: correcto (referencia)
********************************************************************************

clear all
set more off
set seed 99999
set obs 2000

gen double habilidad_innata = rnormal()          // U: no observable
gen double educacion        = rnormal()           // D: exógeno en este DGP

* Proxy contaminado: test tomado después del programa (afectado por D y U)
gen double test_tardio = 0.8 * educacion + 1.2 * habilidad_innata + rnormal()

* Resultado: afectado por D (efecto = 2) y por U (habilidad también importa)
gen double salario = 2 * educacion + 1.5 * habilidad_innata + rnormal()

di as text "=== CASO 3: Sin control (D exógeno aquí) — coef no capta U ==="
reg salario educacion, vce(robust)

di as text "=== CASO 3: Con proxy contaminado (MAL CONTROL) — coef distorsionado ==="
reg salario educacion test_tardio, vce(robust)

di as text "=== CASO 3: Con U directamente (referencia ideal) — coef ≈ 2 ==="
reg salario educacion habilidad_innata, vce(robust)


********************************************************************************
* MONTE CARLO — CASO 3: PROXY CONTAMINADO
********************************************************************************

capture program drop mc_proxy
program define mc_proxy, rclass
    clear
    set obs 1000
    gen double U = rnormal()
    gen double D = rnormal()
    gen double L = 0.8 * D + 1.2 * U + rnormal()
    gen double Y = 2 * D + 1.5 * U + rnormal()
    quietly reg Y D
    return scalar b_sin_control   = _b[D]
    quietly reg Y D L
    return scalar b_proxy_malo    = _b[D]
    quietly reg Y D U
    return scalar b_control_bueno = _b[D]
end

simulate b_sin_control=r(b_sin_control)     ///
         b_proxy_malo=r(b_proxy_malo)       ///
         b_control_bueno=r(b_control_bueno), ///
    reps(300) seed(99999): mc_proxy

di as text "=== MC Caso 3: b_control_bueno ≈ 2; b_proxy_malo distorsionado ==="
summarize b_sin_control b_proxy_malo b_control_bueno

twoway (hist b_sin_control,   width(.04) color(gray%40))   ///
       (hist b_proxy_malo,    width(.04) color(red%50))    ///
       (hist b_control_bueno, width(.04) color(navy%50)),  ///
       legend(order(1 "Sin control" 2 "Proxy contaminado (mal)" 3 "Control correcto (U)")) ///
       xline(2, lcolor(black) lpattern(dash))              ///
       title("Proxy contaminado: distribución del coeficiente de educación") ///
       xtitle("Estimado de educación") ytitle("Frecuencia")

di as text "=== FIN DEL DO FILE ==="
