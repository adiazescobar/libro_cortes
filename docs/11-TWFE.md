# TWFE y DiD Moderno en Stata

##  Contexto rápido {-}

* **¿Qué verás?** Cómo se comporta el estimador **TWFE** (two-way fixed effects) en distintos escenarios de DiD: 2×2, panel largo, más de dos unidades con **heterogeneidad de efectos**, y **adopción escalonada**.
* **¿Por qué importa?** Con heterogeneidad y/o timing escalonado, TWFE puede promediar **mal** (incluso con **pesos negativos**), sesgando el estimador.
* **¿Cómo lo detectamos?** Con la **descomposición de Bacon** y estudios de eventos.
* **¿Qué alternativas hay?** Estimadores modernos: **CSDID (Callaway & Sant’Anna)**, **did\_imputation (BJS)**, **eventstudyinteract (Sun & Abraham)**, **did\_multiplegt**, **did2s**, **stackedev**, etc.

> 💡 **Nota de ejecución**: Algunas secciones usan comandos/paquetes de SSC. Debes instalarlos al menos una vez (abajo incluyo la instalación con `ssc install …, replace`).

---

## Modelo básico 2×2 DiD {-}

### Explicación didáctica {-}

* Construimos un panel simple con **2 unidades** (`id = 1,2`) y **2 periodos** (`t = 1979, 1980`).
* Tratamiento **solo** para `id==2` en el **año final** (`D = 1` si `id==2 & t==fin`).
* Generamos un outcome `Y` con nivel por unidad (`id`), tendencia temporal (`3*t`) y un “empujón” de tratamiento cuando `D==1` (`tau=2`).
* Graficamos la evolución de `Y` por grupo (control vs tratado).
* Estimamos DiD con:

  * `xtreg Y D t, fe` (FE de unidad + control lineal por `t`), y
  * `reghdfe Y D, absorb(id t)` (FE completos de unidad y de tiempo).

### Código (copiar/pegar)

```stata
*----------------------------------------------------------
* Archivo DO: TWFE y Diferencias en Diferencias (DiD)
*  modelo TWFE, DiD y Triple DiD
* Autor: Ana Maria Diaz
* ----------------------------------------------------------

* -------------------------
* 1. Modelo básico 2x2 DiD
* -------------------------

clear
* Definir parámetros flexibles
local unidades = 2
local inicio = 1979
local fin    = 1980

local tiempo = `fin' - `inicio' + 1
local obs    = `unidades' * `tiempo'
set obs `obs'

* Crear variables id y t (compatibles con años)
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

* Generar tratamiento (id==2 tratado en año final)
gen D = id==2 & t==`fin'
label variable D "Tratamiento (id==2 en año final)"

* Efecto del tratamiento
gen tau = cond(D==1, 2, 0)
gen Y = id + 3*t + tau*D 
*gen Y = id + 3*t + tau*D + rnormal()
label variable Y "Variable dependiente"

twoway ///
  (connected Y t if id==1, msymbol(circle) lcolor(blue)) ///
  (connected Y t if id==2, msymbol(triangle) lcolor(red)) ///
  , ///
  title("Evolución de Y: Control vs Tratado") ///
  xtitle("Año") ytitle("Y") ///
  xlabel(`inicio'(1)`fin') ///
  legend(order(1 "Control (id=1)" 2 "Tratado (id=2)")) ///
  name(grafico1, replace)

```
![Evolución de Y](dofile/11_TWFE/g1.png)

```stata
* Estimar DiD con efectos fijos
xtreg Y D t, fe

* Alternativa con reghdfe
reghdfe Y D, absorb(id t)
```

---

## Panel largo con 2 unidades y tratamiento “desde un año en adelante” {-}

* Mantenemos **2 unidades**, pero ampliamos a **1980–1990** (11 años).
* Tratamiento para `id==2` **desde 1985 en adelante** (escalón).
* Outcome `Y` con nivel por unidad + tendencia temporal + efecto de tratamiento (`tau=5`).
* Estimamos `xtreg`/`reghdfe` y **DiD oficial** (`xtdidreg`) con diagnósticos de tendencias (`estat trendplots`, `estat ptrends`).


```stata
* ---------------------------------------------
* 2. Añadir más periodos de tiempo (10 por unidad)
* ---------------------------------------------

clear
local unidades = 2
local inicio = 1980
local fin    = 1990

local tiempo = `fin' - `inicio' + 1
local obs    = `unidades' * `tiempo'
set obs `obs'

* Crear variables id y t (funciona con años)
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

* Tratamiento desde cierto año en adelante para id==2
gen D = id==2 & t >= 1985  // ejemplo: tratamiento inicia en 1985
label variable D "Tratado"

* Efecto del tratamiento
gen tau = cond(D==1, 5, 0)
gen Y = id + 3*t + tau*D
label variable Y "Variable dependiente"

*Visualizar
twoway ///
  (connected Y t if id==1, msymbol(circle) lcolor(blue)) ///
  (connected Y t if id==2, msymbol(triangle) lcolor(red)) ///
  , ///
  title("Evolución de Y: Tratado vs. Control") ///
  xlabel(`inicio'(1)`fin') ///
  ylabel(, angle(horizontal)) ///
  xtitle("Año") ///
  ytitle("Y") ///
  legend(order(1 "Control (id=1)" 2 "Tratado (id=2)")) ///
  name(graficoY, replace)


xtreg Y D t, fe
reghdfe Y D, absorb(id t)

xtdidreg (Y) (D), group(id) time(t)
estat trendplots
estat ptrends
```

![Evolución de Y](dofile/11_TWFE/g2.png)
![TendeciasParalelas](dofile/11_TWFE/g3.png)

---

## Más unidades, **mismo** año de inicio, **efectos heterogéneos** {-}

### Versión donde `Y` refleja solo el tamaño del efecto por tratado {-}

* **3 unidades** (id 1=control; id 2 y 3 tratados desde 1985), con **distinto tamaño de efecto**: 2 vs 4.
* Muestra que TWFE hace un **promedio no trivial** de estos efectos.

```stata
* --------------------------------------------------
* 3. Más unidades, mismo tiempo de tratamiento, distinto efecto
* --------------------------------------------------

clear
* Parámetros flexibles
local unidades = 3
local inicio = 1980
local fin    = 1989

local tiempo = `fin' - `inicio' + 1
local obs = `unidades' * `tiempo'
set obs `obs'

* Crear variables id y t (con años reales)
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

* Tratamiento para id >= 2 desde t >= 1985
gen D = 0
replace D = 1 if id >= 2 & t >= 1985
label variable D "Tratado desde 1985 para id>=2"

* Variable dependiente con diferentes intensidades de tratamiento
cap drop Y
gen Y = 0
replace Y = cond(D == 1, 2, 0) if id == 2
replace Y = cond(D == 1, 4, 0) if id == 3
label variable Y "Variable dependiente"
sum Y if  D == 1

* -------------------------------
* Visualización
* -------------------------------
twoway ///
  (connected Y t if id == 1, msymbol(circle)) ///
  (connected Y t if id == 2, msymbol(triangle)) ///
  (connected Y t if id == 3, msymbol(square)) ///
  , ///
  xline(1985, lpattern(dash)) ///
  xlabel(`inicio'(1)`fin') ///
  legend(order(1 "id=1 (Control)" 2 "id=2 (Tratado - efecto 2)" 3 "id=3 (Tratado - efecto 4)")) ///
  title("Efectos de distinto tamaño") ///
  xtitle("Año") ///
  ytitle("Y")

* -------------------------------
* Estimación
* -------------------------------
xtreg Y D t, fe

* Comparación con especificaciones alternativas
reg Y D                       // sin efectos fijos
reg Y D i.t                   // solo efectos fijos de tiempo
reg Y D i.id                  // solo efectos fijos de unidad
reg Y D i.t i.id              // ambos (especificación correcta)
```
![Grafico](dofile/11_TWFE/g4.png)



### 3B) Versión con `Y = id + t + efecto_heterogéneo` {-}


* Añadimos **nivel por unidad** y **tendencia temporal** a `Y`, además de heterogeneidad en el efecto (`2` y `4`).
* Refuerza la idea: **β̂ TWFE** no necesariamente es el ATT promedio.


```stata
*-----------------------------------------------
clear
* Parámetros flexibles
local units = 3
local start = 1980
local end   = 1989

local time = `end' - `start' + 1
local obsv = `units' * `time'
set obs `obsv'

* Crear variables de panel y tiempo
gen id = .
gen t  = .

forvalues i = 1/`units' {
    forvalues j = 0/`=`time'-1' {
        local obsnum = (`i' - 1)*`time' + `j' + 1
        replace id = `i' in `obsnum'
        replace t  = `start' + `j' in `obsnum'
    }
}

sort id t
xtset id t

label variable id "Unidad (panel)"
label variable t  "Año"

* Tratamiento para id >= 2 desde t >= 1985
gen D = 0
replace D = 1 if id >= 2 & t >= 1985
label variable D "Tratamiento desde 1985 para id >= 2"

* Generar outcome con efectos heterogéneos
cap drop Y
gen Y = 0
replace Y = id + t + cond(D == 1, 0, 0) if id == 1
replace Y = id + t + cond(D == 1, 2, 0) if id == 2
replace Y = id + t + cond(D == 1, 4, 0) if id == 3
label variable Y "Variable dependiente (Y)"

* ----------------------------------
* Gráfico adaptado para años reales
* ----------------------------------
twoway ///
	(connected Y t if id == 1, msymbol(circle)) ///
	(connected Y t if id == 2, msymbol(triangle)) ///
	(connected Y t if id == 3, msymbol(square)) ///
	, ///
	xline(1985, lpattern(dash)) ///
	xlabel(`start'(1)`end') ///
	legend(order(1 "id=1 (Control)" 2 "id=2 (Tratado - efecto 2)" 3 "id=3 (Tratado - efecto 4)")) ///
	title("Evolución de Y con tratamiento heterogéneo") ///
	xtitle("Año") ///
	ytitle("Y")


* -------------------------------
* Estimación
* -------------------------------
xtreg Y D t, fe

* Comparación con especificaciones alternativas
reg Y D                       // sin efectos fijos
reg Y D i.t                   // solo efectos fijos de tiempo
reg Y D i.id                  // solo efectos fijos de unidad
reg Y D i.t i.id              // ambos (especificación correcta)
```

![Grafico](dofile/11_TWFE/g5.png)



## Más unidades, **distinto tiempo de tratamiento** (staggered) y **distinto efecto** {-}

* **id=2** se trata desde **1985**; **id=3** desde **1988** (escalonado).
* Los **tamaños de efecto** difieren (2 vs 4).
* Estimamos varias especificaciones y usamos **Bacon decomposition** para ver **qué comparaciones** y **con qué peso** componen el β̂ TWFE.
* Finalmente, estimamos pares 2×2 (id 1–2 e id 1–3) para ver los **componentes** del promedio.


```stata
* --------------------------------------------------
* 4. Más unidades, distinto tiempo de tratamiento y distinto efecto
* --------------------------------------------------

clear
* Parámetros flexibles
local unidades = 3
local inicio = 1980
local fin    = 1989

local tiempo = `fin' - `inicio' + 1
local obs = `unidades' * `tiempo'
set obs `obs'

* Crear variables id y t con años reales
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

* Tratamiento escalonado: id 2 desde 1985, id 3 desde 1988
gen D = 0
replace D = 1 if id == 2 & t >= 1985
replace D = 1 if id == 3 & t >= 1988
label variable D "Tratamiento escalonado"

* Efectos distintos por grupo y tiempo
gen Y = 0
replace Y = D * 2 if id == 2 & t >= 1985
replace Y = D * 4 if id == 3 & t >= 1988
label variable Y "Variable dependiente"

* ----------------------------------
* Gráfico adaptado para años reales
* ----------------------------------
twoway ///
  (connected Y t if id == 1, msymbol(circle)) ///
  (connected Y t if id == 2, msymbol(triangle)) ///
  (connected Y t if id == 3, msymbol(square)) ///
  , ///
  xline(1985 1988, lpattern(dash)) ///
  xlabel(`inicio'(1)`fin') ///
  legend(order(1 "id=1 (Control)" 2 "id=2 (Tratado desde 1985)" 3 "id=3 (Tratado desde 1988)")) ///
  title("Tratamiento con distintos tiempos y efectos") ///
  xtitle("Año") ///
  ytitle("Y")

```

![Grafico](dofile/11_TWFE/g6.png)

```stata
* ----------------------------------
* Estimaciones
* ----------------------------------
reg Y D
reg Y D i.t
reg Y D i.id
reg Y D i.t i.id

xtreg Y D i.t, fe
reghdfe Y D, absorb(id t)

```

* Descomposición de Bacon

```stata
ssc install bacondecomp, replace
bacondecomp Y D, ddetail

*LEER: Goodman-Bacon, A. (2021). Difference-in-differences with variation in treatment timing. Journal of Econometrics.

xtreg Y D i.t if (id==1 | id==2), fe robust
xtreg Y D i.t if (id==1 | id==3), fe robust	
```

##Descomposición de Bacon y “efecto real” en DiD con adopción escalonada {-}

Cuando el tratamiento se **adopta en momentos distintos** (staggered DiD), el coeficiente de un modelo **TWFE** (efectos fijos de unidad y tiempo) **no** es un simple DiD clásico.  
Goodman-Bacon muestra que ese coeficiente es un **promedio ponderado de muchos DiD 2×2** construidos a partir de las comparaciones disponibles en tus datos.

En concreto, TWFE se puede escribir (a nivel intuitivo) como:

\[
\widehat\beta^{\text{TWFE}} \;=\; \sum_{k} w_k \,\widehat\beta^{(2\times2)}_k,
\]

donde cada \( \widehat\beta^{(2\times2)}_k \) es un DiD “pequeño” entre dos grupos y dos periodos relevantes, y \( w_k \) es su **peso** (depende de tamaños muestrales y de cuánta variación en tratamiento aporta esa comparación).


## Las tres familias de comparaciones 2×2 {-}

1. **Tratados vs. nunca tratados (T vs. U)**  
   - Comparas una cohorte **tratada** con una **nunca tratada** (usando pre vs. post del tratado).

2. **Tratados temprano vs. tratados tarde (Te vs. Cl)** — *antes* de que los tardíos reciban el tratamiento  
   - Los **tardíos** (aún no tratados) actúan como **control provisional** para los **tempranos**.

3. **Tratados tarde vs. tratados temprano (Tl vs. Ce)** — *después* de que los tempranos ya están tratados  
   - Los **tempranos** (ya tratados) sirven como “control” **no válido** para los **tardíos** porque ya tienen efecto del tratamiento.

> **Clave:** En (2) y (3) estás comparando **tratados con tratados** en momentos distintos.  
> Si el **efecto varía por cohorte** (entre unidades) o **en el tiempo desde la adopción** (dinámico), esas comparaciones **no** miden el efecto causal que normalmente te interesa.


## ¿Cuál es “el efecto real” que queremos reportar? {-}

En DiD moderno, el objetivo suele ser el **promedio de efectos entre las observaciones tratadas** en los periodos post, ponderando por su tamaño:

\[
\textbf{ATT}_{\text{overall}}
\;=\;
\frac{\displaystyle\sum_{g,t:\,D=1} N_{g,t}\cdot \text{ATT}(g,t)}
     {\displaystyle\sum_{g,t:\,D=1} N_{g,t}},
\]

donde:
- \( g \) es la **cohorte** (grupo que adopta en un mismo momento),
- \( t \) es el **periodo**,
- \( \text{ATT}(g,t) \) es el efecto en la cohorte-tiempo,
- \( N_{g,t} \) es el número de observaciones tratadas en esa celda.

A esto lo llamamos aquí el **“promedio del tratamiento”** (o **ATT overall**).  
**TWFE** coincide con este promedio **solo si** el efecto **no varía** ni por cohorte ni en el tiempo (homogeneidad).



## ¿Por qué TWFE puede fallar con heterogeneidad? {-}

- **Heterogeneidad entre cohortes**: el tamaño del efecto **difiere por unidad/grupo** (p. ej., id=2 tiene +2; id=3 tiene +4).  
- **Heterogeneidad dinámica**: el efecto **cambia con el tiempo desde la adopción** (p. ej., crece cada periodo post).

En ambos casos, TWFE **mezcla** comparaciones de tipo **Te vs Cl** y **Tl vs Ce**, que **no** capturan el \(\text{ATT}(g,t)\) “puro” porque los “controles” ya tienen (o tendrán) efecto en parte de la ventana.  
El promedio ponderado resultante puede **alejarse** del **ATT overall** que quieres reportar.


## Mini-ejemplos para fijar ideas {-}

### Caso 3 (tu ejemplo con tres unidades, mismo inicio y efecto por cohorte)

- Cohortes tratadas desde 1985:  
  - id=2 con efecto **+2** (constante post)  
  - id=3 con efecto **+4** (constante post)  
- Ambos tienen **el mismo número de periodos post** (1985–1989: 5 cada uno).

**ATT overall (promedio del tratamiento)**  
Promedio entre todas las **observaciones tratadas post**:
\[
\frac{5\cdot 2 \;+\; 5\cdot 4}{5+5} \;=\; \frac{10+20}{10} \;=\; \mathbf{3}.
\]

> Aquí el promedio “intuitivo” **sí** es 3.  
> TWFE puede aproximarlo cuando solo hay heterogeneidad entre cohortes pero no dinámica y las ventanas son simétricas. Aun así, **Bacon** te dirá qué pesos están usando tus comparaciones.

---

###  Caso con **adopción escalonada** y **efectos dinámicos** {-}

- id=2 adopta en 1985 y su efecto **crece** con \(t-\text{timing}\).  
- id=3 adopta en 1988 y su efecto también **crece** con \(t-\text{timing}\).

**ATT overall correcto**:  
haz el promedio de \(\text{ATT}(g,t)\) solo sobre **celdas tratadas** \((g,t)\), ponderado por \(N_{g,t}\).

**TWFE**:  
promedia **también** comparaciones **tratado vs tratado** (tempranos vs tardíos y viceversa) ⇒ si los efectos crecen, esas comparaciones **miden diferencias de efectos**, no el efecto verdadero, y el promedio puede **sesgarse** (incluso con **pesos negativos** en ciertas descomposiciones).

---

## Cómo leer la salida (intuición) {-}

- **`T vs U`**: comparaciones “limpias” (tratados vs nunca tratados).  
- **`Te vs Cl`** y **`Tl vs Ce`**: comparaciones **entre tratados** en distintos momentos.  
- Si los pesos se concentran en estas dos últimas y sospechas **heterogeneidad**, **no confíes** en TWFE como estimador del ATT overall.


## Buenas prácticas (en una lista corta) {-}

- **Define tu parámetro objetivo**: normalmente, **ATT ** (promedio sobre celdas tratadas).  
- **Estima \(\text{ATT}(g,t)\)** con métodos modernos (p. ej., Callaway-Sant’Anna, Sun-Abraham, did-imputation, did2s).  
- **Promedia correctamente**: construye el **ATT ** ponderando por \(N_{g,t}\) solo sobre celdas tratadas.  
- **Usa Bacon como diagnóstico**: revisa qué comparaciones y pesos aporta TWFE; si domina “tratado vs tratado” y hay heterogeneidad, reporta el estimador moderno.

---


- **Qué quieres**: el **promedio del tratamiento** entre las observaciones tratadas (ATT overall).  
- **Qué hace TWFE**: un **promedio ponderado** de múltiples DiD 2×2, incluyendo **tratado vs tratado**.  
- **Cuándo coincide**: solo con **efectos homogéneos** (sin variación por cohorte ni en el tiempo).  
- **Qué hacer**: estima \(\text{ATT}(g,t)\) y promedia **sobre tratadas**; usa Bacon para **auditar** a TWFE.




## ¿Cómo interpretar la salida de `bacondecomp`? {-}


```

Computing decomposition across 3 timing groups
including a never-treated group
-------------------------------

```
       Y | Coefficient  Std. err.      z    P>|z|     [95% conf. interval]
-------------+----------------------------------------------------------------
         D |   2.709677   .3030118     8.94   0.000     2.115785     3.30357
```

**Bacon Decomposition:**

| Tipo                | Beta         | TotalWeight   |
|---------------------|--------------|---------------|
| Early_v_Late        | 2            | 0.2419354906  |
| Late_v_Early        | 4            | 0.0967741935  |
| Never_v_timing      | 2.780487813  | 0.6612903158  |

```

---

##  ¿Qué significa cada fila? {-}

- **Early_v_Late (β=2, peso=0.2419)**  
  Compara **tratados tempranos** vs **tratados tardíos** **en los períodos en que los tardíos aún no están tratados**.  
  → Identifica el efecto de la cohorte **temprana** (≈ 2 en tu simulación).

- **Late_v_Early (β=4, peso=0.0968)**  
  Compara **tratados tardíos** vs **tratados tempranos** **cuando los tempranos ya están tratados**.  
  → Usa **tratados** como “control” (no ideal si hay heterogeneidad). En tu caso rinde ≈ **4** (efecto de la cohorte tardía).

- **Never_v_timing (β≈2.7805, peso=0.6613)**  
  Compara **cohortes tratadas** (temprana y tardía) vs **nunca tratados**.  
  → Es un promedio (con pesos internos) de las comparaciones “tratados vs nunca tratados” a través de los distintos timings; por eso el β cae **entre 2 y 4** y está más cerca de 2 porque la cohorte temprana aporta más periodos post.

> **Idea clave**: El coeficiente **TWFE** final es la **media ponderada** de estos DiD 2×2.

---

## ¿Cuadra el promedio ponderado con el β de TWFE?  {-}

Sí. Los **pesos** suman 1:
- 0.2419354906 + 0.0967741935 + 0.6612903158 = **1.0000** (≈)

Promedio ponderado:
- Aporte Early_v_Late: 2 × 0.2419354906 = **0.483871**
- Aporte Late_v_Early: 4 × 0.0967741935 = **0.387097**
- Aporte Never_v_timing: 2.780487813 × 0.6612903158 = **1.838710**

**Suma**: 0.483871 + 0.387097 + 1.838710 = **2.709677**  
→ Coincide con el **β̂ de TWFE = 2.709677**.

---

## ¿Por qué este β̂ puede **no** ser “el efecto real promedio”?  {-}

Lo que normalmente quieres reportar es el **ATT overall**: el **promedio del efecto entre las observaciones tratadas** (cohorte-tiempo) ponderado por su tamaño:

\[
\text{ATT}_{\text{overall}}
=
\frac{\sum_{g,t: D=1} N_{g,t}\cdot \text{ATT}(g,t)}
     {\sum_{g,t: D=1} N_{g,t}}.
\]

- En **adopción escalonada** (tempranos con más años post que tardíos) y/o con **efectos que difieren por cohorte** o en el **tiempo desde el tratamiento**, **TWFE** mezcla **tres tipos de comparaciones** (incluyendo **tratado vs tratado**), y su promedio ponderado **puede alejarse** del **ATT overall**.

**Ejemplo mental (ilustrativo):**  
Si la cohorte temprana vale **2** (más años post) y la tardía **4** (menos años post), el **promedio correcto entre celdas tratadas** puede ser algo como  
\[
\frac{5\cdot 2 + 2\cdot 4}{5+2} = \frac{18}{7} \approx 2.571,
\]
mientras que **TWFE** te dio **2.7097** porque **incluye** las comparaciones **Early_v_Late** y **Late_v_Early** con pesos 24% y 9.7%.



## ¿Cómo usar esta descomposición en la práctica?  {-}

- Si **Never_v_timing** concentra **casi todo el peso** y los efectos son **homogéneos**, **TWFE** suele estar cerca del **ATT overall**.  
- Si hay **peso relevante** en **Early_v_Late** o **Late_v_Early** **y** sospechas **heterogeneidad** (por cohorte o dinámica), **no** confíes en TWFE para el efecto promedio.

**Recomendación**: Estima \(\text{ATT}(g,t)\) con métodos modernos (p. ej., **csdid**, **eventstudyinteract**, **did_imputation**, **did2s**) y luego construye el **ATT overall** promediando **solo sobre celdas tratadas** con pesos \(N_{g,t}\).



## Resumen en una línea  {-}

**Tu tabla dice**: el β̂ de TWFE (=**2.7097**) es un **promedio ponderado** de tres DiD 2×2; la mayor parte (66.1%) viene de **tratados vs nunca tratados** (≈2.78), y el resto de **tratados vs tratados** (2 y 4).  
**Conclusión**: Para reportar “el efecto real promedio”, usa el **ATT overall** (promedio sobre observaciones tratadas), no el β̂ de TWFE cuando hay heterogeneidad o adopción escalonada.



## Simulación grande (30 unidades × 60 periodos) con **timings y efectos crecientes**  {-}



* Asignamos a cada unidad una **cohorte** (0–5), un **timing de adopción** aleatorio, y un **tamaño de efecto** aleatorio.
* El outcome `Y` crece con el tiempo y **acumula** efecto post-tratamiento: `effect * (t - timing)`.
* Graficamos las trayectorias y estimamos **TWFE** + **Bacon**.



```stata
*********************************************************

clear
local units = 30
local start = 1
local end   = 60

local time = `end' - `start' + 1
local obsv = `units' * `time'
set obs `obsv'

egen id	   = seq(), b(`time')  
egen t 	   = seq(), f(`start') t(`end') 	

sort  id t
xtset id t

lab var id "Panel variable"
lab var t  "Time  variable"
set seed 13082021


cap drop Y
cap drop D
cap drop cohort
cap drop effect
cap drop timing

gen Y 	   = 0		// outcome variable	
gen D 	   = 0		// intervention variable
gen cohort = .  	// total treatment variables
gen effect = .		// treatment effect size
gen timing = .		// when the treatment happens for each cohort
levelsof id, local(lvls)
foreach x of local lvls {
	local chrt = runiformint(0,5)	
	replace cohort = `chrt' if id==`x'
}

levelsof cohort , local(lvls)  //  let all cohorts be treated for now
foreach x of local lvls {
	
	// (a) effect
	
	local eff = runiformint(2,10)
		replace effect = `eff' if cohort==`x'
		
	// (b) timing	
	
	local timing = runiformint(`start' + 5,`end' - 5)	
	replace timing = `timing' if cohort==`x'
		replace D = 1 if cohort==`x' & t>= `timing' 
}

replace Y = id + t + cond(D==1, effect * (t - timing), 0)


levelsof cohort
local items = `r(r)'

local lines
levelsof id

forval x = 1/`r(r)' {
	
	qui summ cohort if id==`x'
	local color = `r(mean)' + 1
	colorpalette tableau, nograph
		
	local lines `lines' (line Y t if id==`x', lc("`r(p`color')'") lw(vthin))	||
}

twoway ///
	`lines', legend(off)
	
	
	xtreg Y i.t D, fe
	reghdfe Y D, absorb(id t)  


bacondecomp Y D, ddetail
```

> 🔍 **Claves**: Este es el entorno “realista” donde TWFE más sufre: **heterogeneidad + escalonamiento** + **dinámicas** → pesos complicados (a veces negativos).



## “Soluciones”: estimadores modernos y gráfico comparativo de estudios de evento  {-}

### Explicación didáctica  {-}

* Simulamos nuevamente con **first\_treat** y **rel\_time** para construir **leads** (`F_#`) y **lags** (`L_#`).
* Definimos cohortes de **nunca tratados** y variables de **cohorte/gvar** para los métodos que lo requieren.
* **Instalamos** paquetes (hazlo una sola vez; luego puedes comentar estas líneas).
* Estimamos:

  * **TWFE** (con `reghdfe`),
  * **csdid**,
  * **did\_imputation**,
  * **did\_multiplegt\_dyn**,
  * **eventstudyinteract**,
  * **did2s**,
  * **stackedev**;
    y graficamos todos juntos con `event_plot`.

> ⚠️ **Detalle técnico**: en `reghdfe Y L_* F_*, absorb(id t) cluster(i)`, tu código agrupa por `i`. Si tu panel es `id`, lo más habitual es `cluster(id)`. **Mantengo tu comando exactamente** (como pediste), pero sugiero revisar el cluster según tu diseño.


```stata
**********************************************************************************************
*Soluciones

clear

local units = 30
local start = 1
local end 	= 60

local time = `end' - `start' + 1
local obsv = `units' * `time'
set obs `obsv'

egen id	   = seq(), b(`time')  
egen t 	   = seq(), f(`start') t(`end') 	

sort  id t
xtset id t


set seed 20211222

gen Y 	   		= 0		// outcome variable	
gen D 	   		= 0		// intervention variable
gen cohort      = .  	// treatment cohort
gen effect      = .		// treatment effect size
gen first_treat = .		// when the treatment happens for each cohort
gen rel_time	= .     // time - first_treat

levelsof id, local(lvls)
foreach x of local lvls {
	local chrt = runiformint(0,5)	
	replace cohort = `chrt' if id==`x'
}


levelsof cohort , local(lvls) 
foreach x of local lvls {
	
	local eff = runiformint(2,10)
		replace effect = `eff' if cohort==`x'
			
	local timing = runiformint(`start',`end' + 20)	// 
	replace first_treat = `timing' if cohort==`x'
	replace first_treat = . if first_treat > `end'
		replace D = 1 if cohort==`x' & t>= `timing' 
}

replace rel_time = t - first_treat
replace Y = id + t + cond(D==1, effect * rel_time, 0) + rnormal()



// generate leads and lags (used in some commands)

summ rel_time
local relmin = abs(r(min))
local relmax = abs(r(max))

	// leads
	cap drop F_*
	forval x = 2/`relmin' {  // drop the first lead
		gen F_`x' = rel_time == -`x'
	}

	
	//lags
	cap drop L_*
	forval x = 0/`relmax' {
		gen L_`x' = rel_time ==  `x'
	}
	
	
// generate the control_cohort variables  (used in some commands)

gen never_treat = first_treat==.

sum first_treat
gen last_cohort = first_treat==r(max) // dummy for the latest- or never-treated cohort


// generate the gvar variabls (used in some commands)
gen gvar = first_treat
recode gvar (. = 0)
*************************************

*Los colegas creativos
// supporting packages
ssc install schemepack, replace
ssc install avar, replace 
ssc install reghdfe, replace
ssc install event_plot, replace
ssc install palettes, replace
ssc install colrspace, replace

// DiD packages
ssc install drdid, replace
ssc install csdid, replace
ssc install did_imputation, replace
ssc install eventstudyinteract, replace
ssc install did_multiplegt, replace
ssc install stackedev, replace
ssc install did2s, replace


************
*** TWFE ***
************

reghdfe Y L_* F_*, absorb(id t) cluster(i)

estimates store twfe 

*************
*** csdid ***
*************

csdid Y, ivar(id) time(t) gvar(gvar) notyet

estat event, window(-10 10) estore(csdd) 

***********************
*** did_imputation  ***
***********************

did_imputation Y i t first_treat, horizons(0/10) pretrend(10) minn(0) 

estimates store didimp	
	
***********************
*** did_multiplegt  ***
***********************

did_multiplegt_dyn Y id t D, effects(10) placebo(10) cluster(id)

matrix didmgt_b = e(estimates) 
matrix didmgt_v = e(variances)

*****************************
***  eventstudyinteract   ***
*****************************

eventstudyinteract Y L_* F_*, vce(cluster id) absorb(id t) cohort(first_treat) control_cohort(never_treat)	

matrix evtstint_b = e(b_iw) 
matrix evtstint_v = e(V_iw)

***************		
*** did2s   ***
***************

did2s Y, first_stage(id t) second_stage(F_* L_*) treatment(D) cluster(id)

matrix did2s_b = e(b)
matrix did2s_v = e(V)

******************
*** stackedev  ***
******************

	
stackedev Y F_* L_* ref, cohort(first_treat) time(t) never_treat(never_treat) unit_fe(id) clust_unit(id)
	
matrix stackedev_b = e(b)
matrix stackedev_v = e(V)



colorpalette tableau, nograph	

event_plot    twfe	csdd    didimp  dcdh_b#dcdh_v   sa_b#sa_v   stackedev_b#stackedev_v did2s_b#did2s_v , 	///
	stub_lag( L_#   Tp#     tau#    Effect_#        L_#         L_#                     L_# 			) 		///
	stub_lead(F_# 	Tm#     pre#    Placebo_#       F_#         F_#                     F_# 			)		///
		together perturb(-0.30(0.10)0.30) trimlead(20) trimlag(20) noautolegend 									///
		plottype(scatter) ciplottype(rspike)  																	    ///
			lag_opt1(msymbol(+)   msize(1.2) mlwidth(0.3) color(black)) 	lag_ci_opt1(color(black)     lw(0.15)) 	///
			lag_opt2(msymbol(lgx) msize(1.2) mlwidth(0.3) color("`r(p1)'")) lag_ci_opt2(color("`r(p1)'") lw(0.15)) 	///
			lag_opt3(msymbol(Dh)  msize(1.2) mlwidth(0.3) color("`r(p2)'")) lag_ci_opt3(color("`r(p2)'") lw(0.15)) 	///
			lag_opt4(msymbol(Th)  msize(1.2) mlwidth(0.3) color("`r(p3)'")) lag_ci_opt4(color("`r(p3)'") lw(0.15)) 	///
			lag_opt5(msymbol(Sh)  msize(1.2) mlwidth(0.3) color("`r(p4)'")) lag_ci_opt5(color("`r(p4)'") lw(0.15)) 	///
			lag_opt6(msymbol(Oh)  msize(1.2) mlwidth(0.3) color("`r(p5)'")) lag_ci_opt6(color("`r(p5)'") lw(0.15)) 	///	 
			lag_opt7(msymbol(V)   msize(1.2) mlwidth(0.3) color("`r(p6)'")) lag_ci_opt7(color("`r(p6)'") lw(0.15)) 	///		
					graph_opt(												///
								title("DiD event study plot") 						///
								xtitle("") 									///
								ytitle("Average effect") xlabel(-20(2)20)	///
								legend(order(1 "TWFE" 3 "csdid (CS 2020)" 5 "did_imputation (BJS 2021)" 7 "did_multiplegt (CD 2020)"  9 "eventstudyinteract (SA 2020)" 11 "stackedev (CDLZ 2019)" 13 "did2s (G 2021)") pos(6) rows(3) region(style(none))) 	///
								xline(-0.5, lc(gs8) lp(dash)) ///
								yline(   0, lc(gs8) lp(dash)) ///
							 ) 
```



## Puntos de aprendizaje (resumen)  {-}

* **TWFE** funciona bien en **2×2** simples, pero con **heterogeneidad** y **timing escalonado** puede:

  * Promediar comparaciones no deseadas,
  * Asignar **pesos negativos**,
  * Usar como “controles” unidades ya tratadas.
* La **descomposición de Bacon** ayuda a entender **quién compara con quién** y **con qué peso**.
* Estimadores modernos (**csdid, eventstudyinteract, did\_imputation, did2s, did\_multiplegt, stackedev**) estiman **ATT(g,t)** y permiten **event studies** consistentes, evitando sesgos típicos de TWFE en estos contextos.

---

## 8) Notas prácticas de ejecución

* **Instalación**: Ejecuta una vez las líneas `ssc install …, replace`.
* **Clustering**: Revisa la variable correcta según tu panel (en tu bloque TWFE final usas `cluster(i)`; si la unidad es `id`, suele ser `cluster(id)`).
* **`xtdidreg`**: Tu comando está como `xtdidreg`. En Stata 17/18 el oficial es `xtdidregress`. Si tu Stata no reconoce `xtdidreg`, usa `xtdidregress` con la sintaxis correspondiente.
* **Semillas**: Mantengo tus `set seed` donde aplican para replicabilidad.

