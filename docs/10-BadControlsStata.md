# Malos controles — Clase empírica {#bad-controls-stata}

::: {.boxinfo}
**Metas de aprendizaje**

- Entender con precisión qué es un mal control y por qué hace daño.
- Ver los tres casos clásicos: post-tratamiento/mediador, colisionador, y proxy contaminado.
- Confirmar con simulaciones en Stata que el sesgo es real y sistemático.
:::

---

## La idea central: controlar no siempre ayuda {-}

Cuando estimamos el efecto de un tratamiento $D$ sobre un resultado $Y$, la tentación es incluir tantas variables de control como sea posible. La lógica parece razonable: "más controles = más preciso".

Eso es **falso**.

Hay variables que, si las incluimos, **distorsionan** el coeficiente de $D$ en vez de corregirlo. No porque estén mal medidas ni porque haya ruido — sino porque la **estructura causal** hace que condicionar en ellas abra caminos espurios o bloquee el efecto que queremos medir.

La regla de oro es simple:

> Solo incluir variables que son **causas** de $D$ o de $Y$ (o de ambas) **y que fueron determinadas antes del tratamiento**. Nunca incluir variables que son consecuencia del tratamiento, ni variables causadas por $D$ y por $Y$ al mismo tiempo.

En esta sección vemos los tres casos en que esa regla se viola, con código que muestra el sesgo en datos simulados.

---

## Buen control vs. mal control: la diferencia en una imagen {-}

**Buen control**: $X$ causa tanto $D$ como $Y$. Condicionar en $X$ cierra un camino de confusión y ayuda a identificar el efecto de $D$.

$$X \rightarrow D \rightarrow Y \quad \text{y} \quad X \rightarrow Y$$

**Mal control**: $Z$ es consecuencia de $D$, o es causada por $D$ y por $Y$ al mismo tiempo. Condicionar en $Z$ distorsiona el coeficiente.

| Estructura | Nombre | Qué pasa si controlas |
|---|---|---|
| $D \rightarrow M \rightarrow Y$ | Mediador / post-tratamiento | Bloqueas parte del efecto total |
| $D \rightarrow C \leftarrow U \rightarrow Y$ | Colisionador | Abres un camino espurio |
| $D \rightarrow L \leftarrow U \rightarrow Y$ | Proxy contaminado | Mezclas tratamiento y no observables |

---

## Caso 1: Post-tratamiento / mediador {-}

### Intuición paso a paso {-}

Imagina que quieres saber cuánto **aumenta el salario** una persona que completó la universidad.

La educación universitaria actúa así:

$$\text{Educación} \rightarrow \text{Tipo de empleo} \rightarrow \text{Salario}$$

La educación te permite acceder a empleos de mayor calificación, y esos empleos pagan mejor. Ese es el **mecanismo** por el que la educación sube el salario.

**¿Por qué parece buena idea controlar por tipo de empleo?**

Alguien podría pensar: "quiero comparar personas con el mismo tipo de empleo, para aislar el efecto puro de la educación". Suena razonable.

**¿Por qué en realidad es un error?**

El tipo de empleo **es parte del efecto**. Si controlas por él, estás preguntando: "entre personas que ya tienen el mismo tipo de empleo, ¿cuánto diferencia la educación el salario?". Esa pregunta ya eliminó la mayor parte del impacto.

Es como querer medir si un medicamento reduce la fiebre, pero controlar por la temperatura corporal post-tratamiento. Obviamente no vas a ver diferencia — ya bloqueaste el mecanismo.

**El efecto total pasa por el mediador. Si lo tapas, no mides el total.**

::: {.boxcerebro}
**Para recordar:** Si $M$ está en el camino causal entre $D$ y $Y$, controlar $M$ convierte tu estimación del efecto total en una estimación del efecto directo (que en muchos casos es cero o casi cero). No es que el tratamiento "no funcione" — es que bloqueaste la ruta.
:::

### Ejemplo cotidiano {-}

Quieres saber si hacer ejercicio reduce el riesgo de infarto.

El mecanismo es: ejercicio $\rightarrow$ presión arterial baja $\rightarrow$ menor riesgo de infarto.

Si controlas por presión arterial, estás preguntando: "entre personas con la misma presión, ¿hace diferencia el ejercicio?". El efecto casi desaparece — no porque el ejercicio no sirva, sino porque bloqueaste el canal por el que actúa.

### Ejemplo econométrico y código Stata {-}

Generamos datos con la estructura $D \rightarrow M \rightarrow Y$. El parámetro que queremos recuperar es el **efecto total**: $a \times b = 2 \times 1 = 2$.

```stata
********************************************************************************
* CASO 1: MEDIADOR / POST-TRATAMIENTO
* Estructura: tratamiento -> mediador -> resultado
* Pregunta: ¿cuál es el efecto TOTAL del tratamiento sobre el resultado?
* Respuesta correcta: 2 (= a × b)
* Respuesta con mal control: ≈ 0 (bloqueamos el canal)
********************************************************************************

clear all
set more off
set seed 2468        // fija la semilla para reproducibilidad
set obs 10000        // muestra grande para ver el sesgo con claridad

* Definimos los parámetros verdaderos
scalar a = 2         // efecto del tratamiento sobre el mediador
scalar b = 1         // efecto del mediador sobre el resultado

* Generamos las variables con nombres intuitivos
gen byte   tratamiento = (runiform() < 0.5)           // D: tratamiento aleatorio (0/1)
gen double mediador    = a * tratamiento + rnormal()   // M: afectado por D
gen double resultado   = b * mediador    + rnormal()   // Y: afectado por M (no por D directamente)

* ---------------------------------------------------------------
* REGRESIÓN CORRECTA: solo tratamiento
* El coeficiente de tratamiento debe ser ≈ a*b = 2
* ---------------------------------------------------------------
di as text "=== Regresión correcta: efecto TOTAL del tratamiento ==="
reg resultado tratamiento, vce(robust)

* ---------------------------------------------------------------
* REGRESIÓN CON MAL CONTROL: incluimos el mediador
* El coeficiente de tratamiento cae a ≈ 0
* Interpretación: el tratamiento no "causa" directamente el resultado
* sin pasar por el mediador → efecto directo ≈ 0
* ---------------------------------------------------------------
di as text "=== Regresión con MAL CONTROL (mediador): efecto directo ==="
reg resultado tratamiento mediador, vce(robust)
```

**Lectura esperada de los resultados:**

- `reg resultado tratamiento`: coeficiente $\approx 2$ — recupera el efecto total.
- `reg resultado tratamiento mediador`: coeficiente de `tratamiento` $\approx 0$ — no porque el tratamiento no funcione, sino porque bloqueamos la ruta $D \rightarrow M \rightarrow Y$.

### Mini Monte Carlo: el sesgo es sistemático {-}

Una sola muestra podría tener ruido. El Monte Carlo nos muestra que el sesgo ocurre en promedio, en cientos de muestras distintas.

```stata
********************************************************************************
* MONTE CARLO: Mediador
* Verifica que el sesgo no es accidental — ocurre en promedio
********************************************************************************

capture program drop mc_mediador
program define mc_mediador, rclass
    clear
    set obs 3000
    gen byte   trat = (runiform() < 0.5)
    gen double med  = 2 * trat + rnormal()
    gen double res  = 1 * med  + rnormal()
    quietly reg res trat        // sin mediador
    return scalar efecto_total   = _b[trat]
    quietly reg res trat med    // con mediador (mal control)
    return scalar efecto_directo = _b[trat]
end

simulate efecto_total=r(efecto_total) efecto_directo=r(efecto_directo), ///
    reps(300) seed(2468): mc_mediador

* Esperado: promedio de efecto_total ≈ 2; promedio de efecto_directo ≈ 0
summarize efecto_total efecto_directo

* Visualización: distribuciones de los dos estimadores
twoway (hist efecto_total,   width(.05) color(navy%50))   ///
       (hist efecto_directo, width(.05) color(red%50)),   ///
       legend(order(1 "Efecto total (correcto)" 2 "Con mediador (mal control)")) ///
       xline(2, lcolor(navy) lpattern(dash))              ///
       xline(0, lcolor(red)  lpattern(dash))              ///
       title("Mediador: distribución del coeficiente de tratamiento") ///
       xtitle("Estimado de tratamiento") ytitle("Frecuencia")
```

El histograma azul se centra en 2 (valor verdadero). El histograma rojo se centra en 0 (sesgo severo por controlar el mediador).

---

## Caso 2: Colisionador / selección endógena {-}

### Intuición paso a paso {-}

Este caso es más sorprendente porque el sesgo **aparece por controlar**, no por omitir.

La estructura es: $D$ y un factor no observado $U$ son **causas independientes** de una variable $C$. Además, $U$ también afecta $Y$. Si $D$ y $Y$ no tienen ninguna relación causal entre sí, la regresión de $Y$ sobre $D$ debería dar cero.

Pero si agregas $C$ como control, el coeficiente de $D$ se vuelve distinto de cero. ¿Por qué?

**La variable $C$ "recibe dos flechas"**. Condicionar en ella crea una dependencia artificial entre sus causas: si sabes que $C = 1$, entonces saber el valor de $D$ te da información sobre $U$, y viceversa. Abriste un canal de información entre $D$ y $U$ que antes estaba cerrado.

Como $U$ afecta $Y$, ahora $D$ está correlacionado con $Y$ a través de ese canal — aunque en la realidad no haya ninguna relación causal.

**¿Por qué parece buena idea controlar por $C$?**

Porque $C$ es observable y parece relevante. El investigador podría pensar que "más controles es mejor" sin darse cuenta de que $C$ es un colisionador.

**¿Por qué en realidad genera problema?**

Porque el camino $D \leftarrow C \rightarrow U \rightarrow Y$ estaba bloqueado **precisamente** porque nadie lo había abierto. Al condicionar en $C$, lo abriste tú.

::: {.boxcerebro}
**Para recordar:** Un colisionador es una variable que recibe flechas de dos fuentes causalmente separadas. Condicionar en él abre un camino que antes estaba cerrado. El sesgo no existía antes de controlar — lo creamos nosotros al agregar la variable.
:::

### Ejemplo cotidiano {-}

Quieres saber si tener conexiones (red de contactos) aumenta la productividad en el trabajo.

En realidad, no hay relación directa: las conexiones no hacen a nadie más productivo por sí solas.

Pero hay un colisionador: la **contratación**. Tanto las conexiones como la habilidad (no observable) llevan a que te contraten.

Si estudias solo a los empleados (es decir, condicionas en "fue contratado"), introduces una correlación artificial: entre los empleados, quien tiene pocas conexiones tiende a haber sido contratado por su alta habilidad, y quien tiene muchas conexiones puede tener habilidad más variable. Ahora conexiones y habilidad están correlacionadas dentro de la muestra, aunque no lo estén en la población.

Como la habilidad afecta la productividad, las conexiones parecen estar "relacionadas" con la productividad — aunque no lo estén causalmente.

**Restringir la muestra a "solo empleados" es una forma de condicionar en un colisionador.**

### Ejemplo econométrico y código Stata {-}

El efecto verdadero de $D$ sobre $Y$ es **cero**. El colisionador $C$ es causado por $D$ y por $Y$.

```stata
********************************************************************************
* CASO 2: COLISIONADOR
* Estructura: D -> C <- U -> Y  (donde U afecta tanto C como Y)
* Versión simplificada: D -> C <- Y  (D y Y independientes)
* Efecto verdadero de D sobre Y: 0
* Sin controlar C: estimado ≈ 0 (correcto)
* Controlando C: estimado ≠ 0 (sesgo inducido)
********************************************************************************

clear all
set more off
set seed 12345
set obs 1000

* D y Y son independientes — no hay relación causal entre ellos
gen double conexiones   = rnormal()   // D: tratamiento (conexiones laborales)
gen double productividad = rnormal()   // Y: resultado (productividad)

* C es el colisionador: causado por D y por Y (o por un U que afecta Y)
* Piénsalo como "fue contratado" = función de conexiones y habilidad (proxy de productividad)
gen double contratacion = 2 * conexiones - 0.5 * productividad + rnormal()

* ---------------------------------------------------------------
* REGRESIÓN CORRECTA: solo D
* El coeficiente de conexiones debe ser ≈ 0
* ---------------------------------------------------------------
di as text "=== Regresión correcta: sin colisionador ==="
reg productividad conexiones, vce(robust)

* ---------------------------------------------------------------
* REGRESIÓN CON MAL CONTROL: incluimos el colisionador
* El coeficiente de conexiones se aleja de 0 — sesgo por abrir el camino espurio
* ---------------------------------------------------------------
di as text "=== Regresión con MAL CONTROL (colisionador) ==="
reg productividad conexiones contratacion, vce(robust)
```

**Lectura esperada:**

- Sin `contratacion`: coeficiente de `conexiones` $\approx 0$ (no hay efecto causal).
- Con `contratacion`: coeficiente de `conexiones` es significativamente distinto de 0 — sesgo que **creamos** al incluir el colisionador.

### Monte Carlo: el sesgo es consistente {-}

```stata
********************************************************************************
* MONTE CARLO: Colisionador
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

* Esperado: b_sin_c ≈ 0 (insesgado); b_con_c ≠ 0 (sesgo por colisionador)
summarize b_sin_c b_con_c

twoway (hist b_sin_c, width(.03) color(navy%50))  ///
       (hist b_con_c, width(.03) color(red%50)),  ///
       legend(order(1 "Sin colisionador (correcto)" 2 "Con colisionador (sesgo)")) ///
       xline(0, lcolor(black) lpattern(dash))     ///
       title("Colisionador: distribución del coeficiente de conexiones") ///
       xtitle("Estimado de conexiones") ytitle("Frecuencia")
```

El histograma azul se centra en 0 (verdadero efecto). El histograma rojo está desplazado — el sesgo es sistemático y no desaparece con más datos.

### Selección muestral como colisionador {-}

Un caso frecuente en la práctica es **restringir la muestra** a un subgrupo que fue determinado por el tratamiento (o por variables relacionadas). Esto equivale a condicionar en un colisionador.

Ejemplos clásicos:

- **Solo empleados**: si estudias el efecto de la educación sobre el salario usando solo personas que trabajan, condicionas en "estar empleado" — que depende tanto de la educación como de factores no observados (salud, redes, etc.).
- **Solo sobrevivientes**: si evalúas el desempeño de empresas que sobrevivieron a un choque, condicionas en "supervivencia" — que depende tanto del choque como de la resiliencia no observable.
- **Solo admitidos**: en estudios de universidades selectivas, el acceso a datos puede estar restringido a quienes fueron admitidos — lo que introduce sesgo de colisionador.

::: {.boxcerebro}
**Para recordar:** Siempre pregunta: ¿mi muestra fue seleccionada o filtrada de alguna forma que depende del tratamiento o del resultado? Si la respuesta es sí, tienes un posible problema de colisionador.
:::

---

## Caso 3: Proxy contaminado {-}

### Intuición paso a paso {-}

Este caso combina elementos de los dos anteriores y es quizás el más difícil de detectar en la práctica.

Supongamos que hay una variable no observada $U$ que confunde la relación entre $D$ y $Y$. Idealmente querríamos controlar por $U$, pero no la tenemos. Sin embargo, tenemos $L$, que parece un buen proxy de $U$.

El problema: $L$ no solo captura $U$ — también es afectada por $D$.

La estructura es: $D \rightarrow L \leftarrow U \rightarrow Y$, con $L \rightarrow Y$ también.

**¿Por qué parece buena idea controlar por $L$?**

Porque $L$ contiene información sobre $U$, y controlar por $U$ sería lo correcto. El investigador ve que $L$ está correlacionado con $U$ y piensa que lo está usando como proxy del confounder.

**¿Por qué en realidad genera problema?**

Porque $L$ también recibe el efecto de $D$. Al condicionar en $L$, estás parcialmente controlando el efecto del tratamiento mismo — como en el caso del mediador — y además abriendo el camino espurio $D \leftrightarrow U \rightarrow Y$ — como en el caso del colisionador.

Es el peor de los dos mundos: sesga el coeficiente en dos direcciones distintas a la vez.

::: {.boxcerebro}
**Para recordar:** Si el supuesto "proxy de $U$" fue medido **después** del tratamiento, es casi seguro que está contaminado. Un proxy de confounder debe ser **pretratamiento**.
:::

### Ejemplo cotidiano {-}

Quieres saber si la educación universitaria aumenta el salario, controlando por habilidad (que no observas).

Tienes un test de habilidad disponible — pero el test fue tomado **después** de que la persona completó la universidad. La educación universitaria probablemente mejoró el desempeño en el test.

Así que el test es: $D \rightarrow \text{test} \leftarrow U_{\text{habilidad innata}} \rightarrow \text{salario}$.

Si controlas por el test, estás bloqueando parte del efecto de la educación (el que opera a través de habilidades adquiridas) y además abriendo el camino espurio entre educación y habilidad innata.

### Ejemplo econométrico y código Stata {-}

```stata
********************************************************************************
* CASO 3: PROXY CONTAMINADO
* Estructura: D -> L <- U -> Y  (y L -> Y también)
* L parece proxy de U, pero también es afectado por D
* Efecto verdadero de D sobre Y: 2
* Sin L: ≈ 2 (correcto, si D es exógeno)
* Con L: sesgo (mezcla efecto directo y camino espurio)
********************************************************************************

clear all
set more off
set seed 99999
set obs 2000

* U es el factor no observado (ej: habilidad innata)
gen double habilidad_innata = rnormal()         // U: no observable

* D es el tratamiento (ej: educación universitaria)
gen double educacion = rnormal()                // D: exógeno en este DGP

* L es el proxy contaminado (ej: test de habilidad tomado después del programa)
* Afectado por D (la educación mejora el test) y por U (habilidad innata también)
gen double test_tardio = 0.8 * educacion + 1.2 * habilidad_innata + rnormal()

* Y es el resultado (ej: salario)
* Afectado por D (efecto causal = 2) y por U (la habilidad también importa para el salario)
gen double salario = 2 * educacion + 1.5 * habilidad_innata + rnormal()

* ---------------------------------------------------------------
* REGRESIÓN CORRECTA: solo D (sin el proxy contaminado)
* Si D es exógeno (como en este DGP), recupera ≈ 2
* ---------------------------------------------------------------
di as text "=== Regresión correcta: efecto de educación sobre salario ==="
reg salario educacion, vce(robust)

* ---------------------------------------------------------------
* REGRESIÓN CON MAL CONTROL: incluimos el proxy contaminado
* El coeficiente de educación se desvía del valor verdadero (2)
* ---------------------------------------------------------------
di as text "=== Regresión con MAL CONTROL (proxy contaminado) ==="
reg salario educacion test_tardio, vce(robust)

* Nota: si pudiéramos controlar por habilidad_innata directamente, eso sería correcto.
* Pero test_tardio es un proxy impuro — contiene tanto U como D.
di as text "=== (Referencia) Regresión con control CORRECTO (si tuviéramos U) ==="
reg salario educacion habilidad_innata, vce(robust)
```

**Lectura esperada:**

- `reg salario educacion`: coeficiente $\approx 2$ (correcto en este DGP con $D$ exógeno, pero sin controlar el confusor).
- `reg salario educacion test_tardio`: coeficiente distorsionado — el proxy contaminado introduce sesgo.
- `reg salario educacion habilidad_innata`: coeficiente $\approx 2$ (control correcto, si lo tuviéramos).

### Monte Carlo: sesgo del proxy contaminado {-}

```stata
********************************************************************************
* MONTE CARLO: Proxy contaminado vs. control correcto vs. sin control
********************************************************************************

capture program drop mc_proxy
program define mc_proxy, rclass
    clear
    set obs 1000
    gen double U   = rnormal()                        // habilidad innata
    gen double D   = rnormal()                        // educación
    gen double L   = 0.8 * D + 1.2 * U + rnormal()   // proxy contaminado
    gen double Y   = 2 * D + 1.5 * U + rnormal()     // salario
    quietly reg Y D                      // sin controlar U ni proxy
    return scalar b_sin_control   = _b[D]
    quietly reg Y D L               // con proxy contaminado (mal)
    return scalar b_proxy_malo    = _b[D]
    quietly reg Y D U               // con U directamente (correcto, infactible)
    return scalar b_control_bueno = _b[D]
end

simulate b_sin_control=r(b_sin_control)     ///
         b_proxy_malo=r(b_proxy_malo)       ///
         b_control_bueno=r(b_control_bueno), ///
    reps(300) seed(99999): mc_proxy

* Valor verdadero de D sobre Y: 2
summarize b_sin_control b_proxy_malo b_control_bueno

twoway (hist b_sin_control,   width(.04) color(gray%40))   ///
       (hist b_proxy_malo,    width(.04) color(red%50))    ///
       (hist b_control_bueno, width(.04) color(navy%50)),  ///
       legend(order(1 "Sin control" 2 "Proxy contaminado (mal)" 3 "Control correcto (U)")) ///
       xline(2, lcolor(black) lpattern(dash))              ///
       title("Proxy contaminado: distribución del coeficiente de educación") ///
       xtitle("Estimado de educación") ytitle("Frecuencia")
```

---

## Tabla resumen: los tres casos {-}

<table style="width:100%; border-collapse:collapse; font-family:sans-serif; font-size:14px;">
  <thead>
    <tr style="background:#1e3a5f; color:white;">
      <th style="padding:10px; text-align:left;">Caso</th>
      <th style="padding:10px; text-align:left;">DAG</th>
      <th style="padding:10px; text-align:left;">Qué pasa si controlas</th>
      <th style="padding:10px; text-align:left;">Ejemplo</th>
    </tr>
  </thead>
  <tbody>
    <tr style="border-bottom:1px solid #e5e7eb;">
      <td style="padding:8px;"><b>1. Mediador / post-tratamiento</b></td>
      <td style="padding:8px;"><code>D → M → Y</code></td>
      <td style="padding:8px;">Bloqueas la ruta causal; el efecto total cae (a veces a cero)</td>
      <td style="padding:8px;">Educación → tipo de empleo → salario</td>
    </tr>
    <tr style="border-bottom:1px solid #e5e7eb;">
      <td style="padding:8px;"><b>2. Colisionador</b></td>
      <td style="padding:8px;"><code>D → C ← U → Y</code></td>
      <td style="padding:8px;">Abres un camino espurio entre D y U; el sesgo aparece por controlar</td>
      <td style="padding:8px;">Conexiones → contratación ← habilidad → productividad</td>
    </tr>
    <tr>
      <td style="padding:8px;"><b>3. Proxy contaminado</b></td>
      <td style="padding:8px;"><code>D → L ← U → Y</code></td>
      <td style="padding:8px;">Mezclas efecto del tratamiento y no observables; sesgo en ambas direcciones</td>
      <td style="padding:8px;">Educación → test tardío ← habilidad innata → salario</td>
    </tr>
  </tbody>
</table>

---

## Checklist antes de agregar un control a la regresión {-}

Antes de incluir cualquier variable de control, hazte estas preguntas:

1. **Timing:** ¿La variable fue determinada *antes* de que ocurriera el tratamiento? Si fue después, probablemente es un mal control.

2. **Causalidad:** ¿Podría el tratamiento haber afectado esta variable? Si la respuesta es sí, es un mediador o un proxy contaminado.

3. **Estructura DAG:** ¿Esta variable recibe flechas de dos fuentes distintas — una de ellas el tratamiento o el resultado? Si es así, puede ser un colisionador.

4. **Estimando:** ¿Qué quiero medir — efecto total o efecto directo? Si quiero el efecto total, no debo controlar nada que esté en la ruta causal.

5. **Selección muestral:** ¿Filtré o restringí la muestra de alguna forma que depende del tratamiento o del resultado? Si es así, introduje un colisionador implícito.

6. **Proxy tentador:** ¿Este "proxy del confounder" fue medido después del tratamiento? Si fue después, está contaminado.

::: {.boxcerebro}
**Regla simple para no equivocarse:**

Controla solo por variables que sean causas de $D$, causas de $Y$, o causas de ambas — **y que estén determinadas antes del tratamiento**. Nada más.

Si tienes dudas, dibuja el DAG. Esa es la herramienta para decidir.
:::

---

## Descarga los archivos {-}

**Descargar Stata do file:**
[Descargar Stata](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/10_BadControls/10_stata.do)

**Descargar R script:**
[Descargar R](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/10_BadControls/10_R.R)

**Descargar Python Notebook:**
[Descargar Python](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/10_BadControls/10_phyton.ipynb)

[![Abrir en Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/adiazescobar/libro_cortes/blob/main/dofile/10_BadControls/10_phyton.ipynb)
