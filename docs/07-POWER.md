
# Poder Estadístico en Stata {#poder-estadistico-stata}


## Introducción {-}

En esta clase trabajaremos con el comando `power` de Stata para calcular tamaños de muestra, efectos mínimos detectables (MDE) y explorar cómo cambian estos cálculos cuando incorporamos controles consideramos cumplimiento parcial o diseñamos experimentos aleatorizados por conglomerados. Usaremos el *do-file* proporcionado y desglosaremos cada sección con su explicación y las fórmulas subyacentes. Nos basamos en la documentación oficial de Stata sobre el comando `power`: [Stata Power and Sample Size Reference Manual](https://www.stata.com/manuals/pss.pdf). y el do file de Poverty Action Lab desarrollado por Sabhya Gupta with input from Jack Cavanagh, Maya Duru, Mike Gibson, Sarah Kopperud, and Chris Udry: [Poverty Action Lab - Power Calculations in Stata](https://www.povertyactionlab.org/resource/power-calculations). Super recomendado leer el manual y el do file para entender todos los detalles.

## Sin controles{-}

### Tamaño de muestra para un efecto dado {-}

La primera sección calcula el tamaño de muestra requerido para detectar un efecto específico. Se define el poder deseado, la razón de asignación entre tratamiento y control (\(nratio\)), el nivel de significancia \(\alpha\), y se calcula la media (\(\bar{Y}\)) y la desviación estándar (\(s\)) de la variable de interés.

```stata
local power  = 0.8           // Poder deseado
local nratio = 1             // Razón N2/N1 (1 = tamaños iguales)
local alpha  = 0.05          // Nivel de significancia

// Calcular media y desviación estándar del outcome
sum $outcome if !missing($outcome)
local sd      = `r(sd)'
local baseline= `r(mean)'

// Definir el efecto como 0.3 desviaciones estándar
local effect  = `sd'*0.3
local treat   = `baseline' + `effect'

// Calcular tamaño de muestra para detectar ese efecto
power twomeans `baseline' `treat', power(`power') sd(`sd') nratio(`nratio') table

// Calcular tamaños de muestra para un rango de efectos posibles con 4870 observaciones
power twomeans `baseline', power(`power') sd(`sd') nratio(`nratio') n(4870) table
````

**Fórmula utilizada:** Para una prueba bilateral de igualdad de medias con varianzas iguales ($\sigma_1 = \sigma_2$), el tamaño de muestra total $N$ requerido para detectar una diferencia $\delta = \mu_2 - \mu_1$ con poder $1-\beta$ y nivel $\alpha$ es:

$$
N = 2 \times \frac{\bigl(z_{1-\alpha/2} + z_{1-\beta}\bigr)^2\,\sigma^2}{\delta^2},
$$

donde $\sigma$ es la desviación estándar común.

En el código, definimos $\delta = 0.3 \times s$ y llamamos a `power twomeans` para que Stata resuelva $N$ usando esta fórmula.

Al final, convertimos el efecto a un valor redondeado y extraemos el tamaño de muestra calculado:

```stata
local effect    = round(`effect',0.0001)
local samplesize= r(N)

// Mensaje explicativo
di as error "La muestra requerida es `samplesize' para detectar un efecto de `effect' con una probabilidad de `power' si el efecto es verdadero y nratio=`nratio'"
```

A continuación se exploran diferentes escenarios variando el efecto o la desviación estándar:

```stata
// Variar el tamaño del efecto (diff) y ver cómo cambia el N
power twomeans `baseline', power(`power') sd(`sd') nratio(`nratio') diff(0.1(0.15)2) table

// Variar la desviación estándar de 0.5 a 2 en pasos de 0.1
power twomeans `baseline' `treat', power(`power') sd(0.5(0.1)2) nratio(`nratio') table
```

Estas tablas permiten comparar cómo afectan los cambios en $\delta$ o en $\sigma$ al tamaño de muestra requerido.

### Efecto mínimo detectable (MDE) para un N dado {-}

En esta sección se fija el tamaño de muestra total $N$ y se calcula el efecto mínimo detectable ($\delta_{\min}$).

```stata
local power  = 0.8
local nratio = 1
local alpha  = 0.05
local N      = _N          // Número total de observaciones del dataset actual

quietly sum $outcome if !missing($outcome)
local sd       = `r(sd)'
local baseline = `r(mean)'

// Calcular el MDE dado N
power twomeans `baseline', n(`N') power(`power') sd(`sd') nratio(`nratio') table

local mde = round(`r(delta)',0.0001)

// Mensaje explicativo
di as error "El efecto mínimo detectable es `mde' dado un tamaño de muestra `N', nratio=`nratio' y poder=`power'"
```

**Fórmula utilizada:** Con $N$ fijo y asignación balanceada ($n_1=n_2=N/2$), el efecto mínimo detectable es:

$$
\delta_{\min} = \bigl(z_{1-\alpha/2} + z_{1-\beta}\bigr)\, \sigma\, \sqrt{\frac{2}{N}}.
$$

El código muestra cómo varía el MDE al modificar el tamaño de muestra o la razón de asignación:

```stata
// Cambiar N de 10 000 a 20 000 en pasos de 2 000
power twomeans `baseline', power(`power') sd(`sd') n(10000(2000)20000) nratio(`nratio') table

// Cambiar la razón de tratados a controles
power twomeans `baseline', n(`N') power(`power') sd(`sd') nratio(1(-0.2)0.1) table
```

Una disminución en `nratio` implica asignar una proporción mayor al grupo de control, lo que en general reduce el poder y aumenta el MDE.

### MDE para un N dado con variable binaria {-}

Cuando el resultado es binario (proporción de éxitos), se utiliza el comando `power twoproportions`.

```stata
global outcome2 desn_cr  // Variable binaria

local power  = 0.8
local nratio = 1
local alpha  = 0.05
local N      = _N

quietly sum $outcome2 if !missing($outcome2)
local sd       = `r(sd)'
local baseline = `r(mean)'

// Ejemplo: comparar una proporción de 0.08 frente a un rango de proporciones alternativas
power twoproportions 0.08 (0.01(0.005)0.1), power(0.8 0.9) graph

local mde = round(`r(delta)',0.0001)
```

**Fórmula utilizada:** Para comparar dos proporciones $p_1$ y $p_2$ con tamaño de muestra $n$ por grupo, el tamaño de muestra necesario es:

$$
n = \frac{\bigl(z_{1-\alpha/2} + z_{1-\beta}\bigr)^2\bigl[ p_1(1 - p_1) + p_2(1 - p_2) \bigr]}{(p_1 - p_2)^2},
$$

y el MDE se obtiene despejando $|p_1 - p_2|$ de esta expresión.

## Incorporando controles {-}

Para aumentar el poder, es común incluir covariables que expliquen parte de la variabilidad del resultado. Esto reduce la desviación estándar residual y permite detectar efectos más pequeños.

### Tamaño de muestra para un efecto dado con covariables {-}

```stata
local power  = 0.8
local nratio = 1
local alpha  = 0.05

local covariates $X         // Variables de control
local number_covariates: word count `covariates'

// Ajustar el outcome por las covariables
regress $outcome `covariates'

local res_sd = round(sqrt(`e(rss)'/`e(df_r)'), 0.0001)  // Desviación estándar residual

quietly sum $outcome if !missing($outcome)
local baseline = `r(mean)'
local sd       = `r(sd)'

local effect_cov = `sd' * 0.3
local treat      = `baseline' + `effect_cov'

// Calcular tamaño de muestra usando la desviación estándar residual
power twomeans `baseline' `treat', power(`power') sd(`res_sd') nratio(`nratio') alpha(`alpha') table

local effect_cov   = round(`effect_cov', 0.0001)
local samplesize_cov = `r(N)'

di as error "Se necesita un tamaño de muestra de `samplesize_cov' para detectar un efecto de `effect_cov' con poder=`power' y desviación residual=`res_sd' dado nratio=`nratio'"
```

La desviación residual reemplaza a $\sigma$ en la fórmula de tamaño de muestra:

$$
N = 2 \times \frac{\bigl(z_{1-\alpha/2}+z_{1-\beta}\bigr)^2\,\sigma_{\text{res}}^2}{\delta^2}.
$$

### MDE para un N dado con covariables {-}

```stata
local power  = 0.8
local nratio = 1
local alpha  = 0.05
local N_cov  = _N  // Tamaño de muestra total

// Ajustar el outcome por las covariables
regress $outcome `covariates'

local res_sd = round(sqrt(`e(rss)'/`e(df_r)'), 0.0001)

quietly sum $outcome if !missing($outcome)
local baseline = `r(mean)'

// Calcular MDE con la desviación residual
power twomeans `baseline', n(`N_cov') power(`power') sd(`res_sd') nratio(`nratio') alpha(`alpha') table

local mde_cov = round(`r(delta)', 0.0001)

di as error "El MDE es `mde_cov' dado N=`N_cov', nratio=`nratio', poder=`power' y desviación residual=`res_sd'"
```

## Tamaño de muestra con cumplimiento parcial (take-up) {-}

Cuando no todos los asignados al tratamiento lo reciben (o algunos controles sí lo reciben), el efecto observable se reduce. El efecto ajustado es el efecto real multiplicado por la diferencia de tasas de participación entre grupos:

$$
\delta_{\text{efectivo}} = \delta \times (\pi_{\text{treat}} - \pi_{\text{control}}).
$$

```stata
local power  = 0.8
local nratio = 1
local alpha  = 0.05

local takeup_treat   = 0.9
local takeup_control = 0.1

quietly sum $outcome if !missing($outcome)
local sd       = `r(sd)'
local baseline = `r(mean)'

local effect_tu = `sd' * 0.3        // Efecto con cumplimiento perfecto
local tu        = `takeup_treat' - `takeup_control'
local effect_tu = `effect_tu' * `tu'   // Efecto ajustado por cumplimiento
local treat_tu  = `baseline' + `effect_tu'

power twomeans `baseline' `treat_tu', power(`power') sd(`sd') nratio(`nratio') table

local samplesize_tu = `r(N)'
local effect_tu     = round(`effect_tu', 0.01)

di as error "Se necesita una muestra de `samplesize_tu' para detectar un efecto de `effect_tu' con poder=`power' si nratio=`nratio'"
```

## Diseños por conglomerados o grupos {-}

Cuando la unidad de aleatorización es un grupo (por ejemplo, escuelas), el cálculo de poder debe ajustarse por la correlación intra-grupo. Se introduce el ICC ($\rho$) y el efecto de diseño $DE = 1 + \rho(M-1)$, donde $M$ es el tamaño del clúster.

###  Número de clústeres para un efecto y tamaño de clúster dados {-}

```stata
global cluster_var educa_jefe  // Variable de conglomerado

local power  = 0.8
local nratio = 1
local alpha  = 0.05

quietly sum $outcome if !missing($outcome)
local sd       = `r(sd)'
local baseline = `r(mean)'

local cluster_size_control = 50
local mratio  = 1    // Razón M2/M1
local kratio  = 1    // Razón K2/K1

local effect_cluster = `sd' * 0.3
local treat         = `baseline' + `effect_cluster'

loneway $outcome $cluster_var    // Obtener ICC
local rho = `r(rho)'

// Calcular número de clústeres necesarios
power twomeans `baseline' `treat', cluster m1(`cluster_size_control') mratio(`mratio') kratio(`kratio') power(`power') sd(`sd') rho(`rho') alpha(`alpha') table

local effect_cluster = round(`effect_cluster',0.0001)
local n_clus_t = `r(K2)'
local n_clus_c = `r(K1)'

di as error "Se necesitan `n_clus_c' clústeres de control y `n_clus_t' de tratamiento para detectar un efecto de `effect_cluster' con poder=`power' y tamaño de clúster=`cluster_size_control'"
```

**Fórmula utilizada:** Con tamaños de clúster $M_1$ y $M_2$ fijos, y $\delta$ el efecto esperado, el número de clústeres de control $K_1$ se calcula como:

$$
K_1 = \frac{\bigl(z_{1-\alpha/2}+z_{1-\beta}\bigr)^2}{\delta^2} \left( \frac{\sigma_1^2 DE_1}{M_1} + \frac{\sigma_2^2 DE_2}{M_2 R_k} \right),
$$

donde $DE_j = 1 + \rho (M_j - 1)$ y $R_k = K_2/K_1$.

### Tamaño de clúster para un número dado de clústeres {-}

```stata
local power = 0.8
local nratio = 1
local alpha = 0.05

quietly sum $outcome if !missing($outcome)
local sd       = `r(sd)'
local baseline = `r(mean)'

bysort $cluster_var: gen control_cluster = _n==1
count if control_cluster & D == 0
local num_clusters_control = `r(N)'

local kratio = 1
local effect_cluster = `sd' * 0.3
local treat = `baseline' + `effect_cluster'

loneway $outcome $cluster_var
local rho = `r(rho)'

power twomeans `baseline' `treat', cluster k1(`num_clusters_control') kratio(`kratio') power(`power') sd(`sd') rho(`rho')

local clus_size_t = `r(M2)'
local clus_size_c = `r(M1)'

di as error "El tamaño mínimo de cada clúster debe ser `clus_size_c' en control y `clus_size_t' en tratamiento para detectar un efecto de `effect_cluster'"

drop control_cluster
```

###  MDE para un tamaño de clúster y número de clústeres dados {-}

```stata
local power  = 0.8
local nratio = 1
local alpha  = 0.05

quietly sum $outcome if !missing($outcome)
local sd       = `r(sd)'
local baseline = `r(mean)'

bysort $cluster_var: gen control_cluster = _n==1
count if control_cluster & D == 0
local num_clusters_control = `r(N)'

local kratio = 1
local cluster_size_control = 50
local mratio = 1

loneway $outcome $cluster_var
local rho = `r(rho)'

power twomeans `baseline', cluster k1(`num_clusters_control') kratio(`kratio') mratio(`mratio') m1(`cluster_size_control') power(`power') sd(`sd') rho(`rho') alpha(`alpha') table

local mde_cluster = round(`r(delta)', 0.0001)

di as error "El MDE es `mde_cluster' dado `num_clusters_control' clústeres de control, tamaño de clúster=`cluster_size_control' y kratio=`kratio'"

drop control_cluster
cap log close
```

En un diseño por conglomerados, el MDE se calcula como:

$$
|\delta| = \bigl(z_{1-\alpha/2}+z_{1-\beta}\bigr)\,\sigma_D,
$$

donde $\sigma_D = \sqrt{\sigma_1^2 DE_1/n_1 + \sigma_2^2 DE_2/n_2}$.

---

## Qué puede salir mal en un RCT {-}

::: {.boxinfo}
**🎯 Objetivos de la sección**

- Identificar los principales problemas que amenazan la validez interna y externa de un RCT
- Distinguir entre efectos de equilibrio parcial y general
- Reconocer los efectos de comportamiento (Hawthorne, John Henry, Placebo, Demanda)
- Conocer los tres pilares del Informe Belmont y sus implicaciones éticas
:::

Un experimento aleatorio bien diseñado elimina el sesgo de selección — pero no garantiza que el estimador sea el efecto causal que nos interesa. Hay al menos cinco grandes categorías de problemas que pueden comprometer la validez interna o externa de un RCT.

### 1. Externalidades y efectos de derrame (*spillovers*) {-}

El supuesto de no-interferencia de SUTVA (*Stable Unit Treatment Value Assumption*) exige que el resultado del individuo $i$ dependa **solo** de su propio tratamiento, no del de sus vecinos. Cuando esto falla, el grupo de control se "contamina":

- Un programa de vacunación que reduce la circulación del virus también protege a los no vacunados → el efecto de control sube, el efecto estimado cae.
- Dar microcrédito en un barrio puede hacer que los negocios del control pierdan clientes frente a los tratados.

**Solución:** aleatorizar a nivel de *cluster* (barrio, escuela, aldea) lo suficientemente grandes para que la interferencia quede *dentro* del grupo, no entre grupos. Alternativamente, diseños de dos brazos con zonas buffer.

### 2. Efectos de equilibrio general {-}

Los RCTs identifican el efecto de un programa a **escala pequeña**. Cuando el programa escala, los precios, salarios y comportamientos de equilibrio cambian — y el efecto puede ser muy distinto:

- Un programa de formación laboral para el 1% de los desempleados puede aumentar sus salarios. Para el 50%, la oferta adicional de trabajo capacitado reduce el salario de equilibrio.
- Los subsidios de vivienda que funcionan en pilotos elevan los alquiles cuando se universalizan.

**Implicación:** el efecto de equilibrio parcial (LATE, estimado en el RCT) puede sobrestimar o subestimar el efecto de equilibrio general relevante para política pública.

### 3. Efectos de comportamiento {-}

| Efecto | Descripción | Dirección del sesgo |
|--------|-------------|---------------------|
| **Hawthorne** | Los tratados cambian de comportamiento porque saben que los observan | Sobreestimación |
| **John Henry** | El control trabaja más al saber que está siendo comparado | Subestimación |
| **Placebo** | El control recibe algo y mejora solo por eso | Subestimación |
| **Demanda** | Los tratados responden lo que creen que el investigador espera | Sobreestimación |
| **Anticipación** | Control o tratados cambian antes de recibir el tratamiento | Sesgo ambiguo |

**Solución parcial:** doble ciego cuando sea posible; grupos de control activos; minimizar la visibilidad del experimento.

### 4. Problemas éticos: los tres pilares del Belmont Report {-}

Cualquier experimento con seres humanos debe cumplir tres principios fundamentales del [Informe Belmont (1979)](https://www.hhs.gov/ohrp/regulations-and-policy/belmont-report/index.html), que también regula la IRB (*Institutional Review Board*):

**I. Respeto a las personas (*Respect for persons*)**
Los participantes deben dar **consentimiento informado** libre y voluntario. Las poblaciones vulnerables (niños, prisioneros, personas con discapacidad cognitiva) requieren protecciones adicionales.

```{=html}
<div class="boxaudio">
<p class="audio-title">&#x1F3A7; Escucha antes de la clase &mdash; <em>The Three Pillars of Human Experimentation</em></p>
<p style="margin:0 0 0.8em 0; font-size:0.92em; color:#5b4a9e;">Resumen del Informe Belmont (1979): los tres principios &#xE9;ticos que rigen cualquier experimento con seres humanos.</p>
<audio controls>
  <source src="audio/The_Three_Pillars_of_Human_Experimentation.m4a" type="audio/mp4">
  Tu navegador no soporta el reproductor de audio.
</audio>
</div>
```


**II. Beneficencia (*Beneficence*)**
Maximizar el beneficio y minimizar el daño. Implica:
- No asignar al control cuando se sabe que el tratamiento funciona (problema del grupo de control con "tratamiento cero").
- Detener el experimento si hay evidencia de daño.

**III. Justicia (*Justice*)**
Los beneficios y cargas del experimento deben distribuirse equitativamente. No es justo experimentar solo con los más pobres o vulnerables para beneficiar a otros.

**Ejemplos históricos de violación:**
- Experimento de Tuskegee (1932-1972): 399 hombres afroamericanos con sífilis no recibieron penicilina aunque ya estaba disponible.
- Estudio de Guatemala (1940s): inoculación deliberada de ETS sin consentimiento.

### 5. Otros problemas comunes {-}

**Attrición diferencial:** si el grupo de tratamiento abandona el experimento más que el control (o viceversa), la muestra final ya no es aleatoria. Prueba: comparar tasas de attrición y características de los que salen por grupo.

**Incumplimiento (*non-compliance*):** no todos los asignados al tratamiento lo reciben; algunos del control lo buscan. El estimador OLS da el ITT (*Intent-to-Treat*); con IV se estima el LATE (*Local Average Treatment Effect*) para los *compliers*.

**Validez externa:** el LATE identifica el efecto para los *compliers* en el sitio de estudio. No es claro que ese efecto aplique a otras poblaciones, épocas o contextos → necesidad de múltiples RCTs en distintos contextos.

---

## Ejercicio aplicado: Bertrand y Mullainathan (2004) {-}

::: {.boxejercicio}
**📋 Ejercicio**

Descarga el do-file y los datos. Trabaja cada pregunta antes de ver el código de referencia.

- [Descargar BM_parcial.do](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/07_Power/BM_parcial.do)
- [Descargar bm.dta](https://www.dropbox.com/scl/fi/ephx1kl4opc0q3oxe5ckp/bm.dta?rlkey=zwp0hwtec5z25a4ll9qn8biz7&dl=1)
:::

### El experimento {-}

Marianne Bertrand y Sendhil Mullainathan enviaron **4.870 hojas de vida ficticias** a empleadores en Boston y Chicago en respuesta a anuncios de empleo reales. Las hojas de vida eran idénticas en calificaciones, pero los nombres variaban: algunos eran claramente de personas blancas (*Emily, Greg*) y otros claramente de personas afroamericanas (*Lakisha, Jamal*).

**Variable de tratamiento:** `black` = 1 si el nombre suena afroamericano, 0 si suena blanco.
**Variable de resultado:** `call` = 1 si el empleador llamó de regreso.

Los datos están disponibles aquí: [Descargar bm.dta](https://www.dropbox.com/scl/fi/ephx1kl4opc0q3oxe5ckp/bm.dta?rlkey=zwp0hwtec5z25a4ll9qn8biz7&dl=1)

::: {.boxinfo}
**Variables disponibles en `bm.dta`**

| Variable | Descripción |
|---|---|
| `black` | 1 = nombre afroamericano (tratamiento) |
| `call` | 1 = recibió llamada de regreso (resultado) |
| `female` | 1 = nombre femenino |
| `yearsexp` | Años de experiencia en la hoja de vida |
| `education` | Nivel educativo |
| `ofjobs` | Número de empleos anteriores |
| `computerskills` | 1 = menciona habilidades informáticas |
:::

### Preguntas del ejercicio {-}

**1. ¿Por qué un experimento?**
Explique por qué no sirve comparar simplemente a individuos negros *contratados* vs. blancos *contratados*. ¿Qué tipo de sesgo generaría esa comparación y en qué dirección?

**2. Verificación de la aleatorización**
Evalúe si la asignación aleatoria fue exitosa comparando las características de las hojas de vida entre el grupo de nombres blancos y el de nombres afroamericanos. Interprete los resultados.

**3. Efecto del tratamiento**
Estime el efecto de tener un nombre afroamericano sobre la probabilidad de recibir una llamada:
- Modelo sin controles
- Modelo con controles (educación, experiencia, número de empleos, habilidades informáticas, género)
¿Por qué debería —o no— cambiar el estimador al agregar controles en un experimento aleatorio? ¿Qué pasa en la práctica con estos datos?

**4. Efectos heterogéneos**
Estime al menos dos modelos de efectos heterogéneos:
- Uno con una variable **binaria** (género del nombre: `female`)
- Uno con una variable **continua** (años de experiencia: `yearsexp`)
¿Qué revela la interacción sobre el retorno a la "calidad" de la hoja de vida según raza?

**5. Poder estadístico**
Con la información de los datos:
- ¿Qué poder tiene la muestra de 4.870 observaciones para detectar el efecto observado?
- ¿Cuál es el efecto mínimo detectable (MDE) con esa muestra?
- ¿Cuántas observaciones se necesitarían para detectar la mitad del efecto?

### Código de referencia {-}

```stata
* Cargar datos
use "https://www.dropbox.com/scl/fi/ephx1kl4opc0q3oxe5ckp/bm.dta?rlkey=zwp0hwtec5z25a4ll9qn8biz7&dl=1", clear

* ── Pregunta 2: Balance ──
tabstat yearsexp education ofjobs computerskills female, ///
        by(black) stat(mean) format(%6.3f) nototal

foreach var of varlist yearsexp education ofjobs computerskills female {
    quietly ttest `var', by(black)
    di as text "`var': p = " %5.3f r(p)
}

* ── Pregunta 3: Efecto promedio ──
* Sin controles
reg call black, robust

* Con controles
reg call black female yearsexp education ofjobs computerskills, robust

* ── Pregunta 4a: Heterogeneidad por género (binaria) ──
reg call i.black##i.female, robust
margins female, dydx(black)
marginsplot

* ── Pregunta 4b: Heterogeneidad por experiencia (continua) ──
reg call c.black##c.yearsexp, robust
margins, dydx(black) at(yearsexp=(0(2)20))
marginsplot

* ── Pregunta 5: Poder estadístico ──
quietly sum call if black == 0
local p_white = r(mean)
quietly sum call if black == 1
local p_black = r(mean)

di "Tasa callback blancos:       " %5.4f `p_white'
di "Tasa callback afroamericanos: " %5.4f `p_black'

* 5a. Poder dado N=4870
power twoproportions `p_white' `p_black', n(4870) alpha(0.05)

* 5b. MDE dado N=4870 y poder=0.80
power twoproportions `p_white', n(4870) power(0.80) alpha(0.05)

* 5c. N para detectar la mitad del efecto
local p_mitad = `p_white' - (`p_white' - `p_black') / 2
power twoproportions `p_white' `p_mitad', power(0.80) alpha(0.05)
```

## DESCARGA LOS DOCUMENTOS {-}

**Descargar Stata do file**:
[Descargar Stata](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/07_Power/07_stata.do)

**Descargar do-file ejercicio Bertrand & Mullainathan**:
[Descargar BM_parcial.do](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/07_Power/BM_parcial.do)

**Descargar R script**:
[Descargar R](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/07_Power/07_R.R)

**Descargar Python Notebook**:
[Descargar Python](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/07_Power/07_phyton.ipynb)

[![Abrir en Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/adiazescobar/libro_cortes/blob/main/dofile/07_Power/07_phyton.ipynb)

**Descarga los Datos (ejercicio clase)**:
[Descargar Datos](https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/Clase0_StataBasics/hh_98.dta)

**Descarga los Datos (ejercicio B&M)**:
[Descargar bm.dta](https://www.dropbox.com/scl/fi/ephx1kl4opc0q3oxe5ckp/bm.dta?rlkey=zwp0hwtec5z25a4ll9qn8biz7&dl=1)
