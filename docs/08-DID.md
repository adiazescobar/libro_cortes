
# Diferencias en Diferencias {#did-teoria}

::: {.boxinfo}
**🎯 Metas de aprendizaje**

- Entender el problema de identificación que DiD resuelve.
- Comprender el supuesto de **tendencias paralelas** y por qué es el corazón de la estrategia.
- Derivar el estimador DiD desde los resultados potenciales.
- Interpretar cada coeficiente de la regresión DiD.
- Saber cómo probar (y cómo fallar) el supuesto de tendencias paralelas.
- Implementar DiD en Stata: manualmente, con `diff`, y con regresión.
:::

---

## El problema que DiD resuelve {-}

Suponga que quiere evaluar el efecto de un programa (subsidio, política, capacitación). El problema fundamental es que **no podemos observar al mismo individuo tratado y no tratado al mismo tiempo**.

La comparación más ingenua sería:

$$\text{Efecto aparente} = \underbrace{\bar{Y}_{\text{tratados}} - \bar{Y}_{\text{controles}}}_{\text{diferencia cruda}}$$

Pero esa diferencia puede existir incluso sin programa, simplemente porque los grupos eran distintos desde antes. Eso es **sesgo de selección**.

Una segunda idea: comparar el grupo tratado antes y después del programa:

$$\text{Efecto aparente} = \bar{Y}_{\text{tratados},\, t=1} - \bar{Y}_{\text{tratados},\, t=0}$$

Pero eso puede capturar cualquier cambio que hubiera ocurrido en ese periodo, con o sin programa (una recesión, una reforma, la maduración de los niños). Eso es el **problema de la tendencia temporal**.

**DiD resuelve ambos problemas a la vez** usando dos grupos (tratados y controles) y dos periodos (antes y después):

$$\widehat{\delta}_{DD} = \underbrace{(\bar{Y}_{T,1} - \bar{Y}_{T,0})}_{\text{cambio en tratados}} - \underbrace{(\bar{Y}_{C,1} - \bar{Y}_{C,0})}_{\text{cambio en controles}}$$

La lógica es: el cambio en el grupo de control nos dice cuánto habrían cambiado los tratados **en ausencia del programa**. La diferencia entre los dos cambios es el efecto del programa.

---

## La tabla 2×2 {-}

La estructura de datos de DiD siempre se puede organizar en una tabla:

|  | Antes ($t=0$) | Después ($t=1$) | **Primera diferencia** |
|---|---|---|---|
| **Controles** ($D=0$) | $\bar{Y}_{C,0}$ | $\bar{Y}_{C,1}$ | $\bar{Y}_{C,1} - \bar{Y}_{C,0}$ |
| **Tratados** ($D=1$) | $\bar{Y}_{T,0}$ | $\bar{Y}_{T,1}$ | $\bar{Y}_{T,1} - \bar{Y}_{T,0}$ |
| **Segunda diferencia** | — | — | $\widehat{\delta}_{DD}$ |

La **primera diferencia por fila** elimina diferencias fijas entre grupos.
La **segunda diferencia** (entre filas) elimina la tendencia temporal común.

---

## Resultados potenciales y el ATT {-}

Formalizamos con la notación de resultados potenciales:

- $Y_{it}(1)$: resultado del individuo $i$ en el periodo $t$ **si es tratado**.
- $Y_{it}(0)$: resultado del individuo $i$ en el periodo $t$ **si no es tratado**.

El efecto del tratamiento sobre los tratados (**ATT**) en $t=1$ es:

$$\text{ATT} = E\!\left[Y_{i,1}(1) - Y_{i,1}(0) \mid D_i = 1\right]$$

El problema: $Y_{i,1}(0)$ no se observa para los tratados después del tratamiento. Necesitamos un contrafactual.

DiD propone usar el cambio observado en los controles como estimación del cambio contrafactual en los tratados:

$$E\!\left[Y_{i,1}(0) - Y_{i,0}(0) \mid D_i = 1\right] \approx E\!\left[Y_{i,1}(0) - Y_{i,0}(0) \mid D_i = 0\right]$$

Esa igualdad es exactamente el **supuesto de tendencias paralelas**.

---

## El supuesto de tendencias paralelas {-}

::: {.boxcerebro}
**Supuesto de tendencias paralelas (formal):**

$$E\!\left[Y_{i,1}(0) - Y_{i,0}(0) \mid D_i = 1\right] = E\!\left[Y_{i,1}(0) - Y_{i,0}(0) \mid D_i = 0\right]$$

En ausencia del tratamiento, la evolución media del resultado en el grupo tratado habría sido **idéntica** a la del grupo de control.
:::

**¿Qué dice en palabras?**

No se exige que tratados y controles sean iguales en el periodo base — pueden tener niveles diferentes. Lo que se exige es que sus **tendencias** habrían sido las mismas si el programa no hubiera existido.

**¿Por qué es creíble?**

El supuesto es más plausible cuando:

- Los grupos son similares en características observables.
- El programa no fue asignado en función de la tendencia reciente de $Y$ (*Ashenfelter's dip*: cuidado si los tratados fueron seleccionados porque estaban en declive).
- Los periodos pre-tratamiento muestran tendencias paralelas (evidencia empírica).

**¿Qué lo viola?**

- Diferencias en la tendencia de crecimiento entre grupos (ej: los tratados tenían un crecimiento más rápido de antemano).
- Shocks simultáneos que afectan solo a un grupo.
- Selección de participantes basada en la trayectoria reciente de $Y$.

---

## Identificación del ATT bajo tendencias paralelas {-}

Bajo el supuesto de tendencias paralelas, el estimador DiD recupera el ATT:

$$
\widehat{\delta}_{DD} = \underbrace{E[Y_{i,1} \mid D_i=1]}_{\bar{Y}_{T,1}} - \underbrace{E[Y_{i,0} \mid D_i=1]}_{\bar{Y}_{T,0}} - \underbrace{\Big(E[Y_{i,1} \mid D_i=0] - E[Y_{i,0} \mid D_i=0]\Big)}_{\text{tendencia de los controles}}
$$

Sustituyendo resultados potenciales y usando independencia e igualdad de tendencias:

$$
= E[Y(1) - Y(0) \mid D=1] = \text{ATT}
$$

El estimador es insesgado para el ATT **si y solo si** el supuesto de tendencias paralelas se cumple.

---

## La regresión DiD {-}

El estimador de las cuatro medias se puede recuperar exactamente con:

$$Y_{it} = \alpha + \beta D_i + \gamma t_t + \delta (D_i \times t_t) + \varepsilon_{it}$$

Interpretación de cada coeficiente:

| Coeficiente | Significado |
|---|---|
| $\alpha$ | Media de los controles en el periodo base ($\bar{Y}_{C,0}$) |
| $\beta$ | Diferencia pre-tratamiento entre grupos ($\bar{Y}_{T,0} - \bar{Y}_{C,0}$) |
| $\gamma$ | Cambio temporal en los controles ($\bar{Y}_{C,1} - \bar{Y}_{C,0}$) |
| $\delta$ | **Estimador DiD** = efecto causal del programa (bajo tendencias paralelas) |

::: {.boxcerebro}
**El coeficiente que importa es $\delta$**, el de la interacción $D \times t$.
Los coeficientes $\beta$ y $\gamma$ por sí solos no tienen interpretación causal.
:::

**Equivalencia algebraica:** Los cuatro promedios se recuperan así:

$$\bar{Y}_{C,0} = \hat{\alpha}, \quad \bar{Y}_{T,0} = \hat{\alpha}+\hat{\beta}, \quad \bar{Y}_{C,1} = \hat{\alpha}+\hat{\gamma}, \quad \bar{Y}_{T,1} = \hat{\alpha}+\hat{\beta}+\hat{\gamma}+\hat{\delta}$$

---

## Prueba de tendencias paralelas {-}

El supuesto de tendencias paralelas **no es verificable** en el periodo de tratamiento: el contrafactual $Y_{it}(0)$ para los tratados en $t=1$ no existe. Solo podemos buscar evidencia indirecta.

**Con múltiples periodos pre-tratamiento** (y un identificador individual), Stata ofrece dos pruebas formales distintas — y es importante no confundirlas:

---

### Prueba de tendencias paralelas: `estat ptrends` {-}

```stata
xtdidregress (y) (treatment), group(id) time(year)
estat ptrends
```

**H₀:** Las tendencias lineales son paralelas en el periodo pre-tratamiento.

Lo que hace internamente: estima una regresión sobre los periodos pre-tratamiento con una interacción `tratado × tiempo` (como variable continua) y prueba si ese coeficiente es cero. Si se rechaza H₀, hay evidencia de que los grupos ya tenían pendientes distintas antes del programa.

Esta es la prueba de tendencias paralelas propiamente dicha.

---

### Prueba de anticipación: `estat granger` {-}

```stata
estat granger
```

**H₀:** No hubo efectos del tratamiento en anticipación (antes de que empezara).

Lo que hace internamente: construye indicadores de "leads" — una variable por cada periodo pre-tratamiento que toma valor 1 si el grupo es tratado y el tiempo ya superó ese umbral. Incluye todos esos indicadores en una regresión y los prueba conjuntamente con un F-test acumulado.

::: {.boxcerebro}
**Diferencia clave entre las dos pruebas:**

| Prueba | H₀ | ¿Qué detecta si se rechaza? |
|---|---|---|
| `estat ptrends` | Tendencias lineales iguales antes del tratamiento | Tendencias pre-existentes distintas entre grupos |
| `estat granger` | Sin efectos anticipados del tratamiento | Que los agentes cambiaron su comportamiento antes de que empezara el programa |

Son preguntas distintas. Un rechazo en `ptrends` no implica anticipación, y un rechazo en `granger` no implica que las tendencias no eran paralelas. Siempre se necesita un argumento teórico para interpretar cuál es el problema.
:::

---

### Visualización: `estat trendplots` y `estat grangerplot` {-}

```stata
estat trendplots   * medias observadas + tendencias lineales ajustadas por grupo
estat grangerplot  * event study: efectos específicos por periodo (pre y post)
```

`trendplots` es el gráfico diagnóstico de tendencias. `grangerplot` es el event study completo con los efectos por periodo y sus intervalos de confianza.

---

::: {.boxcerebro}
**Restricción técnica:** el código fuente de Stata exige **al menos 2 periodos pre-tratamiento** para correr `estat ptrends` o `estat granger`. Con solo un periodo antes del tratamiento, ambas pruebas fallan con error. En nuestra base (1 periodo antes, 1 después), estas pruebas no son ejecutables.
:::

**Con solo dos periodos (como en nuestra base):**

Con un único periodo antes y uno después, la prueba formal de pre-tendencias es **imposible**. El gráfico de tendencias medias que construimos en Stata es una *visualización del DiD*, no una prueba del supuesto. Para probar tendencias paralelas se necesitan **al menos 3 periodos** y un identificador de individuo.

---

## Amenazas adicionales a la validez {-}

Además de la violación de tendencias paralelas, DiD puede fallar por cuatro razones principales:

---

**1. Políticas simultáneas**

Si durante el mismo periodo ocurre otro evento o política que afecta **solo a uno de los dos grupos**, el cambio que atribuimos al programa puede ser en realidad causado por ese otro factor.

*Ejemplo:* Evaluamos un programa de empleo para madres en una ciudad. Al mismo tiempo, esa ciudad anuncia una inversión en guarderías públicas. El grupo tratado mejora, pero no podemos saber cuánto fue el programa y cuánto fueron las guarderías.

**¿Cómo detectarlo?** Revisando el contexto histórico y verificando que no hubo shocks diferenciales entre grupos durante el periodo de evaluación.

---

**2. Causalidad inversa**

El supuesto de tendencias paralelas puede fallar si **la selección al tratamiento depende de la trayectoria pasada del resultado**. Si los participantes fueron elegidos (o se autoseleccionaron) precisamente porque su resultado estaba cayendo, habrían subido de todas formas sin el programa — lo que se conoce como el **dip de Ashenfelter**.

*Ejemplo:* Un programa de capacitación laboral selecciona trabajadores que recientemente perdieron el empleo. Su tasa de empleo era baja antes del programa, pero habría subido naturalmente con el tiempo. DiD sobreestima el efecto.

**¿Cómo detectarlo?** Con múltiples periodos pre-tratamiento: si el grupo tratado ya tenía una tendencia ascendente antes del programa, el dip es visible.

---

**3. Spillovers (efectos de derrame)**

Si el tratamiento de algunos individuos **afecta el resultado de quienes están en el grupo de control**, el contrafactual queda contaminado. El grupo de control ya no representa lo que le habría pasado al grupo tratado sin el programa.

*Ejemplo:* Un subsidio a la contratación en empresas tratadas puede desplazar trabajadores desde empresas de control hacia las tratadas. El empleo en control baja no porque el programa fracase, sino porque los trabajadores se mueven. DiD subestima el efecto real.

**¿Cómo detectarlo?** Verificando que tratados y controles estén suficientemente separados geográfica o institucionalmente para que no haya interacciones.

---

**4. Anticipación**

Si los agentes saben que el programa llegará, pueden cambiar su comportamiento **antes** de que empiece. Esto hace que el periodo "antes" del tratamiento ya esté contaminado, y el estimador DiD subestima el efecto verdadero.

*Ejemplo:* Se anuncia en enero un subsidio a la inversión que entrará en vigor en julio. Las empresas tratadas empiezan a invertir desde enero. En julio, la diferencia pre-post parece pequeña, aunque el programa tuvo un efecto real desde el anuncio.

**¿Cómo detectarlo y corregirlo?** Redefinir la fecha de tratamiento como la fecha del **anuncio**, no la de implementación. Con event studies, los coeficientes pre-tratamiento serán distintos de cero si hay anticipación — pero esto es observacionalmente equivalente a tendencias no paralelas (ver sección anterior).

---

**5. Cambios en la composición de los grupos**

Si el tratamiento cambia quién permanece en la muestra (atrición diferencial), las medias post-tratamiento ya no son comparables. Por ejemplo, si el programa provoca que los participantes más débiles abandonen el programa, el promedio post-tratamiento mejora mecánicamente.

**¿Cómo detectarlo?** Comparando tasas de atrición entre tratados y controles, y analizando si las características de los que se van difieren entre grupos.

---

## Implementación en Stata {-}

### Preparación de los datos {-}

Descarga la base de datos y el do-file de esta clase:

::: {.boxejercicio}
📁 **Archivos de la clase**

* [base3.dta](dofile/08_DID/base3.dta) — base de datos (8 000 observaciones, niños con dos periodos de seguimiento)
* [08_DID.do](dofile/08_DID/08_DID.do) — do-file completo
:::

```stata
clear all
set mem 150m
capture log close
cd "RUTA_DE_TU_CARPETA/dofile/08_DID"
use "base3.dta"
```

Las variables clave:

| Variable | Descripción |
|---|---|
| `y` | Talla para la edad (z-score) |
| `D` | Indicador de tratamiento (1 = tratado, 0 = control) |
| `t` | Periodo (0 = antes, 1 = después) |
| `orden_n` | Orden de nacimiento del niño en el hogar |

```stata
* Verificar la estructura del panel
tab t
tab D
tab t D
```

---

### Estadísticas descriptivas y gráfico de tendencias {-}

```stata
* Promedios por grupo y periodo (la tabla 2x2)
table D t, c(mean y)

* Etiquetas para los gráficos
label define t 0 "Antes" 1 "Después", replace
label value t t
label define D 0 "Control" 1 "Tratado", replace
label value D D

* Gráfico de evolución temporal (prueba visual de tendencias paralelas)
preserve
collapse (mean) y, by(t D)
twoway (connected y t if D==1, msymbol(circle) lcolor(navy)) ///
       (connected y t if D==0, msymbol(triangle) lcolor(maroon)), ///
       legend(label(1 "Tratados") label(2 "Controles")) ///
       title("Tendencias medias por grupo") ///
       xtitle("Periodo") ytitle("Talla-para-edad (z-score)")
restore
```

**¿Qué muestra este gráfico?**

Con solo dos periodos, este gráfico **no es una prueba de tendencias paralelas** — es una visualización del DiD. Muestra cuánto cambió cada grupo entre antes y después, y permite ver intuitivamente de dónde viene el estimador. Para probar tendencias paralelas necesitaríamos al menos un periodo previo adicional y un identificador individual.

Lo que sí podemos verificar con este gráfico: que los grupos se movieron de forma diferente (si las pendientes difieren, el DiD captura esa diferencia). Pero no podemos saber si esa diferencia se debe al programa o a una tendencia pre-existente que no alcanzamos a ver.

---

### Comparación de medias por periodo {-}

```stata
* Diferencia tratados-controles antes del programa
ttest y if t == 0, by(D)

* Diferencia tratados-controles después del programa
ttest y if t == 1, by(D)
```

La diferencia cruda cambia entre periodos. **DiD** aísla si ese cambio se debe al programa o a una tendencia pre-existente.

---

### Estimador DiD paso a paso {-}

```stata
* Cuatro medias de la tabla 2x2
sum y if D == 0 & t == 0
scalar y_c0 = r(mean)       // Controles, antes

sum y if D == 0 & t == 1
scalar y_c1 = r(mean)       // Controles, después

sum y if D == 1 & t == 0
scalar y_t0 = r(mean)       // Tratados, antes

sum y if D == 1 & t == 1
scalar y_t1 = r(mean)       // Tratados, después

* Primera diferencia: cambio en cada grupo
scalar delta_tratados  = y_t1 - y_t0
scalar delta_controles = y_c1 - y_c0

* Segunda diferencia: DiD
scalar DD = delta_tratados - delta_controles

di "Cambio en tratados:  " delta_tratados
di "Cambio en controles: " delta_controles
di "Estimador DiD:       " DD
```

---

### Usando el comando `diff` {-}

```stata
ssc install diff, replace
diff y, t(D) p(t)
```

El comando `diff` reporta la tabla 2×2 completa (medias por grupo y periodo, primeras diferencias, y el estimador DiD) con su error estándar y el p-valor del test $H_0: \delta = 0$.

**La opción `test` de `diff`: prueba de balance, no de tendencias paralelas**

```stata
* test requiere especificar covariables con cov()
diff y, t(D) p(t) test cov(orden_n)
```

La opción `test` corre **t-tests de balance en el periodo base** ($t=0$), comparando tratados y controles en la variable de resultado y en las covariables especificadas. Lo que hace internamente es una regresión de cada variable sobre el indicador de tratamiento, restringida a $t=0$.

::: {.boxcerebro}
**Importante:** Esto es una prueba de **balance pre-tratamiento** (¿eran similares los grupos antes del programa?), **no** una prueba de tendencias paralelas (¿habrían tenido la misma trayectoria sin el programa?). Son dos cosas distintas. Un p-valor grande en el test de balance dice que los grupos eran similares en el periodo base, pero no dice nada sobre sus tendencias futuras.
:::

---

### Implementación con regresión {-}

```stata
* Regresión DiD: el coeficiente de D#t es el estimador
reg y D##t, robust

* Verificación: comparar con cálculo manual
* alpha      = y_c0
* beta (D)   = y_t0 - y_c0  (diferencia pre-tratamiento)
* gamma (t)  = y_c1 - y_c0  (tendencia de los controles)
* delta (D#t) = DD           (estimador DiD)
```

**Interpretación del resultado:**

El coeficiente de `D#t` (la interacción) es el estimador DiD. En esta base estima un efecto de aproximadamente **+0.18 desviaciones estándar** en la talla-para-edad, significativo al 5%. Bajo el supuesto de tendencias paralelas, ese es el efecto causal del programa sobre los niños tratados.

---

### Extensión con panel y primeras diferencias {-}

Con datos de panel, el estimador DiD es equivalente a una regresión en **primeras diferencias**:

$$\Delta Y_i = \delta D_i + \Delta \varepsilon_i$$

```stata
* Crear identificador individual (panel balanceado: 4000 niños x 2 periodos)
sort t orden_n
by t: gen id = _n

xtset id t

* Primeras diferencias = DiD en panel balanceado de 2 periodos
reg D.y D.D
```

Los dos métodos (regresión con interacción y primeras diferencias) entregan el mismo estimador cuando hay exactamente dos periodos y el panel es balanceado.

---

## Síntesis {-}

| Componente | ¿Qué elimina? | ¿Qué supone? |
|---|---|---|
| Primera diferencia (dentro de grupos) | Diferencias fijas entre grupos | Grupos observables |
| Segunda diferencia (entre grupos) | Tendencia temporal común | Tendencias paralelas |
| **DiD** | Ambas | **Tendencias paralelas** |

**El supuesto de tendencias paralelas no es verificable en el periodo de tratamiento.** Con múltiples periodos pre-tratamiento puede buscarse evidencia indirecta vía event study, pero con solo dos periodos eso es imposible.

Las principales amenazas a la validez del DiD son: (1) políticas simultáneas que afectan diferencialmente a los grupos, (2) causalidad inversa o dip de Ashenfelter, (3) spillovers que contaminan el grupo de control, (4) anticipación que contamina el periodo base, y (5) atrición diferencial entre grupos.

---

::: {.boxejercicio}
**✍️ Preguntas de reflexión**

1. Calcula el estimador DiD a mano con las cuatro medias de la tabla y verifica que coincide con el coeficiente de la interacción en la regresión.
2. ¿Por qué no podemos interpretar causalmente la diferencia cruda entre tratados y controles en $t=1$?
3. ¿Qué patrón esperarías ver en el gráfico de tendencias si el supuesto de tendencias paralelas **se viola**?
4. ¿Qué diferencia hay entre lo que prueba `estat ptrends` y lo que prueba `estat granger`? ¿Por qué importa distinguirlas?
:::

---

## Ejercicio con datos {-}

::: {.boxejercicio}
📁 **Do-file del ejercicio**

Descarga el do-file, córrelo en Stata y reporta tus resultados en el formulario:

* [08_DID_ejercicio.do](dofile/08_DID/08_DID_ejercicio.do) — ejercicio con `base3.dta` y `hospdd`

**Instrucciones:**

1. Descarga el do-file y cambia la ruta en la línea `cd "..."` a la carpeta donde tienes `base3.dta`.
2. Corre el do-file completo.
3. Anota los valores que aparecen en pantalla al final de cada sección.
4. Ingresa tus respuestas en el formulario a continuación.
:::

**Sección A — DiD básico (`base3.dta`)**

| Pregunta | Tu respuesta |
|---|---|
| A1a. Media controles antes | |
| A1b. Media controles después | |
| A1c. Media tratados antes | |
| A1d. Media tratados después | |
| A2. Estimador DiD (manual) | |
| A3. Estimador DiD (regresión) | |
| A3b. ¿Coinciden A2 y A3? (sí/no) | |
| A4. P-valor del test de balance en t=0 | |
| A4b. ¿Eran similares los grupos antes? | |

**Sección B — DiD múltiples periodos (`hospdd`)**

| Pregunta | Tu respuesta |
|---|---|
| B1. Estimador ATET (nuevo procedimiento) | |
| B2. F-stat de `estat ptrends` | |
| B2b. P-valor de `estat ptrends` | |
| B2c. ¿Se rechaza H0 de tendencias paralelas? | |
| B3. F-stat de `estat granger` | |
| B3b. P-valor de `estat granger` | |
| B3c. ¿Hay evidencia de anticipación? | |

**Envía tus respuestas aquí:**

```{=html}
<!-- Reemplazar FORM_ID por el formulario activo antes de publicar el ejercicio.
<iframe src="https://docs.google.com/forms/d/e/FORM_ID/viewform?embedded=true"
        width="100%" height="800" frameborder="0" marginheight="0" marginwidth="0"
        style="border-radius: 8px; margin-top: 1em;">
  Cargando formulario...
</iframe>
-->
```
