
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

El supuesto de tendencias paralelas no es verificable en el periodo de tratamiento (no existe el contrafactual). Pero sí puede **ponerse a prueba en periodos previos**.

Si los grupos tenían tendencias paralelas *antes* del tratamiento, ganamos confianza en que las habrían tenido en el periodo de tratamiento.

**Prueba de pre-tendencias (event study):**

Con múltiples periodos pre-tratamiento $t = -k, \ldots, -1, 0, 1, \ldots$:

$$Y_{it} = \alpha_i + \lambda_t + \sum_{k \neq -1} \delta_k \cdot \mathbf{1}[t = k] \cdot D_i + \varepsilon_{it}$$

- Los $\delta_k$ para $k < 0$ deben ser estadísticamente indistinguibles de cero.
- Si algún $\delta_k < 0$ es significativo, hay evidencia de pre-tendencias diferenciales → el supuesto falla.

**Con solo dos periodos (como en nuestra base):**

Solo podemos verificar que el nivel inicial difiere pero no la tendencia. La prueba completa requiere al menos tres periodos.

---

## Amenazas adicionales a la validez {-}

Además de la violación de tendencias paralelas, DiD puede fallar por:

**1. El dip de Ashenfelter (*Ashenfelter's dip*)**
Los participantes fueron seleccionados justo cuando estaban en un punto bajo transitorio. Sin el programa, habrían subido de todas formas (regresión a la media). DiD sobre-estima el efecto.

**2. Efectos de derrame (*spillovers*)**
Si el tratamiento afecta también al grupo de control (por competencia, contagio, o cambios de precios), el contrafactual se contamina y DiD puede subestimar el efecto.

**3. Cambios en la composición de los grupos**
Si el tratamiento cambia quién permanece en la muestra (atrición diferencial), las medias post-tratamiento ya no son comparables.

**4. SUTVA**
El resultado potencial de cada individuo debe ser independiente del tratamiento de los demás. Si hay interacciones entre individuos, SUTVA se viola.

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

**¿Qué buscamos en este gráfico?**
Que las dos líneas sean aproximadamente paralelas en el periodo pre-tratamiento. Una pendiente diferente antes del programa ya sería evidencia de tendencias no paralelas.

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

El comando `diff` reporta directamente el estimador, su error estándar y el p-valor del test $H_0: \delta = 0$.

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

**El supuesto de tendencias paralelas no es verificable en el periodo de tratamiento, pero sí en periodos previos.** Siempre reporte el gráfico de tendencias pre-tratamiento como evidencia de validez.

---

::: {.boxejercicio}
**✍️ Ejercicio**

1. Calcula el estimador DiD a mano con las cuatro medias de la tabla y verifica que coincide con el coeficiente de la interacción en la regresión.
2. ¿Por qué no podemos interpretar causalmente la diferencia cruda entre tratados y controles en $t=1$?
3. En el gráfico de tendencias, ¿qué patrón esperarías ver si el supuesto de tendencias paralelas **se viola**?
4. ¿Qué problema introduce el dip de Ashenfelter y cómo lo detectarías con datos de múltiples periodos?
:::
