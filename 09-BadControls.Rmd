# Malos controles — Clase teórica {#bad-controls-teoria}

::: {.boxinfo}
**🎯 Metas de aprendizaje**

- Comprender el **supuesto de tendencias paralelas** en Diferencias en Diferencias (DID).  
- Entender la diferencia entre **buenos y malos controles**.  
- Relacionar los **resultados potenciales** con la validez de las estimaciones.  
- Analizar las implicaciones de usar (o no usar) controles en una regresión.  
:::



## El supuesto de Tendencias Paralelas {-}

El corazón de la estrategia de **Diferencias en Diferencias (DID)** es el supuesto de que, en ausencia del tratamiento, la evolución de los grupos tratados y de control habría sido **paralela** en el tiempo.

\[
Y_{it} = \alpha + \beta D_i + \delta T_t + \gamma (D_i \times T_t) + \varepsilon_{it}
\]

donde:  
- \(D_i\): indicador de tratamiento,  
- \(T_t\): indicador de post-tratamiento,  
- \(\gamma\): estimador DID.  

El supuesto clave es:

\[
E[Y_{i,t=1}(D=0) - Y_{i,t=0}(D=0) \mid D=1] = E[Y_{i,t=1}(D=0) - Y_{i,t=0}(D=0) \mid D=0]
\]

Si esto se cumple, la estimación DID es **insesgada**.


## Resultados Potenciales {-}

Recordemos la notación:

- \(Y_i(D=1)\): resultado del individuo \(i\) si recibe tratamiento.
- \(Y_i(D=0)\): resultado del individuo \(i\) si no recibe tratamiento.

El **ATE** se define como:

\[
ATE = \mathbb{E}[Y_i(D=1) - Y_i(D=0)]
\]

En DID, el estimador busca identificar justamente esa diferencia, bajo el supuesto de tendencias paralelas.


## Primeras Diferencias y DID {-}

En primeras diferencias:

\[
\Delta Y_i = Y_{i,t=1} - Y_{i,t=0}
\]

y en DID:

\[
DID = \big( E[\Delta Y_i \mid D=1] - E[\Delta Y_i \mid D=0] \big)
\]


## Controles en DID {-}

### Buenos controles   {-}
- Características **predeterminadas**, no afectadas por el tratamiento.  
- Ejemplo: educación, experiencia laboral previa, género, edad.  
- Se pueden incluir para:  
  - Mejorar la **precisión** de la estimación.  
  - Aumentar la potencia estadística.  
  - Reducir varianza residual.  

::: {.boxcerebro}
**Definición formal:**  
Un **buen control** es toda característica observable que **no cambia como consecuencia del tratamiento**.  
Son variables predeterminadas que ayudan a explicar el resultado pero no están mediadas por el programa.  
:::

**Ejemplo informal:**  
Si evaluamos el efecto de un programa de formación laboral:  
- **Buen control**: nivel educativo alcanzado antes del programa.  
- **Buen control**: años de experiencia previa.  



### Malos controles  {-}
- Variables que **sí son afectadas por el tratamiento** (variables de resultado o mediadoras).
- Incluirlas rompe la interpretación causal porque introducen **endogeneidad**.  
- Al condicionar en una variable que está “en el camino causal”, se elimina parte del efecto verdadero.  

::: {.boxcerebro}
**Definición formal:**  
Un **mal control** es toda variable que se encuentra **dentro del mecanismo causal** que conecta el tratamiento con el resultado.  
Condicionar en ella genera **sesgo de post-tratamiento**.  
:::

**Ejemplo informal:**  
Siguiendo el programa de formación laboral:  
- **Mal control**: salario después de haber recibido la capacitación.  
- **Mal control**: horas trabajadas después del programa.  
- **Mal control**: sector de empleo obtenido gracias al programa.  

👉 Todas estas variables son consecuencias (directas o indirectas) del tratamiento. Si las incluimos como controles, estamos **borrando** parte del impacto que queremos medir.  

---

### Implicaciones prácticas {-}

- **Con buenos controles**:  
  - El coeficiente del DID mantiene **interpretación causal**.  
  - Mejora la precisión de la estimación.  

- **Con malos controles**:  
  - El estimador se **vuelve inconsistente**.  
  - Perdemos la interpretación causal → ya no estamos midiendo el efecto del programa, sino algo condicionado en una consecuencia del mismo.  

En palabras simples:  
> “Un buen control aclara la foto, un mal control la distorsiona”.


## Supuestos y Consistencia {-}

El estimador DID será insesgado si:

\[
E[Y_{i}(D=0) \mid D=1, t=1] - E[Y_{i}(D=0) \mid D=1, t=0] = E[Y_{i}(D=0) \mid D=0, t=1] - E[Y_{i}(D=0) \mid D=0, t=0]
\]

- Si los grupos no siguen tendencias paralelas → **sesgo**.  
- Si incluimos malos controles → **sesgo adicional**.  
- Con buenos controles → **más precisión**, pero no cambia la interpretación causal.

---

## La demostración formal: sesgo de agrupación {-}

Esta sección desarrolla el argumento de **Angrist y Pischke** (*Mostly Harmless Econometrics*, cap. 3) que muestra exactamente qué pasa cuando incluimos un mal control. Es la demostración que vimos en clase.

### El escenario {-}

Suponga que el tratamiento $D$ afecta **dos** cosas: el resultado de interés $Y$ **y** una variable $F$:

$$D \rightarrow Y \qquad \text{y} \qquad D \rightarrow F$$

Un ejemplo concreto: $D$ = acceso a educación universitaria, $Y$ = salario, $F$ = trabajar en un empleo de alta calificación (STEM). La educación sube el salario *y* aumenta la probabilidad de entrar a STEM.

Como el programa es aleatorio, los resultados potenciales son independientes del tratamiento:

$$Y(D=1),\; Y(D=0),\; F(D=1),\; F(D=0) \;\perp\; D$$

### Sin controlar $F$: el estimador naive es el ATE {-}

Con la regresión sin control:
$$Y = \alpha + \tau D + \varepsilon$$

el estimador de diferencias de medias es:

$$\hat{\tau}_{\text{naive}} = E[Y \mid D=1] - E[Y \mid D=0] = E[Y(D=1)] - E[Y(D=0)] = \text{ATE}$$

Es insesgado y consistente. Todo bien hasta aquí.

### ¿Qué pasa si incluimos $F$ como control? {-}

Ahora agregamos $F$ a la regresión:
$$Y = \alpha + \tau D + \beta F + \varepsilon$$

y comparamos tratados y controles **dentro de cada valor de $F$**. Por ejemplo, para $F=1$:

$$E[Y \mid D=1,\, F=1] \;-\; E[Y \mid D=0,\, F=1] \;=\; ?$$

Para calcular esto usamos la ecuación de switching:

$$Y_i = D_i \cdot Y_i(D=1) + (1-D_i)\cdot Y_i(D=0), \qquad F_i = D_i \cdot F_i(1) + (1-D_i)\cdot F_i(0)$$

**Término izquierdo** — entre los tratados con $F=1$:

$$E[Y \mid D=1,\, F=1] = E\!\left[Y(D=1) \mid D=1,\; F(1)=1\right] = E\!\left[Y(D=1) \mid F(1)=1\right]$$

El último paso usa la independencia $Y(D=1) \perp D$.

**Término derecho** — entre los controles con $F=1$. Aquí está el truco: para los controles ($D=0$), $F=1$ significa que $F(D=0)=1$, **no** $F(D=1)=1$. Son grupos distintos.

$$E[Y \mid D=0,\, F=1] = E\!\left[Y(D=0) \mid D=0,\; F(0)=1\right] = E\!\left[Y(D=0) \mid F(0)=1\right]$$

### La descomposición en cuatro términos {-}

Juntando los dos lados y sumando y restando $E[Y(D=0) \mid F(1)=1]$:

$$
E[Y \mid D=1,\, F=1] - E[Y \mid D=0,\, F=1]
$$

$$
= \underbrace{E[Y(D=1) \mid F(1)=1]}_{\textcircled{1}} - \underbrace{E[Y(D=0) \mid F(0)=1]}_{\textcircled{2}}
$$

Sumamos y restamos $E[Y(D=0)\mid F(1)=1]$:

$$
= \Big(\underbrace{E[Y(D=1) \mid F(1)=1]}_{\textcircled{1}} - \underbrace{E[Y(D=0) \mid F(1)=1]}_{\textcircled{4}}\Big) + \Big(\underbrace{E[Y(D=0) \mid F(1)=1]}_{\textcircled{3}} - \underbrace{E[Y(D=0) \mid F(0)=1]}_{\textcircled{2}}\Big)
$$

Lo que resulta en:

$$
\boxed{E[Y \mid D=1,\, F=1] - E[Y \mid D=0,\, F=1] = \underbrace{E[Y(D=1)-Y(D=0) \mid F(1)=1]}_{\text{ATE}_{F=1}} + \underbrace{E[Y(D=0) \mid F(1)=1] - E[Y(D=0) \mid F(0)=1]}_{\text{Sesgo de agrupación}}}
$$

### ¿Qué significa cada término? {-}

**Término 1 — $\text{ATE}_{F=1}$:** el efecto promedio del tratamiento **para las personas que habrían llegado a $F=1$ si fueran tratadas**. No es el ATE de toda la población; es un ATE condicional en un subgrupo particular.

**Término 2 — Sesgo de agrupación:** compara el resultado contrafactual $Y(D=0)$ de dos grupos diferentes:

- $E[Y(D=0) \mid F(1)=1]$: resultado sin tratamiento de quienes *habrían* llegado a $F=1$ **si fueran tratados**.
- $E[Y(D=0) \mid F(0)=1]$: resultado sin tratamiento de quienes *habrían* llegado a $F=1$ **si no fueran tratados**.

Estos son grupos distintos. El tratamiento $D$ "reorganiza" quién termina en $F=1$: con educación universitaria, más personas entran a empleos STEM (incluyendo algunas con menor habilidad innata). Sin educación, solo los más hábiles llegan a STEM. Por eso $E[Y(D=0)\mid F(1)=1] < E[Y(D=0)\mid F(0)=1]$: el sesgo de agrupación es negativo y **atenúa** el efecto estimado.

::: {.boxcerebro}
**Intuición de una línea:**
Al condicionar en $F$, estamos comparando grupos que son diferentes no solo en $D$, sino también en características no observables correlacionadas con $F$. Eso introduce un sesgo que no desaparece aunque $D$ sea aleatorio.
:::

### El ejemplo de Angrist y Pischke {-}

- $D$ = acceso a educación universitaria (aleatorio)
- $Y$ = salario
- $F$ = trabaja en sector STEM

**Sin controlar $F$:** el estimador recupera el efecto total de la educación sobre el salario. ✓

**Controlando $F$:** comparamos personas en STEM con y sin educación universitaria. Pero quienes están en STEM *sin* educación universitaria son los más talentosos (solo ellos llegan ahí sin el título). Quienes están en STEM *con* educación incluyen personas de habilidad promedio que entraron gracias al título. Al comparar los dos grupos dentro de STEM, el grupo control parece mejor, lo que hace que el efecto estimado de la educación parezca menor. El sesgo de agrupación va en la dirección contraria al efecto real.

**Moraleja:**

$$y = \alpha + \tau D + \beta F + \varepsilon \quad \longrightarrow \quad \hat{\tau} \text{ no mide el ATE}$$

La estimación de $\tau$ ya no es ni insesgada ni consistente para el efecto causal de interés. El coeficiente de $D$ en esta regresión mezcla el $\text{ATE}_{F=1}$ (que no es el ATE de la población) con el sesgo de agrupación.

---

## Moraleja Final {-}

1. El supuesto de tendencias paralelas es **fundamental**.  
2. Los buenos controles ayudan; los malos controles dañan.  
3. DID se entiende mejor desde los **resultados potenciales**.  
4. La interpretación causal depende estrictamente de los **supuestos**.

---

::: {.boxejercicio}
**✍️ Ejercicio para reflexionar**

1. Da un ejemplo de **buen control** y de **mal control** en la evaluación de un programa de transferencias condicionadas.  
2. ¿Por qué incluir el nivel de ingreso post-programa sería un mal control en DID?  
3. ¿Cómo verificarías gráficamente el supuesto de tendencias paralelas antes de correr la regresión?  
:::
