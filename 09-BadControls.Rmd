# Test de Tendencias Paralelas y Malos Controles

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
E[Y_{i,t=1}(0) - Y_{i,t=0}(0) \mid D=1] = E[Y_{i,t=1}(0) - Y_{i,t=0}(0) \mid D=0]
\]

Si esto se cumple, la estimación DID es **insesgada**.


## Resultados Potenciales {-}

Recordemos la notación:

- \(Y_i(1)\): resultado del individuo \(i\) si recibe tratamiento.  
- \(Y_i(0)\): resultado del individuo \(i\) si no recibe tratamiento.  

El **ATE** se define como:

\[
ATE = E[Y_i(1) - Y_i(0)]
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
- Variables que **sí pueden variables de resultados**.  
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
E[Y_{i}(0) \mid D=1, t=1] - E[Y_{i}(0) \mid D=1, t=0] = E[Y_{i}(0) \mid D=0, t=1] - E[Y_{i}(0) \mid D=0, t=0]
\]

- Si los grupos no siguen tendencias paralelas → **sesgo**.  
- Si incluimos malos controles → **sesgo adicional**.  
- Con buenos controles → **más precisión**, pero no cambia la interpretación causal.

---

## Sesgo de Agrupación {-}

Otro problema común es el **sesgo de agrupación**, que ocurre cuando:  
- Se promedian resultados sin respetar la estructura de tratamiento y control.  
- Se incluyen controles endógenos que re-agrupan la variación.

La moraleja es:  
👉 Siempre analizar qué variables se incluyen y si cumplen el criterio de ser predeterminadas.

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
