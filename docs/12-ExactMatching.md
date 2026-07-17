# Emparejamiento Exacto

::: {.boxinfo}
**Metas de aprendizaje**

- Explicar por qué la comparación directa en datos observacionales puede estar sesgada
- Definir el ATT bajo emparejamiento exacto
- Identificar los supuestos de observabilidad, soporte común y suficientes controles
- Entender la maldición de las dimensiones y la motivación para PSM
:::

---

## El problema: identificación sin aleatorización {-}

En un experimento aleatorizado (RCT), la asignación al tratamiento garantiza que los grupos tratado y control son, en promedio, idénticos en todas las características — observables **y no observables**. El estimador de diferencia de medias produce el efecto causal del tratamiento.

En datos **observacionales**, esto no está garantizado. Los individuos se autoseleccionan al tratamiento: las personas que deciden estudiar un posgrado, adoptar una tecnología o participar en un programa social son **sistemáticamente distintas** a quienes no lo hacen. Comparar sus resultados directamente mezcla el efecto del tratamiento con diferencias preexistentes.

$$\underbrace{E[Y_i \mid D_i = 1] - E[Y_i \mid D_i = 0]}_{\text{Diferencia de medias observada}} = \underbrace{ATT}_{\text{Efecto causal}} + \underbrace{E[Y_i(D=0) \mid D_i = 1] - E[Y_i(D=0) \mid D_i = 0]}_{\text{Sesgo de selección}}$$

El término de sesgo de selección refleja que los tratados y los controles tendrían resultados distintos **incluso en ausencia del tratamiento**. El objetivo de los métodos de emparejamiento es eliminar este sesgo controlando por las características observables que generan la selección.

---

## La idea del emparejamiento exacto {-}

La intuición es simple: **construir un clon** para cada individuo tratado.

Si encontramos, para cada persona tratada $i$, un individuo no tratado $j$ que tiene exactamente las mismas características observables $X$ (edad, educación, género, región, etc.), entonces la única diferencia restante entre $i$ y $j$ es el tratamiento. La comparación de sus resultados aproxima el efecto causal.

```text
Tratado i:  X = (35 años, universitario, mujer, Bogotá) → Y_i(D=1)
Control j:  X = (35 años, universitario, mujer, Bogotá) → Y_j(D=0)

ATT ≈ Y_i(D=1) - Y_j(D=0)
```

El emparejamiento exacto replica, de forma no paramétrica, el principio del experimento: dentro de cada "celda" definida por los valores de $X$, la asignación al tratamiento es como si fuera aleatoria.

---

## Los tres requisitos del emparejamiento {-}

Para que el emparejamiento exacto sea válido se necesitan tres condiciones:

### 1. Observabilidad {-}

Todas las variables que determinan simultáneamente la selección al tratamiento **y** el resultado deben ser **observables y medibles**. Esto es el supuesto de **Independencia Condicional (CIA)**:

$$\{Y_i(D=1), Y_i(D=0)\} \perp D_i \mid X_i$$

Una vez controlamos por $X$, el tratamiento es "como si fuera" aleatorio. Si hay variables no observadas que afectan tanto la selección como el resultado (por ejemplo, habilidad no medida, motivación, conexiones sociales), el emparejamiento no resuelve el problema de identificación.

### 2. Soporte común {-}

Para cada valor posible de $X$, debe haber individuos tanto en el grupo tratado como en el grupo de control:

$$0 < P(D_i = 1 \mid X_i = x) < 1 \quad \forall x$$

Si hay valores de $X$ donde solo hay tratados (o solo hay controles), no podemos construir el clon y el ATT no está identificado en esa región del soporte.

### 3. Suficientes controles {-}

El grupo de control debe ser suficientemente grande para encontrar un clon para cada tratado. En la práctica esto significa que necesitamos un grupo de control mucho mayor que el grupo de tratados — la regla general es al menos 5:1.

---

## La maldición de las dimensiones {-}

El emparejamiento exacto enfrenta un problema fundamental cuando el vector $X$ tiene muchas dimensiones: la **maldición de las dimensiones** (*curse of dimensionality*).

Considera el siguiente ejemplo. Si cada variable de control tiene solo dos valores posibles (por ejemplo, hombre/mujer, universitario/no universitario, urbano/rural), el número de celdas crece exponencialmente con el número de variables:

| Variables de control | Celdas posibles | Observaciones necesarias |
|---------------------|-----------------|--------------------------|
| 1 variable binaria  | $2^1 = 2$      | Manejable                |
| 2 variables binarias| $2^2 = 4$       | Manejable                |
| 5 variables binarias| $2^5 = 32$      | Manejable                |
| 10 variables binarias| $2^{10} = 1{,}024$ | Muy exigente          |
| 20 variables binarias| $2^{20} = 1{,}048{,}576$ | Imposible en la práctica |

Con variables continuas (edad en años, salario, puntaje de prueba) el problema es aún más severo: la probabilidad de encontrar dos individuos con exactamente el mismo valor en todas las dimensiones es prácticamente cero.

El resultado es que con muchas variables de control, la mayoría de los individuos tratados **no tienen ningún clon exacto** en el grupo de control, y el método se vuelve inaplicable.

Esta limitación motiva el Propensity Score Matching, que colapsa todas las dimensiones de $X$ en un único número escalar.

---

## Estimación del ATT con emparejamiento exacto {-}

Cuando el emparejamiento exacto es viable (pocas variables, discretas), el estimador del efecto promedio sobre los tratados (**ATT**) es:

$$\hat{\tau}_{ATT} = \frac{1}{N_T} \sum_{i: D_i=1} \left( Y_i - \frac{1}{|M_i|} \sum_{j \in M_i} Y_j \right)$$

donde $M_i$ es el conjunto de controles que son "clones exactos" del tratado $i$ (mismo valor de $X$), y $|M_i|$ es el número de controles en ese conjunto.

La lógica es: para cada tratado, calcula la diferencia entre su resultado y el promedio de sus clones exactos, luego promedia esas diferencias sobre todos los tratados.

---

## En la práctica: `nnmatch` en Stata {-}

Para emparejamiento exacto (o casi exacto) en Stata, el comando `nnmatch` permite especificar variables de exactitud obligatoria:

```stata
* Instalar si no está disponible
ssc install nnmatch

* Emparejamiento exacto en 'genero' y 'educacion',
* vecino más cercano en 'edad' e 'ingreso'
nnmatch outcome treat edad ingreso, ///
    exact(genero educacion)         ///
    tc(att)                         ///
    m(1)
```

La opción `exact()` impone que el emparejamiento sea exacto en las variables listadas, y aproximado (vecino más cercano) en las demás. Cuando no hay ningún control con los valores exactos requeridos, la observación se descarta del análisis.

---

## Resumen y limitaciones {-}

El emparejamiento exacto es conceptualmente transparente y no hace supuestos de forma funcional, pero tiene dos limitaciones prácticas importantes:

1. **Maldición de las dimensiones**: inviable con muchas variables de control
2. **Pérdida de muestra**: los tratados sin clon exacto quedan fuera del análisis

Estas limitaciones motivan el Propensity Score Matching (Capítulo \@ref(psm)), que reduce el vector $X$ a un único escalar — la probabilidad estimada de recibir el tratamiento — preservando las propiedades de identificación del emparejamiento exacto bajo los mismos supuestos (CIA y soporte común).

---

## Lecturas recomendadas {-}

- **Angrist & Pischke (2009)** — *Mostly Harmless Econometrics*, Cap. 3: covariates and regression
- **Imbens & Rubin (2015)** — *Causal Inference for Statistics, Social, and Biomedical Sciences*, Cap. 15
- **Caliendo & Kopeinig (2008)** — "Some practical guidance for the implementation of propensity score matching", *Journal of Economic Surveys* — referencia principal para la implementación práctica
