# Controles sintéticos: diseño de la clase teórica y empírica

**Fecha:** 2026-07-29

**Estado:** diseño aprobado; pendiente de plan de implementación

**Alcance:** dos capítulos nuevos, ubicados después de IPW y antes de variables instrumentales

## 1. Objetivo editorial y pedagógico

El libro incorporará controles sintéticos mediante dos capítulos separados y complementarios:

1. `17-SyntheticControls.Rmd`: **Controles sintéticos — Clase teórica**.
2. `17-SyntheticControlsStata.Rmd`: **Controles sintéticos — Clase empírica**.

La separación evita comprimir la intuición econométrica y la aplicación de Stata en una sola página. El capítulo teórico debe explicar con rigor qué problema resuelve el método, bajo qué condiciones resulta creíble y cómo se interpreta. El capítulo empírico debe permitir reproducir una aplicación completa y pedagógicamente coherente, no una aproximación basada en el promedio simple de los controles.

Los capítulos se insertarán antes de los actuales capítulos de variables instrumentales. Se conservarán los nombres de archivo y las URL de IV y RDD para no romper enlaces existentes. Las nuevas URL serán:

- `controles-sinteticos.html`
- `controles-sinteticos-stata.html`

## 2. Decisión metodológica

La aplicación principal será la Proposición 99 de California y el consumo de cigarrillos per cápita. La proposición fue aprobada en noviembre de 1988 y el periodo tratado se definirá desde 1989. Se usará el estimador canónico implementado con `synth` en Stata. `synth_runner` podrá aparecer únicamente como extensión opcional para automatizar diagnósticos; no reemplazará la construcción y lectura explícita del estimador principal.

El archivo heredado `03_synthetic_controls_stata.do` no es una base académicamente válida para el capítulo definitivo: actualmente compara California con el promedio simple de los demás estados, sin estimar pesos sintéticos. La nueva versión deberá estimar los pesos de los donantes, reconstruir la trayectoria sintética y verificar numéricamente sus resultados.

No se añadirá por ahora una segunda aplicación colombiana. La profundidad de Prop 99 tiene prioridad sobre multiplicar ejemplos.

## 3. Clase teórica

### 3.1 Secuencia conceptual

La exposición seguirá esta secuencia:

1. **El problema:** una intervención afecta una sola unidad agregada y ningún control individual reproduce adecuadamente su trayectoria pretratamiento.
2. **La solución:** construir el contrafactual como combinación convexa de unidades donantes.
3. **El estimador:** definir los pesos de donantes y la matriz de importancia de predictores; relacionar ambos con el ajuste pretratamiento.
4. **Qué hace creíble el diseño:** buen ajuste anterior, ausencia de anticipación, ausencia de interferencia o contaminación del conjunto donante y estabilidad de la relación contrafactual.
5. **Soporte e interpolación:** explicar por qué la envolvente convexa importa y por qué un mal ajuste revela que el tratado no está adecuadamente representado por los donantes.
6. **Inferencia y sensibilidad:** placebos espaciales y temporales, RMSPE pre y postratamiento, razón post/pre, sensibilidad al conjunto donante y análisis leave-one-out.
7. **Limitaciones:** una sola unidad tratada, dependencia del donor pool, choques concurrentes, tratamiento anticipado, spillovers y riesgo de sobreinterpretar un ajuste visual.

### 3.2 Notación

La notación será consistente con el resto del libro. Para la unidad tratada 1:

$$
\widehat Y_{1t}(D=0)=\sum_{j=2}^{J+1}w_jY_{jt},
\qquad w_j\geq 0,\qquad \sum_{j=2}^{J+1}w_j=1,
$$

y el efecto estimado en el periodo posterior será

$$
\widehat\tau_{1t}=Y_{1t}(D=1)-\widehat Y_{1t}(D=0).
$$

No se presentará una regresión de la brecha pretratamiento como una “prueba” de validez. El ajuste anterior se evaluará principalmente mediante trayectorias, RMSPE y comparabilidad de predictores.

### 3.3 Elementos didácticos

El capítulo incluirá bloques de color para:

- intuición;
- supuesto o condición de credibilidad;
- advertencia;
- lectura avanzada;
- conexión entre el resultado gráfico y el estimando.

Incluirá tres preguntas tipo examen sin respuestas públicas. La clave quedará fuera del repositorio.

## 4. Clase empírica

### 4.1 Orden de la página

De acuerdo con el patrón aprobado para clases prácticas, la página comenzará con:

1. materiales descargables —do-file, datos y archivos auxiliares—;
2. lecturas centrales;
3. objetivos de aprendizaje.

Después desarrollará la aplicación completa. Los resultados de Stata aparecerán en la página como tablas y gráficas interpretadas; no se obligará al estudiante a ejecutar el do-file para conocer los resultados centrales.

### 4.2 Flujo de la aplicación Prop 99

La aplicación seguirá este orden:

1. describir la pregunta causal, la unidad tratada, el año de intervención, el resultado y el conjunto donante;
2. declarar la estructura de panel y auditar datos faltantes, periodo disponible y elegibilidad de donantes;
3. graficar California y los estados donantes antes de estimar;
4. justificar los predictores y los años pretratamiento usados para el ajuste;
5. estimar el control sintético con `synth`;
6. presentar en tablas los pesos de donantes, el balance de predictores y las medidas de ajuste;
7. reconstruir la serie sintética usando los pesos estimados y comprobar que coincide con la salida del comando;
8. graficar California frente a California sintética;
9. graficar e interpretar la brecha tratada menos sintética;
10. calcular RMSPE pretratamiento, RMSPE postratamiento y su razón;
11. ejecutar placebos *in space* para los donantes elegibles y comparar sus brechas y razones RMSPE;
12. mostrar todos los placebos y, en una segunda comparación de inferencia, excluir los que tengan un RMSPE pretratamiento superior a cinco veces el de California; el umbral aparecerá antes de mostrar los resultados;
13. ejecutar 1980 como placebo temporal, usando únicamente información compatible con esa fecha ficticia, y explicar que este diagnóstico no sustituye los placebos espaciales;
14. realizar sensibilidad leave-one-out retirando, uno por uno, los donantes con peso positivo;
15. cerrar con un protocolo de interpretación que distinga magnitud, ajuste, rareza frente a placebos y robustez.

### 4.3 Coherencia pedagógica

El ejemplo debe contener la variación necesaria para que los diagnósticos enseñen algo real. No se presentará el promedio de controles como control sintético, no se denominará “p-valor convencional” a la proporción de placebos extremos y no se inferirá validez únicamente porque las líneas pretratamiento parezcan próximas.

La página mostrará explícitamente:

- qué estados reciben peso y cuánto;
- qué tan bien se reproducen los predictores y la trayectoria anterior;
- cuánto cae el consumo de California respecto de su contrafactual;
- si la razón RMSPE de California es inusual dentro de la distribución placebo;
- si el resultado depende de un único donante con peso alto.

### 4.4 Ejercicios y clave privada

La clase empírica incluirá cuatro ejercicios tipo examen, sin respuestas desplegables ni respuestas visibles. Cubrirán:

- interpretación de pesos y soporte;
- diagnóstico de ajuste pretratamiento;
- lectura de la brecha y del estimando;
- inferencia placebo y sensibilidad.

La clave se guardará exclusivamente fuera del repositorio en:

`/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/claves_privadas/17_SyntheticControls_clave.md`

## 5. Archivos y organización

Los materiales definitivos vivirán en un directorio propio, previsto como:

`dofile/17_SyntheticControls/`

Allí se organizarán el do-file principal, la base de Prop 99, resultados canónicos y gráficas. Los materiales heredados en `dofile/16_PSM_IPW_Sinteticos/` se conservarán mientras se valida la migración; no se eliminarán como parte de este trabajo.

El borrador `17-SyntheticControls-DRAFT.Rmd` seguirá fuera de `_bookdown.yml` y servirá únicamente como registro del material anterior. La navegación del libro apuntará a los dos capítulos nuevos.

## 6. Lecturas

Ambos capítulos incluirán enlaces directos a las lecturas centrales según el contrato editorial del libro:

- Bernal y Peña: PDF separado del capítulo 6, como lectura complementaria de métodos de evaluación con datos observacionales.
- Cunningham, *Causal Inference: The Mixtape*: capítulo 10, Synthetic Control.
- Abadie, Diamond y Hainmueller (2010), como artículo metodológico central.

El capítulo teórico podrá incluir en un bloque de lectura avanzada a Abadie, Diamond y Hainmueller (2015) y la discusión posterior sobre inferencia y robustez. Durante la implementación se verificarán los enlaces directos antes de incorporarlos para evitar referencias rotas.

## 7. Validación y criterios de aceptación

La implementación se considerará completa únicamente si cumple todos estos criterios:

1. Los dos capítulos renderizan dentro del libro sin cambiar las URL existentes de IV y RDD.
2. La teoría usa `Y(D=1)` y `Y(D=0)` y no confunde ajuste pretratamiento con una prueba formal de identificación.
3. La aplicación estima un control sintético real con pesos convexos; no usa el promedio simple como sustituto.
4. Los resultados visibles en HTML coinciden con los resultados canónicos producidos por el do-file.
5. La reconstrucción manual con los pesos coincide, dentro de tolerancia numérica, con la trayectoria producida por `synth`.
6. Se presentan balance de predictores, pesos, trayectoria, brecha, RMSPE, placebos y leave-one-out.
7. La regla para placebos con mal ajuste pretratamiento es explícita y se aplica de forma reproducible.
8. Los materiales aparecen al inicio de la clase empírica.
9. Hay tres preguntas teóricas y cuatro empíricas, sin respuestas públicas.
10. La clave privada no aparece en el árbol ni en el historial nuevo de Git.
11. Las pruebas automatizadas existentes siguen pasando y se añaden pruebas de estructura, contenido y concordancia de resultados para los capítulos nuevos.
12. Se genera una vista previa fechada para revisión; `docs/` no se publica ni se modifica sin autorización explícita.

## 8. Fuera de alcance

Este ciclo no incluye:

- slides de la clase;
- una aplicación colombiana adicional;
- generalizaciones para múltiples unidades tratadas o adopción escalonada;
- controles sintéticos aumentados, matriciales o bayesianos;
- publicación del libro.

Estos temas podrán añadirse después como extensiones o lecturas avanzadas sin retrasar la versión canónica de los dos capítulos.
