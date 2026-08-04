# Diseño académico: variables instrumentales y LATE

**Fecha:** 4 de agosto de 2026

**Alcance:** capítulos `18-IV.Rmd` y `19-IVStata.Rmd`, materiales ejecutables y clave privada asociados.

**Estado:** diseño aprobado por la profesora.

## 1. Objetivo

Reorganizar y ampliar el par de capítulos de variables instrumentales para que los estudiantes puedan:

1. explicar por qué OLS no identifica el efecto causal cuando el tratamiento es endógeno;
2. derivar la primera etapa, la forma reducida, el estimador de Wald y 2SLS;
3. interpretar el estimando como LATE bajo los supuestos correspondientes;
4. distinguir *compliers*, *always-takers*, *never-takers* y *defiers*;
5. estimar la proporción y el perfil promedio de los *compliers* sin afirmar que pueden identificarse individualmente;
6. diagnosticar instrumentos débiles y usar inferencia robusta;
7. evaluar la credibilidad sustantiva de independencia, exclusión y monotonicidad;
8. reconocer que una primera etapa fuerte y los tests estadísticos no demuestran la validez del instrumento.

La notación seguirá la convención general del libro, incluida la escritura de resultados potenciales como `Y(D=1)` y `Y(D=0)`.

## 2. Estructura del capítulo teórico

El título será **“Variables instrumentales y LATE — Clase teórica”**, conservando la URL y el identificador actuales.

La secuencia será:

1. **Problema causal:** endogeneidad, sesgo de OLS y estimando de interés.
2. **Lógica del instrumento:** diagrama causal y separación entre relevancia, independencia y exclusión.
3. **Mecánica de IV:** primera etapa, forma reducida, Wald y equivalencia con 2SLS en el caso exactamente identificado.
4. **Controles exógenos:** deben incluirse coherentemente en ambas etapas; la regresión manual reproduce el coeficiente, pero no los errores estándar correctos de 2SLS.
5. **Marco LATE:** tratamiento e instrumento binarios, SUTVA, independencia, exclusión, relevancia y monotonicidad; tabla de tipos de cumplimiento y demostración de que Wald identifica el efecto promedio entre *compliers*.
6. **¿Quiénes son los compliers?:** identificación de proporciones y características promedio, pero no de identidades individuales.
7. **Instrumentos débiles:** diferencia entre la regla histórica `F > 10`, los valores críticos de Stock–Yogo, los diagnósticos robustos a heterocedasticidad y el resultado de Lee, McCrary, Moreira y Porter. El valor 104.7 se presentará como resultado para un contexto específico, no como umbral universal.
8. **Inferencia robusta:** Anderson–Rubin y CLR; diferencia entre fortaleza del instrumento e inferencia válida.
9. **Pruebas y límites:** Durbin–Wu–Hausman, sobreidentificación y pruebas de implicaciones observables. Un no rechazo no probará exclusión ni validez.
10. **Múltiples instrumentos y heterogeneidad:** interpretación cautelosa de 2SLS como promedio ponderado de efectos locales.
11. **Casos sustantivos:** PACES para introducir el LATE y el caso del divorcio para discutir críticamente la exclusión.
12. **Tres preguntas tipo examen**, sin respuestas públicas.

Se utilizarán bloques de colores consistentes con los capítulos aprobados para definiciones, advertencias, intuiciones y lecturas avanzadas.

## 3. Estructura del capítulo empírico

El título será **“Variables instrumentales y LATE — Clase empírica”**, conservando su URL actual. La descarga de materiales aparecerá inmediatamente después del título, seguida por lecturas centrales y objetivos.

La clase tendrá tres partes conectadas. Las partes A y B usarán el mismo proceso generador tipo PACES: la primera revelará la verdad causal y la segunda entregará únicamente las variables que observaría un investigador. Así se evita duplicar ejercicios y se hace explícito qué información desaparece al pasar de una simulación a una aplicación.

### Parte A. Simulación transparente de cumplimiento y efectos heterogéneos

La simulación generará para cada persona:

- asignación instrumental binaria `Z`;
- tratamientos potenciales `D(1)` y `D(0)`;
- tratamiento observado `D`;
- resultados potenciales `Y(D=1)` y `Y(D=0)`;
- tipo de cumplimiento verdadero;
- covariables predeterminadas;
- heterogeneidad deliberada para que ATE, ATT y LATE sean distintos.

Primero se utilizará la información completa del generador para mostrar la verdad poblacional. Después se ocultarán los contrafactuales y el tipo de cumplimiento para reproducir el problema del investigador.

Se verificará:

\[
P(C)=E[D\mid Z=1]-E[D\mid Z=0],
\]

y se compararán la proporción estimada, el perfil estimado mediante pesos de Abadie y los *compliers* verdaderos. La página aclarará que en datos reales no puede asignarse una etiqueta de complier a cada individuo.

### Parte B. Aplicación ficticia tipo PACES

La base tendrá apariencia de una aplicación real: lotería de becas, uso efectivo de la beca, características iniciales y resultados educativos. Estará rotulada inequívocamente como **base simulada inspirada en PACES**, no como datos del estudio original.

El flujo será:

1. descripción de la asignación y del incumplimiento;
2. ITT;
3. primera etapa;
4. forma reducida;
5. Wald y 2SLS;
6. comparación de OLS, ITT y LATE;
7. estimación de la proporción y el perfil de los *compliers*;
8. interpretación de la población para la cual se identifica el efecto;
9. contraste entre estimaciones muestrales y verdad conocida de la simulación.

### Parte C. Aplicación ficticia inspirada en el estudio sobre divorcio

Una segunda base simulada reproducirá la estructura conceptual de Frimmel, Halla y Winter-Ebmer (2024): divorcio parental como tratamiento endógeno, composición de género del lugar de trabajo del padre como instrumento y resultados posteriores de los hijos.

Su propósito no será presentar una nueva estimación del artículo, sino entrenar juicio aplicado:

1. estimar OLS e IV;
2. revisar primera etapa e inferencia;
3. interpretar el parámetro IV local inducido por cambios en la composición del lugar de trabajo, sin equipararlo mecánicamente al LATE de un instrumento binario;
4. discutir por qué un instrumento continuo no produce un único grupo binario de *compliers* comparable al de una lotería;
5. enumerar canales que pueden violar exclusión, como salarios, horas, estabilidad, redes, movilidad ocupacional o comportamiento familiar;
6. concluir qué puede establecerse estadísticamente y qué requiere argumentos institucionales.

La página y los archivos señalarán siempre que los datos son ficticios. El artículo publicado se enlazará como lectura y caso de discusión, sin caracterizarlo como formalmente desacreditado.

## 4. Flujo de Stata y resultados visibles

`ivreg2` será el comando principal de diagnóstico aplicado. La clase mostrará, según corresponda:

- primera etapa y `partial R2`;
- estadísticos de subidentificación;
- Kleibergen–Paap rk Wald F con errores robustos;
- Cragg–Donald y Stock–Yogo únicamente bajo las condiciones que justifican su comparación;
- prueba de endogeneidad, indicando que depende de la validez del instrumento;
- Hansen J en modelos sobreidentificados, sin interpretarlo como prueba definitiva de validez;
- Anderson–Rubin y otras salidas robustas disponibles.

La verificación moderna utilizará comandos oficiales de Stata 19 cuando estén disponibles:

```stata
ivregress 2sls y x (d = z), vce(robust)
estat weakrobust, ci
```

Para LATE y el perfil de los *compliers* se utilizará:

```stata
lateffects ...
estat compliers, genkappa(kappa)
```

Los pesos `kappa` se explicarán como herramientas para recuperar características promedio de la subpoblación complier, no como probabilidades individuales ni etiquetas.

Las tablas y gráficas canónicas se producirán desde Stata y se importarán a la página. No se transcribirán resultados manualmente. La clase mostrará al menos:

- descomposición de tipos de cumplimiento;
- primera etapa, forma reducida y LATE;
- comparación ATE–ATT–LATE en la simulación;
- perfil de *compliers* frente a la población;
- comparación OLS–IV en las dos aplicaciones;
- contraste entre inferencia convencional e inferencia robusta ante un instrumento débil.

Python y R podrán conservarse como extensiones solo si reproducen los mismos parámetros poblacionales y resultados dentro de tolerancias explícitas. Stata será la fuente canónica del capítulo.

## 5. Instrumentos débiles y diseño de la simulación

La fortaleza del instrumento variará de manera independiente del tamaño muestral para evitar que los estudiantes confundan una mayor muestra con un instrumento intrínsecamente más relevante. Se incluirán, como mínimo, un escenario fuerte y uno débil con el mismo proceso causal.

La discusión distinguirá:

- sesgo e inestabilidad de 2SLS con instrumentos débiles;
- distorsión del test t convencional;
- alcance limitado de la regla `F > 10`;
- significado específico del umbral 104.7 para el test t convencional en el caso analizado por Lee et al.;
- utilidad de intervalos AR/CLR cuando la identificación es débil.

No se comparará mecánicamente el estadístico Kleibergen–Paap con valores críticos diseñados para otros supuestos.

## 6. Tests y extensiones sobre compliers

La clase central incluirá:

1. estimación de la proporción de *compliers* mediante la primera etapa;
2. estimación de sus medias de covariables mediante `estat compliers` y pesos de Abadie;
3. prueba e intervalo del LATE con inferencia adecuada a la fortaleza del instrumento;
4. comparación con los tipos verdaderos únicamente en la simulación.

La prueba de Kitagawa (2015) se presentará en un bloque de lectura avanzada. Se explicará que examina implicaciones observables conjuntas de los supuestos del modelo LATE: un rechazo evidencia incompatibilidad, pero no identifica cuál supuesto falla; un no rechazo no certifica validez. No se convertirá en un requisito operativo de la práctica si no existe una implementación estable y reproducible en el entorno del curso.

## 7. Evaluación y clave privada

La clase empírica incluirá cuatro ejercicios tipo examen sin respuestas desplegables ni soluciones públicas. Cubrirán:

1. cálculo de Wald y reconocimiento de la población complier;
2. lectura de diagnósticos de `ivreg2`;
3. comparación entre inferencia convencional y robusta;
4. evaluación de la restricción de exclusión en el caso del divorcio.

Las soluciones se guardarán en una clave externa al repositorio, dentro de la carpeta privada de claves del curso, con permisos restringidos. La clave incluirá respuestas conceptuales, comandos y criterios de calificación para la profesora y el monitor.

## 8. Lecturas

Se incorporarán enlaces directos, cuando estén disponibles legalmente, a:

- el capítulo correspondiente de Bernal y Peña;
- el capítulo de variables instrumentales de *Causal Inference: The Mixtape*;
- Imbens y Angrist y/o Angrist, Imbens y Rubin para LATE;
- Lee, McCrary, Moreira y Porter para inferencia tF;
- Kitagawa para implicaciones comprobables de validez;
- Frimmel, Halla y Winter-Ebmer para el estudio sobre divorcio.

## 9. Verificación y criterios de aceptación

Antes de publicar se comprobará que:

1. ambos capítulos renderizan sin errores y mantienen sus URLs;
2. los títulos siguen el patrón uniforme teórico/empírico;
3. los materiales aparecen al inicio de la clase empírica;
4. todas las cifras y tablas visibles provienen de los archivos canónicos;
5. Wald y 2SLS coinciden en el caso exactamente identificado dentro de tolerancia numérica;
6. la proporción estimada de *compliers* coincide con la primera etapa;
7. ATE, ATT y LATE son deliberadamente distintos y sus valores verdaderos quedan documentados;
8. los escenarios fuerte y débil difieren en relevancia, no solo en tamaño muestral;
9. las afirmaciones sobre 10, 104.7, Stock–Yogo y Kleibergen–Paap incluyen sus condiciones;
10. no se afirma que tests estadísticos prueben exclusión o validez;
11. los datos ficticios están rotulados como tales en página, base y do-file;
12. las preguntas no contienen respuestas públicas y la clave privada no está rastreada por Git;
13. no existe doble numeración en subtítulos;
14. no se publican ni suben cambios adicionales sin aprobación explícita.

## 10. Fuera de alcance

- Obtener o reconstruir los microdatos originales de PACES o del artículo sobre divorcio.
- Clasificar personas reales como *compliers*.
- Presentar una prueba estadística como demostración de la restricción de exclusión.
- Elaborar las diapositivas del curso en esta fase; posteriormente deberán contener los resultados completos.
- Modificar capítulos distintos de IV/LATE salvo ajustes mínimos de navegación estrictamente necesarios.
