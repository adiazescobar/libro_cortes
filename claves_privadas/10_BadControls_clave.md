# Clave privada — Malos controles

Uso exclusivo de la profesora y el monitor. No incluir en `_bookdown.yml` ni copiar a `docs/`.

## BC-T1

- DAG correcto: capacitación \(\rightarrow\) empleo formal \(\rightarrow\) salario, con posibles causas comunes de empleo y salario.
- Condicionar en empleo no identifica el efecto total.
- Para interpretar un efecto directo se necesitan, entre otros, ausencia de confusión no medida en tratamiento–resultado, tratamiento–mediador y mediador–resultado, además de no introducir confusores mediador–resultado causados por el tratamiento.
- Puntaje: DAG (1), estimando (1), explicación del bloqueo (1), supuestos (1).

## BC-T2

- Debe distinguir \(F(D=1)=1\) entre tratados de \(F(D=0)=1\) entre controles.
- La descomposición contiene el efecto en el estrato \(F(D=1)=1\) y el término de composición en \(Y(D=0)\).
- El signo de este último no está determinado sin restricciones adicionales.
- Puntaje: switching (1), condicionamiento correcto (2), suma y resta (1), interpretación (1).

## BC-T3

- Ejemplo válido: \(D\rightarrow C\leftarrow U\rightarrow Y\), donde \(C\) se determina antes de la medición de \(D\), pero sigue siendo colisionador respecto del contraste.
- También es válido un predictor fuerte de \(D\) que amplifique el sesgo de un confusor no medido y apenas prediga \(Y\).
- Puntaje: DAG (2), camino (1), conclusión (1).

## BC-S1

- `regress Y D` debe aproximarse a 2.
- `regress Y D M` debe aproximarse a 0 en el DGP suministrado.
- Solo la primera especificación corresponde al efecto total.
- Puntaje: código (1), tabla (1), estimando (1), interpretación (1).

## BC-S2

- El efecto verdadero debe fijarse en cero.
- Sin colisionador, el coeficiente debe centrarse en cero.
- Al ajustar por \(C\), aparece asociación porque se abre \(D\rightarrow C\leftarrow U\rightarrow Y\).
- Puntaje: DGP (1), estimaciones (1), DAG (1), diagnóstico (1).

## BC-S3

- Debe reportar al menos 200 réplicas y fijar semilla.
- Sesgo: promedio del estimador menos valor verdadero.
- Mayor tamaño muestral reduce dispersión, no el sesgo asintótico del mal control.
- Puntaje: programa (2), tabla (1), gráfico (1), interpretación (1).

## BC-S4

- Empleo y migración pueden ser mediadores; permanencia puede ser selección/colisionador.
- Para el ATT total no deben ajustarse mecánicamente como covariables postratamiento.
- Debe discutir atrición, composición y alternativas como resultados adicionales, cotas o ponderación bajo supuestos explícitos.
- Puntaje: DAG (2), estimando y especificación (1), amenazas (1), alternativas (1).
