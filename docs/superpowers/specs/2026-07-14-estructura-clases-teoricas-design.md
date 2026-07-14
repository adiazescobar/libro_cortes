# Diseño: estructura de las clases teóricas

**Fecha:** 2026-07-14  
**Proyecto:** Libro de Econometría Avanzada  
**Piloto:** Capítulo 5, Experimentos aleatorizados — Clase teórica

## Objetivo

Establecer una estructura común para las clases teóricas que conserve la claridad y previsibilidad del libro de Econometría II, pero responda al nivel formal de Econometría Avanzada.

## Principios

- Cada capítulo debe partir de un problema econométrico o una pregunta causal reconocible.
- La intuición debe preceder a la formalización, sin sustituirla.
- Las demostraciones matemáticas completas permanecerán en el cuerpo del capítulo.
- Los supuestos de identificación deben distinguirse de las condiciones necesarias para inferencia o eficiencia.
- La teoría debe cerrar con un puente explícito hacia la clase práctica correspondiente.
- Las clases teóricas no incluirán un bloque de descargas.

## Estructura común

Cada clase teórica seguirá, salvo justificación sustantiva, este orden:

1. **Objetivos de aprendizaje y lecturas.** Resultados concretos que el estudiante debe alcanzar y bibliografía necesaria o complementaria.
2. **Pregunta causal o problema econométrico.** Qué se quiere aprender de los datos y por qué los métodos más simples no bastan.
3. **Intuición y motivación.** Ejemplo económico, gráfico o comparación que introduzca el mecanismo central.
4. **Notación, parámetros y estimando.** Unidades, variables, resultados potenciales, parámetros y objeto que recupera el estimador.
5. **Supuestos de identificación.** Lista explícita, interpretación económica y función de cada supuesto.
6. **Desarrollo teórico y demostraciones completas.** Derivaciones algebraicas sin omitir pasos sustantivos.
7. **Interpretación del estimador.** Qué comparación realiza, qué población representa y cómo deben leerse signo, magnitud y unidades.
8. **Supuestos, propiedades y condiciones de validez.** Insesgamiento, consistencia, eficiencia o inferencia, según corresponda, diferenciados de la identificación.
9. **Amenazas, limitaciones y errores comunes.** Violaciones plausibles, consecuencias y prácticas que no resuelven el problema.
10. **Resumen o tabla de síntesis.** Mapa compacto que conecte pregunta, estimando, supuestos, estimador y amenazas.
11. **Preguntas y ejercicios conceptuales.** Preguntas de comprensión, derivación e interpretación, sin depender de software.
12. **Puente a la clase práctica.** Datos, especificaciones o diagnósticos que se implementarán a continuación, sin duplicar la práctica.
13. **Referencias.** Fuentes obligatorias y complementarias citadas consistentemente.

## Patrón de las demostraciones

Cada demostración seguirá la secuencia:

> resultado que se quiere demostrar → supuestos utilizados → derivación paso a paso → conclusión econométrica → interpretación intuitiva

La demostración debe declarar el punto de partida y no esconder cambios de supuesto. Cuando existan varias rutas algebraicas, se elegirá la que haga más transparente la identificación. Los pasos auxiliares podrán presentarse como lemas o subsecciones, pero no se trasladarán a apartados desplegables ni a apéndices únicamente para acortar la página.

## Elementos visuales y simulaciones

Se permitirán gráficos, tablas pequeñas y simulaciones cuando aclaren una idea teórica. Deben aparecer cerca del argumento que ilustran y estar acompañados por una lectura económica. El código de generación puede permanecer oculto en la página final si no es parte del objetivo de aprendizaje.

Las simulaciones no reemplazarán una demostración ni se convertirán en una secuencia de comandos propia de la clase práctica.

## Aplicación al piloto RCT

El contenido sustantivo de `05-RCT.Rmd` se conservará. La intervención consistirá principalmente en:

- hacer explícita la pregunta causal inicial;
- agrupar la intuición antes de la formalización;
- distinguir notación, estimandos y supuestos de identificación;
- mantener completas las derivaciones actuales sobre sesgo de selección, variable omitida y los cuatro diseños RCT;
- organizar propiedades, amenazas y errores comunes;
- añadir un resumen estructurado y un puente a `06-RCT2.Rmd`.

No se añadirá al capítulo teórico el bloque de materiales diseñado para las clases prácticas.

## Verificación

El piloto se considerará satisfactorio si:

- conserva todos los argumentos y demostraciones sustantivas del capítulo actual;
- presenta la secuencia común con encabezados claros;
- cada supuesto utilizado en una demostración está declarado;
- diferencia identificación, inferencia y eficiencia;
- los gráficos y simulaciones apoyan la teoría sin dominarla;
- no contiene enlaces de descarga de do-files, bases o notebooks;
- finaliza con ejercicios conceptuales y un puente claro a la práctica;
- renderiza correctamente en vista de escritorio y pantalla angosta.

## Alcance posterior

Una vez aprobado visual y académicamente el piloto RCT, el patrón podrá aplicarse a las demás clases teóricas. La adaptación conservará las particularidades de cada método; no se crearán secciones vacías únicamente para cumplir mecánicamente la plantilla.
