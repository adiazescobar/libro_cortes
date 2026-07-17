# Prueba de Entrada {-}



<style type="text/css">
/* Estilos personalizados para la prueba de entrada */

.quiz-section h2 {
  color: #1F77B4;
  margin-top: 0;
}

.question-number {
  font-weight: bold;
  color: #1F77B4;
  font-size: 1.1em;
}

.resources-box {
  background-color: #fff3cd;
  border-left: 4px solid #ffc107;
  padding: 20px;
  margin: 20px 0;
  border-radius: 5px;
}

.webex-check {
  margin-left: 10px;
}

/* Estilo para respuestas correctas e incorrectas (cuando ya se muestra feedback) */
.webex-correct {
  color: #28a745;
  font-weight: bold;
}

.webex-incorrect {
  color: #dc3545;
}

.tip-box {
  background-color: #d4edda;
  border-left: 4px solid #28a745;
  padding: 15px;
  margin: 15px 0;
  border-radius: 5px;
}

.warning-box {
  background-color: #f8d7da;
  border-left: 4px solid #dc3545;
  padding: 15px;
  margin: 15px 0;
  border-radius: 5px;
}

/* ======= CLAVE 1: Desactivar feedback inmediato de webexercises hasta finalizar ======= */
#prueba-entrada-quiz:not(.show-feedback) .webex-check,
#prueba-entrada-quiz:not(.show-feedback) .webex-feedback {
  display: none !important;
}

/* Los controles siguen visibles; solo se neutralizan sus marcas prematuras. */
#prueba-entrada-quiz:not(.show-feedback) :is(input, select):is(.webex-correct, .webex-incorrect) {
  background: var(--paper) !important;
  border: 1px solid var(--line) !important;
  box-shadow: none !important;
  color: inherit !important;
  font-weight: inherit !important;
}

#prueba-entrada-quiz:not(.show-feedback) .webex-radiogroup label:is(.webex-correct, .webex-incorrect) {
  background: transparent !important;
  border: 1px solid transparent !important;
  box-shadow: none !important;
  color: inherit !important;
  font-weight: inherit !important;
}

#prueba-entrada-quiz:not(.show-feedback) :is(input, select):is(.webex-correct, .webex-incorrect)::before,
#prueba-entrada-quiz:not(.show-feedback) :is(input, select):is(.webex-correct, .webex-incorrect)::after,
#prueba-entrada-quiz:not(.show-feedback) .webex-radiogroup label:is(.webex-correct, .webex-incorrect)::before,
#prueba-entrada-quiz:not(.show-feedback) .webex-radiogroup label:is(.webex-correct, .webex-incorrect)::after {
  content: none !important;
  display: none !important;
}

#prueba-entrada-quiz:not(.show-feedback) .webex-correct + .webex-icon::before,
#prueba-entrada-quiz:not(.show-feedback) .webex-correct + .webex-icon::after,
#prueba-entrada-quiz:not(.show-feedback) .webex-incorrect + .webex-icon::before,
#prueba-entrada-quiz:not(.show-feedback) .webex-incorrect + .webex-icon::after {
  content: none !important;
  display: none !important;
}

#prueba-entrada-quiz:not(.show-feedback) :is(input, select, label):is(.webex-correct, .webex-incorrect)
  :is(.webex-icon, .fa, [class*="icon"]) {
  display: none !important;
}

/* ======= CLAVE 2: Ocultar explicaciones/soluciones hasta finalizar =======
   hide()/unhide() generan bloques con clase .webex-solution.
   Con esto, el estudiante no ve el botón "Ver explicación" ni el contenido
   hasta presionar "Finalizar y calcular puntaje".
*/
#prueba-entrada-quiz:not(.show-feedback) .webex-solution {
  display: none !important;
}

</style>

## Antes de comenzar {-}

<div class="diagnostic-intro">
<h3>Bienvenida a la Prueba de Entrada</h3>

Esta prueba diagnóstica tiene como objetivo evaluar tus conocimientos previos en estadística, regresión lineal, causalidad y manejo básico de Stata.

<strong>Instrucciones:</strong>

- La prueba contiene <strong>20 preguntas</strong> divididas en 4 secciones
- Responde cada pregunta seleccionando la opción correcta o escribiendo tu respuesta
- La retroalimentación, las explicaciones y el puntaje se muestran solo al final, cuando presiones <strong>Finalizar y calcular puntaje</strong>
- Hazla en una sola sesión, sin consultar materiales
- Al final encontrarás recursos para repasar

<strong>Tiempo estimado:</strong> 15–20 minutos

</div>

---

<div id="prueba-entrada-quiz">

## Sección 1: Estadística básica {-}

<div class="quiz-section" data-section="Estadística básica">

Esta sección evalúa conceptos fundamentales de estadística descriptiva e inferencial que son esenciales para el análisis econométrico.

<div class="question-box">
<span class="question-number">Pregunta 1.</span> Si una variable X tiene media 50 y desviación estándar 10, y sigue una distribución normal, ¿qué porcentaje de las observaciones se encuentra entre 30 y 70?

<select class='webex-select'><option value='blank'></option><option value=''>68%</option><option value='answer'>95%</option><option value=''>99%</option><option value=''>50%</option></select>


<div class='webex-solution'><button>Ver explicación</button>

En una distribución normal:

- El 68% de los datos está dentro de 1 desviación estándar de la media
- El 95% de los datos está dentro de 2 desviaciones estándar de la media
- El 99.7% está dentro de 3 desviaciones estándar

Como 30 = 50 - 2(10) y 70 = 50 + 2(10), estamos hablando de 2 desviaciones estándar, por lo tanto es el 95%.

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 2.</span> El p-value (valor p) representa:

<select class='webex-select'><option value='blank'></option><option value=''>La probabilidad de que la hipótesis nula sea verdadera</option><option value='answer'>La probabilidad de observar datos tan extremos o más, dado que \(H_0\) es verdadera</option><option value=''>La probabilidad de que la hipótesis alternativa sea falsa</option><option value=''>El nivel de significancia del test</option></select>


<div class='webex-solution'><button>Ver explicación</button>

El valor p es la probabilidad de obtener un resultado tan extremo o más extremo que el observado, asumiendo que la hipótesis nula es verdadera.

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 3.</span> Rechazar la hipótesis nula cuando en realidad es verdadera se conoce como:

<select class='webex-select'><option value='blank'></option><option value='answer'>Error Tipo I</option><option value=''>Error Tipo II</option><option value=''>Sesgo de selección</option><option value=''>Error estándar</option></select>


<div class='webex-solution'><button>Ver explicación</button>

- Error Tipo I: Rechazar \(H_0\) cuando es verdadera (probabilidad \(\alpha\)).
- Error Tipo II: No rechazar \(H_0\) cuando es falsa (probabilidad \(\beta\)).

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 4.</span> Un intervalo de confianza del 95% significa que:

<select class='webex-select'><option value='blank'></option><option value=''>Hay 95% de probabilidad de que el parámetro verdadero esté en este intervalo específico</option><option value='answer'>En muestreos repetidos, aproximadamente 95% de los intervalos construidos con este procedimiento contendrían el parámetro</option><option value=''>El 95% de los datos está dentro del intervalo</option><option value=''>Estamos 95% seguros de nuestra estimación</option></select>


<div class='webex-solution'><button>Ver explicación</button>

Interpretación frecuentista: si repitiéramos el muestreo muchas veces y construyéramos IC del 95% cada vez, cerca del 95% de esos intervalos contendrían el valor verdadero del parámetro.

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 5.</span> La varianza mide:

<select class='webex-select'><option value='blank'></option><option value=''>La tendencia central de los datos</option><option value='answer'>La dispersión de los datos alrededor de la media</option><option value=''>La relación entre dos variables</option><option value=''>El valor más frecuente</option></select>


<div class='webex-solution'><button>Ver explicación</button>

La varianza mide la dispersión alrededor de la media: \(\operatorname{Var}(X) = E[(X - \mu)^2]\). La desviación estándar es la raíz cuadrada de la varianza.

</div>

</div>

</div>

---

## Sección 2: Regresión lineal {-}

<div class="quiz-section" data-section="Regresión lineal">

Esta sección evalúa tu comprensión del modelo de regresión lineal, sus supuestos e interpretación.

<div class="question-box">
<span class="question-number">Pregunta 6.</span> En el modelo \(Y = \beta_0 + \beta_1 X + e\), el coeficiente \(\beta_1\) representa:

<select class='webex-select'><option value='blank'></option><option value=''>El valor de Y cuando X = 0</option><option value='answer'>El cambio esperado en Y por cada unidad adicional de X</option><option value=''>La correlación entre X e Y</option><option value=''>La varianza de Y explicada por X</option></select>


<div class='webex-solution'><button>Ver explicación</button>

\(\beta_1\) es el cambio esperado en \(Y\) asociado con una unidad adicional de \(X\), ceteris paribus.

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 7.</span> Si \(R^2 = 0.75\), esto significa que:

<select class='webex-select'><option value='blank'></option><option value=''>El coeficiente de correlación es 0.75</option><option value=''>El modelo tiene 75% de probabilidad de ser correcto</option><option value='answer'>El 75% de la variación en Y es explicada por las variables independientes</option><option value=''>El 75% de las observaciones están correctamente predichas</option></select>


<div class='webex-solution'><button>Ver explicación</button>

\(R^2\) es la proporción de la varianza de \(Y\) explicada por el modelo.

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 8.</span> ¿Cuál de las siguientes NO es un supuesto del modelo clásico de regresión lineal (MCO)?

<select class='webex-select'><option value='blank'></option><option value=''>Los errores tienen media cero</option><option value=''>Los errores son homocedásticos</option><option value=''>No hay multicolinealidad perfecta</option><option value='answer'>Los errores deben seguir una distribución uniforme</option></select>


<div class='webex-solution'><button>Ver explicación</button>

OLS no requiere errores uniformes. Para inferencia en muestras pequeñas se suele asumir normalidad.

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 9.</span> Si el p-value asociado a un coeficiente es 0.03, esto significa que:

<select class='webex-select'><option value='blank'></option><option value=''>El coeficiente es significativo al 1%</option><option value='answer'>El coeficiente es significativo al 5%</option><option value=''>El coeficiente no es significativo</option><option value=''>Hay 3% de probabilidad de que el coeficiente sea correcto</option></select>


<div class='webex-solution'><button>Ver explicación</button>

0.03 < 0.05 implica significancia al 5% (y al 10%), pero no al 1%.

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 10.</span> En un modelo \(\log(Y) = \beta_0 + \beta_1 X + e\), el coeficiente \(\beta_1\) se interpreta aproximadamente como:

<select class='webex-select'><option value='blank'></option><option value=''>El cambio absoluto en Y por unidad de X</option><option value=''>El cambio en log(Y) en términos absolutos</option><option value='answer'>Aproximadamente \(100\beta_1\)% por una unidad adicional de \(X\)</option><option value=''>La elasticidad de Y respecto a X</option></select>


<div class='webex-solution'><button>Ver explicación</button>

En un modelo semilogarítmico, \(100\beta_1\) es el cambio porcentual aproximado en \(Y\) por una unidad adicional de \(X\), para cambios pequeños.

</div>

</div>

</div>

---

## Sección 3: Causalidad {-}

<div class="quiz-section" data-section="Causalidad">

Esta sección evalúa la diferencia entre correlación y causalidad, y conceptos básicos de inferencia causal.

<div class="question-box">
<span class="question-number">Pregunta 11.</span> La observación de que "los países con mayor consumo de chocolate tienen más premios Nobel per capita" es un ejemplo de:

<select class='webex-select'><option value='blank'></option><option value=''>Causalidad directa</option><option value=''>Causalidad inversa</option><option value='answer'>Correlación espuria (correlación sin causalidad)</option><option value=''>Efecto placebo</option></select>


<div class='webex-solution'><button>Ver explicación</button>

Ejemplo de correlación espuria: puede haber una tercera variable (p. ej. desarrollo) que explique ambos.

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 12.</span> El sesgo de selección ocurre cuando:

<select class='webex-select'><option value='blank'></option><option value=''>La muestra es muy pequeña</option><option value=''>Los datos tienen errores de medición</option><option value='answer'>Los individuos tratados son sistemáticamente diferentes de los no tratados</option><option value=''>El modelo tiene muchas variables</option></select>


<div class='webex-solution'><button>Ver explicación</button>

Sesgo de selección: \(E[Y_0|D=1] \neq E[Y_0|D=0]\).

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 13.</span> La asignación aleatoria del tratamiento es importante porque:

<select class='webex-select'><option value='blank'></option><option value=''>Aumenta el tamaño de la muestra</option><option value=''>Reduce los costos del estudio</option><option value='answer'>Hace que los grupos de tratamiento y control sean comparables en expectativa</option><option value=''>Elimina los errores de medición</option></select>


<div class='webex-solution'><button>Ver explicación</button>

Aleatorización: balancea (en expectativa) observables y no observables.

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 14.</span> El contrafactual se refiere a:

<select class='webex-select'><option value='blank'></option><option value=''>Los datos que se perdieron en el estudio</option><option value=''>El grupo de control</option><option value='answer'>Lo que habría ocurrido con la misma unidad bajo la condición alternativa</option><option value=''>Los efectos secundarios del tratamiento</option></select>


<div class='webex-solution'><button>Ver explicación</button>

El contrafactual es el resultado potencial de la misma unidad bajo la condición alternativa no observada: sin tratamiento si fue tratada, o con tratamiento si no lo fue.

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 15.</span> El principal problema al comparar simplemente participantes vs. no participantes es:

<select class='webex-select'><option value='blank'></option><option value=''>La diferencia en tamaño de los grupos</option><option value=''>Los errores de medición en los datos</option><option value='answer'>La autoselección (los que participan pueden ser diferentes de los que no)</option><option value=''>La falta de datos</option></select>


<div class='webex-solution'><button>Ver explicación</button>

Autoselección: diferencias previas (motivación, habilidad, redes) contaminan la comparación.

</div>

</div>

</div>

---

## Sección 4: Stata {-}

<div class="quiz-section" data-section="Stata">

Esta sección evalúa tu familiaridad básica con Stata.

<div class="question-box">
<span class="question-number">Pregunta 16.</span> En Stata, ¿cuál comando usarías para ver las primeras observaciones de tu base de datos?

<select class='webex-select'><option value='blank'></option><option value=''>view</option><option value='answer'>browse o list</option><option value=''>show</option><option value=''>display</option></select>


<div class='webex-solution'><button>Ver explicación</button>

`browse` abre el visor de datos. `list` imprime observaciones en Results (por ejemplo: `list in 1/10`).

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 17.</span> En una regresión en Stata, si el coeficiente de x es 2.35, esto significa que:

<select class='webex-select'><option value='blank'></option><option value=''>La variable x tiene un p-value de 2.35</option><option value='answer'>Por cada unidad adicional de x, Y aumenta en 2.35 unidades (en promedio)</option><option value=''>El R-cuadrado del modelo es 2.35</option><option value=''>Hay 2.35 observaciones con esa variable</option></select>


<div class='webex-solution'><button>Ver explicación</button>

El coeficiente es el cambio promedio en Y asociado con una unidad adicional en X, ceteris paribus.

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 18.</span> ¿Cuál es el comando correcto en Stata para realizar una regresión de Y sobre X1 y X2?

<input class='webex-solveme nospaces ignorecase' size='25' data-answer='["regress Y X1 X2"]'/>


<div class='webex-solution'><button>Ver explicación</button>

Comando: `regress Y X1 X2` (abreviado: `reg Y X1 X2`).

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 19.</span> ¿Qué comando crea `ingreso_alto` igual a 1 únicamente para observaciones con `ingreso` mayor que 1000?

<select class='webex-select'><option value='blank'></option><option value=''>replace ingreso_alto = 1 if ingreso > 1000</option><option value='answer'>generate ingreso_alto = 1 if ingreso > 1000</option><option value=''>if ingreso > 1000 generate ingreso_alto = 1</option><option value=''>create ingreso_alto where ingreso > 1000</option></select>


<div class='webex-solution'><button>Ver explicación</button>

`generate` crea una variable nueva y `if` restringe las observaciones a las que se asigna el valor. Las demás quedarán como valores perdidos hasta que se definan explícitamente.

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 20.</span> Después de ejecutar `summarize salario`, ¿cómo muestra Stata la media almacenada por el comando?

<select class='webex-select'><option value='blank'></option><option value=''>display e(mean)</option><option value='answer'>display r(mean)</option><option value=''>display _b[mean]</option><option value=''>show mean(salario)</option></select>


<div class='webex-solution'><button>Ver explicación</button>

`summarize` es un comando de clase `r`; guarda la media en `r(mean)` hasta que otro comando sobrescriba esos resultados.

</div>

</div>

</div>

</div> <!-- cierre prueba-entrada-quiz -->

---

## Puntaje (automático) {-}

<div class="scoring-guide">
  <button type="button" id="btn-finalizar" class="btn-score">
    Finalizar y calcular puntaje
  </button>

  <div id="score-result" style="display:none; margin-top: 15px;"></div>

  <p style="margin-top: 12px;">
    Nota: el puntaje se calcula en tu navegador (HTML).
  </p>
</div>

```{=html}
<script>
(function () {

  function normalizeText(s, ignoreCase, noSpaces) {
    let out = (s ?? "").toString().trim();
    if (noSpaces) out = out.replace(/\s+/g, "");
    if (ignoreCase) out = out.toLowerCase();
    return out;
  }

  function fitbCorrect(input) {
    const raw = (input.value ?? "").toString();
    const answered = raw.trim().length > 0;

    const ignoreCase =
      input.classList.contains("ignorecase") ||
      input.classList.contains("ignore_case") ||
      input.classList.contains("ignore-case");

    const noSpaces = input.classList.contains("nospaces");

    if (!answered) return { answered: false, correct: false };

    let answers = [];
    if (input.dataset && input.dataset.answer) {
      try { answers = JSON.parse(input.dataset.answer); }
      catch (e) { answers = [input.dataset.answer]; }
    }

    const isNum = input.classList.contains("num");
    const tol = (input.dataset && input.dataset.tol) ? parseFloat(input.dataset.tol) : null;
    const isRegex = input.classList.contains("regex");

    if (isNum) {
      const v = parseFloat(raw);
      if (Number.isNaN(v)) return { answered: true, correct: false };
      for (const a of answers) {
        const af = parseFloat(a);
        if (Number.isNaN(af)) continue;
        if (tol !== null) {
          if (Math.abs(v - af) <= tol) return { answered: true, correct: true };
        } else {
          if (v === af) return { answered: true, correct: true };
        }
      }
      return { answered: true, correct: false };
    }

    if (isRegex) {
      for (const a of answers) {
        try {
          const re = new RegExp(a);
          if (re.test(raw.trim())) return { answered: true, correct: true };
        } catch (e) {}
      }
      return { answered: true, correct: false };
    }

    const val = normalizeText(raw, ignoreCase, noSpaces);
    for (const a of answers) {
      const ans = normalizeText(a, ignoreCase, noSpaces);
      if (val === ans) return { answered: true, correct: true };
    }
    return { answered: true, correct: false };
  }

  function selectCorrect(sel) {
    const v = (sel.value ?? "").toString();
    if (v === "" || v === "blank") return { answered: false, correct: false };
    return { answered: true, correct: v === "answer" };
  }

  function radioCorrect(group) {
    const checked = group.querySelector("input[type='radio']:checked");
    if (!checked) return { answered: false, correct: false };
    return { answered: true, correct: checked.value === "answer" };
  }

  function scoreQuestionBox(box) {
    const inputs = Array.from(box.querySelectorAll("input.webex-solveme"));
    const selects = Array.from(box.querySelectorAll("select.webex-select"));
    const radios  = Array.from(box.querySelectorAll(".webex-radiogroup"));

    const widgets = inputs.length + selects.length + radios.length;
    if (widgets === 0) return { answered: false, correct: false };

    let answeredAny = false;
    let allCorrect = true;

    inputs.forEach(i => {
      const r = fitbCorrect(i);
      answeredAny = answeredAny || r.answered;
      allCorrect = allCorrect && r.correct;
    });

    selects.forEach(s => {
      const r = selectCorrect(s);
      answeredAny = answeredAny || r.answered;
      allCorrect = allCorrect && r.correct;
    });

    radios.forEach(g => {
      const r = radioCorrect(g);
      answeredAny = answeredAny || r.answered;
      allCorrect = allCorrect && r.correct;
    });

    return { answered: answeredAny, correct: allCorrect };
  }

  function computeScores(root) {
    const qBoxes = Array.from(root.querySelectorAll(".question-box"));
    const sections = Array.from(root.querySelectorAll(".quiz-section"));

    let total = 0, correct = 0, answered = 0;

    qBoxes.forEach(box => {
      total += 1;
      const r = scoreQuestionBox(box);
      if (r.answered) answered += 1;
      if (r.correct) correct += 1;
    });

    const bySection = sections.map(sec => {
      const name = sec.getAttribute("data-section") || "Sección";
      const boxes = Array.from(sec.querySelectorAll(".question-box"));
      let c = 0;
      boxes.forEach(b => { if (scoreQuestionBox(b).correct) c += 1; });
      return { name, total: 5, correct: c };
    });

    return { total: 20, correct, answered, bySection };
  }

  function recommendation(nCorrect) {
    if (nCorrect <= 2) return "Repaso prioritario";
    if (nCorrect === 3) return "Repaso recomendado";
    return "Preparación suficiente";
  }

  document.addEventListener("DOMContentLoaded", () => {
    const root = document.getElementById("prueba-entrada-quiz");
    const btn = document.getElementById("btn-finalizar");
    const out = document.getElementById("score-result");

    if (!root || !btn || !out) return;

    btn.addEventListener("click", () => {
      const s = computeScores(root);
      const pct = s.total > 0 ? Math.round(100 * s.correct / s.total) : 0;
      const pending = Array.from(root.querySelectorAll(".question-box"))
        .map((box, index) => ({ box, number: index + 1 }))
        .filter(item => !scoreQuestionBox(item.box).answered)
        .map(item => item.number);

      const rows = s.bySection.map(x =>
        `<tr><td>${x.name}</td><td>${x.correct} / 5</td><td class="score-recommendation">${recommendation(x.correct)}</td></tr>`
      ).join("");

      const pendingMessage = pending.length > 0
        ? `<p><strong>Preguntas pendientes (contadas como incorrectas):</strong> ${pending.join(", ")}</p>`
        : "<p><strong>Preguntas pendientes:</strong> ninguna.</p>";

      out.innerHTML = `
        <h3>Resultados</h3>
        <p><strong>Puntaje total:</strong> ${s.correct} / 20 (${pct}%)</p>
        <p><strong>Preguntas respondidas:</strong> ${s.answered} / 20</p>
        ${pendingMessage}

        <h4>Desglose por sección</h4>
        <table>
          <thead><tr><th>Sección</th><th>Puntaje</th><th>Recomendación</th></tr></thead>
          <tbody>${rows}</tbody>
        </table>
      `;

      out.style.display = "block";
      root.classList.add("show-feedback");
      out.scrollIntoView({ behavior: "smooth", block: "start" });
    });
  });

})();
</script>
```

---

## Recursos para Repasar {-}

<div class="resources-box">

### Si necesitas repasar Estadística Básica {-}

- Khan Academy - Estadística y Probabilidad
```text
https://es.khanacademy.org/math/statistics-probability
```

- OpenIntro Statistics (libro gratuito)
```text
https://www.openintro.org/book/os/
```

### Si necesitas repasar Regresión Lineal {-}

- Wooldridge, "Introductory Econometrics" (Caps 1-4)

- Khan Academy - Regresión
```text
https://es.khanacademy.org/math/statistics-probability/describing-relationships-quantitative-data
```

- Ben Lambert (playlist)
```text
https://www.youtube.com/playlist?list=PLwJRxp3blEvZyQBTTOMFRP_TDaSdly3gU
```

- Pablo Adrián Garlati-Bertoldi (Pontificia Universidad Javeriana) – Econometría básica (canal y playlist)
```text
Canal: https://www.youtube.com/@adriangarlati
Playlist Econometría básica: https://www.youtube.com/playlist?list=PLOhc9wF2hQxFhTw2qvS6hd3RiezOh4tBl
```

### Si necesitas repasar Causalidad {-}

- Cunningham, "Causal Inference: The Mixtape" (Caps 1-3)
```text
https://mixtape.scunning.com/
```

- Nick Huntington-Klein (playlist)
```text
https://www.youtube.com/playlist?list=PLcTBLulJV_AIuXCxr__V8XAzWZosMQIfW
```

### Si necesitas aprender/repasar Stata {-}

- UCLA IDRE Stata Modules
```text
https://stats.oarc.ucla.edu/stata/modules/
```

- Stata Video Tutorials
```text
https://www.stata.com/links/video-tutorials/
```

</div>

<div class="tip-box">

### Consejos para el Curso {-}

1. Practica con datos reales.
2. No memorices: entiende.
3. Haz los ejercicios.
4. Pregunta en clase u horas de oficina.
5. Forma grupos de estudio.

</div>

<center>
<strong>Buena suerte en el curso</strong>
</center>
