# Prueba de Entrada {-}



<style type="text/css">
/* Estilos personalizados para la prueba de entrada */

.quiz-section {
  background-color: #f8f9fa;
  border-left: 4px solid #1F77B4;
  padding: 20px;
  margin: 20px 0;
  border-radius: 5px;
}

.quiz-section h2 {
  color: #1F77B4;
  margin-top: 0;
}

.question-box {
  background-color: white;
  padding: 15px;
  margin: 15px 0;
  border-radius: 8px;
  box-shadow: 0 2px 4px rgba(0,0,0,0.1);
}

.question-number {
  font-weight: bold;
  color: #1F77B4;
  font-size: 1.1em;
}

.intro-box {
  background: linear-gradient(135deg, #1F77B4 0%, #2E86AB 100%);
  color: white;
  padding: 25px;
  border-radius: 10px;
  margin-bottom: 30px;
}

.intro-box h3 {
  margin-top: 0;
  color: white;
}

.scoring-guide {
  background-color: #e8f4f8;
  border: 2px solid #1F77B4;
  padding: 20px;
  border-radius: 10px;
  margin-top: 30px;
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
#prueba-entrada-quiz:not(.show-feedback) .webex-correct,
#prueba-entrada-quiz:not(.show-feedback) .webex-incorrect,
#prueba-entrada-quiz:not(.show-feedback) input.webex-solveme.webex-correct,
#prueba-entrada-quiz:not(.show-feedback) input.webex-solveme.webex-incorrect,
#prueba-entrada-quiz:not(.show-feedback) .webex-radiogroup label.webex-correct,
#prueba-entrada-quiz:not(.show-feedback) .webex-radiogroup label.webex-incorrect {
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

/* Botón de puntaje */
.btn-score {
  display: inline-block;
  padding: 10px 14px;
  border-radius: 8px;
  border: 1px solid #1F77B4;
  background: #1F77B4;
  color: #fff;
  cursor: pointer;
  font-weight: 600;
}

.btn-score:disabled {
  opacity: 0.6;
  cursor: not-allowed;
}

#score-result table {
  width: 100%;
  border-collapse: collapse;
  margin-top: 10px;
}

#score-result th, #score-result td {
  border: 1px solid #dee2e6;
  padding: 8px;
  text-align: left;
}
</style>

<div class="intro-box">
<h3>Bienvenido a la Prueba de Entrada</h3>

Esta prueba diagnóstica tiene como objetivo evaluar tus conocimientos previos en estadística, regresión lineal, causalidad y manejo básico de Stata.

<strong>Instrucciones:</strong>

- La prueba contiene <strong>18 preguntas</strong> divididas en 4 secciones
- Responde cada pregunta seleccionando la opcion correcta o escribiendo tu respuesta
- La retroalimentación, las explicaciones y el puntaje se muestran solo al final, cuando presiones <strong>Finalizar y calcular puntaje</strong>
- No hay limite de tiempo, pero intenta responder sin consultar materiales
- Al final encontraras recursos para repasar

<strong>Tiempo estimado:</strong> 15-20 minutos

</div>

---

<div id="prueba-entrada-quiz">

## Seccion 1: Estadistica Basica {-}

<div class="quiz-section" data-section="Estadistica Basica">

Esta seccion evalua conceptos fundamentales de estadistica descriptiva e inferencial que son esenciales para el analisis econometrico.

<div class="question-box">
<span class="question-number">Pregunta 1.</span> Si una variable X tiene media 50 y desviación estándar 10, y sigue una distribución normal, ¿qué porcentaje de las observaciones se encuentra entre 30 y 70?

<select class='webex-select'><option value='blank'></option><option value=''>68%</option><option value='answer'>95%</option><option value=''>99%</option><option value=''>50%</option></select>


<div class='webex-solution'><button>Ver explicacion</button>

En una distribucion normal:

- El 68% de los datos esta dentro de 1 desviacion estandar de la media
- El 95% de los datos esta dentro de 2 desviaciones estandar de la media
- El 99.7% esta dentro de 3 desviaciones estandar

Como 30 = 50 - 2(10) y 70 = 50 + 2(10), estamos hablando de 2 desviaciones estandar, por lo tanto es el 95%.

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 2.</span> El p-value (valor p) representa:

<select class='webex-select'><option value='blank'></option><option value=''>La probabilidad de que la hipotesis nula sea verdadera</option><option value='answer'>La probabilidad de observar datos tan extremos o mas, dado que H0 es verdadera</option><option value=''>La probabilidad de que la hipotesis alternativa sea falsa</option><option value=''>El nivel de significancia del test</option></select>


<div class='webex-solution'><button>Ver explicacion</button>

El p-value es la probabilidad de obtener un resultado tan extremo o mas extremo que el observado, asumiendo que la hipotesis nula es verdadera.

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 3.</span> Rechazar la hipotesis nula cuando en realidad es verdadera se conoce como:

<select class='webex-select'><option value='blank'></option><option value='answer'>Error Tipo I</option><option value=''>Error Tipo II</option><option value=''>Sesgo de seleccion</option><option value=''>Error estandar</option></select>


<div class='webex-solution'><button>Ver explicacion</button>

- Error Tipo I: Rechazar H0 cuando es verdadera (probabilidad alpha).
- Error Tipo II: No rechazar H0 cuando es falsa (probabilidad beta).

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 4.</span> Un intervalo de confianza del 95% significa que:

<select class='webex-select'><option value='blank'></option><option value=''>Hay 95% de probabilidad de que el par<U+00E1>metro verdadero est<U+00E9> en este intervalo espec<U+00ED>fico</option><option value='answer'>Si construy<U+00E9>ramos 100 intervalos de esta manera, 95 de ellos contendr<U+00ED>an el par<U+00E1>metro verdadero</option><option value=''>El 95% de los datos est<U+00E1> dentro del intervalo</option><option value=''>Estamos 95% seguros de nuestra estimaci<U+00F3>n</option></select>


<div class='webex-solution'><button>Ver explicacion</button>

Interpretación frecuentista: si repitiéramos el muestreo muchas veces y construyéramos IC del 95% cada vez, cerca del 95% de esos intervalos contendrían el valor verdadero del parámetro.

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 5.</span> La varianza mide:

<select class='webex-select'><option value='blank'></option><option value=''>La tendencia central de los datos</option><option value='answer'>La dispersion de los datos alrededor de la media</option><option value=''>La relacion entre dos variables</option><option value=''>El valor mas frecuente</option></select>


<div class='webex-solution'><button>Ver explicacion</button>

La varianza mide la dispersion alrededor de la media: Var(X) = E[(X - mu)^2]. La desviacion estandar es la raiz cuadrada de la varianza.

</div>

</div>

</div>

---

## Seccion 2: Regresion Lineal {-}

<div class="quiz-section" data-section="Regresion Lineal">

Esta seccion evalua tu comprension del modelo de regresion lineal, sus supuestos e interpretacion.

<div class="question-box">
<span class="question-number">Pregunta 6.</span> En el modelo Y = beta0 + beta1 X + e, el coeficiente beta1 representa:

<select class='webex-select'><option value='blank'></option><option value=''>El valor de Y cuando X = 0</option><option value='answer'>El cambio esperado en Y por cada unidad adicional de X</option><option value=''>La correlacion entre X e Y</option><option value=''>La varianza de Y explicada por X</option></select>


<div class='webex-solution'><button>Ver explicacion</button>

beta1 es el cambio esperado en Y asociado con una unidad adicional de X (ceteris paribus).

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 7.</span> Si R2 = 0.75, esto significa que:

<select class='webex-select'><option value='blank'></option><option value=''>El coeficiente de correlacion es 0.75</option><option value=''>El modelo tiene 75% de probabilidad de ser correcto</option><option value='answer'>El 75% de la variacion en Y es explicada por las variables independientes</option><option value=''>El 75% de las observaciones estan correctamente predichas</option></select>


<div class='webex-solution'><button>Ver explicacion</button>

R2 es la proporcion de la varianza de Y explicada por el modelo.

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 8.</span> Cual de las siguientes NO es un supuesto del modelo clasico de regresion lineal (OLS)?

<select class='webex-select'><option value='blank'></option><option value=''>Los errores tienen media cero</option><option value=''>Los errores son homoced<U+00E1>sticos</option><option value=''>No hay multicolinealidad perfecta</option><option value='answer'>Los errores deben seguir una distribuci<U+00F3>n uniforme</option></select>


<div class='webex-solution'><button>Ver explicacion</button>

OLS no requiere errores uniformes. Para inferencia en muestras pequeñas se suele asumir normalidad.

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 9.</span> Si el p-value asociado a un coeficiente es 0.03, esto significa que:

<select class='webex-select'><option value='blank'></option><option value=''>El coeficiente es significativo al 1%</option><option value='answer'>El coeficiente es significativo al 5%</option><option value=''>El coeficiente no es significativo</option><option value=''>Hay 3% de probabilidad de que el coeficiente sea correcto</option></select>


<div class='webex-solution'><button>Ver explicacion</button>

0.03 < 0.05 implica significancia al 5% (y al 10%), pero no al 1%.

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 10.</span> En un modelo log(Y) = beta0 + beta1 X + e, el coeficiente beta1 se interpreta aproximadamente como:

<select class='webex-select'><option value='blank'></option><option value=''>El cambio absoluto en Y por unidad de X</option><option value=''>El cambio en log(Y) en terminos absolutos</option><option value='answer'>El cambio porcentual en Y por cada unidad adicional de X</option><option value=''>La elasticidad de Y respecto a X</option></select>


<div class='webex-solution'><button>Ver explicacion</button>

En un modelo semi-log, beta1 * 100 es el cambio porcentual aproximado en Y por una unidad adicional de X (para cambios pequeños).

</div>

</div>

</div>

---

## Seccion 3: Causalidad {-}

<div class="quiz-section" data-section="Causalidad">

Esta seccion evalua la diferencia entre correlacion y causalidad, y conceptos basicos de inferencia causal.

<div class="question-box">
<span class="question-number">Pregunta 11.</span> La observacion de que "los paises con mayor consumo de chocolate tienen mas premios Nobel per capita" es un ejemplo de:

<select class='webex-select'><option value='blank'></option><option value=''>Causalidad directa</option><option value=''>Causalidad inversa</option><option value='answer'>Correlacion espuria (correlacion sin causalidad)</option><option value=''>Efecto placebo</option></select>


<div class='webex-solution'><button>Ver explicacion</button>

Ejemplo de correlacion espuria: puede haber una tercera variable (p. ej. desarrollo) que explique ambos.

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 12.</span> El sesgo de seleccion ocurre cuando:

<select class='webex-select'><option value='blank'></option><option value=''>La muestra es muy pequena</option><option value=''>Los datos tienen errores de medicion</option><option value='answer'>Los individuos tratados son sistematicamente diferentes de los no tratados</option><option value=''>El modelo tiene muchas variables</option></select>


<div class='webex-solution'><button>Ver explicacion</button>

Sesgo de seleccion: E[Y0|D=1] != E[Y0|D=0].

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 13.</span> La asignacion aleatoria del tratamiento es importante porque:

<select class='webex-select'><option value='blank'></option><option value=''>Aumenta el tamano de la muestra</option><option value=''>Reduce los costos del estudio</option><option value='answer'>Hace que los grupos de tratamiento y control sean comparables en expectativa</option><option value=''>Elimina los errores de medicion</option></select>


<div class='webex-solution'><button>Ver explicacion</button>

Aleatorizacion: balancea (en expectativa) observables y no observables.

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 14.</span> El contrafactual se refiere a:

<select class='webex-select'><option value='blank'></option><option value=''>Los datos que se perdieron en el estudio</option><option value=''>El grupo de control</option><option value='answer'>Lo que habria ocurrido en ausencia del tratamiento</option><option value=''>Los efectos secundarios del tratamiento</option></select>


<div class='webex-solution'><button>Ver explicacion</button>

Contrafactual: el resultado potencial en el estado no observado.

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 15.</span> El principal problema al comparar simplemente participantes vs. no participantes es:

<select class='webex-select'><option value='blank'></option><option value=''>La diferencia en tamano de los grupos</option><option value=''>Los errores de medicion en los datos</option><option value='answer'>La autoseleccion (los que participan pueden ser diferentes de los que no)</option><option value=''>La falta de datos</option></select>


<div class='webex-solution'><button>Ver explicacion</button>

Autoseleccion: diferencias previas (motivacion, habilidad, redes) contaminan la comparacion.

</div>

</div>

</div>

---

## Seccion 4: Stata {-}

<div class="quiz-section" data-section="Stata">

Esta seccion evalua tu familiaridad basica con Stata.

<div class="question-box">
<span class="question-number">Pregunta 16.</span> En Stata, cual comando usarias para ver las primeras observaciones de tu base de datos?

<select class='webex-select'><option value='blank'></option><option value=''>view</option><option value='answer'>browse o list</option><option value=''>show</option><option value=''>display</option></select>


<div class='webex-solution'><button>Ver explicacion</button>

`browse` abre el visor de datos. `list` imprime observaciones en Results (por ejemplo: `list in 1/10`).

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 17.</span> En una regresion en Stata, si el coeficiente de x es 2.35, esto significa que:

<select class='webex-select'><option value='blank'></option><option value=''>La variable x tiene un p-value de 2.35</option><option value='answer'>Por cada unidad adicional de x, Y aumenta en 2.35 unidades (en promedio)</option><option value=''>El R-cuadrado del modelo es 2.35</option><option value=''>Hay 2.35 observaciones con esa variable</option></select>


<div class='webex-solution'><button>Ver explicacion</button>

El coeficiente es el cambio promedio en Y asociado con una unidad adicional en X, ceteris paribus.

</div>

</div>

<div class="question-box">
<span class="question-number">Pregunta 18.</span> Cual es el comando correcto en Stata para realizar una regresion de Y sobre X1 y X2?

<input class='webex-solveme nospaces ignorecase' size='25' data-answer='["reg Y X1 X2","regress Y X1 X2"]'/>


<div class='webex-solution'><button>Ver explicacion</button>

Comando: `regress Y X1 X2` (abreviado: `reg Y X1 X2`).

</div>

</div>

</div>

</div> <!-- cierre prueba-entrada-quiz -->

---

## Puntaje (automatico) {-}

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
      const name = sec.getAttribute("data-section") || "Seccion";
      const boxes = Array.from(sec.querySelectorAll(".question-box"));
      let t = boxes.length, c = 0;
      boxes.forEach(b => { if (scoreQuestionBox(b).correct) c += 1; });
      return { name, total: t, correct: c };
    });

    return { total, correct, answered, bySection };
  }

  function levelMessage(nCorrect) {
    if (nCorrect >= 16) return "Excelente: base muy solida para el curso.";
    if (nCorrect >= 13) return "Buen nivel: repasa los temas con errores.";
    if (nCorrect >= 10) return "Nivel aceptable: conviene repasar prerequisitos.";
    return "Reforzar prerequisitos: revisa los recursos sugeridos.";
  }

  document.addEventListener("DOMContentLoaded", () => {
    const root = document.getElementById("prueba-entrada-quiz");
    const btn = document.getElementById("btn-finalizar");
    const out = document.getElementById("score-result");

    if (!root || !btn || !out) return;

    btn.addEventListener("click", () => {
      const s = computeScores(root);
      const pct = s.total > 0 ? Math.round(100 * s.correct / s.total) : 0;

      const rows = s.bySection.map(x =>
        `<tr><td>${x.name}</td><td>${x.correct} / ${x.total}</td></tr>`
      ).join("");

      out.innerHTML = `
        <h3>Resultados</h3>
        <p><strong>Puntaje total:</strong> ${s.correct} / ${s.total} (${pct}%)</p>
        <p><strong>Preguntas respondidas:</strong> ${s.answered} / ${s.total}</p>
        <p><strong>Diagnostico:</strong> ${levelMessage(s.correct)}</p>

        <h4>Desglose por seccion</h4>
        <table>
          <thead><tr><th>Seccion</th><th>Puntaje</th></tr></thead>
          <tbody>${rows}</tbody>
        </table>
      `;

      out.style.display = "block";
      root.classList.add("show-feedback"); // ahora sí se muestran correct/incorrect y explicaciones
      out.scrollIntoView({ behavior: "smooth", block: "start" });
    });
  });

})();
</script>
```

---

## Recursos para Repasar {-}

<div class="resources-box">

### Si necesitas repasar Estadistica Basica {-}

- Khan Academy - Estadistica y Probabilidad
```text
https://es.khanacademy.org/math/statistics-probability
```

- OpenIntro Statistics (libro gratuito)
```text
https://www.openintro.org/book/os/
```

### Si necesitas repasar Regresion Lineal {-}

- Wooldridge, "Introductory Econometrics" (Caps 1-4)

- Khan Academy - Regresion
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
