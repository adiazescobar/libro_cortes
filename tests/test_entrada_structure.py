from pathlib import Path
import subprocess


ROOT = Path(__file__).resolve().parents[1]
TEXT = (ROOT / "00-PruebaEntrada.Rmd").read_text(encoding="utf-8")


def test_quiz_never_installs_during_render():
    assert "install.packages" not in TEXT
    assert 'stop("Falta el paquete webexercises' in TEXT
    assert "Instálalo con" in TEXT
    assert "packages('webexercises')" in TEXT


def test_instructions_precede_quiz():
    assert TEXT.index("## Antes de comenzar {-}") < TEXT.index(
        '<div id="prueba-entrada-quiz">'
    )
    for phrase in [
        "20 preguntas",
        "15–20 minutos",
        "diagnóstica",
        "sin consultar materiales",
    ]:
        assert phrase in TEXT


def test_scoring_contract_is_present():
    assert 'id="btn-finalizar"' in TEXT
    assert 'id="score-result"' in TEXT
    assert "Estadística básica" in TEXT
    assert "Regresión lineal" in TEXT
    assert "return { total: 20" in TEXT
    assert "return { name, total: 5" in TEXT
    assert 'if (nCorrect <= 2) return "Repaso prioritario"' in TEXT
    assert 'if (nCorrect === 3) return "Repaso recomendado"' in TEXT
    assert 'return "Preparación suficiente"' in TEXT
    assert "Preguntas pendientes (contadas como incorrectas)" in TEXT
    assert 'root.classList.add("show-feedback")' in TEXT


def test_scoring_javascript_runs_only_on_finalize_and_reports_sections():
    script = TEXT.split("<script>", 1)[1].split("</script>", 1)[0]
    harness = r"""
const events = [];
const sectionScores = [2, 3, 4, 5];
const boxes = [];

sectionScores.forEach((score, sectionIndex) => {
  for (let index = 0; index < 5; index += 1) {
    const pending = sectionIndex === 0 && index === 4;
    const correct = index < score;
    const questionIndex = sectionIndex * 5 + index;
    const select = { value: pending ? "blank" : (correct ? "answer" : "wrong") };
    const input = {
      value: "regress Y X1 X2",
      classList: { contains: value => value === "ignore_case" },
      dataset: { answer: JSON.stringify(["regress Y X1 X2"]) }
    };
    boxes.push({
      querySelectorAll: selector => {
        if (questionIndex === 17 && selector === "input.webex-solveme") return [input];
        if (questionIndex !== 17 && selector === "select.webex-select") return [select];
        return [];
      }
    });
  }
});

const names = ["Estadística básica", "Regresión lineal", "Causalidad", "Stata"];
const sections = names.map((name, index) => ({
  getAttribute: () => name,
  querySelectorAll: () => boxes.slice(index * 5, index * 5 + 5)
}));
const root = {
  querySelectorAll: selector => selector === ".question-box" ? boxes : sections,
  classList: { add: value => events.push(`reveal:${value}`) }
};
const button = { addEventListener: (_event, listener) => { button.listener = listener; } };
const output = {
  style: {},
  scrollIntoView: () => {},
  set innerHTML(value) { this.html = value; events.push("render"); },
  get innerHTML() { return this.html; }
};
global.document = {
  addEventListener: (_event, listener) => listener(),
  getElementById: id => ({
    "prueba-entrada-quiz": root,
    "btn-finalizar": button,
    "score-result": output
  })[id]
};
"""
    assertions = r"""
if (events.length !== 0) throw new Error("feedback before finalize");
button.listener();
const expected = [
  "14 / 20", "2 / 5", "3 / 5", "4 / 5", "5 / 5",
  "Repaso prioritario", "Repaso recomendado", "Preparación suficiente",
  "Preguntas pendientes (contadas como incorrectas):</strong> 5"
];
expected.forEach(value => {
  if (!output.html.includes(value)) throw new Error(`missing: ${value}`);
});
if (events.join("|") !== "render|reveal:show-feedback") {
  throw new Error(`wrong event order: ${events.join("|")}`);
}
"""
    subprocess.run(
        ["node", "-e", harness + script + assertions],
        check=True,
        capture_output=True,
        text=True,
    )
