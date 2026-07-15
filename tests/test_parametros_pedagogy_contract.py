import re
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
THEORY = (ROOT / "03-Parametros.Rmd").read_text(encoding="utf-8")
PRACTICE = (ROOT / "04-ParametrosStata.Rmd").read_text(encoding="utf-8")


def _pedagogy_boxes(text):
    """Return fenced divs whose opening class is one of the book's box classes."""
    boxes = []
    lines = text.splitlines()
    index = 0
    while index < len(lines):
        opening = re.match(r"^(?P<fence>:{3,})\s+\{[^}]*\.box[^}]*\}\s*$", lines[index])
        if not opening:
            index += 1
            continue
        fence = opening.group("fence")
        end = index + 1
        while end < len(lines) and lines[end].strip() != fence:
            end += 1
        assert end < len(lines), "Bloque pedagógico sin cierre"
        boxes.append("\n".join(lines[index + 1:end]))
        index = end + 1
    return boxes


def _exam_question_boxes(text, code_pattern):
    pattern = re.compile(rf"(?<![A-Z0-9-]){code_pattern}(?![A-Z0-9-])")
    return [(box, pattern.findall(box)) for box in _pedagogy_boxes(text) if pattern.search(box)]


def test_theory_has_colored_learning_blocks():
    boxes = _pedagogy_boxes(THEORY)
    assert len(boxes) >= 6
    for label in ["Intuición", "Resultado clave", "Advertencia", "Ejemplo guiado"]:
        assert label in THEORY


def test_theory_has_exactly_three_exam_questions_without_answers():
    questions = _exam_question_boxes(THEORY, r"T-P[1-3]")
    codes = [code for _box, matches in questions for code in matches]
    assert codes == ["T-P1", "T-P2", "T-P3"]
    assert len(questions) == 3
    for block, matches in questions:
        assert len(matches) == 1
        lowered = block.lower()
        assert "puntaje sugerido" in lowered
        assert not any(
            marker in lowered
            for marker in ["respuesta:", "solución:", "pista:", "<details", "hide("]
        )


def test_practice_restores_twelve_guided_stages():
    stages = [
        "Preparación de los datos",
        "Descripción por grupos",
        "Diferencia de medias",
        "Regresión simple",
        "Programa `estimadores`",
        "ATE, ATT, ATU y CATE",
        "Descomposición del sesgo",
        "Duplicación de observaciones",
        "Asignación aleatoria",
        "Monte Carlo con selección",
        "Monte Carlo con aleatorización",
        "Comparación gráfica",
    ]
    missing = [stage for stage in stages if stage not in PRACTICE]
    assert not missing, f"Faltan etapas guiadas: {', '.join(missing)}"
    positions = [PRACTICE.index(stage) for stage in stages]
    assert positions == sorted(positions)


def test_practice_has_required_blocks_and_exam_questions():
    for label in [
        "Comando clave",
        "Salida central",
        "Interpretación",
        "Error frecuente",
        "Resultado clave",
    ]:
        assert label in PRACTICE
    assert len(_pedagogy_boxes(PRACTICE)) >= 10

    questions = _exam_question_boxes(PRACTICE, r"S-P[1-4]")
    codes = [code for _box, matches in questions for code in matches]
    assert codes == ["S-P1", "S-P2", "S-P3", "S-P4"]
    assert len(questions) == 4
    assert all(len(matches) == 1 for _box, matches in questions)


def test_student_material_never_exposes_private_key():
    combined = THEORY + PRACTICE + (ROOT / "_bookdown.yml").read_text(encoding="utf-8")
    assert "clave_parametros_causales" not in combined
    assert "claves_docentes" not in combined
    assert "<details" not in THEORY + PRACTICE
    assert "Ver respuesta" not in THEORY + PRACTICE
