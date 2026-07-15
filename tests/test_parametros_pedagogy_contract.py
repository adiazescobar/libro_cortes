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


def _assert_exact_question_codes(text, prefix, expected_codes):
    """Require every question code globally, exactly once, inside its own box."""
    pattern = re.compile(rf"(?<![A-Z0-9-]){re.escape(prefix)}P\d+(?![A-Z0-9-])")
    global_codes = pattern.findall(text)
    assert global_codes == expected_codes, (
        f"Los códigos {prefix} deben ser únicos, estar en orden y limitarse a "
        f"{expected_codes}; encontrados: {global_codes}"
    )

    questions = [
        (box, pattern.findall(box))
        for box in _pedagogy_boxes(text)
        if pattern.search(box)
    ]
    boxed_codes = [code for _box, matches in questions for code in matches]
    assert boxed_codes == global_codes, "Cada código debe estar dentro de un bloque de pregunta"
    assert len(questions) == len(expected_codes), "Cada pregunta debe tener su propio bloque"
    assert all(len(matches) == 1 for _box, matches in questions)
    return questions


def _contains_answer_marker(block):
    answer_label = re.compile(
        r"(?im)^\s*(?:>\s*)?(?:[-*+]\s+)?(?:#{1,6}\s+)?"
        r"(?:(?:\*\*|__)?(?:respuesta|solución|pista)"
        r"(?:\s*:\s*(?:\*\*|__)?|(?:\*\*|__)\s*:))"
    )
    lowered = block.casefold()
    return bool(answer_label.search(block)) or any(
        marker in lowered for marker in ["<details", "hide(", "ver respuesta"]
    )


def _assert_no_answer_markers(block):
    assert not _contains_answer_marker(block)


def test_answer_marker_detection_allows_legitimate_question_wording():
    assert not _contains_answer_marker("Justifique su respuesta con el supuesto central.")
    assert not _contains_answer_marker("Proponga una solución y discuta sus límites.")
    assert not _contains_answer_marker("Use la pista del enunciado para argumentar.")
    for marker in [
        "Respuesta: texto",
        "  **Solución:** texto",
        "### __Pista__: texto",
        "> **RESPUESTA**: texto",
        "- Pista: texto",
        "<DETAILS>",
        "hide(panel)",
        "VER RESPUESTA",
    ]:
        assert _contains_answer_marker(marker)


def test_theory_has_colored_learning_blocks():
    boxes = _pedagogy_boxes(THEORY)
    assert len(boxes) >= 6
    for label in ["Intuición", "Resultado clave", "Advertencia", "Ejemplo guiado"]:
        assert label in THEORY


def test_theory_has_exactly_three_exam_questions_without_answers():
    questions = _assert_exact_question_codes(THEORY, "T-", ["T-P1", "T-P2", "T-P3"])
    for block, matches in questions:
        assert len(matches) == 1
        lowered = block.casefold()
        assert "puntaje sugerido" in lowered
        _assert_no_answer_markers(block)


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

    questions = _assert_exact_question_codes(
        PRACTICE, "S-", ["S-P1", "S-P2", "S-P3", "S-P4"]
    )
    for block, _matches in questions:
        _assert_no_answer_markers(block)


def test_student_material_never_exposes_private_key():
    combined = (
        THEORY + PRACTICE + (ROOT / "_bookdown.yml").read_text(encoding="utf-8")
    ).casefold()
    assert "clave_parametros_causales" not in combined
    assert "claves_docentes" not in combined
    assert "<details" not in combined
    assert "ver respuesta" not in combined
