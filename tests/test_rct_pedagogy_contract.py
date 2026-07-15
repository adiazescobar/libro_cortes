import os
import re
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
THEORY = (ROOT / "05-RCT.Rmd").read_text(encoding="utf-8")
PRACTICE = (ROOT / "06-RCT2.Rmd").read_text(encoding="utf-8")


def _boxes(text):
    """Extract closed fenced divs using one of the book's box classes."""
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


def _has_answer_marker(block):
    answer_label = re.compile(
        r"(?im)^\s*(?:>\s*)?(?:[-*+]\s+)?(?:#{1,6}\s+)?"
        r"(?:(?:\*\*|__)?(?:respuesta|solución|pista)"
        r"(?:\s*:\s*(?:\*\*|__)?|(?:\*\*|__)\s*:))"
    )
    lowered = block.casefold()
    return bool(answer_label.search(block)) or any(
        marker in lowered for marker in ["<details", "hide(", "ver respuesta"]
    )


def _questions(text, family, expected):
    pattern = re.compile(rf"(?<![A-Z0-9-]){re.escape(family)}\d+(?![A-Z0-9-])")
    global_codes = pattern.findall(text)
    assert global_codes == expected, (
        f"Códigos {family} únicos y en orden: esperados {expected}; "
        f"encontrados {global_codes}"
    )
    question_boxes = [box for box in _boxes(text) if pattern.search(box)]
    assert len(question_boxes) == len(expected), "Cada pregunta debe ocupar su propio bloque"
    for code, block in zip(expected, question_boxes):
        assert pattern.findall(block) == [code]
    return question_boxes


def _assert_no_manual_numbering(text):
    forbidden = re.compile(
        r"^#{2,4}\s+(?:(?:PASO|Paso|Etapa)\b\s*\d*|\d+\s*[.):_-])",
        re.MULTILINE,
    )
    assert not forbidden.search(text), "Bookdown, no el Rmd, debe numerar encabezados"


def test_theory_preserves_foundational_content_and_resources():
    required = [
        "Resultados potenciales:",
        "sesgo de selección",
        "Resumen de los cuatro escenarios:",
        "Auto-selección",
        "Aleatorización",
        "CATE(x)",
        "El truco de centrar (Wooldridge)",
        "## Supuestos, propiedades y condiciones de validez {-}",
        "## Amenazas, limitaciones y errores comunes {-}",
        "## Referencias {-}",
        "https://www.youtube.com/embed/eGRd8jBdNYg",
        "https://www.youtube.com/embed/crpuBZv6XtA",
        "https://www.youtube.com/embed/xlX3VtuIfQ0",
    ]
    assert all(fragment in THEORY for fragment in required)
    scenarios = [
        "RCT simple, sin estratos, sin controles",
        "RCT simple, sin estratos, con controles",
        "RCT estratificado (bloques), sin controles adicionales",
        "RCT estratificado + controles adicionales",
    ]
    assert [THEORY.index(fragment) for fragment in scenarios] == sorted(
        THEORY.index(fragment) for fragment in scenarios
    )


def test_theory_has_required_learning_blocks():
    assert len(_boxes(THEORY)) >= 8
    for label in ["Intuición", "Resultado clave", "Demostración", "Advertencia", "Comparación"]:
        assert label in THEORY


def test_theory_has_exact_exam_questions_without_answers():
    for block in _questions(THEORY, "RCT-T", ["RCT-T1", "RCT-T2", "RCT-T3"]):
        assert block.casefold().count("puntaje sugerido") == 1
        assert block.casefold().count("producto esperado") == 1
        assert not _has_answer_marker(block)


def test_practice_has_eighteen_unique_ordered_stages():
    stages = [
        "Pregunta, tratamiento, resultado y unidad de asignación",
        "Inspección y preparación de los datos",
        "Semilla y asignación aleatoria",
        "Aleatorización simple y estratificada",
        "Tabla de balance",
        "Prueba conjunta de balance",
        "RCT simple sin controles",
        "RCT simple con controles",
        "RCT estratificado sin controles adicionales",
        "RCT estratificado con controles adicionales",
        "Comparación de las cuatro especificaciones",
        "Inferencia y unidad de asignación",
        "Cuándo incluir controles",
        "Heterogeneidad mediante interacciones",
        "Efecto base y CATE",
        "Centrado de covariables",
        "Replicación en Python y Colab",
        "Concordancia Stata–Python",
    ]
    h3 = [re.sub(r"\s*\{-\}\s*$", "", item).strip() for item in re.findall(r"^###\s+(.+)$", PRACTICE, re.MULTILINE)]
    selected = [heading for heading in h3 if heading in stages]
    assert selected == stages
    assert all(h3.count(stage) == 1 for stage in stages)


def test_practice_has_required_learning_blocks():
    assert len(_boxes(PRACTICE)) >= 12
    for label in ["Comando clave", "Salida central", "Interpretación", "Error frecuente", "Resultado clave"]:
        assert label in PRACTICE


def test_practice_has_exact_exam_questions_without_answers():
    for block in _questions(PRACTICE, "RCT-S", ["RCT-S1", "RCT-S2", "RCT-S3", "RCT-S4"]):
        lowered = block.casefold()
        for label in ["puntaje sugerido", "comandos permitidos", "producto esperado"]:
            assert lowered.count(label) == 1
        assert not _has_answer_marker(block)


def test_theory_headings_delegate_numbering_to_bookdown():
    _assert_no_manual_numbering(THEORY)


def test_practice_headings_delegate_numbering_to_bookdown():
    _assert_no_manual_numbering(PRACTICE)


def test_tracked_student_material_omits_private_identifiers():
    fragments = [
        "clave" + "_rct",
        "clave" + "_experimentos_aleatorizados",
        "claves" + "_docentes",
    ]
    supplied = os.environ.get("RCT_PRIVATE_IDENTIFIERS", "")
    forbidden = fragments + [item for item in supplied.split(os.pathsep) if item]
    candidates = [ROOT / "_bookdown.yml", *ROOT.glob("*.Rmd")]
    docs = ROOT / "docs"
    if docs.exists():
        candidates.extend(path for path in docs.rglob("*") if path.is_file())
    hits = []
    for path in candidates:
        text = path.read_text(encoding="utf-8", errors="ignore").casefold()
        for token in forbidden:
            if token.casefold() in text:
                hits.append((str(path.relative_to(ROOT)), token))
    assert not hits, f"Identificadores privados rastreables: {hits}"
