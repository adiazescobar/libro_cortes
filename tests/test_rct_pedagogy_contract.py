import os
import re
import subprocess
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[1]
THEORY = (ROOT / "05-RCT.Rmd").read_text(encoding="utf-8")
PRACTICE = (ROOT / "06-RCT2.Rmd").read_text(encoding="utf-8")

DECOMPOSITION_AND_SIMULATION_FRAGMENTS = [
    r"\underbrace{\mathbb{E}[Y_i(D=1) \mid D_i=1] - \mathbb{E}[Y_i(D=0) \mid D_i=1]}_{ATT}",
    r"\underbrace{\mathbb{E}[Y_i(D=0) \mid D_i=1] - \mathbb{E}[Y_i(D=0) \mid D_i=0]}_{\text{sesgo de selección}}",
    "# --- Escenario 1: Auto-selección (los motivados eligen tratarse) ---",
    "D_sesgo <- as.numeric(motivacion + rnorm(N, sd = 0.5) > 0)",
    "Y_sesgo <- D_sesgo * Y1 + (1 - D_sesgo) * Y0",
    "# --- Escenario 2: Aleatorización ---",
    "D_azar <- rbinom(N, 1, 0.5)",
    "Y_azar <- D_azar * Y1 + (1 - D_azar) * Y0",
    'Componente = rep(c("Diferencia observada", "ATT (efecto causal)", "Sesgo de selección"), 2)',
]


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
        r"(?:\*\*|__)?(?:respuesta|solución|pista)(?:\*\*|__)?\s*"
        r"(?::|\.|=|correcta\b|es\b)|\bla\s+respuesta\s+es\b|"
        r"\bsolución\s+correcta\b",
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


def _assert_metadata_line(block, label):
    pattern = re.compile(
        rf"(?im)^\s*(?:[-*+]\s+)?(?:\*\*|__)?{re.escape(label)}"
        rf"(?:\*\*|__)?\s*:"
    )
    assert len(pattern.findall(block)) == 1, f"Se exige una línea estructural {label}:"


def _assert_no_manual_numbering(text):
    forbidden = re.compile(
        r"^#{2,4}\s+(?:(?:PASO|Paso|Etapa)\b\s*\d*|\d+\s*[.):_-])",
        re.MULTILINE,
    )
    assert not forbidden.search(text), "Bookdown, no el Rmd, debe numerar encabezados"


def _assert_global_potential_outcomes_notation(text):
    compact = re.sub(r"(?:\\,|\s)+", "", text)
    assert "Y_i(D=1)" in compact and "Y_i(D=0)" in compact
    incompatible = [
        r"(?<![A-Za-z_])Y\(D=[01]\)",
        r"Y_i\([01]\)",
        r"Y_i\^[{]?[01][}]?",
    ]
    assert not any(re.search(pattern, compact) for pattern in incompatible)


def _private_exposure_counts(tracked_paths, content_by_path, forbidden):
    names = 0
    contents = 0
    lowered_tokens = [token.casefold() for token in forbidden if token]
    for path in tracked_paths:
        lowered_path = path.casefold()
        names += any(token in lowered_path for token in lowered_tokens)
    for text in content_by_path.values():
        lowered_text = text.casefold()
        contents += any(token in lowered_text for token in lowered_tokens)
    return names, contents


def _assert_stage_headings(h3, stages, permitted_non_stages):
    classified = set(stages) | set(permitted_non_stages)
    unexpected = [heading for heading in h3 if heading not in classified]
    assert not unexpected, f"H3 no clasificados como etapa o excepción: {unexpected}"
    stage_headings = [heading for heading in h3 if heading not in permitted_non_stages]
    assert stage_headings == stages


def _assert_decomposition_and_both_simulations(text):
    compact = re.sub(r"[ \t]+", " ", text)
    missing = [
        fragment
        for fragment in DECOMPOSITION_AND_SIMULATION_FRAGMENTS
        if fragment not in compact
    ]
    assert not missing, (
        "Debe preservarse la ecuación de descomposición y el código de "
        f"ambas simulaciones; faltan {len(missing)} fragmentos distintivos"
    )


def test_theory_preserves_foundational_content_and_resources():
    required = [
        "Resultados potenciales:",
        "Resumen de los cuatro escenarios:",
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


def test_theory_preserves_decomposition_and_both_simulations():
    _assert_decomposition_and_both_simulations(THEORY)


def test_decomposition_and_simulation_contract_rejects_each_deleted_object():
    for fragment in DECOMPOSITION_AND_SIMULATION_FRAGMENTS:
        mutated = re.sub(r"[ \t]+", " ", THEORY).replace(fragment, "", 1)
        with pytest.raises(AssertionError):
            _assert_decomposition_and_both_simulations(mutated)


def test_both_chapters_use_only_global_potential_outcomes_notation():
    _assert_global_potential_outcomes_notation(THEORY)
    _assert_global_potential_outcomes_notation(PRACTICE)


def test_potential_outcomes_notation_rejects_real_incompatible_variants():
    compatible = r"$Y_i(D=1)$ y $Y_i(D=0)$"
    _assert_global_potential_outcomes_notation(compatible)
    for rival in [
        r"$Y_i(D=1)$, $Y_i(D=0)$ y $Y(D=1)$",
        r"$Y_i(D=1)$, $Y_i(D=0)$ y $Y_i(1)$",
        r"$Y_i(D=1)$, $Y_i(D=0)$ y $Y_i^{0}$",
    ]:
        try:
            _assert_global_potential_outcomes_notation(rival)
        except AssertionError:
            continue
        raise AssertionError("Una variante incompatible no fue rechazada")


def test_theory_has_required_learning_blocks():
    boxes = _boxes(THEORY)
    assert len(boxes) >= 8
    for label in ["Intuición", "Resultado clave", "Demostración", "Advertencia", "Comparación"]:
        assert any(label in box for box in boxes), f"Falta un bloque etiquetado {label}"


def test_theory_has_exact_exam_questions_without_answers():
    for block in _questions(THEORY, "RCT-T", ["RCT-T1", "RCT-T2", "RCT-T3"]):
        _assert_metadata_line(block, "Puntaje sugerido")
        _assert_metadata_line(block, "Producto esperado")
        assert not _has_answer_marker(block)


def test_answer_detector_rejects_predictable_disclosures():
    assert not _has_answer_marker("Justifique su respuesta y proponga una solución.")
    for disclosure in [
        "Respuesta. El estimador es 4.",
        "Solución correcta = usar controles.",
        "La respuesta es el ATE.",
        "Pista = centre la covariable.",
        "Ver respuesta",
    ]:
        assert _has_answer_marker(disclosure)


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
    permitted_non_stages = {
        "Lecturas",
        "Asignación a nivel individual (dos muestras independientes)",
        "Pruebas",
        "El truco de centrar (Wooldridge)",
    }
    h3 = [
        re.sub(r"\s*\{-\}\s*$", "", item).strip()
        for item in re.findall(r"^###\s+(.+)$", PRACTICE, re.MULTILINE)
    ]
    _assert_stage_headings(h3, stages, permitted_non_stages)


def test_stage_structure_rejects_an_interleaved_extra_h3():
    stages = ["Etapa alfa", "Etapa beta"]
    _assert_stage_headings(["Lecturas", *stages], stages, {"Lecturas"})
    try:
        _assert_stage_headings(
            ["Etapa alfa", "Encabezado inesperado", "Etapa beta"], stages, {"Lecturas"}
        )
    except AssertionError:
        return
    raise AssertionError("Un H3 extra intercalado no fue rechazado")


def test_practice_has_required_learning_blocks():
    boxes = _boxes(PRACTICE)
    assert len(boxes) >= 12
    for label in ["Comando clave", "Salida central", "Interpretación", "Error frecuente", "Resultado clave"]:
        assert any(label in box for box in boxes), f"Falta un bloque etiquetado {label}"


def test_practice_has_exact_exam_questions_without_answers():
    for block in _questions(PRACTICE, "RCT-S", ["RCT-S1", "RCT-S2", "RCT-S3", "RCT-S4"]):
        for label in ["Puntaje sugerido", "Comandos permitidos", "Producto esperado"]:
            _assert_metadata_line(block, label)
        assert not _has_answer_marker(block)


def test_practice_exam_questions_are_self_contained_cases():
    blocks = _questions(PRACTICE, "RCT-S", ["RCT-S1", "RCT-S2", "RCT-S3", "RCT-S4"])
    required_by_question = [
        ["student_id", "20250813", "p=0.50", "semestre", "Formule"],
        ["4.312", "0.476", "4.390", "0.429", "Compare", "Justifique"],
        ["240 estudiantes", "24 escuelas", "asistencia posterior", "Decida", "Explique"],
        ["mujer=1", "libros=4", "4.2956", "4.2100", "Diagnostique", "Calcule"],
    ]
    for block, required in zip(blocks, required_by_question):
        assert all(fragment in block for fragment in required), required
        assert "Producto esperado" in block


def test_practice_stata_examples_are_sequential_and_interactions_are_coherent():
    assert "sort id" not in PRACTICE
    for variable in ["D_simple50", "D_simple30", "D_exact40", "D_estrato50"]:
        assert variable in PRACTICE
    assert "reg y D##" not in PRACTICE
    assert "reg y c.D##" not in PRACTICE
    assert "i.D##i.mujer" in PRACTICE
    assert "i.D##c.libros" in PRACTICE
    assert "margins D, at(" not in PRACTICE
    assert "niveles predichos" in PRACTICE and "dydx(D)" in PRACTICE


def test_practice_qualifies_ate_weighting_centering_and_confidence_intervals():
    forbidden = [
        "En los cuatro casos, \\(\\hat{\\tau}\\) estima el **ATE** de forma consistente. Lo que cambia es la **precisión**.",
        "ahora \\(\\tau = ATE\\) directamente",
        "Ahora el coeficiente de D es directamente el ATE",
        '"Con centrado: coef(D) = ATE promedio."',
        "modelos del PASO 4",
    ]
    assert not any(fragment in PRACTICE for fragment in forbidden)
    for required in [
        "probabilidades de tratamiento iguales entre estratos",
        "ponderación por la población objetivo",
        "efecto ajustado en la media",
        "promedio de los CATE predichos",
        "qt(0.975",
        "grados de libertad residuales",
        "Ejercicios adicionales",
    ]:
        assert required in PRACTICE


def test_practice_exam_command_allowlists_support_required_products():
    blocks = _questions(PRACTICE, "RCT-S", ["RCT-S1", "RCT-S2", "RCT-S3", "RCT-S4"])
    for command in ["sort", "generate", "isid", "tabulate"]:
        assert f"`{command}" in blocks[0]
    assert "`display invttail(" in blocks[1]


def test_confidence_interval_df_are_mapped_by_model_name():
    assert "df_by_model <- c(" in PRACTICE
    for mapping in [
        "m1_simple = 68",
        "m2_controles = 63",
        "m3_estratos = 65",
        "m4_completo = 60",
    ]:
        assert mapping in PRACTICE
    assert "df_residual = unname(df_by_model[modelo])" in PRACTICE
    assert "df_residual = c(68, 63, 65, 60)" not in PRACTICE


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
    tracked_paths = subprocess.run(
        ["git", "ls-files", "-z"],
        cwd=ROOT,
        check=True,
        capture_output=True,
    ).stdout.decode("utf-8").split("\0")
    tracked_paths = [path for path in tracked_paths if path]
    candidates = [ROOT / "_bookdown.yml", *ROOT.glob("*.Rmd")]
    docs = ROOT / "docs"
    if docs.exists():
        candidates.extend(path for path in docs.rglob("*") if path.is_file())
    content_by_path = {}
    for path in candidates:
        content_by_path[str(path.relative_to(ROOT))] = path.read_text(
            encoding="utf-8", errors="ignore"
        )
    name_hits, content_hits = _private_exposure_counts(
        tracked_paths, content_by_path, forbidden
    )
    assert name_hits == 0 and content_hits == 0, (
        "Hay identificadores privados rastreables; "
        f"coincidencias en rutas={name_hits}, contenidos={content_hits}"
    )


def test_private_audit_detects_name_path_and_content_without_echoing_token():
    token = "docente" + "-solo-interno"
    names, contents = _private_exposure_counts(
        [f"docs/{token}/index.html", "05-RCT.Rmd"],
        {"05-RCT.Rmd": f"ruta: ../{token}.md"},
        [token],
    )
    assert (names, contents) == (1, 1)
