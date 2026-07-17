import json
import re
import subprocess

import pytest

import test_power_pedagogy_contract as base

ROOT = base.ROOT
THEORY_PATH = ROOT / "08-DID.Rmd"
PRACTICE_PATH = ROOT / "08-DIDStata.Rmd"
BOOKDOWN = base.BOOKDOWN
CHAPTER_SNAPSHOT = ROOT / "tests/fixtures/did_chapter_baseline.json"

# Fragmentos distintivos que la práctica debe conservar tras la división.
PRACTICE_REQUIRED = [
    "diff y, t(D) p(t)",
    "test cov(orden_n)",
    "xtdidregress",
    "estat ptrends",
    "estat granger",
    "estat trendplots",
    "reg y D##t, robust",
    "reg D.y D",
    "base3.dta",
    "hospdd",
    "08_DID_ejercicio.do",
]

# Fragmentos distintivos que la teoría debe conservar o incorporar.
THEORY_REQUIRED = [
    "Experimentos naturales",
    "Contrafactuales falsos",
    "John Snow",
    "Mariel",
    "Ashenfelter",
    "tendencias paralelas",
    "caso histórico de clase",
]

# Secuencia de aula de Diferencias_en_Diferencias.pdf (22 diapositivas).
# El primer marcador de cada bloque fija el orden; los demás deben aparecer
# dentro del bloque correspondiente.
DID_AULA_SEQUENCE = [
    ("Experimentos naturales", ["Experimentos naturales", "evento fortuito"]),
    ("Contrafactuales falsos", ["Contrafactuales falsos", "selección"]),
    ("John Snow", ["John Snow", "Lambeth", "cólera"]),
    ("Mariel", ["Mariel", "Card"]),
    ("Ejemplo de aula", ["caso histórico de clase"]),
    ("Resultados potenciales y ATT", ["Resultados potenciales y el ATT"]),
    ("Tendencias paralelas", ["El supuesto de tendencias paralelas"]),
    ("Regresión DiD", ["La regresión DiD"]),
    ("Pruebas formales", ["estat ptrends", "estat granger"]),
    ("Amenazas", ["Políticas simultáneas"]),
    ("Síntesis", ["Síntesis"]),
]

PRACTICE_STAGE_SEQUENCE = [
    "Estimando y estructura de los datos",
    "Preparación de la base",
    "La tabla 2×2 con medias observadas",
    "Gráfico de evolución temporal",
    "Comparación de medias por periodo",
    "Estimador DiD paso a paso",
    "Estimación con el comando diff",
    "Balance en el periodo base",
    "Estimación por regresión",
    "Primeras diferencias en panel",
    "DiD con múltiples periodos",
    "Prueba de tendencias paralelas",
    "Prueba de anticipación",
    "Checklist de amenazas para el diseño",
    "Ejercicio aplicado",
    "Replicación en R y Python",
]


def _union_text():
    theory = base._read(THEORY_PATH)
    practice = PRACTICE_PATH.read_text(encoding="utf-8") if PRACTICE_PATH.is_file() else ""
    return theory + "\n" + practice


def _assert_aula_sequence(text):
    positions = []
    for label, markers in DID_AULA_SEQUENCE:
        match = re.search(re.escape(markers[0]), text)
        assert match, f"Falta marcador principal de la clase en {label}"
        positions.append(match.start())
    assert positions == sorted(positions), "La teoría debe conservar el orden de aula"
    for index, (label, markers) in enumerate(DID_AULA_SEQUENCE):
        end = positions[index + 1] if index + 1 < len(DID_AULA_SEQUENCE) else len(text)
        block = text[positions[index]:end]
        for marker in markers[1:]:
            assert re.search(re.escape(marker), block, re.IGNORECASE), (
                f"Falta marcador de la clase en {label}"
            )


def _assert_stage_sequence(headings):
    selected = [heading for heading in headings if heading in PRACTICE_STAGE_SEQUENCE]
    assert selected == PRACTICE_STAGE_SEQUENCE


# ---------------------------------------------------------------- preservación


def test_every_current_did_unit_survives_in_the_union_of_both_chapters():
    snapshot = json.loads(base._read(CHAPTER_SNAPSHOT))
    base._assert_complete_snapshot_preserved(_union_text(), snapshot)


def test_did_practice_keeps_distinctive_commands_and_materials():
    practice = base._read(PRACTICE_PATH)
    missing = [fragment for fragment in PRACTICE_REQUIRED if fragment not in practice]
    assert not missing, f"Faltan fragmentos distintivos en la práctica: {missing}"


def test_did_theory_keeps_and_gains_its_distinctive_content():
    theory = base._read(THEORY_PATH)
    missing = [fragment for fragment in THEORY_REQUIRED if fragment not in theory]
    assert not missing, f"Faltan fragmentos distintivos en la teoría: {missing}"


# --------------------------------------------------------------- arquitectura


def test_did_theory_precedes_practice_in_bookdown():
    files = base.parse_rmd_files(BOOKDOWN)
    assert files.index("08-DID.Rmd") + 1 == files.index("08-DIDStata.Rmd")
    assert files.index("08-DIDStata.Rmd") < files.index("09-BadControls.Rmd")


def test_did_chapters_have_unique_titles_and_expected_anchors():
    theory = base._read(THEORY_PATH)
    practice = base._read(PRACTICE_PATH)
    title_pattern = re.compile(r"^#\s+(.+?)(?:\s+\{#([^} ]+)[^}]*\})?\s*$", re.MULTILINE)
    theory_title = title_pattern.search(theory)
    practice_title = title_pattern.search(practice)
    assert theory_title and practice_title
    assert theory_title.group(1) != practice_title.group(1)
    assert theory_title.group(2) == "did-teoria"
    assert practice_title.group(2) == "did-stata"


def test_did_theory_has_no_download_blocks_and_practice_puts_materials_first():
    theory = base._read(THEORY_PATH)
    assert "class-materials" not in theory
    assert "](dofile/" not in theory
    practice = base._read(PRACTICE_PATH)
    h2 = base._headings(practice, 2)
    assert h2 and h2[0] == "Materiales para la clase"
    assert "class-materials" in practice
    tail = practice[-4000:]
    assert "class-materials" not in tail


# ----------------------------------------------------------------- pedagogía


def test_did_theory_follows_the_classroom_slide_sequence():
    _assert_aula_sequence(base._read(THEORY_PATH))


def test_did_aula_contract_rejects_missing_and_reordered_markers():
    canonical = "\n".join(" | ".join(markers) for _label, markers in DID_AULA_SEQUENCE)
    _assert_aula_sequence(canonical)
    with pytest.raises(AssertionError):
        _assert_aula_sequence(canonical.replace("Lambeth", "", 1))
    reordered = "\n".join(
        " | ".join(markers)
        for _label, markers in [DID_AULA_SEQUENCE[1], DID_AULA_SEQUENCE[0], *DID_AULA_SEQUENCE[2:]]
    )
    with pytest.raises(AssertionError):
        _assert_aula_sequence(reordered)


def test_did_theory_has_blocks_and_exactly_three_questions():
    theory = base._read(THEORY_PATH)
    assert len(base._boxes(theory)) >= 8
    blocks = base._question_boxes(theory, "DID-T", ["DID-T1", "DID-T2", "DID-T3"])
    for block in blocks:
        base._metadata_once(block, "Puntaje sugerido")
        base._metadata_once(block, "Producto esperado")
        base._assert_closed_question_structure(block, ["Puntaje sugerido", "Producto esperado"])


def test_did_practice_has_the_concrete_semantic_stage_sequence():
    headings = base._headings(base._read(PRACTICE_PATH), 3)
    assert 14 <= len(headings) <= 18
    _assert_stage_sequence(headings)


def test_did_stage_contract_rejects_generic_missing_and_reordered_stages():
    _assert_stage_sequence(PRACTICE_STAGE_SEQUENCE)
    for mutated in [
        ["Parte"] * len(PRACTICE_STAGE_SEQUENCE),
        PRACTICE_STAGE_SEQUENCE[:-1],
        [PRACTICE_STAGE_SEQUENCE[1], PRACTICE_STAGE_SEQUENCE[0], *PRACTICE_STAGE_SEQUENCE[2:]],
    ]:
        with pytest.raises(AssertionError):
            _assert_stage_sequence(mutated)


def test_did_practice_has_at_least_twelve_learning_blocks():
    assert len(base._boxes(base._read(PRACTICE_PATH))) >= 12


def test_did_practice_has_exactly_four_self_contained_questions():
    practice = base._read(PRACTICE_PATH)
    blocks = base._question_boxes(
        practice, "DID-S", ["DID-S1", "DID-S2", "DID-S3", "DID-S4"]
    )
    for block in blocks:
        for label in ["Puntaje sugerido", "Comandos permitidos", "Producto esperado"]:
            base._metadata_once(block, label)
        base._assert_closed_question_structure(
            block, ["Puntaje sugerido", "Comandos permitidos", "Producto esperado"]
        )


def test_did_questions_do_not_disclose_answers():
    for path, family, expected in [
        (THEORY_PATH, "DID-T", ["DID-T1", "DID-T2", "DID-T3"]),
        (PRACTICE_PATH, "DID-S", ["DID-S1", "DID-S2", "DID-S3", "DID-S4"]),
    ]:
        text = base._read(path)
        for block in base._question_boxes(text, family, expected):
            assert not base._has_disclosed_answer(block)


# --------------------------------------------------------- numeración y datos


def test_did_headings_delegate_numbering_to_bookdown():
    base._assert_no_manual_numbering(base._read(THEORY_PATH))
    base._assert_no_manual_numbering(base._read(PRACTICE_PATH))


def test_did_practice_reads_canonical_results():
    practice = base._read(PRACTICE_PATH)
    assert re.search(
        r"(?:read\.csv|read_csv|read_required_csv)\(\s*[\"']"
        r"dofile/08_DID/results/",
        practice,
    )


def test_did_classroom_example_is_labeled_historical_not_canonical():
    theory = base._read(THEORY_PATH)
    example = re.search(r"caso histórico de clase", theory)
    assert example, "El ejemplo de aula debe rotularse como caso histórico de clase"


def _did_panel_violations(combined):
    lowered = combined.casefold()
    violations = []
    if "cortes transversales repetidos" not in lowered:
        violations.append("falta identificar base3.dta como cortes transversales repetidos")

    forbidden = {
        "id ficticio": r"\bid\s+ficticio\b",
        "gen id": r"\bgen\s+id\b",
        "gen id_pd": r"\bgen\s+id_pd\b",
        "xtset id": r"\bxtset\s+id\b",
        "reg D.y D": r"\breg\s+D\.y\s+D\b",
    }
    found = [
        label for label, pattern in forbidden.items()
        if re.search(pattern, combined, re.IGNORECASE)
    ]
    if found:
        violations.append(f"la práctica o el do-file todavía construyen panel ficticio: {found}")

    genuine_panel_result = re.search(
        r"resultado.{0,240}panel genuino|panel genuino.{0,240}resultado",
        lowered,
        re.DOTALL,
    )
    if not genuine_panel_result:
        violations.append(
            "la equivalencia en primeras diferencias debe rotularse como resultado "
            "para un panel genuino"
        )

    positive_base3_first_differences = re.search(
        r"\b(?:estim(?:amos|ar|aremos)|aplic(?:amos|ar)|calcul(?:amos|ar)|"
        r"ejecut(?:amos|ar))\s+(?:las\s+)?primeras diferencias\s+"
        r"(?:con|sobre|usando|en)\s+`?base3\.dta`?",
        combined,
        re.IGNORECASE,
    )
    if positive_base3_first_differences:
        violations.append(
            "base3.dta no puede presentarse como base para estimar primeras diferencias"
        )

    return violations


def test_did_repeated_cross_sections_do_not_invent_a_panel():
    practice = base._read(PRACTICE_PATH)
    dofile = base._read(ROOT / "dofile/08_DID/08_DID.do")
    violations = _did_panel_violations(practice + "\n" + dofile)
    assert not violations, "\n".join(violations)


@pytest.mark.parametrize(
    "forbidden",
    ["id ficticio", "gen id", "gen id_pd", "xtset id", "reg D.y D"],
)
def test_did_panel_contract_rejects_each_fictitious_panel_form(forbidden):
    canonical = (
        "base3.dta contiene cortes transversales repetidos.\n"
        "La equivalencia en primeras diferencias es un resultado para un panel genuino.\n"
    )
    assert _did_panel_violations(canonical) == []
    assert _did_panel_violations(canonical + forbidden)


def test_did_panel_contract_rejects_positive_base3_first_difference_claim():
    mutated = (
        "base3.dta contiene cortes transversales repetidos.\n"
        "La equivalencia es un resultado para un panel genuino.\n"
        "Estimamos primeras diferencias usando base3.dta.\n"
    )
    assert _did_panel_violations(mutated)


def test_did_panel_contract_allows_negation_and_genuine_panel_explanation_together():
    allowed = """
## Estructura de los datos
base3.dta contiene cortes transversales repetidos. No se estiman primeras diferencias
con base3.dta. La equivalencia en primeras diferencias se presenta como resultado para
un panel genuino.
"""
    assert _did_panel_violations(allowed) == []


# ------------------------------------------------------------------ privacidad


def test_did_student_material_omits_private_identifiers_without_echoing_them():
    tokens = ["".join(("cla", "ve_did")), "".join(("cla", "ve_dif"))]
    tracked = subprocess.run(
        ["git", "ls-files", "-z"], cwd=ROOT, check=True, capture_output=True
    ).stdout.decode("utf-8").split("\0")
    candidates = [BOOKDOWN, *ROOT.glob("*.Rmd")]
    docs = ROOT / "docs"
    if docs.exists():
        candidates.extend(path for path in docs.rglob("*") if path.is_file())
    contents = {
        str(path.relative_to(ROOT)): path.read_text(encoding="utf-8", errors="ignore")
        for path in candidates
        if path.is_file()
    }
    base._assert_no_private_exposure([path for path in tracked if path], contents, tokens)
