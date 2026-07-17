import csv
import re
from pathlib import Path

import test_power_pedagogy_contract as base


ROOT = base.ROOT
THEORY = ROOT / "09-BadControls.Rmd"
PRACTICE = ROOT / "10-BadControlsStata.Rmd"
DOFILE = ROOT / "dofile/10_BadControls/10_stata.do"
RESULTS = ROOT / "dofile/10_BadControls/results/bad_controls_estimates.csv"
MONTECARLO = ROOT / "dofile/10_BadControls/results/bad_controls_montecarlo.csv"
PRIVATE_KEY = ROOT / "claves_privadas/10_BadControls_clave.md"


def test_titles_anchors_and_book_order_are_stable():
    theory = base._read(THEORY)
    practice = base._read(PRACTICE)
    assert theory.startswith("# Malos controles — Clase teórica {#bad-controls-teoria}")
    assert practice.startswith("# Malos controles — Clase empírica {#bad-controls-stata}")
    files = base.parse_rmd_files(base.BOOKDOWN)
    assert files.index(THEORY.name) + 1 == files.index(PRACTICE.name)


def test_practice_puts_materials_first_and_theory_has_no_downloads():
    theory = base._read(THEORY)
    practice = base._read(PRACTICE)
    assert "class-materials" not in theory
    headings = base._headings(practice, 2)
    assert headings and headings[0] == "Materiales para la clase"
    assert "class-materials" in practice


def test_theory_uses_course_notation_and_precise_identification_language():
    text = base._read(THEORY)
    for marker in [
        "Y_i(D=1)",
        "Y_i(D=0)",
        "ATT",
        "estimando",
        "amplificación de sesgo",
        "control neutral",
        r"D \rightarrow C \leftarrow U \rightarrow Y",
        "puede ser positivo o negativo",
    ]:
        assert marker in text
    assert "En DID, el estimador busca identificar justamente esa diferencia" not in text
    assert "Un **buen control** es toda característica observable que **no cambia" not in text


def test_theory_has_exactly_three_closed_exam_questions():
    text = base._read(THEORY)
    blocks = base._question_boxes(text, "BC-T", ["BC-T1", "BC-T2", "BC-T3"])
    for block in blocks:
        for label in ["Puntaje sugerido", "Producto esperado"]:
            base._metadata_once(block, label)
        base._assert_closed_question_structure(
            block, ["Puntaje sugerido", "Producto esperado"]
        )


def test_practice_has_three_cases_and_exactly_four_closed_exam_questions():
    text = base._read(PRACTICE)
    headings = base._headings(text, 2) + base._headings(text, 3)
    for heading in [
        "Caso del mediador",
        "Caso del colisionador",
        "Caso del proxy postratamiento contaminado",
        "Aplicación a diferencias en diferencias",
        "Replicación en R y Python",
    ]:
        assert heading in headings
    blocks = base._question_boxes(text, "BC-S", ["BC-S1", "BC-S2", "BC-S3", "BC-S4"])
    for block in blocks:
        for label in ["Puntaje sugerido", "Comandos permitidos", "Producto esperado"]:
            base._metadata_once(block, label)
        base._assert_closed_question_structure(
            block, ["Puntaje sugerido", "Comandos permitidos", "Producto esperado"]
        )


def test_public_questions_do_not_disclose_answers():
    for path, family, expected in [
        (THEORY, "BC-T", ["BC-T1", "BC-T2", "BC-T3"]),
        (PRACTICE, "BC-S", ["BC-S1", "BC-S2", "BC-S3", "BC-S4"]),
    ]:
        for block in base._question_boxes(base._read(path), family, expected):
            assert not base._has_disclosed_answer(block)


def test_bookdown_controls_heading_numbering():
    base._assert_no_manual_numbering(base._read(THEORY))
    base._assert_no_manual_numbering(base._read(PRACTICE))


def test_practice_reads_canonical_stata_results():
    text = base._read(PRACTICE)
    assert "dofile/10_BadControls/results/bad_controls_estimates.csv" in text
    assert "dofile/10_BadControls/results/bad_controls_montecarlo.csv" in text
    assert re.search(r"(?:read\.csv|read_csv)\(", text)


def test_canonical_estimates_have_required_schema_and_unique_rows():
    assert RESULTS.is_file()
    with RESULTS.open(encoding="utf-8", newline="") as handle:
        rows = list(csv.DictReader(handle))
    required = {"case", "specification", "estimand", "estimate", "se", "truth"}
    assert rows and required <= set(rows[0])
    keys = [(row["case"], row["specification"]) for row in rows]
    assert len(keys) == len(set(keys))
    assert {"mediator", "collider", "contaminated_proxy"} <= {
        row["case"] for row in rows
    }
    for row in rows:
        float(row["estimate"])
        float(row["se"])
        float(row["truth"])


def test_montecarlo_results_have_all_cases_and_finite_means():
    assert MONTECARLO.is_file()
    with MONTECARLO.open(encoding="utf-8", newline="") as handle:
        rows = list(csv.DictReader(handle))
    assert rows
    assert {"case", "specification", "mean_estimate", "truth", "repetitions"} <= set(
        rows[0]
    )
    assert {row["case"] for row in rows} == {
        "mediator",
        "collider",
        "contaminated_proxy",
    }
    for row in rows:
        float(row["mean_estimate"])
        assert int(float(row["repetitions"])) >= 100


def test_stata_dgp_uses_correct_collider_and_proxy_structures():
    text = base._read(DOFILE)
    assert "D -> C <- U -> Y" in text
    assert re.search(r"gen double D\s*=\s*rnormal\(\)", text)
    assert re.search(r"gen double U\s*=\s*rnormal\(\)", text)
    assert re.search(r"gen double L\s*=\s*[^\\n]*D[^\\n]*U", text)
    assert "postratamiento" in text.casefold()


def test_private_key_exists_but_is_not_part_of_book():
    assert PRIVATE_KEY.is_file()
    key = base._read(PRIVATE_KEY)
    for code in ["BC-T1", "BC-T2", "BC-T3", "BC-S1", "BC-S2", "BC-S3", "BC-S4"]:
        assert code in key
    book_files = base.parse_rmd_files(base.BOOKDOWN)
    assert all("clave" not in path.casefold() for path in book_files)
