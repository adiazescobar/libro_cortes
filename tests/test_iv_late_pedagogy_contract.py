from pathlib import Path
import re


ROOT = Path(__file__).resolve().parents[1]
THEORY_PATH = ROOT / "18-IV.Rmd"
EMPIRICAL_PATH = ROOT / "19-IVStata.Rmd"
THEORY = THEORY_PATH.read_text(encoding="utf-8")
EMPIRICAL = EMPIRICAL_PATH.read_text(encoding="utf-8")
COMBINED = THEORY + "\n" + EMPIRICAL


def test_titles_and_anchors_are_uniform():
    assert THEORY.startswith(
        "# Variables instrumentales y LATE — Clase teórica {#iv}"
    )
    assert EMPIRICAL.startswith(
        "# Variables instrumentales y LATE — Clase empírica {#iv-stata}"
    )


def test_empirical_materials_come_before_readings_and_goals():
    download = EMPIRICAL.index("Descargar do-file")
    readings = EMPIRICAL.index("Lecturas centrales")
    goals = EMPIRICAL.index("Metas de aprendizaje")
    assert download < readings < goals


def test_exam_question_counts_and_no_public_answers():
    assert len(re.findall(r"boxexam", THEORY)) == 3
    assert len(re.findall(r"boxexam", EMPIRICAL)) == 4
    forbidden = ("<details", "Respuesta:", "Solución:", "Mostrar respuesta")
    assert not any(token in COMBINED for token in forbidden)


def test_late_and_complier_content_is_explicit():
    required = (
        "always-takers",
        "never-takers",
        "compliers",
        "defiers",
        "estat compliers",
        "pesos de Abadie",
        "no podemos identificar",
        "Y(D=1)",
        "Y(D=0)",
    )
    assert all(term in COMBINED for term in required)


def test_weak_iv_content_is_qualified():
    required = (
        "104.7",
        "Anderson–Rubin",
        "CLR",
        "Kleibergen–Paap",
        "Stock–Yogo",
    )
    assert all(term in COMBINED for term in required)
    forbidden = (
        "F > 10 garantiza",
        "F > 104.7 garantiza",
        "la validez se comprueba con",
        "Hansen demuestra que",
    )
    assert not any(term in COMBINED for term in forbidden)


def test_simulated_data_and_output_provenance_are_visible():
    assert EMPIRICAL.lower().count("datos ficticios") >= 2
    assert "results/paces_estimators.csv" in EMPIRICAL
    assert "results/divorce_iv_estimators.csv" in EMPIRICAL
    assert "figures/weak_iv_distributions.png" in EMPIRICAL


def test_private_key_is_not_linked_or_tracked_in_chapters():
    assert "claves_privadas" not in COMBINED
    assert "18_IV_LATE_clave" not in COMBINED


def test_headings_do_not_contain_manual_section_numbers():
    heading = re.compile(r"^#{2,4}\s+\d+(?:\.\d+)*[.)]?\s", re.MULTILINE)
    assert not heading.search(THEORY)
    assert not heading.search(EMPIRICAL)
