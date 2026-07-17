import csv
import json
import re

import test_power_pedagogy_contract as base


ROOT = base.ROOT
THEORY = ROOT / "11-TWFE.Rmd"
PRACTICE = ROOT / "11-TWFEStata.Rmd"
DOFILE = ROOT / "dofile/11_TWFE/11_stata.do"
SNAPSHOT = ROOT / "tests/fixtures/twfe_chapter_baseline.json"
PRIVATE_KEY = ROOT / "claves_privadas/11_TWFE_clave.md"
RESULT_DIR = ROOT / "dofile/11_TWFE/results"


def _union():
    practice = PRACTICE.read_text(encoding="utf-8") if PRACTICE.is_file() else ""
    return base._read(THEORY) + "\n" + practice


def test_baseline_units_survive_across_theory_and_practice():
    snapshot = json.loads(base._read(SNAPSHOT))
    combined = _union()
    for family, fragments in snapshot.items():
        missing = [fragment for fragment in fragments if fragment not in combined]
        assert not missing, f"Faltan unidades de {family}: {missing}"


def test_pair_is_consecutive_and_preserves_theory_url():
    theory = base._read(THEORY)
    practice = base._read(PRACTICE)
    assert theory.startswith(
        "# Datos de panel y TWFE — Clase teórica "
        "{#datos-de-panel-did-y-twfe-en-stata}"
    )
    assert practice.startswith(
        "# Datos de panel y TWFE — Clase empírica {#panel-twfe-stata}"
    )
    files = base.parse_rmd_files(base.BOOKDOWN)
    assert files.index(THEORY.name) + 1 == files.index(PRACTICE.name)


def test_materials_are_first_in_practice_and_absent_from_theory():
    theory = base._read(THEORY)
    practice = base._read(PRACTICE)
    assert "class-materials" not in theory
    assert base._headings(practice, 2)[0] == "Materiales para la clase"
    assert "class-materials" in practice


def test_theory_distinguishes_bacon_comparisons_from_negative_causal_weights():
    text = base._read(THEORY)
    for marker in [
        "Goodman-Bacon",
        "comparaciones 2×2",
        "de Chaisemartin",
        "pesos negativos",
        "efectos grupo-periodo",
        "twowayfeweights",
    ]:
        assert marker in text
    forbidden = [
        "Bacon muestra pesos negativos",
        "Goodman-Bacon muestra pesos negativos",
    ]
    assert not any(fragment in text for fragment in forbidden)


def test_theory_separates_algebra_from_causal_identification():
    text = base._read(THEORY)
    for marker in [
        "identidad algebraica",
        "tendencias paralelas",
        "consistencia",
        "no anticipación",
        "composición estable",
        "interferencia",
    ]:
        assert marker in text


def test_method_parameter_map_is_precise():
    text = _union()
    pairs = {
        "csdid": "ATT(g,t)",
        "eventstudyinteract": "interaction-weighted",
        "did_imputation": "imputación",
        "did_multiplegt_dyn": "status quo",
        "did2s": "segunda etapa",
    }
    for method, parameter in pairs.items():
        position = text.find(method)
        assert position >= 0
        assert parameter in text[max(0, position - 300):position + 500]
    assert "todos los métodos modernos estiman ATT(g,t)" not in text.casefold()


def test_event_study_and_trend_warnings_are_explicit():
    text = base._read(THEORY)
    for marker in [
        "event study TWFE tradicional",
        "pretrends aparentes",
        "no repara automáticamente",
        "puede cambiar el estimando",
    ]:
        assert marker in text


def test_exam_questions_are_exact_and_closed():
    for path, family, expected, labels in [
        (
            THEORY,
            "TWFE-T",
            ["TWFE-T1", "TWFE-T2", "TWFE-T3"],
            ["Puntaje sugerido", "Producto esperado"],
        ),
        (
            PRACTICE,
            "TWFE-S",
            ["TWFE-S1", "TWFE-S2", "TWFE-S3", "TWFE-S4"],
            ["Puntaje sugerido", "Comandos permitidos", "Producto esperado"],
        ),
    ]:
        text = base._read(path)
        for block in base._question_boxes(text, family, expected):
            for label in labels:
                base._metadata_once(block, label)
            base._assert_closed_question_structure(block, labels)


def test_bookdown_controls_numbering():
    base._assert_no_manual_numbering(base._read(THEORY))
    base._assert_no_manual_numbering(base._read(PRACTICE))


def test_stata_syntax_uses_canonical_identifier_and_commands():
    text = base._read(DOFILE)
    assert "cluster(i)" not in text
    assert "did_imputation Y i t" not in text
    assert "ssc install did_multiplegt," not in text
    assert "ssc install did_multiplegt_dyn" in text
    assert "twowayfeweights" in text
    assert "first_stage(i.id i.t)" in text
    assert "vce(cluster id)" in text or "cluster(id)" in text


def test_event_plot_only_consumes_created_matrix_pairs():
    text = base._read(DOFILE)
    references = re.findall(r"\b([A-Za-z][A-Za-z0-9_]*)#([A-Za-z][A-Za-z0-9_]*)", text)
    for b_name, v_name in references:
        assert re.search(rf"matrix\s+{re.escape(b_name)}\s*=", text)
        assert re.search(rf"matrix\s+{re.escape(v_name)}\s*=", text)


def test_practice_reads_all_canonical_result_files():
    text = base._read(PRACTICE)
    expected = [
        "panel_estimators.csv",
        "twfe_2x2.csv",
        "twfe_staggered.csv",
        "twfe_eventstudy.csv",
        "method_parameter_map.csv",
    ]
    for filename in expected:
        assert f"dofile/11_TWFE/results/{filename}" in text


def test_result_schemas_and_numeric_estimates():
    schemas = {
        "panel_estimators.csv": {
            "dgp", "method", "parameter", "estimate", "se", "truth"
        },
        "twfe_2x2.csv": {
            "dgp", "method", "parameter", "estimate", "se", "truth"
        },
        "twfe_staggered.csv": {
            "dgp", "method", "parameter", "comparison_sample", "estimate", "se"
        },
        "twfe_eventstudy.csv": {
            "dgp", "method", "parameter", "horizon", "estimate", "se"
        },
        "method_parameter_map.csv": {
            "method", "parameter", "comparison_sample", "horizon"
        },
    }
    for filename, required in schemas.items():
        path = RESULT_DIR / filename
        assert path.is_file(), f"Falta {filename}"
        with path.open(encoding="utf-8", newline="") as handle:
            rows = list(csv.DictReader(handle))
        assert rows and required <= set(rows[0])
        if "estimate" in required:
            for row in rows:
                float(row["estimate"])
                float(row["se"])


def test_private_key_is_complete_and_not_in_book():
    assert PRIVATE_KEY.is_file()
    text = base._read(PRIVATE_KEY)
    for code in [
        "TWFE-T1", "TWFE-T2", "TWFE-T3",
        "TWFE-S1", "TWFE-S2", "TWFE-S3", "TWFE-S4",
    ]:
        assert code in text
    assert all(
        "clave" not in filename.casefold()
        for filename in base.parse_rmd_files(base.BOOKDOWN)
    )
