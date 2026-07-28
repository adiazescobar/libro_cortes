from pathlib import Path

import test_power_pedagogy_contract as base


ROOT = base.ROOT
THEORY = ROOT / "15-IPW.Rmd"
PRACTICE = ROOT / "16-PSM_IPW_SinteticosConsolidado.Rmd"
DOFILE = ROOT / "dofile/16_PSM_IPW_Sinteticos/02_ipw_stata.do"
PRIVATE_KEY = ROOT / "claves_privadas/15_IPW_clave.md"
DRAFT = ROOT / "17-SyntheticControls-DRAFT.Rmd"
RESULTS = ROOT / "dofile/16_PSM_IPW_Sinteticos/results"


def _read(path):
    return path.read_text(encoding="utf-8")


def test_ipw_pair_has_uniform_titles_and_legacy_anchors():
    assert _read(THEORY).startswith(
        "# Ponderación por probabilidad inversa — Clase teórica {#ipw}"
    )
    assert _read(PRACTICE).startswith(
        "# Ponderación por probabilidad inversa — Clase empírica "
        "{#psm-ipw-sinteticos}"
    )


def test_theory_distinguishes_ht_from_hajek():
    text = _read(THEORY)
    assert "Horvitz–Thompson" in text
    assert "Hájek" in text
    assert "no normalizado" in text
    assert "normalizado" in text
    assert text.index("Horvitz–Thompson") < text.index("Hájek")


def test_theory_covers_estimands_identification_and_diagnostics():
    text = _read(THEORY)
    for marker in [
        "Y(D=1)",
        "Y(D=0)",
        "ATE",
        "ATT",
        "positividad",
        "pesos estabilizados",
        "tamaño efectivo",
        r"(\sum_i w_i)^2",
        "balance no demuestra",
        "cambia la población",
    ]:
        assert marker.lower() in text.lower(), marker


def test_theory_states_double_robustness_with_limits():
    text = _read(THEORY)
    for marker in ["AIPW", "IPWRA", "doblemente robust", "confusión no observada"]:
        assert marker.lower() in text.lower(), marker


def test_practice_starts_with_materials_then_readings():
    text = _read(PRACTICE)
    materials = text.index("## Materiales para la clase")
    readings = text.index("**Lecturas centrales**")
    assert materials < readings < 5000
    assert "02_ipw_stata.do" in text
    assert "base6.dta" in text


def test_practice_uses_canonical_results_and_three_graphs():
    text = _read(PRACTICE)
    for filename in [
        "ipw_estimates.csv",
        "ipw_weight_diagnostics.csv",
        "ipw_balance.csv",
        "ipw_positivity_simulation.csv",
        "ipw_support.png",
        "ipw_weights_dist.png",
        "ipw_positivity_weak.png",
    ]:
        assert filename in text
    assert text.count("```text") >= 3


def test_practice_compares_manual_teffects_and_dr_estimators():
    text = _read(PRACTICE)
    for marker in [
        "teffects ipw",
        "teffects aipw",
        "teffects ipwra",
        "Horvitz–Thompson",
        "Hájek",
        "tebalance summarize",
        "tebalance density",
    ]:
        assert marker in text, marker


def test_public_questions_and_private_key_contract():
    combined = _read(THEORY) + _read(PRACTICE)
    assert _read(THEORY).count("::: {.boxexam}") == 3
    assert _read(PRACTICE).count("::: {.boxexam}") == 4
    assert PRIVATE_KEY.is_file()
    key = _read(PRIVATE_KEY)
    for label in ["IPW-T1", "IPW-T2", "IPW-T3", "IPW-S1", "IPW-S2", "IPW-S3", "IPW-S4"]:
        assert label in key
    assert PRIVATE_KEY.name not in _read(ROOT / "_bookdown.yml")
    assert "solución" not in combined.lower()


def test_synthetic_content_is_preserved_but_not_rendered():
    assert DRAFT.is_file()
    draft = _read(DRAFT)
    for marker in ["synth_smoking.dta", "03_synthetic_controls_stata.do", "RMSPE"]:
        assert marker in draft
    assert DRAFT.name not in _read(ROOT / "_bookdown.yml")


def test_dofile_exports_expected_artifacts():
    text = _read(DOFILE)
    for filename in [
        "ipw_estimates.csv",
        "ipw_weight_diagnostics.csv",
        "ipw_balance.csv",
        "ipw_positivity_simulation.csv",
        "ipw_support.png",
        "ipw_weights_dist.png",
        "ipw_positivity_weak.png",
    ]:
        assert filename in text


def test_canonical_result_files_exist():
    for filename in [
        "ipw_estimates.csv",
        "ipw_weight_diagnostics.csv",
        "ipw_balance.csv",
        "ipw_positivity_simulation.csv",
    ]:
        assert (RESULTS / filename).is_file(), filename
