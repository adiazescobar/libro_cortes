import csv
import math
from pathlib import Path

import test_power_pedagogy_contract as base


ROOT = base.ROOT
THEORY = ROOT / "15-IPW.Rmd"
PRACTICE = ROOT / "16-PSM_IPW_SinteticosConsolidado.Rmd"
DOFILE = ROOT / "dofile/16_PSM_IPW_Sinteticos/02_ipw_stata.do"
PRIVATE_KEY = ROOT.parent / "claves_privadas/15_IPW_clave.md"
DRAFT = ROOT / "17-SyntheticControls-DRAFT.Rmd"
RESULTS = ROOT / "dofile/16_PSM_IPW_Sinteticos/results"
BALANCE_GRAPH = ROOT / "dofile/16_PSM_IPW_Sinteticos/ipw_balance_ate_att.png"


def _read(path):
    return path.read_text(encoding="utf-8")


def _csv_rows(path):
    with path.open(encoding="utf-8", newline="") as handle:
        return list(csv.DictReader(handle))


def _estimate(rows, estimator, estimand):
    matches = [
        row
        for row in rows
        if row["estimator"] == estimator and row["estimand"] == estimand
    ]
    assert len(matches) == 1, (estimator, estimand, matches)
    estimate = float(matches[0]["estimate"])
    assert math.isfinite(estimate), (estimator, estimand, estimate)
    return estimate


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
    goals = text.index("**Metas de aprendizaje**")
    assert materials < readings < goals < 5000
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
    assert ROOT not in PRIVATE_KEY.parents
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


def test_positivity_simulation_reports_scenario_specific_precision():
    path = RESULTS / "ipw_positivity_simulation.csv"
    rows = _csv_rows(path)
    assert list(rows[0]) == [
        "estimator",
        "estimate",
        "se",
        "true_effect",
        "max_weight",
        "ess",
        "n_used",
    ]
    assert float(rows[2]["max_weight"]) < float(rows[1]["max_weight"])
    assert float(rows[2]["ess"]) > float(rows[1]["ess"])


def test_weighted_regression_commands_are_shown_in_dofile_and_practice():
    commands = [
        "reg y2 D [pw=w_ate], vce(robust)",
        "reg y2 D [pw=w_att], vce(robust)",
    ]
    for command in commands:
        assert command in _read(DOFILE), command
        assert command in _read(PRACTICE), command


def test_weighted_regression_estimates_match_hajek_and_teffects_ipw():
    rows = _csv_rows(RESULTS / "ipw_estimates.csv")
    for estimand in ["ATE", "ATT"]:
        reg = _estimate(rows, "reg ponderada", estimand)
        hajek = _estimate(rows, "Hajek manual", estimand)
        teffects_ipw = _estimate(rows, "teffects ipw", estimand)
        assert abs(reg - hajek) < 1e-8, (estimand, reg, hajek)
        assert abs(reg - teffects_ipw) < 1e-8, (estimand, reg, teffects_ipw)


def test_balance_csv_uses_complete_long_ate_att_schema():
    path = RESULTS / "ipw_balance.csv"
    rows = _csv_rows(path)
    assert list(rows[0]) == ["estimand", "covariate", "metric", "raw", "weighted"]

    estimands = {"ATE", "ATT"}
    covariates = {
        "personas",
        "orden_n",
        "ocupado_jefe",
        "educa_jefe",
        "ingresos_hogar_jefe",
        "hombre",
    }
    metrics = {"smd", "variance_ratio"}
    expected_cells = {
        (estimand, covariate, metric)
        for estimand in estimands
        for covariate in covariates
        for metric in metrics
    }
    observed_cells = {
        (row["estimand"], row["covariate"], row["metric"]) for row in rows
    }

    assert len(rows) == 24
    assert observed_cells == expected_cells
    assert len(observed_cells) == len(rows)
    for row in rows:
        for column in ["raw", "weighted"]:
            assert math.isfinite(float(row[column])), (row, column)


def test_balance_audit_requires_graph_native_diagnostics_and_safe_respecification():
    dofile = _read(DOFILE)
    practice = _read(PRACTICE)
    for marker in ["ipw_balance_ate_att.png", "tebalance summarize", "tebalance density"]:
        assert marker in dofile, marker
        assert marker in practice, marker
    assert BALANCE_GRAPH.is_file()

    practice_lower = practice.lower()
    assert "balance observable no demuestra cia" in practice_lower
    assert "reespecific" in practice_lower
    assert "sin mirar el efecto" in practice_lower
