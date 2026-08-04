from pathlib import Path
import csv


BASE = Path(__file__).resolve().parents[1]


def rows(name):
    with (BASE / "results" / name).open(
        newline="", encoding="utf-8-sig"
    ) as handle:
        return list(csv.DictReader(handle))


def metrics(name):
    return {row["metric"]: float(row["value"]) for row in rows(name)}


def test_paces_truth_has_deliberately_distinct_estimands():
    m = metrics("paces_truth.csv")
    assert abs(m["late_true"] - m["ate_true"]) > 0.15
    assert abs(m["late_true"] - m["att_true"]) > 0.10
    assert abs(
        m["share_complier"]
        + m["share_always"]
        + m["share_never"]
        - 1
    ) < 1e-10
    assert m["share_defier"] == 0


def test_wald_2sls_and_first_stage_identity():
    m = metrics("paces_estimators.csv")
    assert abs(m["wald"] - m["iv_2sls"]) < 1e-8
    assert abs(m["first_stage"] - m["share_complier_estimated"]) < 1e-8
    assert abs(m["iv_2sls"] - m["late_true"]) < 0.08


def test_complier_profile_has_population_truth_and_estimate():
    data = rows("paces_complier_profile.csv")
    groups = {row["group"] for row in data}
    variables = {row["variable"] for row in data}
    assert groups == {"Population", "True compliers", "Estimated compliers"}
    assert variables == {"Female", "Baseline score", "Low income"}
