import csv
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[3]
DID = ROOT / "dofile/08_DID"
CANONICAL = DID / "results/did_resultados.csv"
VERIFICATION = DID / "results/did_verificacion.csv"

EXPECTED_COLUMNS = {
    "escenario", "comando", "cantidad", "valor_stata", "ee", "estadistico",
    "p_valor", "N", "fuente",
}

REQUIRED_SCENARIOS = {
    "media_C0", "media_C1", "media_T0", "media_T1",
    "did_manual", "did_diff", "did_regresion",
    "hospdd_atet", "ptrends", "granger",
}

BASIC_DID_SCENARIOS = {"did_manual", "did_diff", "did_regresion"}

VERIFICATION_COLUMNS = {
    "escenario", "valor_stata", "valor_alternativo", "diferencia_abs",
    "tolerancia", "metodo_alternativo", "estado",
}


def _rows():
    assert CANONICAL.is_file(), "Falta did_resultados.csv (correr 08_DID.do desde la raíz)"
    with CANONICAL.open(newline="", encoding="utf-8-sig") as handle:
        reader = csv.DictReader(handle)
        assert set(reader.fieldnames or ()) == EXPECTED_COLUMNS
        rows = list(reader)
    assert {row["escenario"] for row in rows} == REQUIRED_SCENARIOS
    return {row["escenario"]: row for row in rows}


def _number(row, name):
    value = row[name].strip()
    return None if value == "" else float(value)


def test_canonical_rows_are_complete_and_sourced():
    rows = _rows()
    for scenario, row in rows.items():
        assert _number(row, "valor_stata") is not None, scenario
        assert row["fuente"] == "08_DID.do"
    for scenario in ["media_C0", "media_C1", "media_T0", "media_T1"]:
        assert rows[scenario]["cantidad"] == "media"
        assert _number(rows[scenario], "N") is not None


def test_manual_did_matches_the_two_by_two_means():
    rows = _rows()
    manual = _number(rows["did_manual"], "valor_stata")
    expected = (
        _number(rows["media_T1"], "valor_stata") - _number(rows["media_T0"], "valor_stata")
    ) - (
        _number(rows["media_C1"], "valor_stata") - _number(rows["media_C0"], "valor_stata")
    )
    assert manual == pytest.approx(expected, abs=1e-6)


def test_estimators_agree_across_methods():
    rows = _rows()
    basic_did_rows = {scenario for scenario in rows if scenario.startswith("did_")}
    assert basic_did_rows == BASIC_DID_SCENARIOS
    manual = _number(rows["did_manual"], "valor_stata")
    assert _number(rows["did_regresion"], "valor_stata") == pytest.approx(manual, abs=1e-6)
    assert _number(rows["did_diff"], "valor_stata") == pytest.approx(manual, abs=1e-4)


def test_inference_columns_are_published_for_estimates_and_tests():
    rows = _rows()
    for scenario in ["did_diff", "did_regresion", "hospdd_atet"]:
        assert _number(rows[scenario], "ee") is not None
        assert _number(rows[scenario], "p_valor") is not None
    for scenario in ["ptrends", "granger"]:
        assert _number(rows[scenario], "estadistico") is not None
        assert _number(rows[scenario], "p_valor") is not None
        assert 0 <= _number(rows[scenario], "p_valor") <= 1


def test_verification_compares_stata_to_independent_method():
    assert VERIFICATION.is_file(), "Falta did_verificacion.csv"
    with VERIFICATION.open(newline="", encoding="utf-8-sig") as handle:
        rows = list(csv.DictReader(handle))
    assert set(rows[0]) == VERIFICATION_COLUMNS
    scenarios = {row["escenario"] for row in rows}
    assert {"did_manual", "did_regresion", "hospdd_atet"}.issubset(scenarios)
    assert all(row["estado"] == "PASS" for row in rows)
    assert all(float(row["tolerancia"]) > 0 for row in rows)


def test_stata_source_posts_returned_scalars():
    source = (DID / "08_DID.do").read_text(encoding="utf-8", errors="replace")
    assert "postfile" in source
    assert "did_resultados" in source
    assert "e(b)" in source or "_b[" in source or "r(mean)" in source
