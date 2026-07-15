import csv
import math
import subprocess
import sys
from pathlib import Path
from statistics import NormalDist

import pytest


ROOT = Path(__file__).resolve().parents[3]
RESULTS = ROOT / "dofile/07_Power/results"
EXPECTED_COLUMNS = {
    "escenario",
    "familia",
    "estimando",
    "valor",
    "alpha",
    "power",
    "asignacion_tratada",
    "fuente",
}
EXPECTED_SCENARIOS = {
    "continuo sin controles",
    "continuo con controles",
    "binario",
    "take-up",
    "atrición",
    "tasa",
    "clúster",
}
ALLOWED_SOURCES = {
    "07_stata.do",
    "07_R.R",
    "07_phyton.ipynb",
    "BM_parcial.do",
    "escenario hipotético",
}
FORBIDDEN_COLUMNS = {
    "respuesta", "respuesta_correcta", "opcion_correcta", "clave",
    "clave_docente", "solucion", "pista",
}
DECLARED_CASES = [
    {"escenario": "continuo sin controles", "familia": "continua", "kind": "means", "sd": 1.0, "delta": 0.30},
    {"escenario": "continuo con controles", "familia": "continua", "kind": "means", "sd": 0.70, "delta": 0.30},
    {"escenario": "binario", "familia": "binaria", "kind": "proportions", "p1": 0.08, "p2": 0.05},
    {"escenario": "take-up", "familia": "continua", "kind": "means", "sd": 1.0, "delta": 0.30 * (0.90 - 0.10)},
    {"escenario": "atrición", "familia": "continua", "kind": "attrition", "sd": 1.0, "delta": 0.30, "retention": 0.80},
    {"escenario": "tasa", "familia": "tasa", "kind": "proportions", "p1": 0.07203, "p2": 0.06},
    {"escenario": "clúster", "familia": "clúster", "kind": "cluster", "sd": 1.0, "delta": 0.30, "rho": 0.05, "m": 50},
]


def _validated_rows(fieldnames, rows):
    columns = set(fieldnames or ())
    assert columns == EXPECTED_COLUMNS
    assert not columns & FORBIDDEN_COLUMNS
    keys = []
    for row in rows:
        assert all(str(row[column]).strip() for column in EXPECTED_COLUMNS)
        value = float(row["valor"])
        alpha = float(row["alpha"])
        power = float(row["power"])
        allocation = float(row["asignacion_tratada"])
        assert all(math.isfinite(number) for number in [value, alpha, power, allocation])
        assert 0 < alpha < 1
        assert 0 < power < 1
        assert 0 < allocation < 1
        assert row["fuente"].strip() in ALLOWED_SOURCES
        if row["fuente"].strip() == "escenario hipotético":
            assert "escenario hipotético" in row["escenario"].casefold()
        else:
            source = ROOT / "dofile/07_Power" / row["fuente"].strip()
            assert source.is_file(), "La fuente canónica debe ser un script rastreable"
        keys.append((row["escenario"].strip().casefold(), row["estimando"].strip().casefold()))
    assert len(keys) == len(set(keys)), "escenario + estimando debe ser una clave única"
    return rows


def _canonical_rows():
    assert RESULTS.is_dir(), "Falta dofile/07_Power/results/"
    csv_paths = sorted(RESULTS.glob("*.csv"))
    assert csv_paths, "Faltan resultados canónicos CSV de POWER"
    rows = []
    for path in csv_paths:
        with path.open(newline="", encoding="utf-8-sig") as handle:
            reader = csv.DictReader(handle)
            assert set(reader.fieldnames or ()) == EXPECTED_COLUMNS, (
                f"{path.name} no satisface el esquema canónico de POWER"
            )
            rows.extend(reader)
    return _validated_rows(EXPECTED_COLUMNS, rows)


def _two_arm_n(sd, delta, alpha=0.05, power=0.80):
    normal = NormalDist()
    z_alpha = normal.inv_cdf(1 - alpha / 2)
    z_power = normal.inv_cdf(power)
    return math.ceil(2 * ((z_alpha + z_power) * sd / delta) ** 2)


def _two_proportion_n(p1, p2, alpha=0.05, power=0.80):
    normal = NormalDist()
    z_alpha = normal.inv_cdf(1 - alpha / 2)
    z_power = normal.inv_cdf(power)
    variance = p1 * (1 - p1) + p2 * (1 - p2)
    return math.ceil(((z_alpha + z_power) ** 2 * variance) / ((p1 - p2) ** 2))


def _regenerated_rows():
    rows = []
    for case in DECLARED_CASES:
        if case["kind"] == "proportions":
            value = _two_proportion_n(case["p1"], case["p2"])
        else:
            value = _two_arm_n(case["sd"], case["delta"])
            if case["kind"] == "attrition":
                value = math.ceil(value / case["retention"])
            elif case["kind"] == "cluster":
                value = math.ceil(value * (1 + case["rho"] * (case["m"] - 1)))
        rows.append(
            {
                "escenario": case["escenario"], "familia": case["familia"],
                "estimando": "N_total", "valor": str(value), "alpha": "0.05",
                "power": "0.8", "asignacion_tratada": "0.5", "fuente": "07_stata.do",
            }
        )
    return rows


def _write_rows(path, rows):
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=sorted(EXPECTED_COLUMNS))
        writer.writeheader()
        writer.writerows(rows)


def _assert_rows_match_regeneration(canonical, regenerated, tolerance=1e-9):
    key = lambda row: (row["escenario"].casefold(), row["estimando"].casefold())
    canonical_by_key = {key(row): row for row in canonical}
    regenerated_by_key = {key(row): row for row in regenerated}
    assert canonical_by_key.keys() == regenerated_by_key.keys()
    for row_key, expected in regenerated_by_key.items():
        observed = canonical_by_key[row_key]
        for field in EXPECTED_COLUMNS - {"valor", "alpha", "power", "asignacion_tratada"}:
            assert observed[field] == expected[field]
        for field in ["valor", "alpha", "power", "asignacion_tratada"]:
            assert float(observed[field]) == pytest.approx(float(expected[field]), abs=tolerance)


def test_power_results_use_the_canonical_schema():
    rows = _canonical_rows()
    assert rows, "Los resultados canónicos no pueden estar vacíos"


def test_power_results_cover_all_minimum_scenarios():
    rows = _canonical_rows()
    observed = {row["escenario"].strip().casefold() for row in rows}
    assert EXPECTED_SCENARIOS <= observed


def test_result_validator_rejects_sensitive_extra_columns_bad_types_ranges_sources_and_duplicates():
    valid = {
        "escenario": "continuo sin controles", "familia": "continua",
        "estimando": "N", "valor": "800", "alpha": "0.05", "power": "0.8",
        "asignacion_tratada": "0.5", "fuente": "07_stata.do",
    }
    _validated_rows(EXPECTED_COLUMNS, [valid])
    mutations = [
        (EXPECTED_COLUMNS | {"clave_docente"}, [{**valid, "clave_docente": "B"}]),
        (EXPECTED_COLUMNS, [{**valid, "valor": "abc"}]),
        (EXPECTED_COLUMNS, [{**valid, "alpha": "0"}]),
        (EXPECTED_COLUMNS, [{**valid, "power": "999"}]),
        (EXPECTED_COLUMNS, [{**valid, "asignacion_tratada": "-4"}]),
        (EXPECTED_COLUMNS, [{**valid, "fuente": "inventada"}]),
        (EXPECTED_COLUMNS, [valid, valid.copy()]),
    ]
    for columns, rows in mutations:
        with pytest.raises((AssertionError, ValueError)):
            _validated_rows(columns, rows)


def test_hypothetical_result_requires_literal_source_and_scenario_label():
    hypothetical = {
        "escenario": "escenario hipotético de atrición", "familia": "continua",
        "estimando": "N", "valor": "800", "alpha": "0.05", "power": "0.8",
        "asignacion_tratada": "0.5", "fuente": "escenario hipotético",
    }
    _validated_rows(EXPECTED_COLUMNS, [hypothetical])
    with pytest.raises(AssertionError):
        _validated_rows(EXPECTED_COLUMNS, [{**hypothetical, "escenario": "atrición"}])


def test_canonical_values_equal_independent_regeneration_in_temporary_csv(tmp_path):
    regenerated_path = tmp_path / "power_regenerated.csv"
    _write_rows(regenerated_path, _regenerated_rows())
    with regenerated_path.open(newline="", encoding="utf-8") as handle:
        regenerated = list(csv.DictReader(handle))
    _assert_rows_match_regeneration(_canonical_rows(), regenerated)


def test_printing_reproducible_cannot_replace_independent_numeric_comparison(tmp_path):
    stub = tmp_path / "fake_verifier.py"
    stub.write_text("print('reproducible')\n", encoding="utf-8")
    completed = subprocess.run([sys.executable, str(stub)], capture_output=True, text=True)
    assert completed.returncode == 0 and completed.stdout.strip() == "reproducible"
    fabricated = _regenerated_rows()
    fabricated[0] = {**fabricated[0], "valor": str(float(fabricated[0]["valor"]) + 1)}
    with pytest.raises(AssertionError):
        _assert_rows_match_regeneration(fabricated, _regenerated_rows())
