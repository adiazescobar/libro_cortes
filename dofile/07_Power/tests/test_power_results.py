import csv
import math
import subprocess
import sys
from pathlib import Path

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


def test_canonical_values_are_reproducible_from_declared_sources():
    verifier = ROOT / "dofile/07_Power/verify_power_results.py"
    assert verifier.is_file(), "Falta el verificador reproducible de valores canónicos"
    completed = subprocess.run(
        [sys.executable, str(verifier), "--check", str(RESULTS)],
        cwd=ROOT,
        capture_output=True,
        text=True,
    )
    assert completed.returncode == 0
    assert "reproducible" in completed.stdout.casefold()
