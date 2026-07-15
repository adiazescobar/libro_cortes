import csv
from pathlib import Path


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


def _canonical_rows():
    assert RESULTS.is_dir(), "Falta dofile/07_Power/results/"
    csv_paths = sorted(RESULTS.glob("*.csv"))
    assert csv_paths, "Faltan resultados canónicos CSV de POWER"
    rows = []
    for path in csv_paths:
        with path.open(newline="", encoding="utf-8-sig") as handle:
            reader = csv.DictReader(handle)
            assert EXPECTED_COLUMNS <= set(reader.fieldnames or ()), (
                f"{path.name} no satisface el esquema canónico de POWER"
            )
            rows.extend(reader)
    return rows


def test_power_results_use_the_canonical_schema():
    rows = _canonical_rows()
    assert rows, "Los resultados canónicos no pueden estar vacíos"
    for row in rows:
        assert all(row[column].strip() for column in EXPECTED_COLUMNS)


def test_power_results_cover_all_minimum_scenarios():
    rows = _canonical_rows()
    observed = {row["escenario"].strip().casefold() for row in rows}
    assert EXPECTED_SCENARIOS <= observed
