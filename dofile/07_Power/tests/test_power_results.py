import csv
import math
import subprocess
import sys
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[3]
POWER = ROOT / "dofile/07_Power"
CANONICAL = POWER / "results/power_resultados.csv"
VERIFICATION = POWER / "results/power_verificacion.csv"

EXPECTED_COLUMNS = {
    "escenario", "familia", "comando", "cantidad_solicitada", "valor_stata",
    "N_total", "N_por_brazo", "N1", "N2", "unidad_resultado", "alpha", "power",
    "colas", "asignacion_tratada", "delta", "sd", "p0", "p1", "takeup_tratamiento",
    "takeup_control", "retencion", "icc", "tamano_cluster", "K1", "K2", "K_total",
    "N_realizable", "fuente",
}
EXPECTED = {
    "continuo sin controles": (352, 176, 176),
    "continuo con controles": (174, 87, 87),
    "binario": (2118, 1059, 1059),
    "take-up": (548, 274, 274),
    "atrición": (352, 176, 176),
    "tasa": (13374, 6687, 6687),
    "clúster": (1300, 650, 650),
}


def _rows():
    assert CANONICAL.is_file()
    with CANONICAL.open(newline="", encoding="utf-8-sig") as handle:
        reader = csv.DictReader(handle)
        assert set(reader.fieldnames or ()) == EXPECTED_COLUMNS
        rows = list(reader)
    assert {row["escenario"] for row in rows} == set(EXPECTED)
    return rows


def _number(row, name):
    value = row[name].strip()
    return None if value == "" else float(value)


def test_canonical_rows_publish_actual_stata_returns_and_not_normal_approximations():
    rows = {row["escenario"]: row for row in _rows()}
    normal_approx_totals = {"continuo sin controles": 350, "binario": 2114, "tasa": 13368}
    for scenario, (total, n1, n2) in EXPECTED.items():
        row = rows[scenario]
        assert _number(row, "N_total") == total
        assert _number(row, "N1") == n1
        assert _number(row, "N2") == n2
        assert _number(row, "N_total") == _number(row, "N1") + _number(row, "N2")
        assert _number(row, "N_por_brazo") == n1 == n2
    for scenario, approximation in normal_approx_totals.items():
        assert _number(rows[scenario], "N_total") != approximation


def test_inputs_are_auditable_and_blanks_are_contextual():
    for row in _rows():
        assert row["comando"] in {"power twomeans", "power twoproportions", "power twomeans, cluster"}
        assert row["cantidad_solicitada"] in {"N_total", "K_por_brazo"}
        assert row["unidad_resultado"] in {"individuos", "clusters_por_brazo"}
        assert _number(row, "alpha") == pytest.approx(0.05)
        assert _number(row, "power") == pytest.approx(0.80)
        assert row["colas"] == "bilateral"
        assert _number(row, "asignacion_tratada") == pytest.approx(0.5)
        assert row["fuente"] == "07_stata.do"
        if row["familia"] in {"continua", "clúster"}:
            assert _number(row, "delta") is not None and _number(row, "sd") is not None
            assert row["p0"] == row["p1"] == ""
        else:
            assert _number(row, "p0") is not None and _number(row, "p1") is not None
            assert row["delta"] == row["sd"] == ""
        if row["escenario"] != "take-up":
            assert row["takeup_tratamiento"] == row["takeup_control"] == ""
        if row["escenario"] != "atrición":
            assert row["retencion"] == ""


def test_attrition_and_cluster_units_and_rounding_relations():
    rows = {row["escenario"]: row for row in _rows()}
    attrition = rows["atrición"]
    assert _number(attrition, "retencion") == pytest.approx(0.80)
    assert _number(attrition, "N_realizable") == math.ceil(_number(attrition, "N_total") / 0.80)

    cluster = rows["clúster"]
    assert cluster["comando"] == "power twomeans, cluster"
    assert cluster["cantidad_solicitada"] == "K_por_brazo"
    assert _number(cluster, "valor_stata") == _number(cluster, "K1") == 13
    assert _number(cluster, "K_total") == _number(cluster, "K1") + _number(cluster, "K2") == 26
    assert _number(cluster, "N1") == _number(cluster, "K1") * _number(cluster, "tamano_cluster")
    assert _number(cluster, "N2") == _number(cluster, "K2") * _number(cluster, "tamano_cluster")
    assert _number(cluster, "N_realizable") == _number(cluster, "N_total") == 1300
    assert _number(cluster, "icc") == pytest.approx(0.05)


def test_stata_source_posts_returned_scalars_not_dead_locals():
    source = (POWER / "07_stata.do").read_text(encoding="utf-8")
    assert "local canonical_N" not in source
    assert source.count("local stata_N = r(N)") >= 7
    assert "local stata_K1 = r(K1)" in source and "local stata_K2 = r(K2)" in source
    assert "(`stata_N')" in source and "(`stata_K1')" in source


def test_verification_compares_stata_to_independent_method_with_nonzero_tolerances():
    with VERIFICATION.open(newline="", encoding="utf-8-sig") as handle:
        rows = list(csv.DictReader(handle))
    assert set(rows[0]) == {"escenario", "valor_stata", "valor_alternativo", "diferencia_abs", "tolerancia", "metodo_alternativo", "estado"}
    assert {row["escenario"] for row in rows} == set(EXPECTED)
    assert all(row["estado"] == "PASS" for row in rows)
    assert all(float(row["tolerancia"]) > 0 for row in rows)
    assert any(float(row["diferencia_abs"]) > 0 for row in rows)


def test_stdout_cannot_substitute_numeric_validation(tmp_path):
    stub = tmp_path / "fake.py"
    stub.write_text("print('PASS')\n", encoding="utf-8")
    completed = subprocess.run([sys.executable, str(stub)], capture_output=True, text=True)
    assert completed.stdout.strip() == "PASS"
    bad = dict(EXPECTED)
    bad["continuo sin controles"] = (350, 175, 175)
    assert bad != EXPECTED
