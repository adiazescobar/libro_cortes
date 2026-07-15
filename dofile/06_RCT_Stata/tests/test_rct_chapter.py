import csv
import re
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[3]
CHAPTER = ROOT / "06-RCT2.Rmd"
BASE = ROOT / "dofile/06_RCT_Stata"
RESULTS = BASE / "results"


def _columns(path):
    with path.open(newline="", encoding="utf-8-sig") as handle:
        return set(next(csv.reader(handle)))


def _assert_canonical_verification(rows):
    expected_models = ["m1_simple", "m2_controles", "m3_estratos", "m4_completo"]
    assert len(rows) == 4
    assert [row["modelo"] for row in rows] == expected_models
    assert len({(row["modelo"], row["termino"]) for row in rows}) == 4
    for row in rows:
        assert row["termino"] == "D"
        assert row["estado"] == "PASS"
        assert row["N_igual"].casefold() == "true"
        assert int(float(row["N_stata"])) == int(float(row["N_python"])) == 70
        coef_diff = abs(float(row["coeficiente_stata"]) - float(row["coeficiente_python"]))
        se_diff = abs(float(row["error_estandar_stata"]) - float(row["error_estandar_python"]))
        r2_diff = abs(float(row["R2_stata"]) - float(row["R2_python"]))
        assert float(row["coef_abs_diff"]) == pytest.approx(coef_diff, abs=1e-15)
        assert float(row["se_abs_diff"]) == pytest.approx(se_diff, abs=1e-15)
        assert float(row["R2_abs_diff"]) == pytest.approx(r2_diff, abs=1e-15)
        assert coef_diff < 1e-3
        assert se_diff < 1e-3
        assert r2_diff < 1e-2


def test_all_download_links_resolve_to_current_materials():
    text = CHAPTER.read_text(encoding="utf-8")
    targets = re.findall(
        r"https://raw\.githubusercontent\.com/adiazescobar/libro_cortes/main/"
        r"(dofile/06_RCT_Stata/[^)]+)",
        text,
    )
    assert targets, "La sección de materiales debe enlazar archivos descargables"
    assert all((ROOT / target).is_file() for target in targets)


def test_chapter_consumes_all_canonical_csvs_instead_of_transcribed_output():
    text = CHAPTER.read_text(encoding="utf-8")
    canonical = [
        "resultados_stata.csv",
        "balance_stata.csv",
        "heterogeneidad_stata.csv",
        "verificacion_stata_python.csv",
    ]
    for filename in canonical:
        path = f"dofile/06_RCT_Stata/results/{filename}"
        direct_read = re.search(
            rf"(?:read\.csv|read_required_csv)\(\s*[\"']{re.escape(path)}[\"']",
            text,
        )
        assigned_path = re.search(
            rf"(?m)^\s*(\w+)\s*<-\s*[\"']{re.escape(path)}[\"']\s*$",
            text,
        )
        indirect_read = assigned_path and re.search(
            rf"read\.csv\(\s*{re.escape(assigned_path.group(1))}\b", text
        )
        assert direct_read or indirect_read
        assert (RESULTS / filename).is_file()
    assert "Linear regression                               Number of obs" not in text


def test_four_model_results_contract_is_preserved():
    with (RESULTS / "resultados_stata.csv").open(newline="", encoding="utf-8-sig") as handle:
        rows = list(csv.DictReader(handle))
    treatment = [row for row in rows if row["termino"] == "D"]
    assert [row["modelo"] for row in treatment] == [
        "m1_simple",
        "m2_controles",
        "m3_estratos",
        "m4_completo",
    ]
    assert all(row[field] for row in treatment for field in ["coeficiente", "error_estandar", "N", "R2"])


def test_balance_heterogeneity_and_verification_contracts_are_preserved():
    assert {"variable", "media_tratado", "media_control", "diferencia", "p_value"} <= _columns(
        RESULTS / "balance_stata.csv"
    )
    assert {"moderador", "termino", "coeficiente", "error_estandar", "N"} <= _columns(
        RESULTS / "heterogeneidad_stata.csv"
    )
    verification_path = RESULTS / "verificacion_stata_python.csv"
    assert {
        "modelo", "termino", "estado", "N_stata", "N_python", "N_igual",
        "coeficiente_stata", "coeficiente_python", "error_estandar_stata",
        "error_estandar_python", "R2_stata", "R2_python", "coef_abs_diff",
        "se_abs_diff", "R2_abs_diff",
    } <= _columns(verification_path)
    with verification_path.open(newline="", encoding="utf-8-sig") as handle:
        _assert_canonical_verification(list(csv.DictReader(handle)))


def test_verification_contract_rejects_directed_mutations():
    path = RESULTS / "verificacion_stata_python.csv"
    with path.open(newline="", encoding="utf-8-sig") as handle:
        canonical = list(csv.DictReader(handle))
    mutations = [
        (0, "estado", "FAIL"),
        (0, "termino", "constante"),
        (0, "N_igual", "False"),
        (0, "N_python", "69"),
        (0, "coef_abs_diff", "0.001"),
        (0, "se_abs_diff", "0.001"),
        (0, "R2_abs_diff", "0.01"),
        (0, "coeficiente_python", "99"),
        (3, "modelo", "modelo_extra"),
    ]
    for index, field, value in mutations:
        altered = [row.copy() for row in canonical]
        altered[index][field] = value
        with pytest.raises(AssertionError):
            _assert_canonical_verification(altered)


def test_chapter_preserves_balance_heterogeneity_and_verification_sections():
    text = CHAPTER.read_text(encoding="utf-8")
    required = [
        "Verificar el balance de covariables",
        "los cuatro escenarios",
        "Efectos heterogéneos (HET)",
        "El truco de centrar (Wooldridge)",
        "Replicación en Python / Google Colab",
        "Verificación Stata vs. Python",
    ]
    assert all(fragment in text for fragment in required)
