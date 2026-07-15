import csv
import re
from pathlib import Path


ROOT = Path(__file__).resolve().parents[3]
CHAPTER = ROOT / "06-RCT2.Rmd"
BASE = ROOT / "dofile/06_RCT_Stata"
RESULTS = BASE / "results"


def _columns(path):
    with path.open(newline="", encoding="utf-8-sig") as handle:
        return set(next(csv.reader(handle)))


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
    verification = _columns(RESULTS / "verificacion_stata_python.csv")
    assert {"modelo", "termino", "estado"} <= verification


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
