import csv
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
RMD = (ROOT / "02-StataBasics.Rmd").read_text(encoding="utf-8")


def test_materials_are_first():
    assert RMD.index("## Materiales para la clase {-}") < RMD.index(
        "### Objetivos de aprendizaje {-}"
    )
    assert "## DESCARGA LOS DOCUMENTOS {-}" not in RMD


def test_download_files_exist():
    base = ROOT / "dofile/Clase0_StataBasics"
    required = ["Clase00_Stata.do", "clase0_R.R", "clase0_phyton.ipynb", "hh_98.dta"]
    assert all((base / name).is_file() for name in required)


def test_canonical_results_schema():
    path = ROOT / "dofile/Clase0_StataBasics/results/stata_basics_results.csv"
    with path.open(newline="", encoding="utf-8-sig") as handle:
        columns = set(next(csv.reader(handle)))
    assert {"ejemplo", "variable", "valor", "N"} <= columns
