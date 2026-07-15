import csv
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
TEXT = (ROOT / "04-ParametrosStata.Rmd").read_text(encoding="utf-8")
BASE = ROOT / "dofile/04_ParametrosStata"


def test_downloads_are_first_and_complete():
    assert TEXT.index("## Materiales para la clase {-}") < TEXT.index("## Objetivos {-}")
    required = [
        "04_stata.do", "04_data.dta", "04_R.R", "04_phyton.ipynb",
        "results/parameters_results.csv", "results/monte_carlo_summary.csv",
        "04_stata.log",
    ]
    assert all((BASE / path).is_file() for path in required)


def test_page_consumes_canonical_results():
    assert 'read.csv("dofile/04_ParametrosStata/results/parameters_results.csv"' in TEXT
    assert 'read.csv("dofile/04_ParametrosStata/results/monte_carlo_summary.csv"' in TEXT
    assert "Linear regression                               Number of obs" not in TEXT
    assert "..." not in TEXT


def test_results_schema():
    with (BASE / "results/parameters_results.csv").open(newline="", encoding="utf-8-sig") as handle:
        columns = set(next(csv.reader(handle)))
    assert {"escenario", "estimando", "valor", "N"} <= columns
