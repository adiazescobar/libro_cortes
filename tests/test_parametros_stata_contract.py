import csv
import re
from pathlib import Path

import pandas as pd


ROOT = Path(__file__).resolve().parents[1]
TEXT = (ROOT / "04-ParametrosStata.Rmd").read_text(encoding="utf-8")
BASE = ROOT / "dofile/04_ParametrosStata"
DO_TEXT = (BASE / "04_stata.do").read_text(encoding="utf-8")


REQUIRED_DOWNLOADS = [
    "04_stata.do", "04_data.dta", "04_R.R", "04_phyton.ipynb",
    "results/parameters_results.csv", "results/monte_carlo_summary.csv",
    "04_stata.log",
]


def test_downloads_are_first_and_complete():
    h2_headings = re.findall(r"^##\s+[^\n]+", TEXT, re.MULTILINE)
    assert h2_headings[0] == "## Materiales para la clase {-}"
    assert all((BASE / path).is_file() for path in REQUIRED_DOWNLOADS)
    for path in REQUIRED_DOWNLOADS:
        linked_path = f"dofile/04_ParametrosStata/{path}"
        assert re.search(rf"\[[^]]+]\((?:<[^>]*|[^)]*){re.escape(linked_path)}(?:[^>]*>|[^)]*)\)", TEXT)


def test_canonical_do_file_exports_exactly_the_page_artifacts():
    csv_exports = set(re.findall(r'export\s+delimited\s+using\s+["\']([^"\']+\.csv)["\']', DO_TEXT, re.IGNORECASE))
    graph_exports = set(re.findall(r'graph\s+export\s+["\']([^"\']+)["\']', DO_TEXT, re.IGNORECASE))
    assert csv_exports == {
        "results/parameters_results.csv",
        "results/monte_carlo_summary.csv",
    }
    assert graph_exports == {
        "sesgo_con_seleccion.png",
        "sesgo_con_aleatorizacion.png",
        "comparacion_escenarios.png",
    }


def test_page_consumes_canonical_results():
    assert 'read.csv("dofile/04_ParametrosStata/results/parameters_results.csv"' in TEXT
    assert 'read.csv("dofile/04_ParametrosStata/results/monte_carlo_summary.csv"' in TEXT
    assert "Linear regression                               Number of obs" not in TEXT
    assert "..." not in TEXT


def test_results_schema():
    with (BASE / "results/parameters_results.csv").open(newline="", encoding="utf-8-sig") as handle:
        columns = set(next(csv.reader(handle)))
    assert {"escenario", "estimando", "valor", "N"} <= columns


def test_monte_carlo_outputs_have_complete_scenarios():
    with (BASE / "results/monte_carlo_summary.csv").open(newline="", encoding="utf-8-sig") as handle:
        summary = list(csv.DictReader(handle))
    assert {"escenario", "N", "media", "desv_est", "p5", "mediana", "p95"} <= set(summary[0])
    assert {row["escenario"] for row in summary} == {"seleccion", "aleatorizacion"}
    assert {int(row["N"]) for row in summary} == {1000}

    draws = BASE / "results/monte_carlo_draws.dta"
    assert draws.is_file() and draws.stat().st_size > 0
    draws_df = pd.read_stata(draws, convert_categoricals=False)
    assert list(draws_df.columns) == ["escenario", "rep", "sesgo"]
    assert len(draws_df) == 2000
    assert not draws_df.isna().any().any()
    for scenario in ["seleccion", "aleatorizacion"]:
        scenario_draws = draws_df.loc[draws_df["escenario"] == scenario]
        assert len(scenario_draws) == 1000
        assert scenario_draws["rep"].is_unique
        assert set(scenario_draws["rep"]) == set(range(1, 1001))


def test_all_three_stata_graphs_exist():
    for name in [
        "sesgo_con_seleccion.png",
        "sesgo_con_aleatorizacion.png",
        "comparacion_escenarios.png",
    ]:
        graph = BASE / name
        assert graph.is_file() and graph.stat().st_size > 0
