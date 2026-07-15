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


def _stata_blocks():
    return re.findall(
        r"^```stata\s*$\n(?P<code>.*?)^```\s*$",
        TEXT,
        re.MULTILINE | re.DOTALL | re.IGNORECASE,
    )


def _executable_stata_text():
    blocks = _stata_blocks()
    assert blocks, "La página debe incluir bloques de código Stata"
    for block in blocks:
        assert "..." not in block, "Los bloques Stata no pueden contener pseudocódigo con ..."
    return "\n".join(blocks)


def _has_selection_assignment(code):
    """Accept a D rule whose condition or probability actually depends on X/yd0."""
    assignment_lines = re.findall(
        r"(?im)^\s*(?:gen(?:erate)?|replace)\s+D\s*=\s*([^\n]+)", code
    )
    for expression in assignment_lines:
        depends_on_covariate = re.search(r"(?i)\b(?:X|yd0)\b", expression)
        uses_condition = re.search(r"(?:<=|>=|==|!=|<|>)", expression)
        uses_probability = re.search(r"(?i)\bruniform\s*\(\s*\)", expression)
        if depends_on_covariate and (uses_condition or uses_probability):
            return True
    return False


def _has_random_half_assignment(code):
    return bool(re.search(
        r"(?im)^\s*(?:gen(?:erate)?|replace)\s+D\s*=\s*"
        r"\(?\s*runiform\s*\(\s*\)\s*<\s*(?:0?\.5|1/2)\s*\)?\s*(?://.*|$)",
        code,
    ))


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


def test_page_contains_complete_executable_stata_workflow():
    code = _executable_stata_text()
    required_commands = {
        "resultado observado": r"(?im)^\s*generate\s+y\s*=",
        "efecto individual": r"(?im)^\s*generate\s+tau\s*=",
        "creación de X": r"(?im)^\s*generate\s+X\s*=",
        "diferencia de medias": r"(?im)^\s*ttest\s+\w+\s*,\s*by\s*\(",
        "regresión": r"(?im)^\s*regress\s+",
        "programa estimadores": r"(?im)^\s*program\s+define\s+estimadores\b",
        "medias por condición": r"(?im)^\s*summarize\s+\w+\s+if\s+",
        "duplicación": r"(?im)^\s*expand\s+10000\b",
        "semilla": r"(?im)^\s*set\s+seed\s+\d+\b",
        "simulación": r"(?im)^\s*simulate\s+",
    }
    missing = [label for label, pattern in required_commands.items() if not re.search(pattern, code)]
    assert not missing, f"Faltan comandos ejecutables para: {', '.join(missing)}"


def test_page_contains_both_executable_assignment_rules():
    code = _executable_stata_text()
    assert _has_selection_assignment(code), (
        "Falta una regla ejecutable donde la selección dependa de X o yd0 "
        "mediante una condición o probabilidad"
    )
    assert _has_random_half_assignment(code), (
        "Falta la regla ejecutable de asignación aleatoria runiform() < .5"
    )


def test_assignment_rule_matchers_reject_superficial_mentions():
    assert not _has_selection_assignment("generate D = X")
    assert not _has_selection_assignment("* selección depende de X")
    assert _has_selection_assignment("generate D = X > 0")
    assert _has_selection_assignment("gen D = runiform() < invlogit(yd0)")
    assert not _has_random_half_assignment("La regla es runiform() < .5")
    assert not _has_random_half_assignment("generate D = runiform() < .4")
    assert _has_random_half_assignment("generate D = runiform() < .5")


def test_results_schema():
    with (BASE / "results/parameters_results.csv").open(newline="", encoding="utf-8-sig") as handle:
        columns = set(next(csv.reader(handle)))
    assert {"escenario", "estimando", "valor", "N"} <= columns


def test_point_results_export_visible_group_counts_and_means():
    with (BASE / "results/parameters_results.csv").open(newline="", encoding="utf-8-sig") as handle:
        rows = list(csv.DictReader(handle))
    original = {
        row["estimando"] for row in rows if row["escenario"] == "datos_originales"
    }
    assert {
        "N_D0", "N_D1", "MEDIA_Y_D0", "MEDIA_Y_D1",
        "ATE", "ATT", "ATU", "CATE_X0", "CATE_X1", "NAIVE", "SESGO_ATT",
    } <= original


def test_regression_output_exports_robust_uncertainty_for_d():
    with (BASE / "results/parameters_results.csv").open(
        newline="", encoding="utf-8-sig"
    ) as handle:
        rows = list(csv.DictReader(handle))
    original = {
        row["estimando"]: float(row["valor"])
        for row in rows if row["escenario"] == "datos_originales"
    }
    required = {
        "COEF_REG_D", "SE_ROBUST_REG_D", "IC95_INF_REG_D", "IC95_SUP_REG_D",
        "COEF_REG_CONSTANTE", "SE_ROBUST_REG_CONSTANTE",
        "IC95_INF_REG_CONSTANTE", "IC95_SUP_REG_CONSTANTE",
    }
    assert required <= set(original)
    assert original["SE_ROBUST_REG_D"] > 0
    assert original["IC95_INF_REG_D"] < original["COEF_REG_D"] < original["IC95_SUP_REG_D"]
    assert re.search(r"regress\s+y\s+D\s*,\s*(?:vce\(robust\)|robust)", DO_TEXT, re.I)
    assert "_se[D]" in DO_TEXT


def test_s_p1_displays_interpolated_canonical_regression_table():
    assert "tabla_regresion" in TEXT
    for estimand in [
        "COEF_REG_D", "SE_ROBUST_REG_D", "IC95_INF_REG_D", "IC95_SUP_REG_D"
    ]:
        assert estimand in TEXT
    s_p1 = TEXT.split("S-P1", 1)[1].split(":::", 1)[0]
    assert "knitr::kable(tabla_regresion" in s_p1


def test_practice_has_objectives_prerequisites_sequence_and_bridge():
    headings = [
        "## Materiales para la clase {-}",
        "## Objetivos {-}",
            "## Describir los datos y construir el resultado observado",
            "## Relacionar diferencia de medias y regresión",
            "## Calcular parámetros causales y heterogeneidad",
            "## Duplicar observaciones no resuelve la selección",
            "## Reasignar el tratamiento al azar",
            "## Repetir el experimento: Monte Carlo",
            "## Ejercicios",
            "## Síntesis",
        "## Puente al capítulo siguiente {-}",
    ]
    positions = [TEXT.index(heading) for heading in headings]
    assert positions == sorted(positions)
    assert "Conocimientos previos" in TEXT
    assert "05-RCT.Rmd" in TEXT


def test_download_label_says_python_and_iframes_have_titles():
    assert "[Notebook de Python (`04_phyton.ipynb`)]" in TEXT
    assert "Notebook histórico" not in TEXT
    for iframe in re.findall(r"<iframe\b[^>]*>", TEXT, re.IGNORECASE):
        assert re.search(r'\btitle="[^"]+"', iframe, re.IGNORECASE)


def test_visible_results_are_interpolated_not_transcribed():
    forbidden = [
        "cuatro tratadas y cuatro controles",
        "producen 6.75",
        "son 0.75",
        "CATE(0)=1.25",
        "CATE(1)=0.25",
        "es 6.75",
        "sesgo sigue siendo 6",
        "naïve de 0.751",
        "ATE de 0.75",
        "media es 3.941",
    ]
    assert all(phrase not in TEXT for phrase in forbidden)
    assert "`r " in TEXT
    assert "(N)" not in TEXT


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
