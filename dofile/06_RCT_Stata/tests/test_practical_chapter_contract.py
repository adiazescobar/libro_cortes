import csv
from pathlib import Path


ROOT = Path(__file__).resolve().parents[3]
CHAPTER = ROOT / "06-RCT2.Rmd"


def _columns(path):
    with path.open(newline="", encoding="utf-8-sig") as handle:
        return set(next(csv.reader(handle)))


def test_materials_precede_empirical_question():
    text = CHAPTER.read_text(encoding="utf-8")
    assert "## Materiales para la clase {-}" in text
    assert text.index("## Materiales para la clase {-}") < text.index(
        "::: {.box-stata}"
    )
    assert text.index("## Materiales para la clase {-}") < text.index(
        "## Pregunta empírica {-}"
    )
    assert "## DESCARGA LOS DOCUMENTOS {-}" not in text


def test_download_targets_exist():
    required = [
        "clase6_stata.do",
        "clase6_R.R",
        "clase6_python.ipynb",
        "data.dta",
        "results/resultados_stata.csv",
        "results/verificacion_stata_python.csv",
    ]
    base = ROOT / "dofile/06_RCT_Stata"
    assert all((base / path).is_file() for path in required)


def test_chapter_reads_canonical_stata_results():
    text = CHAPTER.read_text(encoding="utf-8")
    assert 'read.csv("dofile/06_RCT_Stata/results/resultados_stata.csv"' in text


def test_canonical_stata_tables_have_required_columns():
    results = ROOT / "dofile/06_RCT_Stata/results"
    assert {
        "variable",
        "media_tratado",
        "media_control",
        "diferencia",
        "p_value",
    } <= _columns(results / "balance_stata.csv")
    assert {
        "modelo",
        "termino",
        "coeficiente",
        "error_estandar",
        "N",
        "R2",
    } <= _columns(results / "resultados_stata.csv")
    assert {
        "moderador",
        "termino",
        "coeficiente",
        "error_estandar",
        "N",
    } <= _columns(results / "heterogeneidad_stata.csv")
