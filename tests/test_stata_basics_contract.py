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


def test_postfile_lesson_matches_canonical_flow():
    assert 'export delimited using "results/stata_basics_results.csv", replace' in RMD
    assert (
        "`postfile` define las variables a guardar (aquí: ejemplo, nombre de la "
        "variable, media y número de observaciones)."
    ) in RMD
    assert "guardar también la desviación estándar (`r(sd)`)" in RMD
    assert "ampliar la definición de `postfile`" in RMD
    assert "guardar también el número de observaciones (`r(N)`)" not in RMD


def test_objectives_and_resources_use_standard_accents():
    assert "Al finalizar este capítulo podrás:" in RMD
    assert "lógica básica" in RMD
    assert "aquí:" in RMD
    assert "capitulo" not in RMD
    assert "podras" not in RMD


def test_language_equivalences_are_executable_and_accurate():
    assert "`display abs(-2)`" in RMD
    assert "`sqrt()` pertenece a R base y no requiere `library()`" in RMD
    assert "En R necesitas `library()` si usas `sqrt`" not in RMD


def test_scalars_cover_numeric_and_string_types():
    assert "Un **scalar** de Stata puede almacenar un valor numérico o una cadena" in RMD
    assert "en este capítulo usamos scalars numéricos" in RMD


def test_elementary_data_workflow_and_final_checklist_are_present():
    for snippet in [
        "clear all",
        "set more off",
        "generate precio_miles",
        "replace precio_miles",
        "keep if",
        "drop precio_miles",
        "Checklist de preparación",
    ]:
        assert snippet in RMD


def test_core_programming_patterns_have_visible_outputs_and_interpretation():
    for heading in [
        "### Patrón completo con macros {-}",
        "### Loop con `foreach` {-}",
        "### Loop con `forvalues` {-}",
        "### Loop con `while` {-}",
        "#### a) Con `args` {-}",
        "#### b) Con `syntax` {-}",
    ]:
        assert heading in RMD
    assert RMD.count("**Salida**") >= 9
    assert RMD.count("**Interpretación**") >= 6
