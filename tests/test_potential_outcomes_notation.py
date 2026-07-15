import re
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
FORBIDDEN = re.compile(
    r"Y\s*(?:_\s*(?:\{[^}\n]+\}|[A-Za-z0-9]+))?\s*\(\s*(?:0|1|[dD])\s*\)",
)


def test_notation_pattern_covers_variants_without_rejecting_valid_time_notation():
    abbreviated = ["Y(1)", "Y_i( 0 )", "Y_j(d)", "Y_{k} ( 1 )", "Y_{it}(d)", "Y(D)", "Y_i(D)"]
    valid = [
        "Y_i(D=1)",
        "Y(D=0)",
        "Y_{it}(D=1)",
        "Y_{it} ( D = 0 )",
        "ttest y, by(D)",
        "rdrobust y Z, fuzzy(D)",
    ]
    assert all(FORBIDDEN.fullmatch(value) for value in abbreviated)
    assert all(FORBIDDEN.search(value) is None for value in valid)


def test_all_chapters_use_class_potential_outcomes_notation():
    offenders = {}
    for path in ROOT.glob("*.Rmd"):
        matches = FORBIDDEN.findall(path.read_text(encoding="utf-8"))
        if matches:
            offenders[path.name] = matches
    assert offenders == {}
