import re
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]


def test_all_chapters_use_class_potential_outcomes_notation():
    forbidden = re.compile(r"Y(?:_i)?\((?:1|0)\)")
    offenders = {}
    for path in ROOT.glob("*.Rmd"):
        matches = forbidden.findall(path.read_text(encoding="utf-8"))
        if matches:
            offenders[path.name] = matches
    assert offenders == {}
