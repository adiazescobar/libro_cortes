from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
INTRO = ROOT / "01-intro.Rmd"
RCT_THEORY = ROOT / "05-RCT.Rmd"
RCT_PRACTICE = ROOT / "06-RCT2.Rmd"


def test_intro_chapter_has_explicit_ascii_navigation_anchor():
    first_line = INTRO.read_text(encoding="utf-8").splitlines()[0]
    assert first_line == (
        "# Introducción a la inferencia causal "
        "{#introduccion-a-la-inferencia-causal}"
    )


def test_rct_chapters_have_explicit_ascii_navigation_anchors():
    assert RCT_THEORY.read_text(encoding="utf-8").splitlines()[0] == (
        "# Experimentos aleatorizados — Clase teórica "
        "{#experimentos-aleatorizados-clase-teorica}"
    )
    assert RCT_PRACTICE.read_text(encoding="utf-8").splitlines()[0] == (
        "# Experimentos aleatorizados — Clase empírica "
        "{#experimentos-aleatorizados-clase-empirica}"
    )
