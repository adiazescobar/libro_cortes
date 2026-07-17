import re

import pytest

import test_power_pedagogy_contract as base


ROOT = base.ROOT

EXPECTED = {
    "03-Parametros.Rmd": ("Parámetros causales — Clase teórica", "parametros-causales-teoria"),
    "04-ParametrosStata.Rmd": (
        "Parámetros causales — Clase empírica",
        "parametros-causales-stata",
    ),
    "05-RCT.Rmd": ("Experimentos aleatorizados — Clase teórica", None),
    "06-RCT2.Rmd": ("Experimentos aleatorizados — Clase empírica", None),
    "07-POWER-Teoria.Rmd": (
        "Poder estadístico — Clase teórica",
        "poder-estadistico-teoria",
    ),
    "07-POWER.Rmd": ("Poder estadístico — Clase empírica", "poder-estadistico-stata"),
    "08-DID.Rmd": ("Diferencias en diferencias — Clase teórica", "did-teoria"),
    "08-DIDStata.Rmd": ("Diferencias en diferencias — Clase empírica", "did-stata"),
    "09-BadControls.Rmd": ("Malos controles — Clase teórica", "bad-controls-teoria"),
    "10-BadControlsStata.Rmd": (
        "Malos controles — Clase empírica",
        "bad-controls-stata",
    ),
}

H1 = re.compile(r"^#\s+(.+?)(?:\s+\{#([^}\s]+)[^}]*\})?\s*$", re.MULTILINE)


def _first_h1(text):
    match = H1.search(text)
    assert match, "El capítulo debe tener un H1"
    return match.group(1).strip(), match.group(2)


@pytest.mark.parametrize(("filename", "expected"), EXPECTED.items())
def test_chapter_has_exact_title_and_stable_anchor(filename, expected):
    assert _first_h1(base._read(ROOT / filename)) == expected


@pytest.mark.parametrize("filename", EXPECTED)
def test_chapter_h2_to_h4_delegate_numbering_to_bookdown(filename):
    text = base._read(ROOT / filename)
    headings = [
        heading
        for level in (2, 3, 4)
        for heading in base._headings(text, level)
    ]
    assert headings, f"{filename} debe contener encabezados H2–H4"
    base._assert_no_manual_numbering(text)


def test_manual_numbering_contract_rejects_a_numbered_subtitle():
    with pytest.raises(AssertionError):
        base._assert_no_manual_numbering("## 1. Subtítulo")
