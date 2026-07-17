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

H1 = re.compile(r"^#\s+(.+?)(?:\s+\{#([^}\s]+)[^}]*\})?\s*$")
FENCE = re.compile(r"^[ \t]{0,3}(`{3,}|~{3,})")


def _first_h1(text):
    fence_character = None
    fence_length = 0
    for line in text.splitlines():
        fence = FENCE.match(line)
        if fence:
            marker = fence.group(1)
            if fence_character is None:
                fence_character = marker[0]
                fence_length = len(marker)
                continue
            if marker[0] == fence_character and len(marker) >= fence_length:
                fence_character = None
                fence_length = 0
                continue
        if fence_character is None:
            match = H1.match(line)
            if match:
                return match.group(1).strip(), match.group(2)
    raise AssertionError("El capítulo debe tener un H1 real")


def _assert_title_and_anchor(text, expected):
    assert _first_h1(text) == expected


@pytest.mark.parametrize(("filename", "expected"), EXPECTED.items())
def test_chapter_has_exact_title_and_stable_anchor(filename, expected):
    _assert_title_and_anchor(base._read(ROOT / filename), expected)


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


@pytest.mark.parametrize("fence", ["```", "~~~"])
def test_first_real_h1_ignores_fenced_examples(fence):
    text = (
        f"{fence}markdown\n"
        "# Título ilustrativo {#no-es-anchor}\n"
        f"{fence}\n"
        "# Título real {#anchor-real}\n"
    )
    assert _first_h1(text) == ("Título real", "anchor-real")


def test_title_contract_rejects_a_title_only_mutation():
    expected = ("Título correcto", "anchor-estable")
    with pytest.raises(AssertionError):
        _assert_title_and_anchor("# Título incorrecto {#anchor-estable}", expected)


def test_title_contract_rejects_an_anchor_only_mutation():
    expected = ("Título correcto", "anchor-estable")
    with pytest.raises(AssertionError):
        _assert_title_and_anchor("# Título correcto {#anchor-cambiado}", expected)
