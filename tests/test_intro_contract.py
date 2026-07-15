from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
TEXT = (ROOT / "01-intro.Rmd").read_text(encoding="utf-8")


def test_intro_sections_are_ordered():
    headings = [
        "## Objetivos y mapa del capítulo {-}",
        "## Por qué importa la inferencia causal {-}",
        "## Cómo formular una pregunta causal {-}",
        "## Contrafactual y resultados potenciales {-}",
        "## El problema fundamental {-}",
        "## Diferencia observada y sesgo de selección {-}",
        "## Comparaciones que no identifican causalidad {-}",
        "## Estrategias del curso {-}",
        "## Mapa del libro y puente a Stata {-}",
    ]
    positions = [TEXT.index(h) for h in headings]
    assert positions == sorted(positions)


def test_intro_factual_and_copy_fixes():
    assert "México, 1990" not in TEXT
    assert "Lanzado en México en 1997" in TEXT
    assert "Por últimpo" not in TEXT
    assert "## DESCARGA" not in TEXT


def test_rdd_diagnostic_states_both_identification_conditions():
    assert (
        "¿Es plausible la ausencia de ordenamiento preciso alrededor del umbral y son continuos los "
        "resultados potenciales?"
    ) in TEXT
    assert "¿Hay ordenamiento preciso alrededor del umbral" not in TEXT


def test_intro_bridge_follows_book_chronology():
    assert "el próximo capítulo introduce Stata" in TEXT
    assert "A continuación, el capítulo **Parámetros causales**" in TEXT


def test_intro_does_not_expose_slide_layout_markup():
    assert ".pull-left[" not in TEXT
    assert ".pull-right[" not in TEXT
