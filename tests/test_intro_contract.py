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
        "¿Hay ordenamiento preciso alrededor del umbral y son continuos los "
        "resultados potenciales?"
    ) in TEXT
    assert "¿Las unidades pueden manipular la variable de asignación?" not in TEXT
