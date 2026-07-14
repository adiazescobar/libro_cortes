from pathlib import Path


ROOT = Path(__file__).resolve().parents[3]
TEXT = (ROOT / "05-RCT.Rmd").read_text(encoding="utf-8")


def test_theory_sections_are_ordered():
    headings = [
        "## Pregunta causal {-}",
        "## Intuición y motivación {-}",
        "## Notación, parámetros y estimandos {-}",
        "## Supuestos de identificación {-}",
        "## Desarrollo teórico y demostraciones {-}",
        "## Amenazas, limitaciones y errores comunes {-}",
        "## Resumen {-}",
        "## Preguntas para clase {-}",
        "## Puente a la clase práctica {-}",
        "## Referencias {-}",
    ]
    positions = [TEXT.index(heading) for heading in headings]
    assert positions == sorted(positions)


def test_theory_has_no_download_block():
    assert "DESCARGA LOS DOCUMENTOS" not in TEXT
    assert "Materiales para la clase" not in TEXT


def test_core_derivations_remain():
    required = [
        "sesgo de selección",
        r"\text{Cov}(D,M)",
        "RCT simple, sin estratos, sin controles",
        "RCT estratificado + controles adicionales",
        "El truco de centrar (Wooldridge)",
    ]
    assert all(item in TEXT for item in required)
