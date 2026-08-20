from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
PROGRAM = ROOT / "index.Rmd"


def test_programa_includes_resources_and_updated_policies():
    text = PROGRAM.read_text(encoding="utf-8")

    expected_resources = {
        "Ben Lambert – Econometrics on YouTube": "https://www.youtube.com/playlist?list=PLwJRxp3blEvZyQBTTOMFRP_TDaSdly3gU",
        "Mastering Econometrics (MRU)": "https://mru.org/mastering-econometrics",
        "AEA Journal of Economic Perspectives – Classroom": "https://www.aeaweb.org/journals/jep/classroom",
        "Google Dataset Search": "https://toolbox.google.com/datasetsearch",
        "Stata Cheat Sheets": "https://geocenter.github.io/StataTraining/pdf/AllCheatSheets.pdf",
        "Seeing Theory – Visual Probability": "https://seeing-theory.brown.edu/",
    }

    assert "## Recursos adicionales {-}" in text
    for label, url in expected_resources.items():
        assert f"[{label}]({url})" in text

    assert (
        "Este curso da la bienvenida a personas de todas las edades, orígenes, "
        "creencias, etnias, géneros, identidades, orientaciones sexuales y capacidades. "
        "Se espera un ambiente respetuoso e inclusivo."
    ) in text
    assert (
        "La Universidad Javeriana fomenta la honestidad y establece sanciones por fraude "
        "o plagio según el reglamento de estudiantes. Cualquier uso no autorizado de "
        "materiales durante evaluaciones se considera falta grave."
    ) in text

    assert text.index("## Recursos adicionales {-}") < text.index("## Inclusión {-}")
    assert text.index("## Inclusión {-}") < text.index("## Integridad académica {-}")

