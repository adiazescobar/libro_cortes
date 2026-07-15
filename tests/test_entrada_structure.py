from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
TEXT = (ROOT / "00-PruebaEntrada.Rmd").read_text(encoding="utf-8")


def test_quiz_never_installs_during_render():
    assert "install.packages" not in TEXT
    assert 'stop("Falta el paquete webexercises' in TEXT


def test_instructions_precede_quiz():
    assert TEXT.index("## Antes de comenzar {-}") < TEXT.index(
        '<div id="prueba-entrada-quiz">'
    )
    for phrase in [
        "20 preguntas",
        "15–20 minutos",
        "diagnóstica",
        "sin consultar materiales",
    ]:
        assert phrase in TEXT


def test_scoring_contract_is_present():
    assert 'id="btn-finalizar"' in TEXT
    assert 'id="score-result"' in TEXT
    assert "Estadística básica" in TEXT
    assert "Regresión lineal" in TEXT
