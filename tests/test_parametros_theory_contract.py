from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
TEXT = (ROOT / "03-Parametros.Rmd").read_text(encoding="utf-8")


def test_theory_sections_follow_approved_order():
    headings = [
        "## Objetivos y lecturas {-}",
        "## Pregunta causal y población de interés {-}",
        "## Resultados potenciales {-}",
        "## El problema fundamental {-}",
        "## ATE, ATT, ATU y CATE {-}",
        "## Diferencia observada y sesgo de selección {-}",
        "## Supuestos de identificación {-}",
        "## Comparación antes-después {-}",
        "## SUTVA {-}",
        "## Síntesis {-}",
        "## Ejercicios {-}",
        "## Puente a la práctica {-}",
        "## Referencias {-}",
    ]
    positions = [TEXT.index(heading) for heading in headings]
    assert positions == sorted(positions)


def test_cate_and_aggregation_are_present():
    assert "CATE(x)" in TEXT
    assert "ATE=\\mathbb{E}[CATE(X_i)]" in TEXT
    assert "ATE=P(D_i=1)ATT+P(D_i=0)ATU" in TEXT


def test_theory_keeps_videos_but_not_long_chatgpt_prompt():
    assert TEXT.count("youtube.com/embed/") == 2
    assert "PROMPT DE CHATGPT PARA REFLEXIÓN PROFUNDA" not in TEXT
    for phrase in ["estimando", "contrafactual faltante", "supuesto", "amenazas"]:
        assert phrase in TEXT.lower()
