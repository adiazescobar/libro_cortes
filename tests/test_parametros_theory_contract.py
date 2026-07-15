import re
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
TEXT = (ROOT / "03-Parametros.Rmd").read_text(encoding="utf-8")
COMPACT = re.sub(r"\s+", "", TEXT)


def _section_body(title):
    match = re.search(rf"^(?P<marks>##{{1,3}})\s+{title}[^\n]*$", TEXT, re.MULTILINE | re.IGNORECASE)
    assert match, f"Falta una sección titulada {title}"
    level = len(match.group("marks"))
    following = re.search(rf"^#{{1,{level}}}\s+", TEXT[match.end():], re.MULTILINE)
    end = match.end() + following.start() if following else len(TEXT)
    return TEXT[match.end():end]


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


def test_all_four_causal_parameters_are_fully_defined():
    definitions = [
        r"ATE=\mathbb{E}[Y_i(D=1)-Y_i(D=0)]",
        r"ATT=\mathbb{E}[Y_i(D=1)-Y_i(D=0)\midD_i=1]",
        r"ATU=\mathbb{E}[Y_i(D=1)-Y_i(D=0)\midD_i=0]",
        r"CATE(x)=\mathbb{E}[Y_i(D=1)-Y_i(D=0)\midX_i=x]",
    ]
    for definition in definitions:
        assert definition in COMPACT


def test_theory_has_no_download_section_or_downloadable_file_links():
    assert not re.search(r"^##\s+.*(?:materiales|descargas)", TEXT, re.MULTILINE | re.IGNORECASE)
    downloadable_link = re.compile(
        r"\[[^]]+]\([^)]*\.(?:do|dta|r|ipynb|csv|log|zip|xlsx?)(?:[?#][^)]*)?\)",
        re.IGNORECASE,
    )
    assert downloadable_link.search(TEXT) is None
    assert not re.search(r"<a\b[^>]*(?:\bdownload\b|href=[\"'][^\"']*\.(?:do|dta|r|ipynb|csv|log|zip|xlsx?))", TEXT, re.IGNORECASE)


def test_theory_keeps_original_videos_and_has_a_brief_activity():
    video_urls = re.findall(r'https://www\.youtube\.com/embed/[A-Za-z0-9_-]+', TEXT)
    assert video_urls == [
        "https://www.youtube.com/embed/ln5LBKiF8hE",
        "https://www.youtube.com/embed/iPBV3BlV7jk",
    ]
    assert "PROMPT DE CHATGPT PARA REFLEXIÓN PROFUNDA" not in TEXT
    activity = _section_body(r"Actividad(?: breve)?")
    assert len(activity.strip()) <= 1_200
    for phrase in ["estimando", "contrafactual faltante", "supuesto", "dos amenazas"]:
        assert phrase in activity.lower()


def test_both_theory_iframes_have_descriptive_titles():
    iframes = re.findall(r"<iframe\b[^>]*>", TEXT, re.IGNORECASE)
    assert len(iframes) == 2
    for iframe in iframes:
        title = re.search(r'\btitle="([^"]+)"', iframe, re.IGNORECASE)
        assert title and len(title.group(1).split()) >= 2
