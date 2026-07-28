from pathlib import Path
import re


ROOT = Path(__file__).resolve().parents[1]
CHAPTER = ROOT / "12-ExactMatching.Rmd"


def _text():
    return CHAPTER.read_text(encoding="utf-8")


def test_introductory_scope_and_stable_title():
    source = _text()
    assert source.startswith(
        "# Emparejamiento exacto — Introducción {#emparejamiento-exacto}"
    )
    assert "Descargue antes de comenzar" not in source
    assert "dofile/12_" not in source


def test_identification_assumptions_are_explicit():
    source = _text().lower()
    for marker in (
        "no confusión condicional",
        "soporte común",
        "sutva",
        "pretratamiento",
    ):
        assert marker in source
    assert "y_i(d=1)" in source
    assert "y_i(d=0)" in source


def test_forbidden_claims_and_legacy_syntax_are_absent():
    source = _text().lower()
    for forbidden in (
        "la única diferencia restante",
        "al menos 5:1",
        "ssc install nnmatch",
        "exact(",
        "tc(att)",
    ):
        assert forbidden not in source


def test_manual_example_makes_the_target_population_change_explicit():
    source = _text().lower()
    for marker in (
        "sin match",
        "población emparejada",
        "att",
        "soporte común",
        r"\mathcal s",
    ):
        assert marker in source


def test_psm_bridge_is_cautious_about_balance_and_identification():
    source = _text().lower()
    assert "puntaje de balance" in source
    assert re.search(r"no (?:garantiza|asegura).{0,80}balance", source)
    assert re.search(r"no (?:garantiza|asegura).{0,80}(?:causal|identific)", source)


def test_two_public_exam_questions_have_no_embedded_answers():
    source = _text()
    assert set(re.findall(r"EXACT-T[12]", source)) == {"EXACT-T1", "EXACT-T2"}
    assert "<details" not in source.lower()
    question_blocks = re.findall(
        r"(?ms)^:::\s*\{\.boxpregunta\}(.*?)^:::\s*$", source
    )
    coded_blocks = [block for block in question_blocks if "EXACT-T" in block]
    assert len(coded_blocks) == 2
    assert all(
        not re.search(r"(?i)respuesta\s*:|solución\s*:|rúbrica\s*:", block)
        for block in coded_blocks
    )
