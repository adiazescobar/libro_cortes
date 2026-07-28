from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
THEORY = (ROOT / "13-PSM.Rmd").read_text(encoding="utf-8")
PRACTICE = (ROOT / "14-PSMStata.Rmd").read_text(encoding="utf-8")
PRIVATE_KEY = ROOT / "claves_privadas/13_PSM_clave.md"


def test_psm_titles_follow_course_contract():
    assert THEORY.startswith("# Propensity score matching — Clase teórica {#psm}")
    assert PRACTICE.startswith("# Propensity score matching — Clase empírica {#psm-stata}")


def test_theory_states_identification_before_algorithm():
    cia = THEORY.index("## Identificación")
    algorithm = THEORY.index("## Algoritmos de emparejamiento")
    assert cia < algorithm
    assert "Y(D=1)" in THEORY and "Y(D=0)" in THEORY
    assert "balance no demuestra" in THEORY.lower()


def test_theory_contains_modern_psm_warning_and_valid_inference():
    assert "King y Nielsen" in THEORY
    assert "Abadie e Imbens" in THEORY
    assert "bootstrap" in THEORY.lower()
    assert "no es, en general, válido" in THEORY.lower()
    assert "logit del propensity score" in THEORY.lower()


def test_practice_downloads_appear_before_first_substantive_section():
    download = PRACTICE.index("## Materiales de la clase")
    workflow = PRACTICE.index("## Pregunta causal")
    assert download < workflow
    assert "01_psm_stata_CLASSROOM.do" in PRACTICE
    assert "base6.dta" in PRACTICE


def test_practice_uses_psmatch2_as_main_and_teffects_as_check():
    assert PRACTICE.count("psmatch2") >= 6
    assert "teffects psmatch" in PRACTICE
    assert "No esperamos igualdad mecánica" in PRACTICE
    assert "estimando" in PRACTICE.lower()
    assert "empates" in PRACTICE.lower()


def test_practice_has_causal_lasso_guardrails():
    assert "telasso" in PRACTICE
    assert "confusores obligatorios" in PRACTICE.lower()
    assert "no descubre los confusores" in PRACTICE.lower()
    assert "pretratamiento" in PRACTICE.lower()


def test_practice_shows_outputs_graphs_and_exam_questions():
    assert PRACTICE.count("```text") >= 3
    assert PRACTICE.count("::: {.boxexam}") >= 2
    assert "pscore_distribution.png" in PRACTICE
    assert "psm_balance.png" in PRACTICE
    assert "respuestas desplegables" not in PRACTICE.lower()


def test_no_obsolete_balance_or_bootstrap_advice():
    combined = THEORY + PRACTICE
    assert "< 20% es aceptable" not in combined
    assert "Bootstrap (recomendado)" not in combined
    assert "bwidth() para bootstrap" not in combined


def test_private_key_exists_but_is_not_published():
    assert PRIVATE_KEY.is_file()
    assert "Uso exclusivo de la profesora y el monitor" in PRIVATE_KEY.read_text(
        encoding="utf-8"
    )
    bookdown = (ROOT / "_bookdown.yml").read_text(encoding="utf-8")
    assert PRIVATE_KEY.name not in bookdown
