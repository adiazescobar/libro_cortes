import csv
import re
from collections import Counter
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
RMD = (ROOT / "00-PruebaEntrada.Rmd").read_text(encoding="utf-8")
AUDIT = ROOT / "docs/audits/prueba_entrada_academica.csv"


def test_quiz_has_twenty_balanced_questions():
    numbers = re.findall(r'question-number">Pregunta (\d+)\.', RMD)
    assert numbers == [str(i) for i in range(1, 21)]
    sections = re.findall(r'<div class="quiz-section" data-section="([^"]+)">', RMD)
    assert sections == ["Estadística básica", "Regresión lineal", "Causalidad", "Stata"]


def test_academic_audit_is_complete():
    with AUDIT.open(newline="", encoding="utf-8") as handle:
        rows = list(csv.DictReader(handle))
    assert len(rows) == 20
    assert Counter(row["seccion"] for row in rows) == {
        "Estadística básica": 5,
        "Regresión lineal": 5,
        "Causalidad": 5,
        "Stata": 5,
    }
    assert all(row["estado"] == "aprobada" for row in rows)
    assert all(row["clave"] and row["justificacion"] for row in rows)


def test_question_18_has_one_answer_aligned_with_audit():
    question = re.search(
        r"Pregunta 18\..*?</div>",
        RMD,
        flags=re.DOTALL,
    ).group()
    assert '`r fitb("regress Y X1 X2", width = 25, ignore_case = TRUE)`' in question
    assert "fitb(c(" not in question

    with AUDIT.open(newline="", encoding="utf-8") as handle:
        rows = list(csv.DictReader(handle))
    assert rows[17]["clave"] == "regress Y X1 X2"


def test_question_14_defines_counterfactual_as_unobserved_alternative_condition():
    question = re.search(r"Pregunta 14\..*?</div>", RMD, flags=re.DOTALL).group()
    expected = "Lo que habría ocurrido con la misma unidad bajo la condición alternativa"
    assert f'answer = "{expected}"' in question
    with AUDIT.open(newline="", encoding="utf-8") as handle:
        rows = list(csv.DictReader(handle))
    assert rows[13]["clave"] == expected
