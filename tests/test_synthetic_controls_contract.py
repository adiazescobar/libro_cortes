from pathlib import Path
import csv
import math
import re
import subprocess

import test_power_pedagogy_contract as base


ROOT = base.ROOT
THEORY = ROOT / "17-SyntheticControls.Rmd"
PRACTICE = ROOT / "17-SyntheticControlsStata.Rmd"
DOFILE = ROOT / "dofile/17_SyntheticControls/01_synthetic_controls.do"
RESULTS = ROOT / "dofile/17_SyntheticControls/results"
PRIVATE_KEY = Path.home() / "Dropbox/ClasesR/EconometriaAV/claves_privadas/17_SyntheticControls_clave.md"


def read(path):
    return path.read_text(encoding="utf-8")


def rows(name):
    with (RESULTS / name).open(encoding="utf-8", newline="") as handle:
        return list(csv.DictReader(handle))


def private_answer_phrases(key, labels):
    phrases = []
    for label in labels:
        start = key.index(label)
        end = min(
            (key.index(other) for other in labels if key.index(other) > start),
            default=len(key),
        )
        section = key[start:end]
        answer = re.search(
            r"(?im)^\\s*(?:[-*]\\s+)?(?:\\*\\*)?Respuesta esperada(?:\\*\\*)?\\s*:\\s*(.+)$",
            section,
        )
        assert answer, label
        phrase = re.sub(r"\\s+", " ", answer.group(1)).strip()
        assert len(phrase) >= 20, label
        phrases.append(phrase)
    return phrases


def test_pair_is_inserted_before_iv_with_stable_anchors():
    book = base.parse_rmd_files(ROOT / "_bookdown.yml")
    assert book.index("17-SyntheticControls.Rmd") < book.index("17-SyntheticControlsStata.Rmd") < book.index("18-IV.Rmd")
    assert read(THEORY).startswith("# Controles sintéticos — Clase teórica {#controles-sinteticos}")
    assert read(PRACTICE).startswith("# Controles sintéticos — Clase empírica {#controles-sinteticos-stata}")


def test_practice_starts_with_materials_readings_and_goals():
    text = read(PRACTICE)
    assert text.index("## Materiales para la clase") < text.index("**Lecturas centrales**") < text.index("**Metas de aprendizaje**") < 5000


def test_theory_covers_identification_support_and_inference():
    text = read(THEORY).lower()
    for marker in ["y(d=1)", "y(d=0)", "envolvente convexa", "no anticipación", "interferencia", "rmspe", "placebo", "leave-one-out"]:
        assert marker in text, marker
    assert read(THEORY).count("::: {.boxexam}") == 3


def test_practice_uses_real_synth_and_complete_diagnostics():
    do = read(DOFILE)
    page = read(PRACTICE)
    for marker in ["synth cigsale", "trunit(3)", "trperiod(1989)", "synth_weights.csv", "synth_predictor_balance.csv", "synth_paths.csv", "synth_rmspe.csv", "synth_placebos.csv", "synth_leave_one_out.csv"]:
        assert marker in do, marker
        assert marker in page, marker
    assert "promedio simple" in page.lower()
    assert read(PRACTICE).count("::: {.boxexam}") == 4


def test_private_key_stays_outside_repository():
    assert PRIVATE_KEY.is_file()
    assert ROOT not in PRIVATE_KEY.parents
    assert not (ROOT / "claves_privadas/17_SyntheticControls_clave.md").exists()
    theory_labels = ["SC-T1", "SC-T2", "SC-T3"]
    practice_labels = ["SC-S1", "SC-S2", "SC-S3", "SC-S4"]
    theory = read(THEORY)
    practice = read(PRACTICE)
    key = read(PRIVATE_KEY)
    for label in theory_labels:
        assert theory.count(label) == 1, label
        assert practice.count(label) == 0, label
        assert key.count(label) == 1, label
    for label in practice_labels:
        assert practice.count(label) == 1, label
        assert theory.count(label) == 0, label
        assert key.count(label) == 1, label

    assert "Uso exclusivo de la profesora y el monitor" in key
    assert key.count("Respuesta esperada") == len(theory_labels + practice_labels)
    assert key.count("Criterio de calificación") == len(theory_labels + practice_labels)
    private_markers = private_answer_phrases(key, theory_labels + practice_labels)
    tracked = subprocess.run(
        ["git", "ls-files", "-z"], cwd=ROOT, check=True, capture_output=True
    ).stdout.decode("utf-8").split("\0")
    contents = {}
    for relative in tracked:
        path = ROOT / relative
        if not path.is_file():
            continue
        try:
            contents[relative] = path.read_text(encoding="utf-8", errors="ignore")
        except OSError:
            continue
    base._assert_no_private_exposure(
        [path for path in tracked if path], contents, private_markers
    )


def test_weights_are_convex_and_reconstruction_matches_synth():
    weights = rows("synth_weights.csv")
    values = [float(r["weight"]) for r in weights]
    assert values
    assert all(w >= -1e-8 for w in values)
    assert abs(sum(values) - 1) < 1e-6
    paths = rows("synth_paths.csv")
    assert max(abs(float(r["synthetic"]) - float(r["manual_synthetic"])) for r in paths) < 1e-8


def test_rmspe_and_placebo_filter_are_reproducible():
    rmspe = rows("synth_rmspe.csv")
    ca = next(r for r in rmspe if r["unit"] == "California")
    assert float(ca["pre_rmspe"]) > 0
    assert math.isclose(float(ca["ratio"]), float(ca["post_rmspe"]) / float(ca["pre_rmspe"]), rel_tol=1e-9)
    placebos = rows("synth_placebos.csv")
    cutoff = 5 * float(ca["pre_rmspe"])
    assert all((r["eligible"] == "1") == (float(r["pre_rmspe"]) <= cutoff) for r in placebos)
