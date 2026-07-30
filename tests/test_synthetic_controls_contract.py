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
ADH_2010_URL = (
    "https://economics.mit.edu/sites/default/files/publications/"
    "Synthetic%20Control%20Methods.pdf"
)


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
            r"(?im)^\s*(?:[-*]\s+)?(?:\*\*)?Respuesta esperada(?:\*\*)?\s*:\s*(.+)$",
            section,
        )
        assert answer, label
        phrase = re.sub(r"\s+", " ", answer.group(1)).strip()
        assert len(phrase) >= 20, label
        phrases.append(phrase)
    return phrases


def test_private_answer_parser_reads_a_normal_key_line():
    key = "SC-T1\nRespuesta esperada: La trayectoria debe mantener ajuste previo sólido.\n"
    assert private_answer_phrases(key, ["SC-T1"]) == [
        "La trayectoria debe mantener ajuste previo sólido."
    ]


def test_pair_is_inserted_before_iv_with_stable_anchors():
    book = base.parse_rmd_files(ROOT / "_bookdown.yml")
    assert book.index("17-SyntheticControls.Rmd") < book.index("17-SyntheticControlsStata.Rmd") < book.index("18-IV.Rmd")
    assert read(THEORY).startswith("# Controles sintéticos — Clase teórica {#controles-sinteticos}")
    assert read(PRACTICE).startswith("# Controles sintéticos — Clase empírica {#controles-sinteticos-stata}")


def test_practice_starts_with_materials_readings_and_goals():
    text = read(PRACTICE)
    assert text.index("## Materiales para la clase") < text.index("**Lecturas centrales**") < text.index("**Metas de aprendizaje**") < 5000


def test_materials_are_tracked_reproducible_and_expose_no_license_metadata():
    text = read(PRACTICE)
    materials = text[
        text.index("## Materiales para la clase") : text.index("**Lecturas centrales**")
    ]
    targets = re.findall(r"\[[^]]+\]\(([^)]+)\)", materials)
    assert targets == [
        "dofile/17_SyntheticControls/01_synthetic_controls.do",
        "dofile/17_SyntheticControls/synth_smoking.dta",
    ]
    assert ".log" not in text.lower()

    tracked = set(
        subprocess.run(
            ["git", "ls-files"], cwd=ROOT, check=True, capture_output=True, text=True
        ).stdout.splitlines()
    )
    forbidden = (b"stata license:", b"serial number:", b"licensed to:")
    for target in targets:
        assert target in tracked, target
        payload = (ROOT / target).read_bytes().lower()
        assert not any(marker in payload for marker in forbidden), target


def test_both_central_reading_blocks_link_the_primary_adh_2010_article():
    for page in (THEORY, PRACTICE):
        text = read(page)
        block = text[
            text.index("**Lecturas centrales**") : text.index(
                "**Metas de aprendizaje**"
            )
        ]
        assert "Abadie, Diamond y Hainmueller (2010)" in block, page.name
        assert ADH_2010_URL in block, page.name


def test_theory_covers_identification_support_and_inference():
    text = read(THEORY).lower()
    for marker in ["y(d=1)", "y(d=0)", "envolvente convexa", "no anticipación", "interferencia", "rmspe", "placebo", "leave-one-out"]:
        assert marker in text, marker
    assert read(THEORY).count("::: {.boxexam}") == 3


def test_theory_separates_predictor_averaging_v_loss_and_optimization():
    text = read(THEORY).lower()
    for marker in ("xperiod", "mspeperiod", "nested", "in-sample"):
        assert marker in text, marker
    assert "periodos pretratamiento reservados" not in text


def test_dofile_uses_real_synth_for_canonical_estimate():
    do = read(DOFILE)
    normalized = re.sub(r"\s+", " ", re.sub(r"\s*///\s*", " ", do))
    canonical_command = (
        "synth cigsale beer(1984(1)1988) lnincome retprice age15to24 "
        "cigsale(1988) cigsale(1980) cigsale(1975), trunit(3) "
        "trperiod(1989) xperiod(1980(1)1988) mspeperiod(1970(1)1988) "
        "nested keep(`main_native') replace"
    )
    assert canonical_command in normalized
    assert "california_synth_native.dta" not in do
    for marker in ["synth_weights.csv", "synth_predictor_balance.csv", "synth_v_weights.csv", "synth_paths.csv", "synth_rmspe.csv"]:
        assert marker in do, marker


def test_sample_audit_proves_panel_balance_and_predictor_availability():
    audit = rows("synth_sample_audit.csv")
    assert list(audit[0]) == [
        "analysis",
        "variable",
        "window",
        "expected",
        "observed",
        "missing",
        "pass",
    ]
    observed = {
        (row["analysis"], row["variable"], row["window"]): (
            int(row["expected"]),
            int(row["observed"]),
            int(row["missing"]),
            int(row["pass"]),
        )
        for row in audit
    }
    expected = {
        ("panel", "states", "1970-2000"): (39, 39, 0, 1),
        ("panel", "years", "1970-2000"): (31, 31, 0, 1),
        ("panel", "unit_years", "1970-2000"): (1209, 1209, 0, 1),
        ("donor_pool", "eligible_donors", "1970-2000"): (38, 38, 0, 1),
        ("main", "cigsale", "1970-2000"): (1209, 1209, 0, 1),
        ("main", "beer", "1984-1988"): (195, 195, 0, 1),
        ("main", "lnincome", "1980-1988"): (351, 351, 0, 1),
        ("main", "retprice", "1980-1988"): (351, 351, 0, 1),
        ("main", "age15to24", "1980-1988"): (351, 351, 0, 1),
        ("main", "cigsale", "1975"): (39, 39, 0, 1),
        ("main", "cigsale", "1980"): (39, 39, 0, 1),
        ("main", "cigsale", "1988"): (39, 39, 0, 1),
        ("time_placebo", "lnincome", "1972-1979"): (312, 312, 0, 1),
        ("time_placebo", "retprice", "1972-1979"): (312, 312, 0, 1),
        ("time_placebo", "age15to24", "1972-1979"): (312, 312, 0, 1),
        ("time_placebo", "cigsale", "1970"): (39, 39, 0, 1),
        ("time_placebo", "cigsale", "1975"): (39, 39, 0, 1),
        ("time_placebo", "cigsale", "1979"): (39, 39, 0, 1),
    }
    assert observed == expected


def test_v_weights_match_predictors_and_are_visible_in_practice():
    balance = rows("synth_predictor_balance.csv")
    v_weights = rows("synth_v_weights.csv")
    assert list(v_weights[0]) == ["predictor", "importance"]
    assert [row["predictor"] for row in v_weights] == [
        row["predictor"] for row in balance
    ]
    values = [float(row["importance"]) for row in v_weights]
    assert all(math.isfinite(value) and value >= 0 for value in values)
    assert math.isclose(sum(values), 1.0, abs_tol=1e-8)
    page = read(PRACTICE).lower()
    assert "synth_v_weights.csv" in page
    assert "importancia" in page and "lnincome" in page


def test_practice_covers_complete_diagnostics():
    do = read(DOFILE)
    page = read(PRACTICE)
    for marker in ["synth cigsale", "trunit(3)", "trperiod(1989)", "synth_weights.csv", "synth_predictor_balance.csv", "synth_paths.csv", "synth_rmspe.csv", "synth_placebos.csv", "synth_leave_one_out.csv"]:
        assert marker in do, marker
        assert marker in page, marker
    assert "promedio simple" in page.lower()
    assert read(PRACTICE).count("::: {.boxexam}") == 4


def test_practice_documents_sample_timing_versions_and_rmspe_precision():
    text = re.sub(r"\s+", " ", read(PRACTICE).lower())
    for marker in (
        "noviembre de 1988",
        "enero de 1989",
        "distrito de columbia",
        "38 donantes",
        "1970--2000",
        "convención docente",
        "5\\times mspe",
        "statanow/se 19.5",
        "synth 0.0.7",
        "1.756235",
        "1.754306",
    ):
        assert marker in text, marker


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
    canonical_states = {"Colorado", "Connecticut", "Montana", "Nevada", "Utah"}
    canonical_weight = sum(float(r["weight"]) for r in weights if r["state"] in canonical_states)
    assert canonical_weight > 0.99
    paths = rows("synth_paths.csv")
    assert max(abs(float(r["synthetic"]) - float(r["manual_synthetic"])) for r in paths) < 1e-8


def test_rmspe_and_placebo_filter_are_reproducible():
    rmspe = rows("synth_rmspe.csv")
    assert list(rmspe[0]) == [
        "unit",
        "pre_rmspe",
        "post_rmspe",
        "ratio",
        "native_pre_rmspe",
        "native_minus_recomputed",
    ]
    ca = next(r for r in rmspe if r["unit"] == "California")
    assert float(ca["pre_rmspe"]) > 0
    assert math.isclose(float(ca["ratio"]), float(ca["post_rmspe"]) / float(ca["pre_rmspe"]), rel_tol=1e-9)
    assert math.isclose(
        float(ca["native_minus_recomputed"]),
        float(ca["native_pre_rmspe"]) - float(ca["pre_rmspe"]),
        abs_tol=1e-12,
    )
    assert 0 < float(ca["native_minus_recomputed"]) < 0.01
    placebos = rows("synth_placebos.csv")
    cutoff = 5 * float(ca["pre_rmspe"])
    assert all((r["eligible"] == "1") == (float(r["pre_rmspe"]) <= cutoff) for r in placebos)


def test_placebos_cover_donors_and_leave_one_out_covers_positive_weights():
    placebos = rows("synth_placebos.csv")
    assert len({r["unit"] for r in placebos}) == 39
    assert sum(r["unit"] == "California" for r in placebos) == 1
    assert all(
        math.isfinite(float(r[field]))
        for r in placebos
        for field in ("pre_rmspe", "post_rmspe", "ratio")
    )
    fallback = [r for r in placebos if r["optimization"] != "nested"]
    optimization_contract = (
        not fallback
        and sum(r["optimization"] == "nested" for r in placebos) == 39
    ) or (
        [(r["unit"], r["optimization"]) for r in fallback]
        == [("Utah", "default_fallback_after_rc430")]
        and sum(r["optimization"] == "nested" for r in placebos) == 38
    )
    assert optimization_contract
    do = read(DOFILE)
    assert "assert r(N) == 38" not in do
    positive = {
        r["state"] for r in rows("synth_weights.csv") if float(r["weight"]) > 1e-8
    }
    loo = {r["omitted_state"] for r in rows("synth_leave_one_out.csv")}
    assert loo == positive


def test_time_placebo_and_leave_one_out_gaps_are_complete_and_finite():
    time_placebo = rows("synth_time_placebo.csv")
    assert [int(r["year"]) for r in time_placebo] == list(range(1970, 1989))
    assert all(math.isfinite(float(r["gap"])) for r in time_placebo)
    leave_one_out = rows("synth_leave_one_out.csv")
    assert all(math.isfinite(float(r["gap"])) for r in leave_one_out)
    assert all(
        len([r for r in leave_one_out if r["omitted_state"] == state]) == 31
        for state in {r["omitted_state"] for r in leave_one_out}
    )


def test_time_placebo_fit_cannot_use_post_1979_information():
    normalized = re.sub(r"\s+", " ", re.sub(r"\s*///\s*", " ", read(DOFILE)))
    command = (
        "synth cigsale lnincome retprice age15to24 cigsale(1979) "
        "cigsale(1975) cigsale(1970), trunit(3) trperiod(1980) "
        "xperiod(1972(1)1979) mspeperiod(1970(1)1979) nested"
    )
    assert command in normalized
