import os
import re
import subprocess
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
THEORY_PATH = ROOT / "07-POWER-Teoria.Rmd"
PRACTICE_PATH = ROOT / "07-POWER.Rmd"
BOOKDOWN = ROOT / "_bookdown.yml"

PRACTICE_REQUIRED = [
    "power twomeans",
    "power twoproportions",
    "cumplimiento parcial",
    "cluster m1(",
    "Externalidades y efectos de derrame",
    "Belmont Report",
    "Bertrand y Mullainathan",
    "07_stata.do",
    "07_R.R",
    "07_phyton.ipynb",
]


def _read(path):
    assert path.is_file(), f"Falta el archivo requerido: {path.name}"
    return path.read_text(encoding="utf-8")


def parse_rmd_files(path):
    text = _read(path)
    match = re.search(r"(?ms)^rmd_files:\s*\n(?P<body>(?:\s+-\s+.*\n?)+)", text)
    assert match, "_bookdown.yml debe declarar rmd_files"
    return [
        re.split(r"\s+#", line, maxsplit=1)[0].strip().strip("'\"")
        for line in re.findall(r"^\s+-\s+(.+)$", match.group("body"), re.MULTILINE)
    ]


def _boxes(text):
    boxes = []
    lines = text.splitlines()
    index = 0
    while index < len(lines):
        opening = re.match(r"^(?P<fence>:{3,})\s+\{[^}]*\.box[^}]*\}\s*$", lines[index])
        if not opening:
            index += 1
            continue
        fence = opening.group("fence")
        end = index + 1
        while end < len(lines) and lines[end].strip() != fence:
            end += 1
        assert end < len(lines), "Bloque pedagógico sin cierre"
        boxes.append("\n".join(lines[index + 1:end]))
        index = end + 1
    return boxes


def question_codes(text, family):
    return re.findall(rf"(?<![A-Z0-9-]){re.escape(family)}\d+(?![A-Z0-9-])", text)


def _question_boxes(text, family, expected):
    assert question_codes(text, family) == expected
    pattern = re.compile(rf"(?<![A-Z0-9-]){re.escape(family)}\d+(?![A-Z0-9-])")
    selected = [box for box in _boxes(text) if pattern.search(box)]
    assert len(selected) == len(expected), "Cada pregunta debe vivir en una caja independiente"
    assert [pattern.findall(box) for box in selected] == [[code] for code in expected]
    return selected


def _has_disclosed_answer(block):
    label = re.compile(
        r"(?im)^\s*(?:>\s*)?(?:[-*+]\s+)?(?:#{1,6}\s+)?"
        r"(?:\*\*|__)?(?:respuesta|solución|pista)(?:\*\*|__)?\s*"
        r"(?::|\.|=|correcta\b|es\b)|\bla\s+respuesta\s+es\b|"
        r"\bsolución\s+correcta\b"
    )
    lowered = block.casefold()
    return bool(label.search(block)) or any(
        marker in lowered for marker in ["<details", "hide(", "ver respuesta"]
    )


def _metadata_once(block, label):
    pattern = re.compile(
        rf"(?im)^\s*(?:[-*+]\s+)?(?:\*\*|__)?{re.escape(label)}"
        rf"(?:\*\*|__)?\s*:"
    )
    assert len(pattern.findall(block)) == 1, f"Cada pregunta exige una línea {label}:"


def _headings(text, level):
    return [
        re.sub(r"\s*\{[^}]*\}\s*$", "", value).strip()
        for value in re.findall(rf"^#{{{level}}}\s+(.+)$", text, re.MULTILINE)
    ]


def _assert_no_manual_numbering(text):
    forbidden = re.compile(
        r"^#{2,4}\s+(?:(?:PASO|Paso|Etapa)\b(?:\s+\d+)?|\d+\s*[.):_-])",
        re.MULTILINE,
    )
    assert not forbidden.search(text), "Bookdown, no el Rmd, debe numerar encabezados"


def _private_exposure_counts(paths, contents, tokens):
    lowered_tokens = [token.casefold() for token in tokens if token]
    path_hits = sum(
        any(token in path.casefold() for token in lowered_tokens) for path in paths
    )
    content_hits = sum(
        any(token in content.casefold() for token in lowered_tokens)
        for content in contents.values()
    )
    return path_hits, content_hits


def test_current_power_practice_content_is_preserved():
    practice = _read(PRACTICE_PATH)
    missing = [fragment for fragment in PRACTICE_REQUIRED if fragment not in practice]
    assert not missing, f"Se perdieron {len(missing)} fragmentos distintivos del capítulo"


def test_power_theory_precedes_practice_in_bookdown():
    files = parse_rmd_files(BOOKDOWN)
    assert files.index("07-POWER-Teoria.Rmd") + 1 == files.index("07-POWER.Rmd")
    assert files.index("07-POWER.Rmd") < files.index("08-DID.Rmd")


def test_power_chapters_have_unique_titles_and_anchors():
    theory = _read(THEORY_PATH)
    practice = _read(PRACTICE_PATH)
    title_pattern = re.compile(r"^#\s+(.+?)(?:\s+\{#([^} ]+)[^}]*\})?\s*$", re.MULTILINE)
    theory_title = title_pattern.search(theory)
    practice_title = title_pattern.search(practice)
    assert theory_title and practice_title
    assert theory_title.group(1) != practice_title.group(1)
    assert theory_title.group(2) and practice_title.group(2)
    assert theory_title.group(2) != practice_title.group(2)
    assert practice_title.group(2) == "poder-estadistico-stata"


def test_power_theory_follows_the_approved_conceptual_sequence():
    theory = _read(THEORY_PATH)
    sequence = [
        "Pregunta causal",
        "Intuición y motivación",
        "Notación, parámetros y estimandos",
        "Supuestos de identificación",
        "Desarrollo teórico y demostraciones",
        "Amenazas, limitaciones y errores comunes",
        "Resumen",
        "Preguntas para clase",
        "Puente a la clase práctica",
        "Referencias",
    ]
    h2 = _headings(theory, 2)
    assert [heading for heading in h2 if heading in sequence] == sequence


def test_power_theory_has_blocks_and_exactly_three_questions():
    theory = _read(THEORY_PATH)
    assert len(_boxes(theory)) >= 8
    blocks = _question_boxes(theory, "POWER-T", ["POWER-T1", "POWER-T2", "POWER-T3"])
    for block in blocks:
        _metadata_once(block, "Puntaje sugerido")
        _metadata_once(block, "Producto esperado")
        assert not _has_disclosed_answer(block)


def test_power_practice_has_between_fourteen_and_eighteen_semantic_stages():
    practice = _read(PRACTICE_PATH)
    h3 = _headings(practice, 3)
    assert 14 <= len(h3) <= 18


def test_power_practice_headings_delegate_numbering_to_bookdown():
    practice = _read(PRACTICE_PATH)
    _assert_no_manual_numbering(practice)


def test_power_practice_has_at_least_twelve_learning_blocks():
    practice = _read(PRACTICE_PATH)
    assert len(_boxes(practice)) >= 12


def test_power_practice_has_exactly_four_self_contained_questions():
    practice = _read(PRACTICE_PATH)
    blocks = _question_boxes(
        practice, "POWER-S", ["POWER-S1", "POWER-S2", "POWER-S3", "POWER-S4"]
    )
    for block in blocks:
        for label in ["Puntaje sugerido", "Comandos permitidos", "Producto esperado"]:
            _metadata_once(block, label)
        assert not _has_disclosed_answer(block)
    hypothetical = [block for block in blocks if "hipotét" in block.casefold()]
    assert all("escenario hipotético" in block.casefold() for block in hypothetical)


def test_answer_detector_rejects_disclosures_but_allows_question_wording():
    assert not _has_disclosed_answer("Justifique su respuesta y proponga una solución.")
    for disclosure in [
        "Respuesta: 800 observaciones.",
        "Solución correcta = aumentar clústeres.",
        "La respuesta es el MDE.",
        "Pista: use power.",
        "<details>",
        "hide(panel)",
    ]:
        assert _has_disclosed_answer(disclosure)


def test_power_theory_headings_delegate_numbering_to_bookdown():
    _assert_no_manual_numbering(_read(THEORY_PATH))


def test_power_practice_reads_canonical_results():
    practice = _read(PRACTICE_PATH)
    assert re.search(
        r"(?:read\.csv|read_csv|read_required_csv)\(\s*[\"']"
        r"dofile/07_Power/results/",
        practice,
    )


def test_power_practice_places_materials_first_instead_of_downloads_last():
    practice = _read(PRACTICE_PATH)
    h2 = _headings(practice, 2)
    assert h2 and h2[0] == "Materiales para la clase"
    assert "DESCARGA LOS DOCUMENTOS" not in h2


def test_student_material_omits_private_identifiers_without_echoing_them():
    fragments = ["clave" + "_power", "clave" + "_poder", "claves" + "_docentes"]
    supplied = os.environ.get("POWER_PRIVATE_IDENTIFIERS", "")
    tokens = fragments + [item for item in supplied.split(os.pathsep) if item]
    tracked = subprocess.run(
        ["git", "ls-files", "-z"], cwd=ROOT, check=True, capture_output=True
    ).stdout.decode("utf-8").split("\0")
    candidates = [BOOKDOWN, *ROOT.glob("*.Rmd"), *ROOT.glob("*.html")]
    docs = ROOT / "docs"
    if docs.exists():
        candidates.extend(path for path in docs.rglob("*") if path.is_file())
    contents = {
        str(path.relative_to(ROOT)): path.read_text(encoding="utf-8", errors="ignore")
        for path in candidates
        if path.is_file()
    }
    path_hits, content_hits = _private_exposure_counts(
        [path for path in tracked if path], contents, tokens
    )
    assert (path_hits, content_hits) == (0, 0), (
        "Hay identificadores privados; "
        f"coincidencias en rutas={path_hits}, contenidos={content_hits}"
    )


def test_private_audit_detects_path_and_content_without_returning_token():
    token = "docente" + "-solo-power"
    assert _private_exposure_counts(
        [f"docs/{token}/index.html"], {"temporal.html": token}, [token]
    ) == (1, 1)
