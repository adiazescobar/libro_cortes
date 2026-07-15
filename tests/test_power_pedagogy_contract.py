import os
import re
import subprocess
from collections import Counter
import hashlib
import json
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[1]
THEORY_PATH = ROOT / "07-POWER-Teoria.Rmd"
PRACTICE_PATH = ROOT / "07-POWER.Rmd"
BOOKDOWN = ROOT / "_bookdown.yml"
PRACTICE_SNAPSHOT = ROOT / "tests/fixtures/power_practice_baseline.json"

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

PRACTICE_INVENTORY = {
    "sections": [
        "Introducción", "Sin controles", "Tamaño de muestra para un efecto dado",
        "Efecto mínimo detectable (MDE) para un N dado",
        "MDE para un N dado con variable binaria", "Incorporando controles",
        "Tamaño de muestra para un efecto dado con covariables",
        "MDE para un N dado con covariables",
        "Tamaño de muestra con cumplimiento parcial (take-up)",
        "Diseños por conglomerados o grupos",
        "Número de clústeres para un efecto y tamaño de clúster dados",
        "Tamaño de clúster para un número dado de clústeres",
        "MDE para un tamaño de clúster y número de clústeres dados",
        "Qué puede salir mal en un RCT",
        "Externalidades y efectos de derrame (*spillovers*)",
        "Efectos de equilibrio general", "Efectos de comportamiento",
        "Problemas éticos: los tres pilares del Belmont Report",
        "Otros problemas comunes",
        "Ejercicio aplicado: Bertrand y Mullainathan (2004)",
        "El experimento", "Preguntas del ejercicio", "Código de referencia",
        "DESCARGA LOS DOCUMENTOS",
    ],
    "formulas_and_cases": [
        r"z_{1-\alpha/2} + z_{1-\beta}",
        r"\delta_{\min}", r"p_1(1 - p_1) + p_2(1 - p_2)",
        r"\sigma_{\text{res}}^2", r"\delta_{\text{efectivo}}",
        r"DE = 1 + \rho(M-1)", r"K_1 = \frac",
        "Hawthorne", "John Henry", "Tuskegee", "Guatemala (1940s)",
        "4.870 hojas de vida ficticias", "Emily, Greg", "Lakisha, Jamal",
        "reg call i.black##i.female", "reg call c.black##c.yearsexp",
    ],
    "link_destinations": [
        "https://www.stata.com/manuals/pss.pdf",
        "https://www.povertyactionlab.org/resource/power-calculations",
        "https://www.hhs.gov/ohrp/regulations-and-policy/belmont-report/index.html",
        "https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/07_Power/BM_parcial.do",
        "https://www.dropbox.com/scl/fi/ephx1kl4opc0q3oxe5ckp/bm.dta?rlkey=zwp0hwtec5z25a4ll9qn8biz7&dl=1",
        "https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/07_Power/07_stata.do",
        "https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/07_Power/07_R.R",
        "https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/07_Power/07_phyton.ipynb",
        "https://colab.research.google.com/assets/colab-badge.svg",
        "https://colab.research.google.com/github/adiazescobar/libro_cortes/blob/main/dofile/07_Power/07_phyton.ipynb",
        "https://raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/Clase0_StataBasics/hh_98.dta",
    ],
}

POWER_PPT_SEQUENCE = [
    ("Errores tipo I y II", ["error tipo I", "error tipo II", "poder estadístico"]),
    ("Continuo sin controles 7.1", ["7.1", "resultado continuo", "sin controles", "Bogotá"]),
    ("Continuo con controles 7.2", ["7.2", "resultado continuo", "con controles", "Bogotá"]),
    ("Binario sin controles 7.3", ["7.3", "resultado binario", "sin controles", "Bogotá"]),
    ("Binario con controles 7.4", ["7.4", "resultado binario", "con controles", "Zambia"]),
    ("Tasas", ["tasas", "Senegal"]),
    ("Clústeres", ["clústeres", "Sudáfrica"]),
]

PRACTICE_STAGE_SEQUENCE = [
    "Error tipo I, error tipo II y poder",
    "Tamaño de muestra para un resultado continuo sin controles",
    "MDE para un resultado continuo sin controles",
    "Tamaño de muestra para un resultado continuo con controles",
    "MDE para un resultado continuo con controles",
    "Resultado binario sin controles",
    "Resultado binario con controles",
    "Comparación de tasas",
    "Cumplimiento parcial (take-up)",
    "Atrición",
    "Diseño por clústeres",
    "Caso Bogotá",
    "Caso Zambia",
    "Caso Senegal",
    "Caso Sudáfrica",
    "Bertrand y Mullainathan",
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
        r"(?:\*\*|__)?(?:respuesta|solución|pista|opción correcta|clave|"
        r"resultado esperado|valor esperado|resultado)(?:\*\*|__)?\s*"
        r"(?::|\.|=|correcta\b|es\b)|\bla\s+respuesta\s+es\b|"
        r"\bsolución\s+correcta\b|\bla\s+opción\s+\w+\s+es\s+correcta\b|"
        r"\b(?:el|la)\s+(?:MDE|poder|tamaño de muestra)\s+"
        r"(?:correct[oa]\s+)?es\s+[-+]?\d+(?:[.,]\d+)?|"
        r"\b(?:se\s+requieren?|obtenemos?|resulta(?:n)?|asciende(?:n)?\s+a)\s+"
        r"[-+]?\d+(?:[.,]\d+)?(?:\s*(?:%|observaciones|clústeres|individuos))?"
    )
    lowered = block.casefold()
    return bool(label.search(block)) or any(
        marker in lowered
        for marker in ["<details", "hide(", "collapse", "ver respuesta"]
    )


def _metadata_once(block, label):
    pattern = re.compile(
        rf"(?im)^\s*(?:[-*+]\s+)?(?:\*\*|__)?{re.escape(label)}"
        rf"(?::(?:\*\*|__)|(?:\*\*|__)\s*:|\s*:)",
    )
    assert len(pattern.findall(block)) == 1, f"Cada pregunta exige una línea {label}:"


def _assert_closed_question_structure(block, required_labels):
    field_pattern = re.compile(
        r"(?im)^\s*(?:[-*+]\s+)?(?:\*\*|__)([^:*_\n]+?)"
        r"(?::(?:\*\*|__)|(?:\*\*|__)\s*:)"
    )
    fields = [field.strip() for field in field_pattern.findall(block)]
    allowed = set(required_labels) | {"Código", "Tipo", "Fuente", "Enunciado"}
    assert set(required_labels) <= set(fields)
    assert len(fields) == len(set(fields)), "Los campos estructurales no pueden repetirse"
    assert set(fields) <= allowed, f"Campo no permitido en caja estudiantil: {set(fields) - allowed}"
    assert not _has_disclosed_answer(block)


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


def _assert_no_private_exposure(paths, contents, tokens):
    path_hits, content_hits = _private_exposure_counts(paths, contents, tokens)
    assert (path_hits, content_hits) == (0, 0), (
        "Hay identificadores privados; "
        f"coincidencias en rutas={path_hits}, contenidos={content_hits}"
    )


def _assert_inventory_preserved(text, inventory):
    headings = [_headings(text, level) for level in (2, 3, 4)]
    normalized_headings = {
        re.sub(r"^\d+\.\s+", "", heading)
        for group in headings
        for heading in group
    }
    assert set(inventory["sections"]) <= normalized_headings
    assert all(fragment in text for fragment in inventory["formulas_and_cases"])
    found_urls = Counter(re.findall(r"https?://[^\s)>\"']+", text))
    required_urls = Counter(inventory["link_destinations"])
    assert all(found_urls[url] >= count for url, count in required_urls.items())


def _normalize_unit(text):
    return re.sub(r"\s+", " ", text).strip()


def _snapshot_units(text):
    prose = []
    code = []
    current = []
    code_lines = []
    in_code = False
    for line in text.splitlines():
        if re.match(r"^`{3,}", line):
            if in_code:
                normalized = _normalize_unit("\n".join(code_lines))
                if normalized:
                    code.append(normalized)
                code_lines = []
                in_code = False
            else:
                normalized = _normalize_unit("\n".join(current))
                if normalized:
                    prose.append(normalized)
                current = []
                in_code = True
            continue
        if in_code:
            code_lines.append(line)
            continue
        if not line.strip():
            normalized = _normalize_unit("\n".join(current))
            if normalized:
                prose.append(normalized)
            current = []
        elif not re.match(r"^#{1,6}\s+|^:{3,}\s*(?:\{|$)", line):
            current.append(line)
    normalized = _normalize_unit("\n".join(code_lines if in_code else current))
    if normalized:
        (code if in_code else prose).append(normalized)

    digest = lambda unit: hashlib.sha256(unit.encode("utf-8")).hexdigest()
    return {
        "prose_sha256": Counter(digest(unit) for unit in prose),
        "code_sha256": Counter(digest(unit) for unit in code),
        "url_destinations": Counter(re.findall(r"https?://[^\s)>\"']+", text)),
    }


def _assert_complete_snapshot_preserved(text, snapshot):
    observed = _snapshot_units(text)
    for family in ["prose_sha256", "code_sha256", "url_destinations"]:
        required = Counter(snapshot[family])
        assert not (required - observed[family]), f"Se eliminaron unidades base de {family}"


def _assert_ppt_sequence(text):
    positions = []
    for label, markers in POWER_PPT_SEQUENCE:
        match = re.search(re.escape(markers[0]), text, re.IGNORECASE)
        assert match, f"Falta marcador principal de POWER.pptx en {label}"
        positions.append(match.start())
    assert positions == sorted(positions), "Los casos de POWER.pptx deben conservar orden conceptual"
    for index, (label, markers) in enumerate(POWER_PPT_SEQUENCE):
        end = positions[index + 1] if index + 1 < len(positions) else len(text)
        case_block = text[positions[index]:end]
        for marker in markers[1:]:
            assert re.search(re.escape(marker), case_block, re.IGNORECASE), (
                f"Falta marcador de POWER.pptx en {label}"
            )


def _assert_hypothetical_labels(blocks, classification):
    assert classification, "Debe existir una clasificación explícita de hipotéticos"
    by_code = {code: block for code, block in blocks}
    classified = [code for code, metadata in classification.items() if metadata["fuente"] == "hipotetico"]
    assert classified, "La clasificación debe incluir al menos un caso hipotético"
    for code in classified:
        assert code in by_code
        assert "escenario hipotético" in by_code[code].casefold()


def _assert_stage_sequence(headings):
    selected = [heading for heading in headings if heading in PRACTICE_STAGE_SEQUENCE]
    assert selected == PRACTICE_STAGE_SEQUENCE


def test_current_power_practice_content_is_preserved():
    practice = _read(PRACTICE_PATH)
    missing = [fragment for fragment in PRACTICE_REQUIRED if fragment not in practice]
    assert not missing, f"Se perdieron {len(missing)} fragmentos distintivos del capítulo"


def test_complete_current_practice_inventory_is_preserved_while_allowing_reordering():
    _assert_inventory_preserved(_read(PRACTICE_PATH), PRACTICE_INVENTORY)


def test_every_normalized_prose_code_and_repeated_url_unit_is_preserved():
    snapshot = json.loads(_read(PRACTICE_SNAPSHOT))
    _assert_complete_snapshot_preserved(_read(PRACTICE_PATH), snapshot)


def test_complete_snapshot_rejects_unlisted_prose_code_and_one_repeated_url_deletion():
    practice = _read(PRACTICE_PATH)
    snapshot = json.loads(_read(PRACTICE_SNAPSHOT))
    prose = "Una disminución en `nratio` implica asignar una proporción mayor al grupo de control, lo que en general reduce el poder y aumenta el MDE."
    code = "local mde = round(`r(delta)',0.0001)"
    repeated_url = "https://www.dropbox.com/scl/fi/ephx1kl4opc0q3oxe5ckp/bm.dta?rlkey=zwp0hwtec5z25a4ll9qn8biz7&dl=1"
    for mutated in [
        practice.replace(prose, "", 1),
        practice.replace(code, "", 1),
        practice.replace(repeated_url, "", 1),
    ]:
        with pytest.raises(AssertionError):
            _assert_complete_snapshot_preserved(mutated, snapshot)


def test_inventory_contract_rejects_each_deleted_section_formula_case_and_link():
    practice = _read(PRACTICE_PATH)
    for section in PRACTICE_INVENTORY["sections"]:
        mutated = re.sub(
            rf"^#{{2,4}}\s+(?:\d+\.\s+)?{re.escape(section)}\s*\{{[^}}]*\}}\s*$",
            "",
            practice,
            flags=re.MULTILINE,
        )
        with pytest.raises(AssertionError):
            _assert_inventory_preserved(mutated, PRACTICE_INVENTORY)
    for fragment in PRACTICE_INVENTORY["formulas_and_cases"]:
        with pytest.raises(AssertionError):
            _assert_inventory_preserved(practice.replace(fragment, ""), PRACTICE_INVENTORY)
    for url in PRACTICE_INVENTORY["link_destinations"]:
        with pytest.raises(AssertionError):
            _assert_inventory_preserved(practice.replace(url, ""), PRACTICE_INVENTORY)


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


def test_power_theory_follows_power_ppt_cases_in_conceptual_order():
    _assert_ppt_sequence(_read(THEORY_PATH))


def test_power_ppt_contract_rejects_missing_case_marker_and_reordered_cases():
    canonical = "\n".join(
        " | ".join(markers) for _label, markers in POWER_PPT_SEQUENCE
    )
    _assert_ppt_sequence(canonical)
    with pytest.raises(AssertionError):
        _assert_ppt_sequence(canonical.replace("Bogotá", "", 1))
    reordered = "\n".join(
        " | ".join(markers)
        for _label, markers in [POWER_PPT_SEQUENCE[1], POWER_PPT_SEQUENCE[0], *POWER_PPT_SEQUENCE[2:]]
    )
    with pytest.raises(AssertionError):
        _assert_ppt_sequence(reordered)


def test_power_theory_has_blocks_and_exactly_three_questions():
    theory = _read(THEORY_PATH)
    assert len(_boxes(theory)) >= 8
    blocks = _question_boxes(theory, "POWER-T", ["POWER-T1", "POWER-T2", "POWER-T3"])
    for block in blocks:
        _metadata_once(block, "Puntaje sugerido")
        _metadata_once(block, "Producto esperado")
        _assert_closed_question_structure(block, ["Puntaje sugerido", "Producto esperado"])


def test_power_practice_has_between_fourteen_and_eighteen_semantic_stages():
    practice = _read(PRACTICE_PATH)
    h3 = _headings(practice, 3)
    assert 14 <= len(h3) <= 18


def test_power_practice_has_the_concrete_semantic_stage_sequence():
    headings = _headings(_read(PRACTICE_PATH), 3)
    _assert_stage_sequence(headings)


def test_semantic_stage_contract_rejects_generic_missing_and_reordered_stages():
    _assert_stage_sequence(PRACTICE_STAGE_SEQUENCE)
    for mutated in [
        ["Parte"] * len(PRACTICE_STAGE_SEQUENCE),
        PRACTICE_STAGE_SEQUENCE[:-1],
        [PRACTICE_STAGE_SEQUENCE[1], PRACTICE_STAGE_SEQUENCE[0], *PRACTICE_STAGE_SEQUENCE[2:]],
    ]:
        with pytest.raises(AssertionError):
            _assert_stage_sequence(mutated)


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
        _assert_closed_question_structure(
            block, ["Puntaje sugerido", "Comandos permitidos", "Producto esperado"]
        )
    classification = {
        "POWER-S1": {"fuente": "canonico"},
        "POWER-S2": {"fuente": "hipotetico"},
        "POWER-S3": {"fuente": "canonico"},
        "POWER-S4": {"fuente": "hipotetico"},
    }
    _assert_hypothetical_labels(
        list(zip(["POWER-S1", "POWER-S2", "POWER-S3", "POWER-S4"], blocks)),
        classification,
    )


def test_answer_detector_rejects_disclosures_but_allows_question_wording():
    assert not _has_disclosed_answer("Justifique su respuesta y proponga una solución.")
    for disclosure in [
        "Respuesta: 800 observaciones.",
        "Solución correcta = aumentar clústeres.",
        "La respuesta es el MDE.",
        "Pista: use power.",
        "Opción correcta: B",
        "Clave: B",
        "Resultado esperado: 800",
        "Valor esperado = 0.8",
        "Resultado: 800",
        "<details>",
        "hide(panel)",
        "collapse = TRUE",
        "El tamaño de muestra correcto es 800.",
        "La opción B es correcta.",
        "Se requieren 800 observaciones.",
        "Con estos datos obtenemos 800.",
        "El cálculo resulta 800.",
        "El valor calculado asciende a 0.8.",
    ]:
        assert _has_disclosed_answer(disclosure)
    assert not _has_disclosed_answer("Producto esperado: cálculo y justificación.")


def test_closed_question_structure_allows_only_student_fields_and_no_solution_field():
    valid = """**Código:** POWER-S1
**Puntaje sugerido:** 5
**Comandos permitidos:** power
**Producto esperado:** cálculo y justificación

Calcule el tamaño de muestra y explique su estrategia."""
    _assert_closed_question_structure(
        valid, ["Puntaje sugerido", "Comandos permitidos", "Producto esperado"]
    )
    for field in ["Solución", "Respuesta", "Clave", "Resultado esperado"]:
        with pytest.raises(AssertionError):
            _assert_closed_question_structure(
                valid + f"\n**{field}:** 800",
                ["Puntaje sugerido", "Comandos permitidos", "Producto esperado"],
            )


def test_hypothetical_classification_is_non_vacuous_and_requires_literal_label():
    classification = {"POWER-S2": {"fuente": "hipotetico"}}
    _assert_hypothetical_labels(
        [("POWER-S2", "Este es un escenario hipotético para calcular el MDE.")],
        classification,
    )
    with pytest.raises(AssertionError):
        _assert_hypothetical_labels([("POWER-S2", "Calcule el MDE.")], classification)
    with pytest.raises(AssertionError):
        _assert_hypothetical_labels([], {})


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
    _assert_no_private_exposure([path for path in tracked if path], contents, tokens)


def test_private_audit_detects_path_and_content_without_returning_token():
    token = "docente" + "-solo-power"
    assert _private_exposure_counts(
        [f"docs/{token}/index.html"], {"temporal.html": token}, [token]
    ) == (1, 1)
    with pytest.raises(AssertionError) as failure:
        _assert_no_private_exposure(
            [f"docs/{token}/index.html"], {"temporal.html": token}, [token]
        )
    assert token not in str(failure.value)
