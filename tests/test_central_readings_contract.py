from pathlib import Path

import test_power_pedagogy_contract as base


ROOT = base.ROOT

EXPECTED_READINGS = {
    "00-PruebaEntrada.Rmd": ([2, 3], ["04-potential_outcomes"]),
    "01-intro.Rmd": ([2, 3], ["01-introduction", "04-potential_outcomes"]),
    "02-StataBasics.Rmd": ([2], ["02-probability_and_regression"]),
    "03-Parametros.Rmd": ([2, 3], ["04-potential_outcomes"]),
    "04-ParametrosStata.Rmd": ([2, 3], ["04-potential_outcomes"]),
    "05-RCT.Rmd": ([4], ["04-potential_outcomes"]),
    "06-RCT2.Rmd": ([4], ["04-potential_outcomes"]),
    "07-POWER-Teoria.Rmd": ([4], ["04-potential_outcomes"]),
    "07-POWER.Rmd": ([4], ["04-potential_outcomes"]),
    "08-DID.Rmd": ([5], ["09-difference_in_differences"]),
    "08-DIDStata.Rmd": ([5], ["09-difference_in_differences"]),
    "09-BadControls.Rmd": ([3], ["03-directed_acyclical_graphs"]),
    "10-BadControlsStata.Rmd": ([3], ["03-directed_acyclical_graphs"]),
    "11-TWFE.Rmd": ([5], ["08-panel_data", "09-difference_in_differences"]),
    "11-TWFEStata.Rmd": ([5], ["08-panel_data", "09-difference_in_differences"]),
    "12-ExactMatching.Rmd": ([6], ["05-matching_and_subclassification"]),
    "13-PSM.Rmd": ([6], ["05-matching_and_subclassification"]),
    "14-PSMStata.Rmd": ([6], ["05-matching_and_subclassification"]),
    "15-IPW.Rmd": ([6], ["05-matching_and_subclassification"]),
    "16-PSM_IPW_SinteticosConsolidado.Rmd": (
        [6],
        ["05-matching_and_subclassification"],
    ),
    "17-SyntheticControls.Rmd": ([6], ["10-synthetic_control"]),
    "17-SyntheticControlsStata.Rmd": ([6], ["10-synthetic_control"]),
    "18-IV.Rmd": ([7], ["07-instrumental_variables"]),
    "19-IVStata.Rmd": ([7], ["07-instrumental_variables"]),
    "20-RDD.Rmd": ([8], ["06-regression_discontinuity"]),
    "21-RDDStata.Rmd": ([8], ["06-regression_discontinuity"]),
}

BERNAL_REMOTE = {
    2: "https://www.dropbox.com/s/zsqa2gcbbgdi5i3/Capitulo%202%20Bernal%20y%20Pe%C3%B1a.pdf?dl=1",
    3: "https://www.dropbox.com/s/837u3ea36r7t5me/Capitulo%203%20Bernal%20y%20Pe%C3%B1a.pdf?dl=1",
    4: "https://www.dropbox.com/s/vxpgxt22pvphwx3/Capitulo%204%20Bernal%20y%20Pe%C3%B1a.pdf?dl=1",
}

BERNAL_LOCAL = {
    chapter: f"lecturas/bernal-pena/capitulo-{chapter:02d}.pdf"
    for chapter in (5, 6, 7, 8)
}

MIXTAPE_URLS = {
    f"https://mixtape.scunning.com/{slug}"
    for _, slugs in EXPECTED_READINGS.values()
    for slug in slugs
}

ADH_2010_URL = (
    "https://economics.mit.edu/sites/default/files/publications/"
    "Synthetic%20Control%20Methods.pdf"
)


def _book_files():
    return base.parse_rmd_files(base.BOOKDOWN)


def _text(filename):
    return (ROOT / filename).read_text(encoding="utf-8")


def test_every_teaching_page_has_a_reading_mapping():
    assert set(_book_files()) - {"index.Rmd"} == set(EXPECTED_READINGS)


def test_each_page_has_both_reference_families():
    for filename, (chapters, mixtape_slugs) in EXPECTED_READINGS.items():
        text = _text(filename)
        assert "**Lecturas centrales**" in text, filename
        for chapter in chapters:
            assert f"Bernal y Peña — capítulo {chapter}" in text, filename
            target = (
                BERNAL_REMOTE[chapter]
                if chapter in BERNAL_REMOTE
                else BERNAL_LOCAL[chapter]
            )
            assert target in text, filename
        for slug in mixtape_slugs:
            assert f"https://mixtape.scunning.com/{slug}" in text, filename


def test_synthetic_control_pair_links_adh_2010_in_the_central_block():
    for filename in ("17-SyntheticControls.Rmd", "17-SyntheticControlsStata.Rmd"):
        text = _text(filename)
        block = text[
            text.index("**Lecturas centrales**") : text.index(
                "**Metas de aprendizaje**"
            )
        ]
        assert "Abadie, Diamond y Hainmueller (2010)" in block, filename
        assert ADH_2010_URL in block, filename


def test_reading_block_is_near_the_start_of_each_page():
    for filename in EXPECTED_READINGS:
        text = _text(filename)
        assert text.index("**Lecturas centrales**") < 6500, filename


def test_empirical_materials_remain_before_readings():
    empirical = [
        filename
        for filename in EXPECTED_READINGS
        if "Stata" in filename
        or filename in {
            "06-RCT2.Rmd",
            "07-POWER.Rmd",
            "16-PSM_IPW_SinteticosConsolidado.Rmd",
            "17-SyntheticControlsStata.Rmd",
        }
    ]
    for filename in empirical:
        text = _text(filename)
        material_markers = [
            marker
            for marker in (
                "Materiales para la clase",
                "Materiales de la clase",
                "**Descargar archivos ejecutables:**",
            )
            if marker in text
        ]
        if material_markers:
            assert any(
                text.index(marker) < text.index("**Lecturas centrales**")
                for marker in material_markers
            ), filename


def test_local_pdf_targets_exist():
    for path in BERNAL_LOCAL.values():
        assert (ROOT / path).is_file(), path


def test_blocks_have_no_placeholders_or_private_keys():
    for filename in EXPECTED_READINGS:
        text = _text(filename)
        marker = text.index("**Lecturas centrales**") if "**Lecturas centrales**" in text else 0
        excerpt = text[marker : marker + 1800]
        assert "](LINK)" not in excerpt
        assert "TODO" not in excerpt
        assert "claves_privadas" not in excerpt
