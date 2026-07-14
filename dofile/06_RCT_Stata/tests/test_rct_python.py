from pathlib import Path
import sys

import pandas as pd


MODULE_DIR = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(MODULE_DIR))

from rct_python import EXPECTED_COLUMNS, fit_main_models, prepare_rct_data


DATA_PATH = MODULE_DIR / "data.dta"


def test_prepare_rct_data_matches_stata_definitions():
    raw = pd.read_stata(DATA_PATH)
    prepared = prepare_rct_data(DATA_PATH)

    assert len(prepared) == len(raw) == 70
    assert prepared["y"].equals(pd.to_numeric(raw["resultado"], errors="coerce"))
    assert prepared["D"].equals((raw["grupo"] == "B").astype(int))
    assert prepared["mujer"].equals((raw["genero"] == "Mujer").astype(int))
    assert prepared["pregrado"].equals((raw["programa"] == "Pregrado").astype(int))
    assert prepared["maestria"].equals((raw["programa"] == "Maestría").astype(int))
    assert list(prepared["semestre_f"].cat.categories) == sorted(raw["semestre"].unique())


def test_fit_main_models_returns_canonical_contract():
    prepared = prepare_rct_data(DATA_PATH)
    results = fit_main_models(prepared)

    assert list(results.columns) == EXPECTED_COLUMNS
    assert results[["modelo", "termino"]].to_dict("records") == [
        {"modelo": "m1_simple", "termino": "D"},
        {"modelo": "m2_controles", "termino": "D"},
        {"modelo": "m3_estratos", "termino": "D"},
        {"modelo": "m4_completo", "termino": "D"},
    ]
    assert results[["coeficiente", "error_estandar", "N", "R2"]].notna().all().all()
    assert (results["N"] == 70).all()

