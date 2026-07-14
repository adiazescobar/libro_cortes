from pathlib import Path
import sys

import pandas as pd
import pytest


MODULE_DIR = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(MODULE_DIR))

from verify_stata_python import compare_results


def canonical_frame(coef=1.0, se=0.2, n=70, r2=0.5):
    return pd.DataFrame(
        {
            "modelo": ["m1_simple"],
            "termino": ["D"],
            "coeficiente": [coef],
            "error_estandar": [se],
            "N": [n],
            "R2": [r2],
            "prueba": [""],
            "estadistico": [pd.NA],
            "p_value": [0.01],
        }
    )


def write_pair(tmp_path, stata, python):
    stata_path = tmp_path / "stata.csv"
    python_path = tmp_path / "python.csv"
    stata.to_csv(stata_path, index=False)
    python.to_csv(python_path, index=False)
    return stata_path, python_path


def test_compare_results_passes_inside_tolerances(tmp_path):
    paths = write_pair(
        tmp_path,
        canonical_frame(),
        canonical_frame(coef=1.0009, se=0.2009, r2=0.509),
    )
    compared = compare_results(*paths)
    assert compared.loc[0, "estado"] == "PASS"
    assert bool(compared.loc[0, "N_igual"])


@pytest.mark.parametrize(
    "python_frame",
    [
        canonical_frame(n=69),
        canonical_frame(coef=1.002),
        canonical_frame(se=0.202),
        canonical_frame(r2=0.511),
    ],
)
def test_compare_results_fails_outside_any_tolerance(tmp_path, python_frame):
    paths = write_pair(tmp_path, canonical_frame(), python_frame)
    compared = compare_results(*paths)
    assert compared.loc[0, "estado"] == "FAIL"


def test_compare_results_fails_when_key_is_missing(tmp_path):
    python = canonical_frame()
    python["modelo"] = "m2_controles"
    paths = write_pair(tmp_path, canonical_frame(), python)
    compared = compare_results(*paths)
    assert set(compared["estado"]) == {"FAIL"}


def test_compare_results_rejects_missing_required_column(tmp_path):
    paths = write_pair(
        tmp_path,
        canonical_frame().drop(columns="error_estandar"),
        canonical_frame(),
    )
    with pytest.raises(ValueError, match="error_estandar"):
        compare_results(*paths)

