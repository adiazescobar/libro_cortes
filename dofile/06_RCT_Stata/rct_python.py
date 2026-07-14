"""Replicación en Python de los cuatro modelos principales del módulo RCT."""

from pathlib import Path

import pandas as pd
import statsmodels.formula.api as smf


EXPECTED_COLUMNS = [
    "modelo",
    "termino",
    "coeficiente",
    "error_estandar",
    "N",
    "R2",
    "prueba",
    "estadistico",
    "p_value",
]

REQUIRED_COLUMNS = {
    "resultado",
    "grupo",
    "genero",
    "programa",
    "semestre",
    "edad",
    "libros",
}

CONTROLS = ["edad", "mujer", "libros", "pregrado", "maestria"]

MODEL_FORMULAS = {
    "m1_simple": "y ~ D",
    "m2_controles": "y ~ D + edad + mujer + libros + pregrado + maestria",
    "m3_estratos": "y ~ D + C(semestre_f)",
    "m4_completo": (
        "y ~ D + C(semestre_f) + edad + mujer + libros + pregrado + maestria"
    ),
}


def prepare_rct_data(path: str | Path) -> pd.DataFrame:
    """Lee la base congelada y reproduce las definiciones usadas en Stata."""
    data_path = Path(path)
    if not data_path.is_file():
        raise FileNotFoundError(f"No se encontró la base RCT: {data_path}")

    df = pd.read_stata(data_path)
    missing = sorted(REQUIRED_COLUMNS.difference(df.columns))
    if missing:
        raise ValueError(f"Faltan variables requeridas en data.dta: {', '.join(missing)}")

    df = df.copy()
    df["y"] = pd.to_numeric(df["resultado"], errors="coerce")
    df["D"] = (df["grupo"] == "B").astype(int)
    df["mujer"] = (df["genero"] == "Mujer").astype(int)
    df["pregrado"] = (df["programa"] == "Pregrado").astype(int)
    df["maestria"] = (df["programa"] == "Maestría").astype(int)
    categories = sorted(df["semestre"].dropna().unique())
    df["semestre_f"] = pd.Categorical(df["semestre"], categories=categories)
    return df


def fit_main_models(df: pd.DataFrame) -> pd.DataFrame:
    """Estima los cuatro modelos con HC1 y devuelve el término de tratamiento."""
    rows = []
    for model_name, formula in MODEL_FORMULAS.items():
        model = smf.ols(formula, data=df).fit(cov_type="HC1")
        rows.append(
            {
                "modelo": model_name,
                "termino": "D",
                "coeficiente": float(model.params["D"]),
                "error_estandar": float(model.bse["D"]),
                "N": int(model.nobs),
                "R2": float(model.rsquared),
                "prueba": "",
                "estadistico": pd.NA,
                "p_value": float(model.pvalues["D"]),
            }
        )
    return pd.DataFrame(rows, columns=EXPECTED_COLUMNS)

