"""Compara resultados canónicos del piloto RCT entre Stata y Python."""

from pathlib import Path

import pandas as pd


REQUIRED_COLUMNS = {
    "modelo",
    "termino",
    "coeficiente",
    "error_estandar",
    "N",
    "R2",
}


def _read_results(path: str | Path, source: str) -> pd.DataFrame:
    frame = pd.read_csv(path)
    missing = sorted(REQUIRED_COLUMNS.difference(frame.columns))
    if missing:
        raise ValueError(f"Faltan columnas en resultados {source}: {', '.join(missing)}")
    if frame.duplicated(["modelo", "termino"]).any():
        raise ValueError(f"Hay claves modelo–termino duplicadas en resultados {source}")
    return frame


def compare_results(
    stata_path: str | Path,
    python_path: str | Path,
    coefficient_tolerance: float = 1e-3,
    standard_error_tolerance: float = 1e-3,
    r2_tolerance: float = 1e-2,
) -> pd.DataFrame:
    """Combina ambos contratos y clasifica cada estimación como PASS o FAIL."""
    stata = _read_results(stata_path, "Stata")
    python = _read_results(python_path, "Python")
    compared = stata.merge(
        python,
        on=["modelo", "termino"],
        how="outer",
        suffixes=("_stata", "_python"),
        indicator=True,
    )

    compared["coef_abs_diff"] = (
        compared["coeficiente_stata"] - compared["coeficiente_python"]
    ).abs()
    compared["se_abs_diff"] = (
        compared["error_estandar_stata"] - compared["error_estandar_python"]
    ).abs()
    compared["N_igual"] = compared["N_stata"].eq(compared["N_python"])
    compared["R2_abs_diff"] = (compared["R2_stata"] - compared["R2_python"]).abs()

    passed = (
        compared["_merge"].eq("both")
        & compared["coef_abs_diff"].lt(coefficient_tolerance)
        & compared["se_abs_diff"].lt(standard_error_tolerance)
        & compared["N_igual"]
        & compared["R2_abs_diff"].lt(r2_tolerance)
    )
    compared["estado"] = passed.map({True: "PASS", False: "FAIL"})
    return compared.drop(columns="_merge")


def main() -> int:
    module_dir = Path(__file__).resolve().parent
    results_dir = module_dir / "results"
    compared = compare_results(
        results_dir / "resultados_stata.csv",
        results_dir / "resultados_python.csv",
    )
    output_path = results_dir / "verificacion_stata_python.csv"
    compared.to_csv(output_path, index=False, encoding="utf-8", float_format="%.17g")
    print(compared.to_string(index=False))
    return 1 if (compared["estado"] == "FAIL").any() else 0


if __name__ == "__main__":
    raise SystemExit(main())

