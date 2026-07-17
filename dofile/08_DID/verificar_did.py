"""Verificación cruzada de los resultados canónicos de DID.

Reproduce en Python (pandas/statsmodels) los estimadores centrales calculados
por Stata en 08_DID.do y escribe results/did_verificacion.csv con una fila por
comparación. Correr desde la raíz del repositorio:

    /private/tmp/libro_cortes_rct_venv/bin/python dofile/08_DID/verificar_did.py
"""
import csv
from pathlib import Path

import pandas as pd
import statsmodels.formula.api as smf

DID = Path(__file__).resolve().parent
RESULTS = DID / "results/did_resultados.csv"
HOSPDD = DID / "results/hospdd_verificacion_input.csv"
OUT = DID / "results/did_verificacion.csv"


def stata_values():
    rows = {}
    with RESULTS.open(newline="", encoding="utf-8-sig") as handle:
        for row in csv.DictReader(handle):
            rows[row["escenario"]] = float(row["valor_stata"])
    return rows


def main():
    stata = stata_values()
    base = pd.read_stata(DID / "base3.dta", convert_categoricals=False)
    base["D"] = base["D"].astype(int)
    base["t"] = base["t"].astype(int)

    means = base.groupby(["D", "t"])["y"].mean()
    manual = (means[1, 1] - means[1, 0]) - (means[0, 1] - means[0, 0])

    ols = smf.ols("y ~ D * t", data=base).fit(cov_type="HC1")
    regresion = ols.params["D:t"]

    hosp = pd.read_csv(HOSPDD)
    twfe = smf.ols(
        "satis ~ procedure + C(hospital) + C(month)", data=hosp
    ).fit()
    atet = twfe.params["procedure"]

    comparisons = [
        ("did_manual", manual, 1e-6, "pandas groupby means"),
        ("did_regresion", regresion, 1e-6, "statsmodels OLS HC1"),
        ("hospdd_atet", atet, 1e-6, "statsmodels TWFE OLS"),
    ]
    with OUT.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.writer(handle)
        writer.writerow([
            "escenario", "valor_stata", "valor_alternativo", "diferencia_abs",
            "tolerancia", "metodo_alternativo", "estado",
        ])
        for escenario, alterno, tolerancia, metodo in comparisons:
            diferencia = abs(stata[escenario] - alterno)
            estado = "PASS" if diferencia <= tolerancia else "FAIL"
            writer.writerow([
                escenario, f"{stata[escenario]:.10f}", f"{alterno:.10f}",
                f"{diferencia:.2e}", f"{tolerancia:.0e}", metodo, estado,
            ])
            print(escenario, estado, f"dif={diferencia:.2e}")


if __name__ == "__main__":
    main()
