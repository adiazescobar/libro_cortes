"""Contrasta escalares Stata con aproximaciones normales independientes."""
import csv
import math
from pathlib import Path
from statistics import NormalDist

HERE = Path(__file__).resolve().parent
RESULTS = HERE / "results"


def means_per_arm(sd, delta, alpha=0.05, power=0.80):
    normal = NormalDist()
    zsum = normal.inv_cdf(1-alpha/2) + normal.inv_cdf(power)
    return math.ceil(2 * (zsum * sd / delta) ** 2)


def proportions_per_arm(p0, p1, alpha=0.05, power=0.80):
    normal = NormalDist()
    zsum = normal.inv_cdf(1-alpha/2) + normal.inv_cdf(power)
    variance = p0*(1-p0) + p1*(1-p1)
    return math.ceil(zsum**2 * variance / (p0-p1)**2)


base_per_arm = means_per_arm(1, 0.30)
checks = {
    "continuo sin controles": (2*base_per_arm, 4, "normal bilateral; total=2*N_por_brazo"),
    "continuo con controles": (2*means_per_arm(0.70, 0.30), 4, "normal bilateral; total=2*N_por_brazo"),
    "binario": (2*proportions_per_arm(0.08, 0.05), 10, "normal bilateral para dos proporciones"),
    "take-up": (2*means_per_arm(1, 0.30*(0.90-0.10)), 4, "normal bilateral con delta ajustado por take-up"),
    "atrición": (2*base_per_arm, 4, "normal bilateral antes de inflar por atrición"),
    "tasa": (2*proportions_per_arm(0.07203, 0.06), 10, "normal bilateral para dos proporciones"),
    "clúster": (math.ceil(base_per_arm*(1+0.05*(50-1))/50), 1, "efecto de diseño y redondeo a clúster completo por brazo"),
}

with (RESULTS / "power_resultados.csv").open(newline="", encoding="utf-8-sig") as handle:
    stata = {row["escenario"]: float(row["valor_stata"]) for row in csv.DictReader(handle)}

fields = ["escenario", "valor_stata", "valor_alternativo", "diferencia_abs", "tolerancia", "metodo_alternativo", "estado"]
with (RESULTS / "power_verificacion.csv").open("w", newline="", encoding="utf-8") as handle:
    writer = csv.DictWriter(handle, fieldnames=fields, lineterminator="\n")
    writer.writeheader()
    for escenario, (alternative, tolerance, method) in checks.items():
        difference = abs(stata[escenario] - alternative)
        writer.writerow({
            "escenario": escenario,
            "valor_stata": stata[escenario],
            "valor_alternativo": alternative,
            "diferencia_abs": difference,
            "tolerancia": tolerance,
            "metodo_alternativo": method,
            "estado": "PASS" if difference <= tolerance else "FAIL",
        })
