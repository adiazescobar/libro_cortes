"""Verificación independiente de los resultados POWER con Python stdlib."""
import csv
import math
from pathlib import Path
from statistics import NormalDist

HERE = Path(__file__).resolve().parent
RESULTS = HERE / "results"


def two_means(sd, delta, alpha=0.05, power=0.80):
    z = NormalDist()
    return math.ceil(2 * ((z.inv_cdf(1-alpha/2) + z.inv_cdf(power)) * sd / delta) ** 2)


def two_proportions(p1, p2, alpha=0.05, power=0.80):
    z = NormalDist()
    variance = p1*(1-p1) + p2*(1-p2)
    return math.ceil((z.inv_cdf(1-alpha/2) + z.inv_cdf(power)) ** 2 * variance / (p1-p2) ** 2)


base = two_means(1, 0.30)
alternative = {
    "continuo sin controles": base,
    "continuo con controles": two_means(0.70, 0.30),
    "binario": two_proportions(0.08, 0.05),
    "take-up": two_means(1, 0.30*(0.90-0.10)),
    "atrición": math.ceil(base/0.80),
    "tasa": two_proportions(0.07203, 0.06),
    "clúster": math.ceil(base*(1+0.05*(50-1))),
}

with (RESULTS / "power_resultados.csv").open(newline="", encoding="utf-8-sig") as handle:
    stata = {row["escenario"]: float(row["valor"]) for row in csv.DictReader(handle)}

fields = ["escenario", "valor_stata", "valor_alternativo", "diferencia_abs", "tolerancia", "estado"]
with (RESULTS / "power_verificacion.csv").open("w", newline="", encoding="utf-8") as handle:
    writer = csv.DictWriter(handle, fieldnames=fields)
    writer.writeheader()
    for escenario, alt in alternative.items():
        difference = abs(stata[escenario] - alt)
        tolerance = 0.0
        writer.writerow({
            "escenario": escenario, "valor_stata": stata[escenario],
            "valor_alternativo": alt, "diferencia_abs": difference,
            "tolerancia": tolerance, "estado": "PASS" if difference <= tolerance else "FAIL",
        })
