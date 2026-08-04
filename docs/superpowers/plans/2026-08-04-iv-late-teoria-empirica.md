# IV and LATE Teaching Pair Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Rebuild the IV/LATE theory and empirical chapters around a transparent PACES-style simulation, a critical divorce-IV application, modern weak-instrument inference, and honest characterization of compliers.

**Architecture:** Stata is the canonical data and results engine. One do-file generates the PACES-style data twice conceptually—first with latent truth visible and then as an investigator-facing dataset—and a separate divorce-style simulated dataset; it exports stable CSV tables and PNG figures consumed by the R Markdown chapter. Contract and numerical tests protect pedagogy, notation, output provenance, and the distinction between binary-instrument LATE and continuous-instrument local IV interpretation.

**Tech Stack:** Bookdown/R Markdown, StataNow 19, `ivreg2`, official `ivregress` postestimation, Python `pytest` for static and numerical contracts, CSV/PNG artifacts.

## Global Constraints

- Work directly in the existing `main` branch; do not create agents or expensive parallel jobs.
- Preserve the URLs and anchors `{#iv}` and `{#iv-stata}`.
- Use the exact title pattern “Variables instrumentales y LATE — Clase teórica” and “Variables instrumentales y LATE — Clase empírica”.
- Use potential-outcome notation `Y(D=1)` and `Y(D=0)` throughout.
- Put downloads immediately after the empirical title, followed by central readings and learning goals.
- Stata is the canonical source for public tables and figures; do not hand-transcribe results.
- Label every simulated dataset in the page, data labels, and do-file as fictitious.
- Use `ivreg2` for applied diagnostics and `estat weakrobust, ci` for modern weak-IV verification.
- Treat 104.7 as a context-specific result, not a universal strength threshold.
- Do not mechanically compare Kleibergen–Paap statistics with Stock–Yogo critical values derived under different assumptions.
- Do not claim that nonrejection of a statistical test proves exclusion or instrument validity.
- Use `lateffects` and `estat compliers` only when StataNow provides them; retain a transparent manual calculation as fallback.
- Include three theory and four empirical exam questions without public solutions or collapsible answers.
- Store the answer key outside Git in `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/claves_privadas/18_IV_LATE_clave.md` with mode `0600`.
- Do not render the full book, publish `docs/`, or push to GitHub without explicit approval.
- Preserve all unrelated modified and untracked files; stage only files named in each task.

---

## File map

**Modify**

- `18-IV.Rmd`: complete theory chapter and advanced readings.
- `19-IVStata.Rmd`: empirical chapter consuming canonical outputs.
- `dofile/18_IV_LATE/IV_LATE_simulacion.do`: canonical Stata generator, analyses, exports, and graphs.
- `dofile/18_IV_LATE/IV_LATE_simulacion.py`: optional lightweight numerical cross-check using the same documented parameters; remove any claim of exact equivalence if it is not tested.
- `dofile/18_IV_LATE/IV_LATE_simulacion.R`: same policy as the Python extension.

**Create**

- `tests/test_iv_late_pedagogy_contract.py`: chapter structure, language, notation, privacy, and provenance contracts.
- `dofile/18_IV_LATE/tests/test_iv_late_outputs.py`: numerical assertions over canonical CSV outputs.
- `dofile/18_IV_LATE/results/paces_truth.csv`: true ATE, ATT, LATE, compliance shares, and sample estimates.
- `dofile/18_IV_LATE/results/paces_estimators.csv`: ITT, first stage, reduced form, Wald, OLS, 2SLS, and complier-profile values.
- `dofile/18_IV_LATE/results/weak_iv_comparison.csv`: strong/weak scenario estimates and inference summaries.
- `dofile/18_IV_LATE/results/divorce_iv_estimators.csv`: OLS/IV and first-stage diagnostics for the fictitious divorce case.
- `dofile/18_IV_LATE/figures/compliance_types.png`: true compliance composition.
- `dofile/18_IV_LATE/figures/complier_profile.png`: complier versus population profile.
- `dofile/18_IV_LATE/figures/weak_iv_distributions.png`: strong versus weak IV sampling behavior.
- `dofile/18_IV_LATE/figures/divorce_first_stage.png`: continuous-instrument first-stage visualization.
- `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/claves_privadas/18_IV_LATE_clave.md`: private instructor/monitor key; never stage.

Generated `.dta` files may be retained in `dofile/18_IV_LATE/data/` only if they are linked as student downloads and remain small enough for the repository.

---

### Task 1: Lock the pedagogical and privacy contracts

**Files:**
- Create: `tests/test_iv_late_pedagogy_contract.py`
- Test: `tests/test_iv_late_pedagogy_contract.py`

**Interfaces:**
- Consumes: current `18-IV.Rmd`, `19-IVStata.Rmd`, and the approved design specification.
- Produces: failing contracts that define required chapter titles, ordering, terminology, output references, exam counts, and privacy boundaries.

- [ ] **Step 1: Write the structural contract**

Create a UTF-8 pytest module with helpers and explicit assertions:

```python
from pathlib import Path
import re

ROOT = Path(__file__).resolve().parents[1]
THEORY = (ROOT / "18-IV.Rmd").read_text(encoding="utf-8")
EMPIRICAL = (ROOT / "19-IVStata.Rmd").read_text(encoding="utf-8")

def test_titles_and_anchors_are_uniform():
    assert THEORY.startswith("# Variables instrumentales y LATE — Clase teórica {#iv}")
    assert EMPIRICAL.startswith("# Variables instrumentales y LATE — Clase empírica {#iv-stata}")

def test_empirical_materials_come_before_readings_and_goals():
    download = EMPIRICAL.index("Descargar do-file")
    readings = EMPIRICAL.index("Lecturas centrales")
    goals = EMPIRICAL.index("Metas de aprendizaje")
    assert download < readings < goals

def test_exam_question_counts_and_no_public_answers():
    assert len(re.findall(r"boxexam", THEORY)) == 3
    assert len(re.findall(r"boxexam", EMPIRICAL)) == 4
    forbidden = ("<details", "Respuesta:", "Solución:", "Mostrar respuesta")
    assert not any(token in THEORY + EMPIRICAL for token in forbidden)
```

- [ ] **Step 2: Add academic-content contracts**

Add tests requiring the exact core concepts while prohibiting known misleading claims:

```python
def test_late_and_complier_content_is_explicit():
    required = (
        "always-takers", "never-takers", "compliers", "defiers",
        "estat compliers", "pesos de Abadie", "no podemos identificar",
        "Y(D=1)", "Y(D=0)",
    )
    assert all(term in THEORY + EMPIRICAL for term in required)

def test_weak_iv_content_is_qualified():
    required = ("104.7", "Anderson–Rubin", "CLR", "Kleibergen–Paap", "Stock–Yogo")
    assert all(term in THEORY + EMPIRICAL for term in required)
    forbidden = (
        "F > 10 garantiza",
        "F > 104.7 garantiza",
        "la validez se comprueba con",
        "Hansen demuestra que",
    )
    assert not any(term in THEORY + EMPIRICAL for term in forbidden)

def test_simulated_data_and_output_provenance_are_visible():
    assert EMPIRICAL.lower().count("datos ficticios") >= 2
    assert "results/paces_estimators.csv" in EMPIRICAL
    assert "results/divorce_iv_estimators.csv" in EMPIRICAL
    assert "figures/weak_iv_distributions.png" in EMPIRICAL
```

- [ ] **Step 3: Add privacy and duplicate-numbering contracts**

```python
def test_private_key_is_not_linked_or_tracked_in_chapters():
    assert "claves_privadas" not in THEORY + EMPIRICAL
    assert "18_IV_LATE_clave" not in THEORY + EMPIRICAL

def test_headings_do_not_contain_manual_section_numbers():
    heading = re.compile(r"^#{2,4}\s+\d+(?:\.\d+)*[.)]?\s", re.MULTILINE)
    assert not heading.search(THEORY)
    assert not heading.search(EMPIRICAL)
```

- [ ] **Step 4: Run the contracts and verify that they fail for the current chapters**

Run:

```bash
python3 -m pytest tests/test_iv_late_pedagogy_contract.py -q
```

Expected: failures for titles, ordering, exam blocks, `estat compliers`, qualified weak-IV material, and output provenance.

- [ ] **Step 5: Commit the failing contracts**

```bash
git add tests/test_iv_late_pedagogy_contract.py
git commit -m "test: define IV and LATE teaching contracts"
```

---

### Task 2: Build the canonical PACES-style simulation and complier outputs

**Files:**
- Modify: `dofile/18_IV_LATE/IV_LATE_simulacion.do`
- Create: `dofile/18_IV_LATE/results/paces_truth.csv`
- Create: `dofile/18_IV_LATE/results/paces_estimators.csv`
- Create: `dofile/18_IV_LATE/figures/compliance_types.png`
- Create: `dofile/18_IV_LATE/figures/complier_profile.png`
- Test: `dofile/18_IV_LATE/tests/test_iv_late_outputs.py`

**Interfaces:**
- Consumes: seed `54687`; binary lottery `Z`; potential treatments `D0`, `D1`; potential outcomes `Y0`, `Y1`; predetermined covariates `female`, `baseline_score`, and `low_income`.
- Produces: investigator-facing variables `Z D Y female baseline_score low_income`, latent truth retained only in the instructor build, and stable result schemas `metric,value` plus `group,variable,mean`.

- [ ] **Step 1: Write the failing numerical tests for the output schemas**

Create `dofile/18_IV_LATE/tests/test_iv_late_outputs.py`:

```python
from pathlib import Path
import csv

BASE = Path(__file__).resolve().parents[1]

def rows(name):
    with (BASE / "results" / name).open(newline="", encoding="utf-8-sig") as handle:
        return list(csv.DictReader(handle))

def metrics(name):
    return {row["metric"]: float(row["value"]) for row in rows(name)}

def test_paces_truth_has_deliberately_distinct_estimands():
    m = metrics("paces_truth.csv")
    assert abs(m["late_true"] - m["ate_true"]) > 0.15
    assert abs(m["late_true"] - m["att_true"]) > 0.10
    assert abs(m["share_complier"] + m["share_always"] + m["share_never"] - 1) < 1e-10
    assert m["share_defier"] == 0

def test_wald_2sls_and_first_stage_identity():
    m = metrics("paces_estimators.csv")
    assert abs(m["wald"] - m["iv_2sls"]) < 1e-8
    assert abs(m["first_stage"] - m["share_complier_estimated"]) < 1e-8
    assert abs(m["iv_2sls"] - m["late_true"]) < 0.08
```

- [ ] **Step 2: Run the numerical tests and verify missing-output failure**

Run:

```bash
python3 -m pytest dofile/18_IV_LATE/tests/test_iv_late_outputs.py -q
```

Expected: FAIL because canonical CSV files do not exist.

- [ ] **Step 3: Replace the old compliance generator with explicit potential treatments**

In the do-file, create one PACES-style population with an explicit `compliance_type`, then define:

```stata
gen byte D0 = compliance_type == 2
gen byte D1 = inlist(compliance_type, 2, 3)
assert D1 >= D0
gen byte D = D0*(1-Z) + D1*Z
```

Use labels `1 "Never-taker" 2 "Always-taker" 3 "Complier"`. Generate heterogeneous individual effects from predetermined covariates and type so the stored true ATE, ATT, and LATE differ by the minimum margins asserted in the tests. Generate `Y0`, `Y1 = Y0 + tau_i`, and observed `Y = D*Y1 + (1-D)*Y0`.

- [ ] **Step 4: Export truth and investigator-facing datasets**

Use `preserve`, `collapse`, `postfile`, or a small Stata frame to export the exact schemas:

```text
paces_truth.csv: metric,value
paces_estimators.csv: metric,value
```

Required metrics are `ate_true`, `att_true`, `late_true`, `share_complier`, `share_always`, `share_never`, `share_defier`, `ols`, `itt`, `first_stage`, `reduced_form`, `wald`, `iv_2sls`, and `share_complier_estimated`. Save a student dataset containing only observed variables and an instructor dataset containing the latent truth; label both as simulated.

- [ ] **Step 5: Estimate and export the complier profile**

Run the transparent manual share calculations first. If available, run:

```stata
capture noisily lateffects kappa (Y) (D) (Z female baseline_score low_income)
if _rc == 0 {
    estat compliers female baseline_score low_income, genkappa(kappa)
}
```

Export population means, true-complier means, and estimated-complier means with columns `group,variable,mean`. Explicitly note in labels that `kappa` is not an individual complier probability.

- [ ] **Step 6: Produce the two PACES figures**

Export `compliance_types.png` from the known latent types and `complier_profile.png` from the three profile series. Use the established book palette and dimensions that remain legible at chapter width.

- [ ] **Step 7: Run Stata once and then run the numerical tests**

Run:

```bash
/Applications/StataNow/StataSE.app/Contents/MacOS/stata-se -b do dofile/18_IV_LATE/IV_LATE_simulacion.do
python3 -m pytest dofile/18_IV_LATE/tests/test_iv_late_outputs.py -q
```

Expected: Stata exit code 0; both tests PASS. Inspect the Stata log for `_rc`, `r(` errors, missing commands, or failed assertions before proceeding.

- [ ] **Step 8: Commit the canonical PACES simulation**

```bash
git add dofile/18_IV_LATE/IV_LATE_simulacion.do dofile/18_IV_LATE/tests/test_iv_late_outputs.py dofile/18_IV_LATE/results/paces_truth.csv dofile/18_IV_LATE/results/paces_estimators.csv dofile/18_IV_LATE/figures/compliance_types.png dofile/18_IV_LATE/figures/complier_profile.png
git commit -m "feat: add PACES-style LATE simulation and complier profiles"
```

---

### Task 3: Add strong/weak IV scenarios and robust inference outputs

**Files:**
- Modify: `dofile/18_IV_LATE/IV_LATE_simulacion.do`
- Modify: `dofile/18_IV_LATE/tests/test_iv_late_outputs.py`
- Create: `dofile/18_IV_LATE/results/weak_iv_comparison.csv`
- Create: `dofile/18_IV_LATE/figures/weak_iv_distributions.png`

**Interfaces:**
- Consumes: fixed sample size and common structural parameters; two first-stage strengths `weak` and `strong`.
- Produces: one row per scenario with `scenario,n,pi,first_stage_F,kp_F,ols,iv,conventional_ci_low,conventional_ci_high,ar_ci_low,ar_ci_high` where the available official Stata result names are documented in comments.

- [ ] **Step 1: Extend the output tests**

```python
def test_weak_and_strong_scenarios_vary_relevance_not_n():
    data = {row["scenario"]: row for row in rows("weak_iv_comparison.csv")}
    assert set(data) == {"weak", "strong"}
    assert int(data["weak"]["n"]) == int(data["strong"]["n"])
    assert float(data["strong"]["first_stage_F"]) > float(data["weak"]["first_stage_F"])
    assert float(data["strong"]["pi"]) > float(data["weak"]["pi"])
```

- [ ] **Step 2: Verify the new test fails**

Run the output test module and expect failure because `weak_iv_comparison.csv` does not exist.

- [ ] **Step 3: Implement equal-N strong and weak scenarios**

Replace the old design that only increases `N` with a program accepting `pi(real)` while holding `N`, structural effect, confounding, and disturbance distributions constant. Retain a small Monte Carlo count sufficient for stable teaching figures, and set the seed explicitly.

- [ ] **Step 4: Add diagnostic commands and weak-robust inference**

For each teaching dataset, run:

```stata
ivreg2 y (D = z), robust first
ivregress 2sls y (D = z), vce(robust)
capture noisily estat weakrobust, ci
```

Record first-stage diagnostics and conventional confidence intervals. Record AR/CLR output only after verifying the official stored results in the installed Stata version; if a bound is unbounded or disjoint, store a text status rather than inventing numeric endpoints.

- [ ] **Step 5: Export results and figure**

Create `weak_iv_comparison.csv` and a two-panel `weak_iv_distributions.png`. The figure must share the same horizontal scale and show the true effect.

- [ ] **Step 6: Run Stata once and re-run numerical tests**

Expected: equal `N`; strong `pi` and F exceed weak values; no hard-coded claim that either threshold proves validity.

- [ ] **Step 7: Commit weak-IV outputs**

```bash
git add dofile/18_IV_LATE/IV_LATE_simulacion.do dofile/18_IV_LATE/tests/test_iv_late_outputs.py dofile/18_IV_LATE/results/weak_iv_comparison.csv dofile/18_IV_LATE/figures/weak_iv_distributions.png
git commit -m "feat: add weak-IV diagnostics and robust inference comparison"
```

---

### Task 4: Add the fictitious divorce-IV case

**Files:**
- Modify: `dofile/18_IV_LATE/IV_LATE_simulacion.do`
- Modify: `dofile/18_IV_LATE/tests/test_iv_late_outputs.py`
- Create: `dofile/18_IV_LATE/results/divorce_iv_estimators.csv`
- Create: `dofile/18_IV_LATE/figures/divorce_first_stage.png`

**Interfaces:**
- Consumes: continuous fictitious workplace gender-balance instrument, endogenous binary divorce treatment, predetermined family controls, and child outcome.
- Produces: OLS/IV estimates and diagnostics without claiming a binary complier classification or guaranteed exclusion.

- [ ] **Step 1: Add output-schema tests**

```python
def test_divorce_case_is_continuous_iv_and_has_diagnostics():
    m = metrics("divorce_iv_estimators.csv")
    required = {"ols", "iv_2sls", "first_stage_slope", "first_stage_p", "kp_F"}
    assert required <= set(m)
    assert m["first_stage_slope"] > 0
```

- [ ] **Step 2: Verify missing-output failure**

Run the output test module and expect only the divorce-output test to fail.

- [ ] **Step 3: Generate the clearly labeled fictitious dataset**

Create an instrument on `[0,1]`, predetermined father/workplace/family covariates, an unobserved family-conflict component affecting divorce and child outcomes, and a binary divorce treatment whose propensity increases with workplace gender balance. Include a documented potential exclusion-violation channel variable for classroom discussion, but keep the baseline estimating equation explicit.

- [ ] **Step 4: Estimate OLS, first stage, and IV**

Use `ivreg2` with robust diagnostics and `ivregress 2sls` as the official cross-check. Do not run `lateffects`, because its instrument must be binary and this exercise deliberately uses a continuous instrument.

- [ ] **Step 5: Export the results and first-stage figure**

Export the `metric,value` file and a binned-scatter-style first-stage plot. Add graph notes stating “Datos ficticios inspirados en la estructura del artículo; no son los datos originales”.

- [ ] **Step 6: Run Stata once and verify all output tests**

Expected: all `dofile/18_IV_LATE/tests/test_iv_late_outputs.py` tests PASS and the Stata log contains no failed assertions.

- [ ] **Step 7: Commit the divorce case**

```bash
git add dofile/18_IV_LATE/IV_LATE_simulacion.do dofile/18_IV_LATE/tests/test_iv_late_outputs.py dofile/18_IV_LATE/results/divorce_iv_estimators.csv dofile/18_IV_LATE/figures/divorce_first_stage.png
git commit -m "feat: add critical divorce-IV teaching case"
```

---

### Task 5: Rebuild the theory chapter

**Files:**
- Modify: `18-IV.Rmd`
- Test: `tests/test_iv_late_pedagogy_contract.py`

**Interfaces:**
- Consumes: approved design, canonical terminology, and verified references.
- Produces: a self-contained theoretical sequence that prepares students for every empirical command without exposing solutions.

- [ ] **Step 1: Replace the title, goals, and opening sequence**

Use the required title and reorganize the opening around the causal problem, estimand, DAG, and distinct assumptions of relevance, independence, and exclusion. Remove the claim that “validity” is one covariance condition combining random assignment and no direct effect.

- [ ] **Step 2: Correct the IV/2SLS derivations and finite-sample language**

Retain Wald and projection-matrix derivations, but distinguish controls included in both stages, exact versus overidentification, correct 2SLS standard errors, and the fact that IV finite-sample behavior is not summarized by a universal bias formula.

- [ ] **Step 3: Expand the LATE framework**

Define `D(1)`, `D(0)`, the four principal strata, SUTVA, independence, exclusion, relevance, and monotonicity. Derive the complier-share identity and Wald LATE using `Y(D=1)` and `Y(D=0)`. Explain why identities are not observed even though shares and covariate means can be estimated.

- [ ] **Step 4: Replace the weak-IV section**

Present `F > 10` as a heuristic; explain Stock–Yogo’s setting; distinguish Cragg–Donald from heteroskedasticity-robust Kleibergen–Paap; explain the restricted setting behind 104.7; and motivate AR/CLR confidence sets. Remove any statement that a threshold guarantees acceptable inference.

- [ ] **Step 5: Add tests and limits of tests**

Explain DWH dependence on instrument validity, Hansen J’s joint-null interpretation, why nonrejection is not validation, and Kitagawa’s testable implications in an advanced block.

- [ ] **Step 6: Add the PACES and divorce comparison**

Use PACES to introduce a binary encouragement and the divorce paper to expose the challenge of exclusion and continuous-instrument locality. Link the open paper and distinguish published evidence from the fictitious classroom data.

- [ ] **Step 7: Add exactly three `boxexam` blocks**

Questions must cover: derivation/estimand, weak-IV inference, and instrument credibility. Do not include answers or `<details>` blocks.

- [ ] **Step 8: Run the theory-relevant contracts**

Run:

```bash
python3 -m pytest tests/test_iv_late_pedagogy_contract.py -q
```

Expected: empirical-output assertions may still fail, but all theory-title, notation, content, and exam-count assertions pass.

- [ ] **Step 9: Commit the theory chapter**

```bash
git add 18-IV.Rmd
git commit -m "feat: rebuild IV and LATE theory chapter"
```

---

### Task 6: Rebuild the empirical chapter from canonical outputs

**Files:**
- Modify: `19-IVStata.Rmd`
- Modify conditionally: `dofile/18_IV_LATE/IV_LATE_simulacion.py`
- Modify conditionally: `dofile/18_IV_LATE/IV_LATE_simulacion.R`
- Test: `tests/test_iv_late_pedagogy_contract.py`

**Interfaces:**
- Consumes: all CSV/PNG outputs from Tasks 2–4.
- Produces: a deep, reproducible empirical class with downloads first, visible Stata results, and four public questions without solutions.

- [ ] **Step 1: Fix the title and top-of-page resource order**

Place Stata do-file and simulated-data downloads directly below the title. Follow with central readings and then learning goals. Identify Stata as canonical; describe Python/R as optional only if their claims have been verified.

- [ ] **Step 2: Present Part A as revealed simulation truth**

Show `D(0)`, `D(1)`, `Y(D=0)`, `Y(D=1)`, compliance types, and true ATE/ATT/LATE. Import `paces_truth.csv` and `compliance_types.png` rather than typing values into Markdown.

- [ ] **Step 3: Present Part B as the investigator-facing PACES exercise**

Hide latent variables conceptually and walk through ITT, first stage, reduced form, Wald, OLS, 2SLS, and the estimand comparison. Import `paces_estimators.csv`, `complier_profile.png`, and show the exact Stata commands that generated them.

- [ ] **Step 4: Explain complier estimation precisely**

Show the three observable-cell mixtures, derive the shares, run `lateffects kappa`/`estat compliers` when available, and show the manual fallback. State that `kappa` weights are neither individual labels nor necessarily individual probabilities.

- [ ] **Step 5: Present weak-instrument diagnostics and inference**

Import `weak_iv_comparison.csv` and `weak_iv_distributions.png`. Interpret `ivreg2`, partial R-squared, KP F, and official weak-robust confidence sets without threshold absolutism.

- [ ] **Step 6: Present Part C as a critical divorce-IV case**

Import the divorce outputs and figure. Require students to distinguish relevance from exclusion and to list plausible direct channels. Explain why the continuous instrument does not create the same simple binary complier group as PACES.

- [ ] **Step 7: Add exactly four `boxexam` blocks**

Cover Wald/compliers, `ivreg2` diagnostics, conventional versus weak-robust inference, and the divorce exclusion restriction. Include no public answers.

- [ ] **Step 8: Resolve optional-language claims**

If Python and R are retained, make their seeds, parameters, schemas, and tolerance checks explicit. Otherwise, relabel them as optional illustrations and remove “producen los mismos resultados”. Do not spend time rebuilding them unless needed for a public download promise.

- [ ] **Step 9: Run all IV/LATE contracts**

```bash
python3 -m pytest tests/test_iv_late_pedagogy_contract.py dofile/18_IV_LATE/tests/test_iv_late_outputs.py -q
```

Expected: all tests PASS.

- [ ] **Step 10: Commit the empirical chapter**

```bash
git add 19-IVStata.Rmd dofile/18_IV_LATE/IV_LATE_simulacion.py dofile/18_IV_LATE/IV_LATE_simulacion.R
git commit -m "feat: rebuild IV and LATE empirical chapter"
```

Stage the Python/R files only if they actually changed.

---

### Task 7: Create the private instructor key

**Files:**
- Create outside repository: `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/claves_privadas/18_IV_LATE_clave.md`
- Test: filesystem permissions and Git exclusion.

**Interfaces:**
- Consumes: the seven final `boxexam` questions and canonical outputs.
- Produces: instructor/monitor solutions with reasoning, commands, expected interpretation, and grading criteria.

- [ ] **Step 1: Create the key with all seven solutions**

For each question include: learning objective, correct answer, required reasoning, relevant Stata output or command, common mistakes, and a point allocation summing to the question total.

- [ ] **Step 2: Restrict permissions**

Run:

```bash
chmod 600 /Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/claves_privadas/18_IV_LATE_clave.md
stat -f '%Sp %N' /Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/claves_privadas/18_IV_LATE_clave.md
```

Expected permission string: `-rw-------`.

- [ ] **Step 3: Verify it is outside Git and absent from public sources**

Run:

```bash
git status --short
rg -n "18_IV_LATE_clave|claves_privadas" 18-IV.Rmd 19-IVStata.Rmd docs || true
```

Expected: the key does not appear in `git status`; chapter/spec references must not expose a clickable path or solution content. The design/plan may name the private destination as an internal implementation requirement.

No commit is created for the private key.

---

### Task 8: Render, visually inspect, and prepare the review build

**Files:**
- Modify only if defects are found: `18-IV.Rmd`, `19-IVStata.Rmd`, canonical do-file, tests, or outputs.
- Generate locally: targeted review HTML or a review build outside tracked `docs/`.

**Interfaces:**
- Consumes: completed chapters and outputs.
- Produces: verified local review pages; no publication or push.

- [ ] **Step 1: Run focused automated verification**

```bash
python3 -m pytest tests/test_iv_late_pedagogy_contract.py dofile/18_IV_LATE/tests/test_iv_late_outputs.py tests/test_chapter_title_contract.py tests/test_potential_outcomes_notation.py -q
```

Expected: all tests PASS.

- [ ] **Step 2: Re-run the canonical Stata file only if source or outputs changed after the last successful run**

Expected: exit code 0, no failed assertions, and no missing-command errors. Do not rerun Monte Carlo merely to refresh timestamps.

- [ ] **Step 3: Render a local review build without publishing**

Use the project’s existing bookdown workflow with an output directory under `/private/tmp/libro_cortes_iv_late_review_20260804`. Do not overwrite tracked `docs/` and do not render more chapters than the toolchain requires for valid cross-references.

- [ ] **Step 4: Inspect both HTML pages visually**

Check: title consistency, no duplicate numbering, downloads above the fold, tables not clipped, figures legible, math rendered, colored blocks consistent, seven exam questions visible without answers, and links valid.

- [ ] **Step 5: Run final repository checks**

```bash
git diff --check
git status --short
git log -8 --oneline
```

Expected: no whitespace errors; unrelated pre-existing changes remain unstaged; only intentional IV/LATE commits were added.

- [ ] **Step 6: Commit only corrections found during QA**

```bash
git add 18-IV.Rmd 19-IVStata.Rmd dofile/18_IV_LATE tests/test_iv_late_pedagogy_contract.py
git commit -m "fix: finalize IV and LATE teaching pair"
```

Stage only files actually corrected. Stop with the local review path and request explicit approval before rebuilding tracked `docs/` or pushing.
