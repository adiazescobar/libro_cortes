# Aligned Causal Parameters Class Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Align the empirical causal-parameters chapter and its Stata, R, and Python materials around the original eight-person exercise, persistence of bias at N=10,000, and random assignment.

**Architecture:** One shared pedagogical sequence is implemented independently in the three languages and consumed by the R Markdown chapter. Deterministic results must match exactly; the only Monte Carlo redraw is treatment assignment, and simulation results are checked with statistical tolerances rather than identical pseudo-random draws.

**Tech Stack:** Bookdown/R Markdown, Stata 19, R with `haven` and base functions, Python/Jupyter with pandas/numpy, pytest.

## Global Constraints

- Keep the same eight profiles and the same `yd0` and `yd1` throughout.
- The observational assignment remains unchanged when expanding to exactly N=10,000.
- Monte Carlo generates only a new random `D` in each repetition; do not use `invlogit()` or a new selection DGP.
- The chapter begins with the established `.class-materials` download box and direct raw GitHub links.
- Stata, R, and Python expose the same four stages and common result names.
- Preserve unrelated changes already present in the dirty worktree.

---

### Task 1: Replace the old pedagogical contracts

**Files:**
- Modify: `tests/test_parametros_stata_contract.py`
- Modify: `tests/test_parametros_pedagogy_contract.py`

**Interfaces:**
- Consumes: the approved design in `docs/superpowers/specs/2026-08-10-parametros-empiricos-alineacion-design.md`.
- Produces: executable contracts for the chapter, do-file, artifact schemas, and cross-language source files.

- [ ] **Step 1: Rewrite tests that require the obsolete selection Monte Carlo**

Require `.class-materials`, “Descarga antes de comenzar”, raw links for the four student files, exactly N=10,000 for the expanded observational data, no `invlogit`, and only the `aleatorizacion` Monte Carlo scenario. Require the guided stages “Ejercicio manual”, “Misma selección con N = 10.000”, “Una asignación aleatoria”, and “Monte Carlo: un D nuevo en cada repetición”.

- [ ] **Step 2: Add cross-language source contracts**

Assert that `04_stata.do`, `04_R.R`, and `04_phyton.ipynb` each contain the four stage labels or equivalent markers, build observed `y` from `D`, `yd1`, and `yd0`, expand/resample to 10,000 without changing observational `D`, and redraw random `D` inside the Monte Carlo loop.

- [ ] **Step 3: Run the focused tests and confirm failure**

Run:

```bash
pytest -q tests/test_parametros_stata_contract.py tests/test_parametros_pedagogy_contract.py
```

Expected: failures identify the old download list, `expand 10000` producing 80,000 rows, the selection scenario, `invlogit()`, and obsolete headings.

- [ ] **Step 4: Commit the new contracts**

```bash
git add tests/test_parametros_stata_contract.py tests/test_parametros_pedagogy_contract.py
git commit -m "test: define aligned parameters class contract"
```

### Task 2: Simplify and validate the Stata pipeline

**Files:**
- Modify: `dofile/04_ParametrosStata/04_stata.do`
- Regenerate: `dofile/04_ParametrosStata/04_stata.log`
- Regenerate: `dofile/04_ParametrosStata/results/parameters_results.csv`
- Regenerate: `dofile/04_ParametrosStata/results/parameters_results.dta`
- Regenerate: `dofile/04_ParametrosStata/results/monte_carlo_summary.csv`
- Regenerate: `dofile/04_ParametrosStata/results/monte_carlo_summary.dta`
- Regenerate: `dofile/04_ParametrosStata/results/monte_carlo_draws.dta`
- Regenerate: `dofile/04_ParametrosStata/sesgo_con_aleatorizacion.png`

**Interfaces:**
- Consumes: `04_data.dta` with variables `yd0`, `yd1`, and observational `D`.
- Produces: point results with scenarios `datos_originales`, `seleccion_n10000`, and `aleatorizacion_unica`; Monte Carlo results with scenario `aleatorizacion` and columns `escenario,N,media,desv_est,p5,mediana,p95`.

- [ ] **Step 1: Implement the transparent eight-person calculations**

Build `X`, `tau`, and `y = D*yd1 + (1-D)*yd0`; calculate ATE, ATT, ATU, CATE(0), CATE(1), NAIVE, `NAIVE-ATT`, and the robust regression equivalence. Keep a compact posting helper only for exporting the common schema.

- [ ] **Step 2: Expand proportionally to exactly 10,000 rows**

Use `expand 1250` on the eight original profiles. Do not change `D`. Post the same estimands as scenario `seleccion_n10000` and assert `_N == 10000` plus equality of NAIVE and selection bias with the eight-row values within `1e-10`.

- [ ] **Step 3: Add one random assignment**

Starting from the 10,000-row profiles, drop observational `D`, set seed 87634, generate `D = runiform() < .5`, reconstruct `y`, and post scenario `aleatorizacion_unica`.

- [ ] **Step 4: Replace both old simulations with one randomization Monte Carlo**

Define an `rclass` program that loads the fixed 10,000-profile population, generates a new Bernoulli(0.5) `D`, reconstructs `y`, and returns `estimador = mean(y|D=1)-mean(y|D=0)`. Run 1,000 repetitions, summarize the estimator around the fixed ATE, and export one histogram with vertical lines at zero and the ATE.

- [ ] **Step 5: Execute Stata and inspect assertions**

Run:

```bash
/Applications/StataNow/StataSE.app/Contents/MacOS/stata-se -b do 04_stata.do
```

from `dofile/04_ParametrosStata`. Expected: exit code 0, “Pipeline canónico completado”, N=10,000 for `seleccion_n10000`, and no assertion failures.

- [ ] **Step 6: Run focused tests**

```bash
pytest -q tests/test_parametros_stata_contract.py
```

Expected: any remaining failures concern chapter or cross-language files, not the Stata pipeline or exported artifacts.

- [ ] **Step 7: Commit the Stata pipeline and artifacts**

```bash
git add dofile/04_ParametrosStata/04_stata.do dofile/04_ParametrosStata/04_stata.log dofile/04_ParametrosStata/results dofile/04_ParametrosStata/sesgo_con_aleatorizacion.png
git commit -m "feat: align Stata parameters demonstration"
```

### Task 3: Align the R and Python materials

**Files:**
- Modify: `dofile/04_ParametrosStata/04_R.R`
- Modify: `dofile/04_ParametrosStata/04_phyton.ipynb`

**Interfaces:**
- Consumes: the same `04_data.dta` and four-stage definitions as Stata.
- Produces: deterministic named results for the original and N=10,000 samples, one random assignment, and 1,000 randomization estimates centered on ATE.

- [ ] **Step 1: Rewrite the R script as the four-stage lesson**

Use a single `calcular_estimandos(data)` function returning a one-row data frame with `ATE`, `ATT`, `ATU`, `CATE_X0`, `CATE_X1`, `NAIVE`, and `SESGO_ATT`. Replicate rows with `rep(seq_len(nrow(df)), each=1250)`, retain observational `D`, then randomize only `D`. In Monte Carlo, redraw `D` on every iteration and store the difference in observed means.

- [ ] **Step 2: Add executable R assertions**

Use `stopifnot(nrow(df_n10000) == 10000)`, exact-tolerance comparisons for original versus expanded NAIVE and `SESGO_ATT`, and a Monte Carlo tolerance based on `3 * sd(mc_estimator)/sqrt(1000)` around ATE.

- [ ] **Step 3: Rewrite the Python notebook cells in the same order**

Create a pure `calculate_estimands(df)` function returning the same seven names. Build N=10,000 with `df.loc[df.index.repeat(1250)]`, preserve observational `D`, and use `numpy.random.default_rng` to generate one assignment and a fresh assignment within every Monte Carlo iteration. Include assertions equivalent to R.

- [ ] **Step 4: Execute both materials**

Run:

```bash
Rscript 04_R.R
jupyter nbconvert --to notebook --execute 04_phyton.ipynb --output /tmp/04_phyton.executed.ipynb --ExecutePreprocessor.timeout=180
```

from `dofile/04_ParametrosStata`. Expected: both exit successfully; deterministic results match Stata exactly; Monte Carlo checks pass.

- [ ] **Step 5: Run focused contracts and commit**

```bash
pytest -q tests/test_parametros_stata_contract.py tests/test_parametros_pedagogy_contract.py
git add dofile/04_ParametrosStata/04_R.R dofile/04_ParametrosStata/04_phyton.ipynb
git commit -m "feat: align R and Python parameters demonstrations"
```

### Task 4: Rebuild the empirical chapter around the four demonstrations

**Files:**
- Modify: `04-ParametrosStata.Rmd`
- Regenerate: `docs/parametros-causales-clase-empirica.html`

**Interfaces:**
- Consumes: the canonical CSV schemas and image generated by Task 2.
- Produces: the student-facing chapter with matching code excerpts and interpolated canonical results.

- [ ] **Step 1: Add the standard download box**

At the top, add `.class-materials`, the exact label “Descarga antes de comenzar”, and raw GitHub links for `04_stata.do`, `04_data.dta`, `04_R.R`, and `04_phyton.ipynb`, plus the Colab badge/link.

- [ ] **Step 2: Replace the body with the four-stage sequence**

Keep the manual parameter definitions and regression equivalence, then show exactly `expand 1250`, unchanged observational `D`, a single random assignment, and a Monte Carlo program that redraws only `D`. Remove the selection `invlogit()` block, the selection histogram, and the two-scenario comparison.

- [ ] **Step 3: Update tables, interpretation boxes, synthesis, and exercises**

Make the central distinction explicit: N=10,000 does not change the probability limit under selection; random assignment makes the difference in means unbiased for ATE in repeated assignment. Replace the obsolete exercise that asks students to reverse the `invlogit()` rule with an exercise interpreting why a single randomized estimate can differ from ATE while its repeated-assignment mean does not.

- [ ] **Step 4: Render the book and inspect the chapter**

Run:

```bash
Rscript -e 'bookdown::render_book("index.Rmd", output_dir="docs")'
```

Expected: successful render; the empirical parameters HTML contains the download box, N=10,000, the four stages, and no `invlogit` or Monte Carlo selection scenario.

- [ ] **Step 5: Run focused tests and commit**

```bash
pytest -q tests/test_parametros_stata_contract.py tests/test_parametros_pedagogy_contract.py tests/test_parametros_theory_contract.py
git add 04-ParametrosStata.Rmd docs/parametros-causales-clase-empirica.html
git commit -m "docs: rebuild empirical parameters class"
```

### Task 5: Full verification and handoff

**Files:**
- Verify only: all files changed in Tasks 1–4.

**Interfaces:**
- Consumes: completed Stata, R, Python, and Bookdown artifacts.
- Produces: evidence that the lesson is internally consistent and unrelated chapters remain intact.

- [ ] **Step 1: Run the complete test suite**

```bash
pytest -q
```

Expected: all tests pass.

- [ ] **Step 2: Re-run the three language materials from clean inputs**

Run the Stata, R, and notebook commands from Tasks 2 and 3. Expected: all assertions pass and deterministic results remain equal.

- [ ] **Step 3: Audit forbidden remnants and downloads**

```bash
rg -n 'invlogit|Monte Carlo con selección|sesgo_con_seleccion|expand 10000|80\.000' 04-ParametrosStata.Rmd dofile/04_ParametrosStata/04_stata.do dofile/04_ParametrosStata/04_R.R
rg -n 'class-materials|Descarga antes de comenzar|raw.githubusercontent.com/adiazescobar/libro_cortes/main/dofile/04_ParametrosStata' 04-ParametrosStata.Rmd
```

Expected: the first command returns no matches; the second returns the download box and direct links.

- [ ] **Step 4: Inspect the final diff without disturbing unrelated changes**

```bash
git status --short
git diff --check HEAD~4..HEAD
```

Expected: no whitespace errors; pre-existing unrelated worktree changes remain uncommitted and untouched.
