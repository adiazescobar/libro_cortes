# Task 3 report: placebos e inferencia de sensibilidad

## Status

Complete.

## Implementation commit

`1cedb5d` — `feat: add synthetic-control placebos and sensitivity`

`6c4530d` — `fix: keep synthetic-control native output temporary`

## Delivered

- 39 spatial placebo assignments, each excluding its treated unit from its donor pool.
- Canonical specification preserved: `beer(1984(1)1988) lnincome retprice age15to24 cigsale(1988) cigsale(1980) cigsale(1975)`, `trperiod(1989)`, `xperiod(1980(1)1988)`, `nested`.
- Authorized Utah handling: first `nested` attempt records `rc=430`; the fallback retains the identical specification and donor pool without `nested`.
- Eligibility defined as pre-treatment RMSPE no larger than five times California's.
- Temporal placebo at 1980 using information only through 1979.
- Leave-one-out estimates for every donor with strictly positive canonical weight, including New Mexico (`0.001`).
- Three CSV outputs and four 1800x1080 PNG outputs.

## Results

- Spatial assignments: 39 unique units, including California once.
- Optimization labels: 38 `nested`; one `default_fallback_after_rc430` (Utah).
- Eligible spatial assignments: 33.
- Proportion of eligible placebos with RMSPE ratio at least as large as California: `1/33 = 0.03030303`. This is reported descriptively, not as a conventional p-value.
- Temporal placebo: 19 finite annual gaps, 1970–1988.
- Leave-one-out: 186 finite rows (31 years for each of Colorado, Connecticut, Montana, Nevada, New Mexico, and Utah).

## Verification

- RED: the focused placebo/leave-one-out tests initially failed because the required CSV files did not exist; the optimization-label assertion then failed before the `optimization` field was added.
- Final focused contract: `python3 -m pytest -q tests/test_synthetic_controls_contract.py -k "rmspe or placebo or leave_one_out"` → `3 passed, 8 deselected`.
- Final artifact audit confirmed unique coverage, finite metrics, exact optimization counts, the 1970–1988 temporal range, and exact agreement between positive-weight donors and leave-one-out states.
- The final Stata batch run completed with `Estimación canónica, placebos y sensibilidad de Prop 99 completados.` and logged Utah's `nested` `rc=430` before the authorized fallback.

## Known unrelated failures

The full `tests/test_synthetic_controls_contract.py` run has five Task 1 failures: the two chapter Rmd files are absent, the chapter is absent from `_bookdown.yml`, and the external private-key requirement is absent. The remaining six tests pass. No Task 1 files were changed here.

## Worktree note

### Reproducibility cleanup (review round 1/5)

The principal `synth, keep()` output now uses the Stata tempfile `main_native` and is consumed only within the same run. The mutable binary `dofile/17_SyntheticControls/results/california_synth_native.dta` was removed from version control and is no longer created in the repository. The contract explicitly rejects any renewed reference to that path.
