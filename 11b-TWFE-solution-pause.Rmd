# Solution: Staggered Adoption {-}

> **Solution to the pause** — *Econometria Avanzada · Javeriana 2026-I*

---

## Panel structure {-}

| Group | id | Treatment onset | $\tau$ |
|-------|----|-------------|--------|
| Never treated | 1, 2 | — | 0 |
| EARLY | 3, 4 | $t = 2$ | 2 |
| MEDIUM | 5, 6 | $t = 4$ | 3 |
| LATE | 7, 8 | $t = 6$ | 5 |

---

## Part A — True ATT {-}

$$\text{ATT} = \frac{\sum_g (\text{periods treated}_g \times n_g \times \tau_g)}{\text{total treated obs.}}$$

| Group | Periods treated | Units | $\tau$ | Contribution |
|-------|---------------|----------|--------|--------------|
| EARLY | 5 ($t=2\ldots6$) | 2 | 2 | 20 |
| MEDIUM | 3 ($t=4\ldots6$) | 2 | 3 | 18 |
| LATE | 1 ($t=6$) | 2 | 5 | 10 |
| **Total** | | | | **18 treated obs.** |

$$\boxed{\text{ATT} = \frac{48}{18} = \frac{8}{3} \approx 2.667}$$

---

## Part B — Clean comparisons {-}

Each DiD restricted to {cohort + never treated} recovers $\tau$ exactly because the control group never receives treatment in any period.

```stata
xtreg Y D i.t if inlist(id,1,2,3,4), fe robust   // --> 2.000
xtreg Y D i.t if inlist(id,1,2,5,6), fe robust   // --> 3.000
xtreg Y D i.t if inlist(id,1,2,7,8), fe robust   // --> 5.000
```

---

## Part C — All possible comparisons {-}

With 3 treated cohorts + 1 never-treated group, TWFE is a weighted average of **9 pairwise 2x2 comparisons** (Bacon decomposition). Key question: which are **clean** and which are **contaminated**?

### When is a comparison clean? {-}

A comparison is **clean** when the control group **has not received treatment** in the relevant period. It is **contaminated** when the control is already treated — in that case, the observed change in the control is not zero even without new treatment; it reflects its own prior effect.

### The 9 comparisons {-}

| # | Treated | Control | Contaminated? | Reason |
|---|---------|---------|:---:|-------|
| 1 | EARLY | Never treated | ✅ Clean | Control never treated |
| 2 | MEDIUM | Never treated | ✅ Clean | Control never treated |
| 3 | LATE | Never treated | ✅ Clean | Control never treated |
| 4 | EARLY | MEDIUM (before $t=4$) | ✅ Clean | MEDIUM not yet treated at $t=1,2,3$ |
| 5 | EARLY | LATE (before $t=6$) | ✅ Clean | LATE not yet treated at $t=1\ldots5$ |
| 6 | MEDIUM | LATE (before $t=6$) | ✅ Clean | LATE not yet treated at $t=1\ldots5$ |
| 7 | MEDIUM | EARLY (after $t=2$) | ❌ Contaminated | EARLY **already treated** when measuring MEDIUM effect |
| 8 | LATE | EARLY (after $t=2$) | ❌ Contaminated | EARLY **already treated** when measuring LATE effect |
| 9 | LATE | MEDIUM (after $t=4$) | ❌ Contaminated | MEDIUM **already treated** when measuring LATE effect |

**Summary:** 6 clean comparisons, 3 contaminated.

### Are contaminated always biased? {-}

Not necessarily. In this DGP the effects are **constant over time** — EARLY always has $Y=2$ after $t=2$, no changes. So when MEDIUM enters treatment at $t=4$ and we compare with EARLY (already treated), the "control" doesn't change ($\Delta Y_\text{EARLY}=0$), which means the comparison still recovers $\tau_\text{MEDIUM}=3$.

The problem arises when effects are **dynamic** (growing or shrinking after treatment). In that case, the already-treated group continues changing, contaminates the DiD, and the comparison fails to recover the correct $\tau$.

### Diagnosis in Stata {-}

```stata
* Full TWFE
reghdfe Y D, absorb(id t) vce(robust)
* --> coefficient != ATT (because Bacon weights != ATT weights)

* See all comparisons and their weights
bacondecomp Y D, ddetail

* Robust estimator (uses only clean comparisons)
xtdidregress (Y) (D), group(id) time(t)
* --> recovers ATT = 2.667
```

`xtdidregress` and `csdid` avoid contaminated comparisons by constructing each DiD using only units that **have not yet been treated** as controls.

---

> **Reference:** Goodman-Bacon, A. (2021). Difference-in-differences with variation in treatment timing. *Journal of Econometrics*, 225(2), 254–277.
