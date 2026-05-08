# Repeat measurement reproducibility — summary and reporting options

*Date: 2026-05-07*

This note summarises the bag-level reproducibility analysis for hand-measurement of cress seedling lengths, and proposes two ways the result could be presented in the ASPS manuscript.

---

## What was done

Four cress bags from a single potency × experiment cell (Mercurius 30x, experiment 3 of ASPS) were measured three times each on the same images, by the same person (AS), using ImageJ.

For each bag and each measurement round, the bag-mean sprout length and bag-mean seedling length were computed. The variation between the three repeats of the same bag quantifies hand-measurement noise.

---

## Two sources of variation in this design

It is important to keep these distinct, because they answer different questions.

**Variation between the four bags** — even though all four bags share the same potency and experiment, they are biologically different bags with their own seedlings. The differences in their bag-means (e.g. one bag averaged ~9.5 mm, another ~11.2 mm for seedlings) reflect natural between-bag biological variability *within a single experimental cell*. This is the same source of variation that contributes to the residual term of the main ANOVA.

**Variation between the three repeats of the same bag** — this is the question of interest. It reflects only differences arising from re-clicking the same seedlings on the same images at different times. With three rounds we can compute a per-bag SD across rounds, then pool those SDs across the four bags.

The first source of variation is biology; the second is hand-measurement noise. The ratio between them tells us how much of the unexplained variance in the main analysis is plausibly just measurement noise.

---

## Numbers

**Measurement reproducibility (within-bag, across rounds):**

| Parameter | n bags | Repeats per bag | Mean (mm) | SD_repeat (mm) | CV_repeat |
|---|---|---|---|---|---|
| Sprout length | 4 | 3 | 3.97 | 0.022 | 0.55% |
| Seedling length | 4 | 3 | 10.23 | 0.186 | 1.82% |

SD_repeat is the pooled within-bag SD across the three measurement rounds, computed as √(mean of bag-level variances). CV_repeat = SD_repeat / mean × 100%.

**Within-cell biological variation (residual from the full ASPS ANOVA):**

| Parameter | n bags | Mean (mm) | SD_within-cell (mm) | CV_within-cell |
|---|---|---|---|---|
| Sprout length | 725 | 3.90 | 0.139 | 3.6% |
| Seedling length | 725 | 10.29 | 0.532 | 5.2% |

SD_within-cell is the residual SD from the Type III ANOVA on bag-level means with potency, experiment number, and their interaction as factors.

---

## What the intraclass correlation coefficient (ICC) tells us

For each parameter we also fit a one-way random-effects model `bag_mean ~ 1 + (1 | image)` on the repeat-measurement data. This decomposes the total variance into:

- **Variance between bags** (var_between) — biological differences between the four bags
- **Variance within bags across rounds** (var_within) — measurement noise

The intraclass correlation coefficient is:

ICC = var_between / (var_between + var_within)

It is interpretable as "the fraction of total variation that comes from real bag-to-bag differences, rather than measurement noise" — or equivalently, "how reliable is one bag-mean measurement as an estimate of that bag's true mean, on a 0–1 scale."

| Parameter | var_between | var_within | ICC |
|---|---|---|---|
| Sprout length | 0.061 | 0.00048 | 0.992 |
| Seedling length | 0.708 | 0.0347 | 0.953 |

By standard reliability benchmarks (Koo & Li 2016: <0.5 poor, 0.5–0.75 moderate, 0.75–0.9 good, >0.9 excellent), both are excellent.

A note on interpreting ICCs: the ICC depends both on measurement noise and on how variable the bags happen to be. Because the four bags here all came from the same potency × experiment cell, the "between-bag" variance in the ICC reflects within-cell biological variation — exactly the source of variation that the main ANOVA residual is trying to capture. This makes the ICC particularly meaningful here, as it is computed against the natural variation present within a single experimental cell.

---

## Two suggested ways to report this in the manuscript

The two versions emphasise different framings. Either is defensible; the choice depends on whether the manuscript prefers a reliability-coefficient framing (ICC) or a variance-decomposition framing (CV + variance ratio).

### Version A — ICC framing

> To assess hand-measurement reproducibility, four bags from a single potency × experiment cell (Mercurius 30x, experiment 3) were measured three times each. The pooled within-bag SD across measurement rounds was 0.022 mm for sprout length and 0.186 mm for seedling length, corresponding to intraclass correlation coefficients of 0.99 and 0.95 respectively. Because the four bags share the same treatment, the ICC reflects measurement reliability against the natural between-bag biological variation present within a single experimental cell — the same source of variation that contributes to the ANOVA residual. The high ICCs indicate that measurement noise contributes substantially less than biological between-bag variation to the unexplained variance in the main analyses.

### Version B — CV and variance-decomposition framing

> Measurement noise was assessed by remeasuring four bags from a single potency × experiment cell (Mercurius 30x, experiment 3) three times each. The pooled within-bag CV across measurement rounds was 0.6% for sprout length and 1.8% for seedling length, compared to within-cell biological CVs of 3.6% and 5.2% respectively (residual SD from the full ANOVA). Because variances add, measurement noise accounts for approximately (0.6/3.6)² = 3% and (1.8/5.2)² = 12% of residual variance, indicating that bag-to-bag biological variation, not hand-measurement error, dominates the unexplained variance in the main analyses.

The explicit `(0.6/3.6)² = 3%` is included to prevent readers from computing the SD ratio (which would give 17% and 34% respectively) and confusing it with the variance contribution. SDs do not decompose additively; variances do, under the standard assumption that biological variation and measurement noise are independent.

---

## Caveats worth keeping in mind

The repeat SD itself is estimated from only four bags (n_bags = 4, df_within = 8), so the point estimates carry meaningful uncertainty. A rough 95% CI on the variance ratio for seedling spans approximately 6% to 24% — well within the "minor contributor" interpretation, but not precisely pinned down.

Hand-measurement noise was characterised by AS only. The JZ subset of the full dataset (experiments 6–10) has its own uncharacterised measurement noise, which could differ. The implicit assumption when comparing the repeat-SD to the full-dataset residual is that JZ's reproducibility is comparable to AS's. If a reviewer raises this, it would be straightforward to add a few JZ-remeasured bags as a cross-check.

The "Mercurius 30x, experiment 3" cell appears slightly more variable than the average within-cell SD across the full dataset (sprout SD 0.25 mm vs full-data 0.14 mm; seedling SD 0.84 mm vs 0.53 mm). This is plausibly just sampling variability from n=4, and does not affect the repeat-SD estimate, which is independent of which cell was sampled.
