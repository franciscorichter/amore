# Model comparison on a real REM dataset

This vignette walks through the canonical “candidate-specifications →
AIC ranking” workflow used in REM literature (Juozaitienė & Wit 2024,
*JRSS-A* 188(4)) — with `amorem`’s bundled real-world dataset and the
one-call
[`compare_models()`](https://franciscorichter.github.io/amorem/reference/compare_models.md)
helper.

``` r

library(amorem)
```

## 1. Load a bundled REM dataset

The package ships four real-world REM datasets directly. We use the
McFarland (2001) high-school classroom session: 691 directed
interactions among 20 students and one instructor, recorded on 16
October 1996.

``` r

data(classroom_events)
data(classroom_actors)
nrow(classroom_events)
#> [1] 691
head(classroom_events, 3)
#>    time sender receiver interaction_type weight
#> 1 0.125     14       12           social      1
#> 2 0.250     12       14           social      1
#> 3 0.375     18       12         sanction      1
table(classroom_actors$role)
#> 
#> instructor   grade_11   grade_12 
#>          2         13          5
```

`classroom_events` follows the package’s `(sender, receiver, time, ...)`
contract — the same contract every downstream helper expects.

## 2. Build candidate specifications

[`compare_models()`](https://franciscorichter.github.io/amorem/reference/compare_models.md)
accepts a named list of character vectors. Each entry is one candidate
specification; the vector contents are the endogenous statistic names.
Here we compare three minimal specifications:

``` r

specs <- list(
  count        = c("reciprocity_count",
                   "transitivity_count"),
  continuous   = c("reciprocity_time_recent",
                   "transitivity_time_recent"),
  interrupted  = c("reciprocity_time_recent_interrupted",
                   "transitivity_time_recent_interrupted"))
```

These are the paper’s definitions r^((1c)/t)(1c) (count),
r^((4ac)/t)(7ac) (continuous timing), and r^((4ai)/t)(7ai) (interrupted
timing).

## 3. Compare by AIC

``` r

res <- compare_models(classroom_events, specs, seed = 11)
res
#>         model n_terms n_obs   log_lik      AIC delta_AIC
#> 1       count       2   691 -305.5233 615.0466    0.0000
#> 2  continuous       2   691 -421.1231 846.2462  231.1996
#> 3 interrupted       2   691 -439.6904 883.3809  268.3343
```

The helper draws one case-control sample (default `n_controls = 1`)
shared across every specification, computes the union of all requested
statistics with
[`endogenous_features()`](https://franciscorichter.github.io/amorem/reference/endogenous_features.md),
builds case-minus-control differences, and fits one no-intercept
binomial GLM per spec. Returned rows are sorted by AIC; the winning spec
has `delta_AIC = 0`.

On Classroom the count-based specification wins this stripped-down
comparison — recall that this is a no-smooth, no-random-effect baseline;
richer fits (thin-plate splines on the differences, sender/receiver
random effects) typically reweight the ranking in favour of the temporal
definitions, matching the empirical findings of the paper.

### Multiple controls per case

For 1:1 matching the helper fits a no-intercept binomial GLM on
case-minus-control differences. Set `n_controls > 1` to switch to
stratified conditional logistic regression via
[`survival::coxph`](https://rdrr.io/pkg/survival/man/coxph.html) — the
right tool when you want more controls per case for tighter inference:

``` r

compare_models(classroom_events, specs,
               n_controls = 3, seed = 11)
#>         model n_terms n_obs   log_lik      AIC delta_AIC
#> 1       count       2   691 -730.3872 1464.774    0.0000
#> 2  continuous       2   691 -850.2339 1704.468  239.6934
#> 3 interrupted       2   691 -931.9619 1867.924  403.1494
```

The `n_obs` column now reports the number of strata (one per case). AIC
values across specs remain comparable because every spec sees the same
shared case-control sample.

## 4. Inspect coefficients of a chosen specification

[`compare_models()`](https://franciscorichter.github.io/amorem/reference/compare_models.md)
returns AIC summaries. To inspect coefficients for a single spec, build
the case-control sample once and fit directly:

``` r

stat_set <- specs$interrupted
cc <- sample_non_events(classroom_events, n_controls = 1,
                        scope = "all", mode = "one", seed = 11)
cc_feat <- endogenous_features(cc, stats = stat_set)
for (st in stat_set) cc_feat[[st]][is.na(cc_feat[[st]])] <- 0

cases <- cc_feat[cc_feat$event == 1L, ]
ctrls <- cc_feat[cc_feat$event == 0L, ]
cases <- cases[order(cases$stratum), ]
ctrls <- ctrls[order(ctrls$stratum), ]

df <- data.frame(
  one      = rep(1, nrow(cases)),
  d_rec    = cases[[stat_set[1]]] - ctrls[[stat_set[1]]],
  d_trans  = cases[[stat_set[2]]] - ctrls[[stat_set[2]]])

fit <- glm(one ~ d_rec + d_trans - 1, family = "binomial", data = df)
summary(fit)$coefficients
#>           Estimate Std. Error   z value     Pr(>|z|)
#> d_rec   -0.1008825 0.01912790 -5.274104 1.334064e-07
#> d_trans -0.1319044 0.02943224 -4.481630 7.407511e-06
```

The same recipe scales to any subset of the statistics in the catalogue.
Use
[`?endogenous_features`](https://franciscorichter.github.io/amorem/reference/endogenous_features.md)
to see the full list.

## 5. Cross-implementation guarantee

Every statistic the post-hoc engine computes is also generated by
[`simulate_relational_events()`](https://franciscorichter.github.io/amorem/reference/simulate_relational_events.md)
using the same paper definitions. The package ships a parity test
(`test-sim-vs-posthoc-parity.R`) that runs the simulator on every shared
stat and verifies that re-running
[`endogenous_features()`](https://franciscorichter.github.io/amorem/reference/endogenous_features.md)
on the resulting event log reproduces the simulator’s columns
row-for-row. This means: if you want to test a model selection pipeline
against ground-truth coefficients, you can simulate data with
[`simulate_relational_events()`](https://franciscorichter.github.io/amorem/reference/simulate_relational_events.md)
using known effects and use
[`compare_models()`](https://franciscorichter.github.io/amorem/reference/compare_models.md)
to confirm the ranking.

``` r

set.seed(2026)
sim <- simulate_relational_events(
  n_events = 600,
  senders   = LETTERS[1:8],
  receivers = LETTERS[1:8],
  baseline_rate = 1,
  allow_loops = FALSE,
  endogenous_stats   = c("reciprocity_count", "transitivity_count"),
  endogenous_effects = c(reciprocity_count = 0.4, transitivity_count = 0.0))

# Among these three specs, the "count" spec is the true generative
# process. compare_models() should rank it first.
res2 <- compare_models(sim, specs, seed = 7)
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
res2
#>         model n_terms n_obs    log_lik      AIC delta_AIC
#> 1       count       2   600  -60.15295 124.3059    0.0000
#> 2  continuous       2   600 -378.42664 760.8533  636.5474
#> 3 interrupted       2   600 -407.21051 818.4210  694.1151
```

## References

- Juozaitienė R, Wit EC (2024). It’s about time: revisiting reciprocity
  and triadicity in relational event analysis. *Journal of the Royal
  Statistical Society Series A* 188(4), 1246–1262.
  <doi:10.1093/jrsssa/qnae132>.
- Vu D, Pattison P, Robins G (2017). Relational event models for social
  learning in MOOCs. *Social Networks* 43, 121–135.
- McFarland D (2001). Student resistance. *AJS* 107(3), 612–678.
