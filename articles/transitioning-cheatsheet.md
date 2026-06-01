# SDS1000 → Base R Cheat Sheet

← Back to [Part 1: Simulation and Data
Summaries](https://emeyers.github.io/SDS1000/articles/transitioning-simulation.md)
 \|  [Part 2: Inference and
Visualization](https://emeyers.github.io/SDS1000/articles/transitioning-inference.md)

🖨 Print / Save as PDF

------------------------------------------------------------------------

## Repeating processes

| Task | SDS1000 | Base R |
|----|----|----|
| Repeat a process *n* times | `do_it(n) * { expr }` | `replicate(n, { expr })` |
| Repeat with explicit indexing | — | `for (i in 1:n) { result[i] <- expr }` |

------------------------------------------------------------------------

## Summarizing categorical data

| Task | SDS1000 | Base R |
|----|----|----|
| Proportion of one category | `get_proportion(v, "cat")` | `mean(v == "cat")` |
| Proportions of all categories | — | `prop.table(table(v))` |
| Counts of all categories | — | `table(v)` |

------------------------------------------------------------------------

## Simulating random outcomes

| Task | SDS1000 | Base R |
|----|----|----|
| Count of heads from *n* flips | `rflip(n, prob)` | `rbinom(1, size = n, prob = prob)` |
| Proportion of heads from *n* flips | `rflip(n, prob) / n` | `rbinom(1, n, prob) / n` |
| Counts for each face of a die | `rroll(n, prob)` | `as.vector(rmultinom(1, size = n, prob = prob))` |

------------------------------------------------------------------------

## Permutation tests and bootstrapping

| Task | SDS1000 | Base R |
|----|----|----|
| Randomly permute a vector | `shuffle(v)` | `sample(v)` |
| Bootstrap resample (paired vectors) | `resample_pairs(v1, v2)` | `idx <- sample(n, replace=TRUE); v1[idx]; v2[idx]` |
| Bootstrap resample (independent groups) | — | `sample(v, length(v), replace = TRUE)` |

------------------------------------------------------------------------

## Computing p-values

| Task | SDS1000 | Base R |
|----|----|----|
| Upper-tail p-value | `ptail(obs, null, lower.tail = FALSE)` | `mean(null >= obs)` |
| Lower-tail p-value | `ptail(obs, null, lower.tail = TRUE)` | `mean(null <= obs)` |

------------------------------------------------------------------------

## Confidence interval critical values

| Task | SDS1000 | Base R |
|----|----|----|
| Normal critical value for C% CI | `cnorm(C)` | `qnorm(1 - (1 - C) / 2)` |
| t critical value for C% CI | `ct(C, df)` | `qt(1 - (1 - C) / 2, df = df)` |
| Both tails (±) | `cnorm(C, side = "both")` | `c(qnorm((1-C)/2), qnorm(1-(1-C)/2))` |

Common z\* values: 90% → 1.645  \|  95% → 1.960  \|  99% → 2.576

------------------------------------------------------------------------

## Plotting theoretical distributions

| Distribution | Density | p-value | SDS1000 |
|----|----|----|----|
| Normal | `dnorm(x, mean, sd)` | [`pnorm()`](https://rdrr.io/r/stats/Normal.html) | [`plot_norm()`](https://emeyers.github.io/SDS1000/reference/plot_norm.md) |
| t | `dt(x, df)` | [`pt()`](https://rdrr.io/r/stats/TDist.html) | [`plot_t()`](https://emeyers.github.io/SDS1000/reference/plot_t.md) |
| Chi-squared | `dchisq(x, df)` | [`pchisq()`](https://rdrr.io/r/stats/Chisquare.html) | [`plot_chisq()`](https://emeyers.github.io/SDS1000/reference/plot_chisq.md) |
| F | `df(x, df1, df2)` | [`pf()`](https://rdrr.io/r/stats/Fdist.html) | [`plot_f()`](https://emeyers.github.io/SDS1000/reference/plot_f.md) |

**Pattern for any distribution:**

``` r

x    <- seq(lower, upper, length.out = 1000)
dens <- d___(x, ...)
plot(x, dens, type = "l", xlab = "...", ylab = "Density")
abline(v = obs_stat, col = "red", lwd = 2)
```
