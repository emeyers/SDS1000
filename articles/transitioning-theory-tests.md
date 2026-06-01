# Transitioning to Base R: Theory-Based Hypothesis Tests

## Overview

In S&DS 1000, you built null distributions by simulation (using
[`do_it()`](https://emeyers.github.io/SDS1000/reference/do_it.md) and
[`shuffle()`](https://emeyers.github.io/SDS1000/reference/shuffle.md) /
[`rflip()`](https://emeyers.github.io/SDS1000/reference/rflip.md)) and
computed p-values by counting. That **randomization** approach works for
almost any statistic and requires very few assumptions.

For many common situations — comparing means, testing correlations,
comparing proportions — statisticians have worked out what the null
distribution looks like *mathematically*. Base R provides dedicated
functions for these **theory-based** tests that return p-values,
confidence intervals, and test statistics in a single call.

**In this article:**

- [t-tests — `t.test()`](#t-tests-comparing-means)
- [Correlation — `cor.test()`](#correlation-cor.test)
- [ANOVA — `aov()` and `summary()`](#one-way-anova-aov)
- [Chi-squared test — `chisq.test()`](#chi-squared-test-chisq.test)
- [Linear regression — `lm()` and `summary()`](#linear-regression-lm)

**Also in this series:**

- [Part 1: Simulation and Data
  Summaries](https://emeyers.github.io/SDS1000/articles/transitioning-simulation.md)
- [Part 2: Inference and
  Visualization](https://emeyers.github.io/SDS1000/articles/transitioning-inference.md)
- [Cheat
  Sheet](https://emeyers.github.io/SDS1000/articles/transitioning-cheatsheet.md)

**A note on assumptions.** Theory-based tests rely on mathematical
approximations that are only valid when certain assumptions hold (e.g.,
approximate normality, independence). Always check that assumptions are
reasonable before trusting a p-value. The randomization approach from
Parts 1 and 2 is more robust when assumptions are uncertain.

------------------------------------------------------------------------

## t-tests: Comparing Means

[`t.test()`](https://rdrr.io/r/stats/t.test.html) covers three common
scenarios: testing a single mean against a reference value, comparing
two independent groups, and comparing paired measurements. All three
return the same type of output — a t-statistic, degrees of freedom,
p-value, and confidence interval.

### One-sample t-test

Tests whether a population mean equals a specified value $`\mu_0`$.

``` math
H_0: \mu = \mu_0 \qquad H_A: \mu \ne \mu_0
```

``` r

# Are Yale students' average sleep different from the national average of 7 hrs?
set.seed(5502)
sleep_hours <- c(6.1, 7.3, 5.8, 6.5, 7.0, 6.8, 5.5, 7.2, 6.9, 6.3,
                 7.1, 5.9, 6.4, 7.4, 6.0, 6.7, 5.6, 7.5, 6.2, 6.6)

t.test(sleep_hours, mu = 7)
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  sleep_hours
    ## t = -3.3825, df = 19, p-value = 0.003126
    ## alternative hypothesis: true mean is not equal to 7
    ## 95 percent confidence interval:
    ##  6.255358 6.824642
    ## sample estimates:
    ## mean of x 
    ##      6.54

Reading the output:

- **t** — the test statistic
- **df** — degrees of freedom (n − 1)
- **p-value** — probability of observing a result this extreme if H₀
  were true
- **95% confidence interval** — a range of plausible values for the true
  mean
- **sample mean** — your observed mean

### Two-sample independent t-test

Tests whether two group means are equal. This is the theory-based
equivalent of the permutation test in Part 2. Using the **calcium
supplement study** from class 17:

``` math
H_0: \mu_\text{treat} = \mu_\text{control} \qquad H_A: \mu_\text{treat} > \mu_\text{control}
```

``` r

treat   <- c(7, -4, 18, 17, -3, -5,  1, 10, 11, -2)
control <- c(-1, 12, -1, -3,  3, -5,  5,  2, -11, -1, -3)

t.test(treat, control, alternative = "greater")
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  treat and control
    ## t = 1.6037, df = 15.591, p-value = 0.06442
    ## alternative hypothesis: true difference in means is greater than 0
    ## 95 percent confidence interval:
    ##  -0.476678       Inf
    ## sample estimates:
    ##  mean of x  mean of y 
    ##  5.0000000 -0.2727273

The `alternative` argument controls the direction of the test:

| Value                   | Hypothesis               |
|-------------------------|--------------------------|
| `"two.sided"` (default) | $`H_A: \mu_1 \ne \mu_2`$ |
| `"greater"`             | $`H_A: \mu_1 > \mu_2`$   |
| `"less"`                | $`H_A: \mu_1 < \mu_2`$   |

By default [`t.test()`](https://rdrr.io/r/stats/t.test.html) uses
Welch’s correction (it does **not** assume equal variances). This is the
safer choice for most real data. If you have strong reason to assume
equal variances, add `var.equal = TRUE`, but this is rarely necessary.

### Paired t-test

When observations come in natural pairs (e.g., the same car measured on
both sides), use `paired = TRUE`. This is more powerful than an
independent t-test because it removes the variation between pairs. Based
on the **tire wear** example from class 23:

``` math
H_0: \mu_\text{diff} = 0 \qquad H_A: \mu_\text{diff} \ne 0
```

``` r

# Tread wear (mm) on left and right tires of the same 14 cars
set.seed(7731)
left_tire  <- c(3.1, 4.2, 2.8, 3.9, 4.5, 3.3, 2.6, 4.8, 3.7, 4.1,
                2.9, 3.5, 4.4, 3.0)
right_tire <- left_tire + rnorm(14, mean = 0.2, sd = 0.3)  # right wears slightly more

# Independent t-test (ignores pairing — less powerful)
t.test(left_tire, right_tire)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  left_tire and right_tire
    ## t = -1.1746, df = 25.94, p-value = 0.2508
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.8799626  0.2400398
    ## sample estimates:
    ## mean of x mean of y 
    ##  3.628571  3.948533

``` r

# Paired t-test (accounts for pairing — more powerful)
t.test(left_tire, right_tire, paired = TRUE)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  left_tire and right_tire
    ## t = -5.4935, df = 13, p-value = 0.0001033
    ## alternative hypothesis: true mean difference is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.4457895 -0.1941333
    ## sample estimates:
    ## mean difference 
    ##      -0.3199614

Notice the paired test gives a smaller p-value — it is more sensitive
because it focuses on the *within-car* differences rather than being
diluted by variation between cars.

### Quick reference

| Task                          | Base R                                    |
|-------------------------------|-------------------------------------------|
| One-sample t-test             | `t.test(x, mu = mu_0)`                    |
| Two-sample independent t-test | `t.test(x, y, alternative = "two.sided")` |
| Paired t-test                 | `t.test(x, y, paired = TRUE)`             |
| Extract the p-value           | `t.test(...)$p.value`                     |
| Extract the CI                | `t.test(...)$conf.int`                    |

------------------------------------------------------------------------

## Correlation: `cor.test()`

[`cor.test()`](https://rdrr.io/r/stats/cor.test.html) tests whether the
correlation between two quantitative variables is significantly
different from zero. This is the theory-based counterpart to the
permutation test for correlation that you built using
[`shuffle()`](https://emeyers.github.io/SDS1000/reference/shuffle.md) in
Part 1.

``` math
H_0: \rho = 0 \qquad H_A: \rho \ne 0
```

Using simulated data inspired by the **cigarette consumption and lung
cancer** example from class 25:

``` r

set.seed(2947)
cigs_per_capita <- runif(44, min = 10, max = 45)
cancer_rate     <- 2 + 0.005 * cigs_per_capita * 1000 + rnorm(44, sd = 5)

cor.test(cigs_per_capita, cancer_rate)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  cigs_per_capita and cancer_rate
    ## t = 61.608, df = 42, p-value < 2.2e-16
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.9899021 0.9970212
    ## sample estimates:
    ##       cor 
    ## 0.9945126

The output includes:

- **t** and **df** — the t-statistic and degrees of freedom used for the
  test
- **p-value** — for H₀: ρ = 0
- **95% confidence interval** — a range of plausible values for the true
  correlation ρ
- **cor** — the sample correlation r

You can also compute the correlation alone with
[`cor()`](https://rdrr.io/r/stats/cor.html):

``` r

cor(cigs_per_capita, cancer_rate)
```

    ## [1] 0.9945126

### Quick reference

| Task                              | Base R                                |
|-----------------------------------|---------------------------------------|
| Test H₀: ρ = 0                    | `cor.test(x, y)`                      |
| Compute correlation only          | `cor(x, y)`                           |
| Spearman (rank-based) correlation | `cor.test(x, y, method = "spearman")` |

------------------------------------------------------------------------

## One-way ANOVA: `aov()`

A one-way analysis of variance tests whether the means of three or more
groups are all equal. In S&DS 1000, you used
[`get_F_stat()`](https://emeyers.github.io/SDS1000/reference/get_F_stat.md)
to compute the F-statistic for a randomization test.
[`get_F_stat()`](https://emeyers.github.io/SDS1000/reference/get_F_stat.md)
is itself a thin wrapper around
[`aov()`](https://rdrr.io/r/stats/aov.html):

``` r

# What get_F_stat() does internally:
get_F_stat <- function(data, grouping) {
  fit         <- aov(data ~ grouping)
  fit_summary <- summary(fit)
  fit_summary[[1]]$`F value`[1]   # extract just the F-statistic
}
```

Calling [`aov()`](https://rdrr.io/r/stats/aov.html) directly gives you
the full picture — not just the F-statistic, but the p-value, degrees of
freedom, and more, all in one step.

### Running the ANOVA

Based on the **Sudoku completion times by major** from class 24:

``` math
H_0: \mu_\text{bio} = \mu_\text{cs} = \mu_\text{econ} = \mu_\text{psych}
\qquad H_A: \text{at least one mean differs}
```

``` r

set.seed(1142)
completion_times <- c(rnorm(10, mean = 22, sd = 4),   # Biology
                      rnorm(10, mean = 20, sd = 4),   # CS
                      rnorm(10, mean = 25, sd = 4),   # Econ
                      rnorm(10, mean = 21, sd = 4))   # Psychology
majors <- rep(c("Biology", "CS", "Econ", "Psychology"), each = 10)

fit <- aov(completion_times ~ majors)
summary(fit)
```

    ##             Df Sum Sq Mean Sq F value Pr(>F)  
    ## majors       3  219.3   73.12   2.759 0.0562 .
    ## Residuals   36  953.9   26.50                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Reading the [`summary()`](https://rdrr.io/r/base/summary.html) output:

| Column | Meaning |
|----|----|
| `Df` | Degrees of freedom (groups − 1; then N − groups) |
| `Sum Sq` | Sum of squares (between-group; within-group) |
| `Mean Sq` | Sum Sq / Df |
| `F value` | The F-statistic — equivalent to [`get_F_stat()`](https://emeyers.github.io/SDS1000/reference/get_F_stat.md) |
| `Pr(>F)` | The p-value |

### Post-hoc comparisons with `TukeyHSD()`

A significant ANOVA F-test tells you that *some* group means differ, but
not *which* ones. Use
[`TukeyHSD()`](https://rdrr.io/r/stats/TukeyHSD.html) (Tukey’s Honest
Significant Difference) to make all pairwise comparisons while
controlling for multiple testing:

``` r

TukeyHSD(fit)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = completion_times ~ majors)
    ## 
    ## $majors
    ##                          diff        lwr       upr     p adj
    ## CS-Biology         -3.6540372 -9.8539550  2.545881 0.3984180
    ## Econ-Biology        2.8102700 -3.3896479  9.010188 0.6179830
    ## Psychology-Biology  0.8276276 -5.3722902  7.027545 0.9838298
    ## Econ-CS             6.4643072  0.2643893 12.664225 0.0382728
    ## Psychology-CS       4.4816648 -1.7182530 10.681583 0.2271289
    ## Psychology-Econ    -1.9826423 -8.1825602  4.217276 0.8245976

Each row compares two groups. The `p adj` column gives p-values adjusted
for the number of comparisons — use these rather than raw p-values to
avoid false positives.

[`aov()`](https://rdrr.io/r/stats/aov.html) assumes approximately normal
residuals and equal variance across groups. For a quick check of the
equal-variance assumption, look at whether the group standard deviations
are roughly similar. The randomization F-test from Part 1 (using
[`get_F_stat()`](https://emeyers.github.io/SDS1000/reference/get_F_stat.md)
inside [`replicate()`](https://rdrr.io/r/base/lapply.html)) makes
neither assumption and is a robust alternative.

### Quick reference

| Task | SDS1000 | Base R |
|----|----|----|
| Compute F-statistic only | `get_F_stat(data, group)` | `summary(aov(data ~ group))[[1]]$'F value'[1]` |
| Full ANOVA with p-value | — | `summary(aov(data ~ group))` |
| Pairwise group comparisons | — | `TukeyHSD(aov(data ~ group))` |

------------------------------------------------------------------------

## Chi-squared test: `chisq.test()`

A chi-squared goodness-of-fit test asks whether the observed counts
across categories match a set of expected proportions.
[`get_chisqr_stat()`](https://emeyers.github.io/SDS1000/reference/get_chisqr_stat.md)
in SDS1000 is a direct wrapper — it simply calls
[`chisq.test()`](https://rdrr.io/r/stats/chisq.test.html) and returns
the statistic:

``` r

# What get_chisqr_stat() does internally:
get_chisqr_stat <- function(observed_counts, expected_proportions) {
  test_output <- chisq.test(observed_counts, p = expected_proportions)
  test_output$statistic   # extract just the chi-squared value
}
```

Calling [`chisq.test()`](https://rdrr.io/r/stats/chisq.test.html)
directly gives you the full test result.

### Goodness-of-fit test

Based on the **Yale student birth months** example from class 23 — are
students equally likely to be born in any month?

``` math
H_0: \text{birth months are uniformly distributed}
\qquad H_A: \text{some months are more common than others}
```

``` r

# Observed birth month counts for 198 Yale students (class 23)
observed_counts <- c(14, 11, 21, 17, 15, 13, 19, 16, 18, 22, 14, 18)
names(observed_counts) <- month.abb

# Test against uniform distribution (equal probability for each month)
chisq.test(observed_counts, p = rep(1/12, 12))
```

    ## 
    ##  Chi-squared test for given probabilities
    ## 
    ## data:  observed_counts
    ## X-squared = 7.2121, df = 11, p-value = 0.7817

Reading the output:

- **X-squared** — the chi-squared test statistic (same value
  [`get_chisqr_stat()`](https://emeyers.github.io/SDS1000/reference/get_chisqr_stat.md)
  returns)
- **df** — degrees of freedom (number of categories − 1)
- **p-value** — probability of observing this much deviation from
  uniform if H₀ were true

### Chi-squared test of independence

[`chisq.test()`](https://rdrr.io/r/stats/chisq.test.html) also tests
whether two categorical variables are *independent* of each other (i.e.,
whether they are associated). Pass a contingency table instead of a
count vector:

``` r

# Are vaccination status and illness related?
vaccine_table <- matrix(c(45, 15, 20, 70),
                        nrow = 2,
                        dimnames = list(c("Vaccinated", "Unvaccinated"),
                                        c("Got Ill", "Stayed Healthy")))
vaccine_table
```

    ##              Got Ill Stayed Healthy
    ## Vaccinated        45             20
    ## Unvaccinated      15             70

``` r

chisq.test(vaccine_table)
```

    ## 
    ##  Pearson's Chi-squared test with Yates' continuity correction
    ## 
    ## data:  vaccine_table
    ## X-squared = 38.716, df = 1, p-value = 4.902e-10

### Quick reference

| Task | SDS1000 | Base R |
|----|----|----|
| Chi-squared statistic only | `get_chisqr_stat(obs, expected_p)` | `chisq.test(obs, p = expected_p)$statistic` |
| Full goodness-of-fit test | — | `chisq.test(obs, p = expected_p)` |
| Test of independence | — | `chisq.test(contingency_table)` |

------------------------------------------------------------------------

## Linear regression: `lm()` and `summary()`

[`lm()`](https://rdrr.io/r/stats/lm.html) fits a linear regression
model. [`summary()`](https://rdrr.io/r/base/summary.html) reports the
theory-based hypothesis test for each coefficient — whether the slope
(or intercept) is significantly different from zero — and
[`confint()`](https://rdrr.io/r/stats/confint.html) gives confidence
intervals.

Using the **cigarette consumption and lung cancer** data from class 25:

``` math
H_0: \beta_1 = 0 \qquad H_A: \beta_1 \ne 0
```

``` r

set.seed(2947)
cigs_per_capita <- runif(44, min = 10, max = 45)
cancer_rate     <- 2 + 0.005 * cigs_per_capita * 1000 + rnorm(44, sd = 5)

lung_lm <- lm(cancer_rate ~ cigs_per_capita)
summary(lung_lm)
```

    ## 
    ## Call:
    ## lm(formula = cancer_rate ~ cigs_per_capita)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -9.1802 -3.8251  0.3541  4.0038 10.7198 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      2.97304    2.31498   1.284    0.206    
    ## cigs_per_capita  4.99792    0.08113  61.608   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.237 on 42 degrees of freedom
    ## Multiple R-squared:  0.9891, Adjusted R-squared:  0.9888 
    ## F-statistic:  3795 on 1 and 42 DF,  p-value: < 2.2e-16

Reading the [`summary()`](https://rdrr.io/r/base/summary.html) output:

| Column       | Meaning                                    |
|--------------|--------------------------------------------|
| `Estimate`   | The fitted intercept (β₀) and slope (β₁)   |
| `Std. Error` | Standard error of each estimate            |
| `t value`    | Test statistic for H₀: coefficient = 0     |
| `Pr(>|t|)`   | p-value (two-sided)                        |
| `R-squared`  | Proportion of variance in y explained by x |

### Confidence intervals for the coefficients

``` r

confint(lung_lm, level = 0.95)
```

    ##                     2.5 %   97.5 %
    ## (Intercept)     -1.698784 7.644857
    ## cigs_per_capita  4.834205 5.161639

The row for `cigs_per_capita` gives a 95% CI for the slope. This is the
theory-based alternative to the bootstrap CI built in Part 1.

### Connecting to the bootstrap

In class 25, you estimated the slope’s uncertainty using
[`resample_pairs()`](https://emeyers.github.io/SDS1000/reference/resample_pairs.md)
inside
[`do_it()`](https://emeyers.github.io/SDS1000/reference/do_it.md). The
theory-based CI from [`confint()`](https://rdrr.io/r/stats/confint.html)
will give a very similar interval when the regression assumptions
(linearity, normality of residuals, constant variance) are met. When
they’re not, the bootstrap is more reliable.

| Task | SDS1000 (bootstrap) | Base R (theory-based) |
|----|----|----|
| Slope estimate | `coef(lm(...))["x"]` | `coef(lm(...))["x"]` |
| CI for slope | [`resample_pairs()`](https://emeyers.github.io/SDS1000/reference/resample_pairs.md) + [`quantile()`](https://rdrr.io/r/stats/quantile.html) | `confint(lm(...))` |
| p-value for slope | `mean(null_dist >= obs)` | `summary(lm(...))$coefficients[2, 4]` |

------------------------------------------------------------------------

## Putting it all together: which test when?

| Research question | Data types | Test |
|----|----|----|
| Is the mean different from a reference value? | One quantitative variable | `t.test(x, mu = mu_0)` |
| Are two independent group means different? | Quantitative + binary group | `t.test(x, y)` |
| Are two paired measurements different? | Two quantitative variables (paired) | `t.test(x, y, paired = TRUE)` |
| Are three or more group means different? | Quantitative + categorical group | `summary(aov(y ~ group))` |
| Is there a linear association between two variables? | Two quantitative variables | `cor.test(x, y)` |
| Do observed counts match expected proportions? | One categorical variable | `chisq.test(counts, p = expected_p)` |
| Are two categorical variables associated? | Two categorical variables | `chisq.test(table)` |
| How does y change with x (and what is the uncertainty)? | Two quantitative variables | `summary(lm(y ~ x))` + `confint(lm(y ~ x))` |
