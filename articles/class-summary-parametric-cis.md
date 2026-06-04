# Parametric Confidence Intervals

## Overview

A **confidence interval** gives a range of plausible values for an
unknown population parameter. A parametric CI uses a mathematical
formula based on the sampling distribution of the estimator. Every CI in
this guide takes the form:

``` math
\text{point estimate} \pm \text{critical value} \times SE
```

The **critical value** comes from a normal or t-distribution and is
found using `cnorm(C)` (normal) or `ct(C, df)` (t) for a confidence
level C. The **standard error (SE)** measures the typical variability of
the estimator.

**In this article:**

- [One proportion](#one-proportion)
- [One mean](#one-mean)
- [Difference of two means](#difference-of-two-means)
- [Correlation](#correlation)
- [Regression slope](#regression-slope)
- [Quick reference](#quick-reference)

  

**Also in Class Summaries:**

- [Randomization Hypothesis
  Tests](https://emeyers.github.io/SDS1000/articles/class-summary-randomization-tests.md)
- [Randomization Confidence
  Intervals](https://emeyers.github.io/SDS1000/articles/class-summary-randomization-cis.md)
- [Parametric Hypothesis
  Tests](https://emeyers.github.io/SDS1000/articles/class-summary-parametric-tests.md)

------------------------------------------------------------------------

## One Proportion

**When to use:** You have a single categorical variable with two
outcomes and want to estimate the population proportion π.

**Conditions:** $`n\hat{p} \geq 10`$ and $`n(1-\hat{p}) \geq 10`$

**Formula:**
``` math
\hat{p} \pm z^* \cdot \hat{SE}, \qquad \hat{SE} = \sqrt{\frac{\hat{p}(1-\hat{p})}{n}}
```

The critical value $`z^*`$ = `cnorm(C)` for confidence level C.

**Example (class 20):** A survey asked 2625 people if they agreed that
“There is only one true love for each person.” 1812 of the respondents
disagreed. Compute a 90% confidence interval for the proportion who
disagreed.

``` r

# Data
n     <- 2625
x     <- 1812   # number who disagreed
p_hat <- x / n
p_hat
```

    ## [1] 0.6902857

``` r

# Check conditions
n * p_hat        # must be >= 10
```

    ## [1] 1812

``` r

n * (1 - p_hat)  # must be >= 10
```

    ## [1] 813

``` r

# Standard error
SE <- sqrt(p_hat * (1 - p_hat) / n)
SE
```

    ## [1] 0.009024651

``` r

# Critical value for a 90% CI
z_star <- cnorm(0.90)
z_star
```

    ## [1] 1.644854

``` r

# Confidence interval
CI <- c(p_hat - z_star * SE, p_hat + z_star * SE)
CI
```

    ## [1] 0.6754415 0.7051299

We are 90% confident that the true proportion of people who disagree
that there is only one true love for each person is between 0.675 and
0.705.

------------------------------------------------------------------------

## One Mean

**When to use:** You have a single quantitative variable and want to
estimate the population mean μ.

**Conditions:** $`n \geq 30`$, or the population is approximately
normal.

**Formula:**
``` math
\bar{x} \pm t^* \cdot SE, \qquad SE = \frac{s}{\sqrt{n}}, \qquad df = n-1
```

The critical value $`t^*`$ = `ct(C, df)` for confidence level C with
$`df = n-1`$ degrees of freedom.

**Example (class 21):** A study by Loyd et al. (2013) used KittyCams to
record all outdoor hunting activity of n = 55 domestic cats. The mean
number of kills per week was 2.4 with a standard deviation of 1.51.
Compute a 99% confidence interval for the mean number of kills per week.

``` r

# Data
n     <- 55
x_bar <- 2.4
s     <- 1.51

# Standard error and degrees of freedom
SE <- s / sqrt(n)
df <- n - 1

# Critical value for a 99% CI
t_star <- ct(0.99, df)
t_star
```

    ## [1] 2.669985

``` r

# Confidence interval
ME <- t_star * SE
CI <- c(x_bar - ME, x_bar + ME)
CI
```

    ## [1] 1.856369 2.943631

We are 99% confident that the mean number of kills per week for outdoor
hunting cats is between 1.856 and 2.944.

**When to use t vs z.** For a single mean, always use the t-distribution
([`ct()`](https://emeyers.github.io/SDS1000/reference/ct.md)). The
t-distribution has heavier tails than the normal, which accounts for the
additional uncertainty from estimating σ with s. For a proportion, use
the normal distribution
([`cnorm()`](https://emeyers.github.io/SDS1000/reference/cnorm.md)).

------------------------------------------------------------------------

## Difference of Two Means

**When to use:** You have a quantitative variable measured in two
independent groups and want to estimate the difference in population
means $`\mu_1 - \mu_2`$.

**Conditions:** Each group has $`n \geq 30`$ or both populations are
approximately normal.

**Formula (Welch’s — does not assume equal variances):**
``` math
(\bar{x}_1 - \bar{x}_2) \pm t^* \cdot SE_{diff}, \qquad
SE_{diff} = \sqrt{\frac{s_1^2}{n_1} + \frac{s_2^2}{n_2}}, \qquad
df = \min(n_1-1,\, n_2-1)
```

**Example (class 22):** A study by Nierenberg et al. (1989) recorded
dietary fiber intake. Compute a 95% confidence interval for the
difference in mean daily fiber intake (grams) between males and females.

``` r

# Summary statistics (simulated to match class 22 Nutrition Study structure)
set.seed(7823)
fiber_males   <- round(rnorm(155, mean = 23.5, sd = 14.2)) # generate fake data
fiber_females <- round(rnorm(662, mean = 20.1, sd = 11.8))

# Group statistics
n1    <- length(fiber_males)    
x_bar1 <- mean(fiber_males)    
s1 <- sd(fiber_males)
n2    <- length(fiber_females)
x_bar2 <- mean(fiber_females)
s2 <- sd(fiber_females)

paste("Males:   n =", n1, "  mean =", round(x_bar1, 2), "  sd =", round(s1, 2))
```

    ## [1] "Males:   n = 155   mean = 24.5   sd = 15.08"

``` r

paste("Females: n =", n2, "  mean =", round(x_bar2, 2), "  sd =", round(s2, 2))
```

    ## [1] "Females: n = 662   mean = 20.39   sd = 11.47"

``` r

# Standard error of the difference
SE_diff <- sqrt((s1^2 / n1) + (s2^2 / n2))

# Degrees of freedom (conservative: smaller of n1-1 and n2-1)
df <- min(n1 - 1, n2 - 1)

# Critical value for a 95% CI
t_star <- ct(0.95, df)

# Confidence interval for the difference (males - females)
obs_diff <- x_bar1 - x_bar2
ME       <- t_star * SE_diff
CI       <- c(obs_diff - ME, obs_diff + ME)
CI
```

    ## [1] 1.565513 6.664504

The 95% CI includes zero, which suggests we do not have strong evidence
of a difference in mean fiber intake between males and females.

------------------------------------------------------------------------

## Correlation

**When to use:** Two quantitative variables; construct a confidence
interval for the population correlation ρ.

**Method:** [`cor.test()`](https://rdrr.io/r/stats/cor.test.html)
returns both a p-value and a confidence interval. Internally it uses
**Fisher’s z-transformation**: the sample correlation r is transformed
to $`z = \tanh^{-1}(r)`$, a CI is built in z-space (where the
distribution is approximately normal), and the endpoints are transformed
back via $`r = \tanh(z)`$.

``` math
z = \tanh^{-1}(r), \quad SE_z = \frac{1}{\sqrt{n-3}}, \quad
CI_z = z \pm z^* \cdot SE_z, \quad
CI_r = \tanh(CI_z)
```

In practice, calling
[`cor.test()`](https://rdrr.io/r/stats/cor.test.html) handles all of
this in one step.

**Example (class 18/25):** Sugar content and calorie content for a
sample of 77 breakfast cereals. Compute a 95% confidence interval for
the correlation.

``` r

set.seed(1123)
n        <- 77
sugar    <- runif(n, 0, 15)  # fake data
calories <- 90 + 3.5 * sugar + rnorm(n, sd = 12)

# cor.test() gives both the CI and the hypothesis test in one call
corr_result <- cor.test(sugar, calories, conf.level = 0.95)

# Point estimate
corr_result$estimate
```

    ##       cor 
    ## 0.8149169

``` r

# 95% confidence interval
corr_result$conf.int
```

    ## [1] 0.7228814 0.8785408
    ## attr(,"conf.level")
    ## [1] 0.95

We are 95% confident that the true correlation between sugar and calorie
content in cereals is between 0.723 and 0.879. Since the interval lies
entirely above zero, the data provide good evidence of a positive
association.

**Manual calculation using the Fisher z-transformation:**

``` r

r    <- cor(sugar, calories)        # sample correlation
z    <- atanh(r)                    # Fisher z-transform of r
SE_z <- 1 / sqrt(n - 3)             # SE in z-space

z_star <- cnorm(0.95)               # critical value for 95% CI

ci_z <- c(z - z_star * SE_z, z + z_star * SE_z)
tanh(ci_z)                          # transform back to correlation scale
```

    ## [1] 0.7228814 0.8785408

------------------------------------------------------------------------

## Regression Slope

**When to use:** Two quantitative variables; estimate how much y changes
on average for a one-unit increase in x.

**Formula:** The parametric CI for β₁ is computed directly from the
fitted regression model using
[`confint()`](https://rdrr.io/r/stats/confint.html), which uses
[`qt()`](https://rdrr.io/r/stats/TDist.html) internally with
$`df = n - 2`$.

**Example (class 25):** Cigarette consumption per capita and lung cancer
rates across 44 U.S. states. Compute a 90% confidence interval for the
slope.

``` r

set.seed(2947)
cigs_per_capita <- runif(44, min = 10, max = 45)  # fake data
cancer_rate     <- 2 + 0.005 * cigs_per_capita * 1000 + rnorm(44, sd = 5)

# Fit the linear model
lung_lm <- lm(cancer_rate ~ cigs_per_capita)

# Point estimate of the slope
slope <- coef(lung_lm)["cigs_per_capita"]
slope
```

    ## cigs_per_capita 
    ##        4.997922

``` r

# 90% CI for all coefficients (matches class 25)
confint(lung_lm, level = 0.90)
```

    ##                        5 %     95 %
    ## (Intercept)     -0.9206509 6.866724
    ## cigs_per_capita  4.8614732 5.134370

We are 90% confident that for each additional hundred cigarettes smoked
per capita, the lung cancer rate increases by between 4.8615 and 5.1344
cases per 100,000 people.

**Manual calculation:** You can also compute the CI by hand using the
regression output from `summary(lung_lm)`. The SE of the slope is in the
`Std. Error` column, and the critical value is `ct(C, df = n-2)`.

------------------------------------------------------------------------

## Quick Reference

| Parameter | Formula | Critical value | SDS1000 function |
|----|----|----|----|
| One proportion π | $`\hat{p} \pm z^* \sqrt{\hat{p}(1-\hat{p})/n}`$ | `cnorm(C)` | [`cnorm()`](https://emeyers.github.io/SDS1000/reference/cnorm.md) |
| One mean μ | $`\bar{x} \pm t^* \cdot s/\sqrt{n}`$ | `ct(C, n-1)` | [`ct()`](https://emeyers.github.io/SDS1000/reference/ct.md) |
| Difference μ₁ − μ₂ | $`(\bar{x}_1-\bar{x}_2) \pm t^* \cdot SE_{diff}`$ | `ct(C, min(n₁,n₂)-1)` | [`ct()`](https://emeyers.github.io/SDS1000/reference/ct.md) |
| Correlation ρ | `cor.test(x, y, conf.level=C)$conf.int` | Fisher z-transform | — |
| Regression slope β₁ | from `confint(lm(...), level = C)` | `ct(C, n-2)` | — |
