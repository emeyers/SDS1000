# Returns the critical value(s) from a t-distribution

This function returns the critical value(s) from a t-distribution given
a central probability and degrees of freedom. This is particular useful
for creating confidence intervals.

## Usage

``` r
ct(p, df, side = c("upper", "both", "lower"))
```

## Arguments

- p:

  The probability associated with the critical value(s).

- df:

  The degrees of freedom of the t-distribution.

- side:

  A string that must be either "upper", "both", or "lower" indicating
  whether the ' area associated with the probability p is in the upper
  tail, both tails, or lower tail.

## Examples

``` r
 # Get the critical value for a 95% confidence interval from the t-distribution with 10 degrees of freedom
 ct(0.95, df = 10, side = "both")
#> [1] -2.228139  2.228139
```
