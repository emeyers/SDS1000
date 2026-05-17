# Returns the critical value(s) from the normal distribution

This function returns the critical value(s) from the normal distribution
given a central probability. This is particular useful for creating
confidence intervals.

## Usage

``` r
cnorm(p, mean = 0, sd = 1, side = c("upper", "both", "lower"))
```

## Arguments

- p:

  The probability associated with the critical value(s).

- mean:

  The mean of the normal distribution. Default is 0.

- sd:

  The standard deviation of the normal distribution. Default is 1.

- side:

  A string that must be either "upper", "both", or "lower" indicating
  whether the ' area associated with the probability p is in the upper
  tail, both tails, or lower tail.

## Examples

``` r
 # Get the critical value for a 95% confidence interval from the standard normal distribution
 cnorm(0.95, side = "both")
#> [1] -1.959964  1.959964
 
```
