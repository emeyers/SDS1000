# Calculates the chi-square statistic

Calculates the chi-square statistic

## Usage

``` r
get_chisqr_stat(observed_counts, expected_proportions)
```

## Arguments

- observed_counts:

  A vector of observed count data.

- expected_proportions:

  A vector indicating the expected probability that data comes from a
  given group.

## Examples

``` r
 set.seed(100)
 observed_counts <- c(138, 99, 106, 115, 104, 164)
 expected_proportions = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)
 get_chisqr_stat(observed_counts, expected_proportions)
#> X-squared 
#>  26.21488 
```
