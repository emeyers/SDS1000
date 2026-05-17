# Calculates the proportion of points in a vector more extreme than a specified value

Calculates the proportion of points in a vector more extreme than a
specified value

## Usage

``` r
ptail(obs_value, x, lower.tail = TRUE)
```

## Arguments

- obs_value:

  A value which will be used to assess how many points in the vector x
  are greater (or less) than this value.

- x:

  A vector of values the observed_value will be compared to

- lower.tail:

  A Boolean indicating whether one should return the proportion of
  points less than or equal to the obs_value value.

## Examples

``` r
 set.seed(100)
 obs_value <- 3
 null_dist <- rnorm(1000)
 ptail(obs_value, null_dist)
#> [1] 0.998
```
