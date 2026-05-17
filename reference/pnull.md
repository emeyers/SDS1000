# Calculates the proportion of points in a vector greater than a specified value

Calculates the proportion of points in a vector greater than a specified
value

## Usage

``` r
pnull(obs_stat, null_dist, lower.tail = TRUE)
```

## Arguments

- obs_stat:

  An "observed statistic" value which will be used to assess how many
  points in the null_dist are greater than this value.

- null_dist:

  A vector of data consistent with a null hypothesis.

- lower.tail:

  A Boolean indicating whether one should return the proportion of
  points less than or equal to the obs_stat value.

## Examples

``` r
 set.seed(100)
 obs_stat <- 3
 null_dist <- rnorm(1000)
 pnull(obs_stat, null_dist)
#> [1] 0.998
```
