# Calculates the F statistic

Calculates the F statistic

## Usage

``` r
get_F_stat(data, grouping, keep_grouping_numeric = FALSE)
```

## Arguments

- data:

  A vector of quantitative data.

- grouping:

  A vector of categorical data indicating which group each value of the
  quantitative data belongs to.

## Examples

``` r
 set.seed(100)
 the_data <- c(rnorm(10, -3), rnorm(10, 0), rnorm(10, 3))
 grouping <- c(rep("A", 10), rep("B", 10), rep("C", 10))
 get_F_stat(the_data, grouping)
#> [1] 172.7616
```
