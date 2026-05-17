# Samples with replacement the same rows from two vectors

Samples with replacement the same rows from two vectors

## Usage

``` r
resample_pairs(vector1, vector2)
```

## Arguments

- vector1:

  A vector of values.

- vector2:

  A vector of values of the same length as vector1.

## Value

Returns a data frame that has three variables: 1. The original sample
index number 2. The values that were randomly selected from vector1 3.
The values that were randomly selected from vector2

## Examples

``` r
 vector1 <- 1:26
 vector2 <- letters
 resample_pairs(vector1, vector2)
#>    original_sample_num vector1 vector2
#> 1                    7       7       g
#> 2                   17      17       q
#> 3                   26      26       z
#> 4                   12      12       l
#> 5                    8       8       h
#> 6                   19      19       s
#> 7                   20      20       t
#> 8                    5       5       e
#> 9                    3       3       c
#> 10                  15      15       o
#> 11                  12      12       l
#> 12                  16      16       p
#> 13                  21      21       u
#> 14                  22      22       v
#> 15                   4       4       d
#> 16                  10      10       j
#> 17                   6       6       f
#> 18                  13      13       m
#> 19                   2       2       b
#> 20                   4       4       d
#> 21                  23      23       w
#> 22                  25      25       y
#> 23                  13      13       m
#> 24                   2       2       b
#> 25                   6       6       f
#> 26                  10      10       j
```
