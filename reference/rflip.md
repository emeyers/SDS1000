# Gives a count/proportion from simulating n coin flips

Gives a count/proportion from simulating n coin flips

## Usage

``` r
rflip(num_flips = 1, prob = 0.5, report_proportion = FALSE)
```

## Arguments

- num_flips:

  The number of times to flip the coin.

- prob:

  The probability of generated a "heads" on each flip.

- report_proportion:

  A Boolean that if set to TRUE will return the proportion of coin flips
  that were "heads" otherwise it returns the number of coin flips that
  were "heads".

## Examples

``` r
 set.seed(100)
 rflip(10)
#> [1] 4

```
