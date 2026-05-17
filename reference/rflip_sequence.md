# Generates a sequence of coin flips

Generates a sequence of coin flips

## Usage

``` r
rflip_sequence(num_flips = 1, prob = 0.5, output_type = "name")
```

## Arguments

- num_flips:

  The number of times to flip the coin.

- prob:

  The probability of generated a "heads" on each flip.

- output_type:

  A string that must either be set to "numeric", "name" or "long name".
  If this string is set to "numeric" the output values are 0's and 1's,
  where a 1 is a "heads". If this string is set to "name" the output
  values are "H" and "T", where a "H" is a "heads". If this string is
  set to "long name" the output values are "Heads" and "Tails".

## Examples

``` r
 set.seed(100)
 rflip_sequence(10, output_type = "numeric")
#>  [1] 0 0 1 0 0 0 1 0 1 0
```
