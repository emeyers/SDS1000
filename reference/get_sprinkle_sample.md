# Generate a random sample of sprinkle colors

\`r lifecycle::badge("deprecated")\`

This function generates a fictional sample of data of sprinkles. This
function was renamed rsprinkles() to be more consistent with other
functions that generate random samples.

## Usage

``` r
get_sprinkle_sample(n)
```

## Arguments

- n:

  The sample size.

## Examples

``` r
 # Generate a sample from 10 sprinkle colors
 get_sprinkle_sample(10)
#>  [1] orange orange white  orange red    green  yellow green  white  red   
#> Levels: green orange pink red white yellow
```
