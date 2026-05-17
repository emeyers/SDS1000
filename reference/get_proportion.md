# Calculates proportion of values in a particular category

Calculates proportion of values in a particular category

## Usage

``` r
get_proportion(v, category_name)
```

## Arguments

- v:

  A vector of categorical data.

- category_name:

  A string specifying the name of a category. The proportion of values
  in that category will be returned.

## Examples

``` r
 set.seed(100)
 sprinkle_sample <- get_sprinkle_sample(100)
#> Error in deprecate_soft("0.2026.3", "get_sprinkle_sample()", "rsprinkles()"): could not find function "deprecate_soft"
 get_proportion(sprinkle_sample, "red")
#> Error: object 'sprinkle_sample' not found
```
