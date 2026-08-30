# Generate a random sample of a president's approval ratings

\`r lifecycle::badge("deprecated")\`

This function generates a sample of fictional data of people approve of
the president. This function was renamed rsprinkles() to be more
consistent with other functions that generate random samples.

## Usage

``` r
get_approval_sample(n, degree_of_approval = FALSE)
```

## Arguments

- n:

  The sample size.

- degree_of_approval:

  A Boolean that if set to TRUE will return a vector with the levels
  "strongly disapprove", "disapprove", "approve", "strongly approve". If
  this is set to false, then a vector that only has the levels
  "disapprove", "approve" will be returned.

## Examples

``` r
 # Generate a sample from 10 individuals
 get_approval_sample(10)
#> Warning: `get_approval_sample()` was deprecated in SDS1000 0.2026.3.
#> ℹ Please use `rapprovals()` instead.
#>  [1] disapprove approve    approve    approve    disapprove disapprove
#>  [7] disapprove disapprove disapprove approve   
#> Levels: disapprove < approve
```
