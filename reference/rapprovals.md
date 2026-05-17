# Generate a random sample of a president's approval ratings

This function generates a sample of fictional data of people approve of
the president.

## Usage

``` r
rapprovals(n, degree_of_approval = FALSE)
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
 rapprovals(10)
#>  [1] approve    approve    approve    disapprove approve    approve   
#>  [7] approve    disapprove approve    disapprove
#> Levels: disapprove < approve
```
