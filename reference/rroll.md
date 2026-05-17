# Generates a vector of random die rolls

This function simulates rolling a k-sided die n times and returns the
number of times each of the k outcomes occured, where the probability of
the k outcomes is given by the vector prob. This function is just a
wrapper for the \`rmultinom()\` function.

## Usage

``` r
rroll(num_rolls, prob = rep(1/6, 6), outcome_names = NULL)
```

## Arguments

- num_rolls:

  The number of times to roll the die.

- prob:

  The probability of getting each side of the die on each roll

- outcome_names:

  A vector of names for the outcomes. Default values is of NULL returns
  sequential integers as the names of the vector.

## Examples

``` r
 set.seed(100)
 # roll a 6-sided fair die (the default) 100 times
 rroll(100, outcome_names = c("one", "two", "three", "four", "five", "six"))
#>   one   two three  four  five   six 
#>    15    15    18    12    20    20 
```
