# Repeats a process many times and returns a vector of results

Repeats a process many times and returns a vector of results

## Usage

``` r
do_it(n = 1L)
```

## Arguments

- n:

  The number of times to repeat the process.

- null_dist:

  A vector of data consistent with a null hypothesis.

- lower.tail:

  A Boolean indicating whether one should return the proportion of
  points less than or equal to the obs_stat value.

## Note

This is a simplified version of the mosiac do() that returns a vector
only. The idea and code are based on the mosaic do() function, which can
be found on github page at:
https://github.com/ProjectMOSAIC/mosaic/blob/master/R/do.R The code uses
S4 objects.

## Examples

``` r
 many_hellos <- do_it(10) * { "hello" }
```
