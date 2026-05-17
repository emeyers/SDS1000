# Statistics by group

This function computes a summary statistic by group for a given vector
of data.

## Usage

``` r
stats_by_group(data_vector, group_vector, stat = mean)
```

## Arguments

- data_vector:

  A numeric vector of data.

- group_vector:

  A factor vector indicating the group membership for each observation
  in data_vector.

  @param stat A statistic function to compute separately for each group.

## Examples

``` r
# Generate some fictional data
set.seed(100)
data_vector <- rnorm(100)
group_vector <- as.factor(sample(c("A", "B", "C"), size = 100, replace = TRUE))
# Compute the mean of data_vector for each group in group_vector
stats_by_group(data_vector, group_vector, stat = mean)
#>           A           B           C 
#> -0.12992515  0.07199512  0.02461430 
                                    
```
