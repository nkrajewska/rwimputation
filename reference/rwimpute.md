# Imputation using Random Walk

Imputes missing values in a data frame or matrix using a random walk
algorithm based on Gower distance similarity.

## Usage

``` r
rwimpute(data, num_steps = 10L, seed = 42L, restart_prob = 0)
```

## Arguments

- data:

  A data frame or matrix containing missing values.

- num_steps:

  Number of steps for the random walk. Default is 10. If -1 and
  restart_prob is set, it is calculated automatically.

- seed:

  Integer, random seed for reproducibility. Default is 42.

- restart_prob:

  Probability of restarting the random walk. Default is 0.0.

## Value

A data frame or matrix (depending on the input) with imputed values.

## Examples

``` r
data(iris)
iris[1:5, 1] <- NA
imputed_data <- rwimpute(iris)
```
