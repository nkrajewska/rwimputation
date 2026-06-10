# Example rwimputation

``` r

library(rwimputation)
my_data <- data.frame(
  Age = c(25, 38, NA, 19, 52, NA, 35),
  Salary = c(5000, 6046, 5500, NA, 9407, 5200, NA),
  Department = factor(c("IT", "HR", "IT", "Sales", NA, "HR", "IT"))
)
print("My data:")
#> [1] "My data:"
print(my_data)
#>   Age Salary Department
#> 1  25   5000         IT
#> 2  38   6046         HR
#> 3  NA   5500         IT
#> 4  19     NA      Sales
#> 5  52   9407       <NA>
#> 6  NA   5200         HR
#> 7  35     NA         IT
imputed_data <- rwimpute(my_data, restart_prob = 0.15, num_steps = 100)
print("Imputed data:")
#> [1] "Imputed data:"
print(imputed_data)
#>        Age   Salary Department
#> 1 25.00000 5000.000         IT
#> 2 38.00000 6046.000         HR
#> 3 33.04412 5500.000         IT
#> 4 19.00000 5872.961      Sales
#> 5 52.00000 9407.000         IT
#> 6 33.64615 5200.000         HR
#> 7 35.00000 5881.013         IT
```
