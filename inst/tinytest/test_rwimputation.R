set.seed(42)
# iris
test_data <- iris[1:50, ] 

test_data[c(2, 15, 30), 1] <- NA # Sepal.Length (numeric)
test_data[c(5, 40), 5] <- NA     # Species (factor)

# walidacja param. wejsciowych
expect_error(rwimpute(test_data, restart_prob = -0.5))
expect_error(rwimpute(test_data, restart_prob = 1.2))

expect_error(rwimpute(c(1, 2, NA, 4)))

one_col_df <- data.frame(A = c(1, 2, NA, 4, 5, 6))
expect_error(rwimpute(one_col_df))

# ramka bez braków danych
complete_res <- rwimpute(iris, seed = 42)
expect_identical(complete_res, iris)

#reprodukowalnosc
res1 <- rwimpute(test_data, seed = 100)
res2 <- rwimpute(test_data, seed = 100)
expect_identical(res1, res2)

res_seed1 <- rwimpute(test_data, seed = 123)
res_seed2 <- rwimpute(test_data, seed = 456)
expect_false(identical(res_seed1, res_seed2))

# typy i poziomy faktorow
expect_true(is.numeric(res1$Sepal.Length))
expect_true(is.factor(res1$Species))

expect_equal(levels(res1$Species), levels(iris$Species))

# prawie puste dane
sparse_data <- iris[1:20, ]
sparse_data[1:18, 1] <- NA 
expect_silent(rwimpute(sparse_data))

