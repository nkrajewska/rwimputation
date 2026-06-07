
create_p_matrix <- function(data) {
    n_row <- nrow(data)

    dist <- as.matrix(cluster::daisy(data, metric = "gower", stand = TRUE))

    p_matrix <- (1 - dist)/rowSums(1-dist, na.rm = TRUE)
    return(p_matrix)
}

est_mix_time <- function(p_matrix, eps = 0.01){
    n <- nrow(p_matrix)
    eig <- RSpectra::eigs(p_matrix, 2, which = 'LM')$values
    lambda <- sort(Mod(eig), decreasing = TRUE)[2]

    gap <- 1 - lambda
    mix_time <- ceiling(log(n / eps) / gap)
    mix_time <- min(mix_time, 50)
    list(
    s_num_steps = mix_time,
    spectral_gap = gap,
    lambda = lambda
  )
}


#' @export
#' @useDynLib rwimputation, .registration = TRUE
rwimpute <- function(data, num_steps = -1L, seed=42L) {
    if (!is.data.frame(data)) data <- as.data.frame(data)
    p_matrix <- create_p_matrix(data)

    if (num_steps == -1L) {
        mix_time <- est_mix_time(p_matrix, eps = 0.01)
        num_steps  <- as.integer( mix_time$s_num_steps)
    } else if (num_steps < 1L) {
        stop("")
    }

    cols <- ncol(data)
    col_types <- integer(cols)
    data_matrix <- matrix(0, nrow = nrow(data), ncol = cols)
    factor_levels <- list()

    for(j in 1:cols){
        column <- data[[j]]
        if (is.factor(column) || is.character(column)) {
            col_types[j] <- 1 
        if (is.character(column)) column <- as.factor(column)
            factor_levels[[j]] <- levels(column)
            data_matrix[, j] <- as.numeric(column)
        } else {
            col_types[j] <- 0 
            data_matrix[, j] <- as.numeric(column)
        }
    }

    storage.mode(data_matrix) <- "double"
    storage.mode(p_matrix) <- "double"
    col_types <- as.integer(col_types)

    raw_result <- .Call("r_rwimpute", data_matrix, p_matrix, col_types, as.double(num_steps), as.integer(seed))
    result_df <- data
    for (j in 1:cols) {
        if (col_types[j] == 1) {
            indices <- round(raw_result[, j])
            result_df[[j]] <- factor(factor_levels[[j]][indices], levels = factor_levels[[j]])
        } else {
            result_df[[j]] <- raw_result[, j]
        }
    }
    return(result_df)

}