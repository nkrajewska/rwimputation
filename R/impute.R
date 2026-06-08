
create_p_matrix <- function(data) {
    n_row <- nrow(data)

    dist <- as.matrix(cluster::daisy(data, metric = "gower", stand = TRUE))

    p_matrix <- (1 - dist)/rowSums(1-dist, na.rm = TRUE)
    return(p_matrix)
}

est_mix_time <- function(p_matrix, eps = 0.01){
    n <- nrow(p_matrix)

    if(n<3) return 2;

    eig <- tryCatch({RSpectra::eigs(p_matrix, 2, which = 'LM')$values},
        error = function(f) {return(c(1, 0.5))
    })
    lambda <- sort(Mod(eig), decreasing = TRUE)[2]

    gap <- 1 - lambda
    if (gap < 1e-5) gap <- 1e-5
    mix_time <- ceiling(log(n / eps) / gap)
    return mix_time; 
}


#' @export
#' @useDynLib rwimputation, .registration = TRUE
rwimpute <- function(data, num_steps = -1L, seed=42L, restart_prob = 0.0) {
    
    if(restart_prob < 0.0 || restart_prob >= 1) stop()

    if (!is.data.frame(data)) data <- as.data.frame(data)
    p_matrix <- create_p_matrix(data)

    if(restart_prob <= 0.0){
        mix_time <- est_mix_time(p_matrix, eps=0.01)

        if(num_steps == -1L){
            num_steps <- as.integer(max(3, floor(mix_time / 2)))
        } else if (num_steps > mix_time) {
           warning(sprintf(""))
           num_steps <- as.integer(mix_time)
        }
    } else{
        if(num_steps == -1L) {
            est_steps <- ceiling(40/restart_prob)
            num_steps <- as.integer(max(50, min(est_steps, 2000)))   
        }
        else if (num_steps > 5000L) {
           warning("")
           num_steps <- 5000L
        }
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
    params <- as.double(c(num_steps, restart_prob))

    raw_result <- .Call("r_rwimpute", data_matrix, p_matrix, as.integer(col_types), params, as.integer(seed))
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