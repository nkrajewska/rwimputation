
create_p_matrix <- function(data, thld) {
    n_row <- nrow(data)
    k <- max(5L, min(50L, as.integer(sqrt(n_row))))


    dist <- as.matrix(suppressWarnings(cluster::daisy(data, metric = "gower", stand = TRUE)))
    max_nnz <- n_row*k
    rows <- integer(n_row + 1L)
    col_ind <- integer(max_nnz)
    wght <- numeric(max_nnz)

    nnz <- 0L
    for( i in seq_len(n_row) ){
        d <- dist[i, ]
        d[i] <- Inf
        d[is.na(d)] <- Inf

        knn <- order(d)[seq_len(k)]
        knn_dist <- d[knn]
        sim <- 1-knn_dist
        valid <- (sim) >= thld        

        knn <- knn[valid]
        sim <- sim[valid]

        if(length(knn) == 0L) {
            best <- which.min(d)
            knn <- best
            sim <- 1.0
        }else {sim <- sim/sum(sim) }

        
        len <- length(knn) 
        col_ind[(nnz + 1L):(nnz + len)] <- as.integer(knn - 1L)
        wght[(nnz + 1L):(nnz + len)] <- sim
        nnz <- nnz + len
        rows[i + 1L] <- nnz
    } 
    length(col_ind) <- nnz
    length(wght) <- nnz

    list(
        rows = rows,
        col_ind = col_ind,
        wght = wght,
        n_row = n_row,
        nnz = nnz
    )
}

#' Imputation using Random Walk
#'
#' @description Imputes missing values in a data frame or matrix using a 
#' random walk algorithm based on Gower distance similarity.
#'
#' @param data A data frame or matrix containing missing values.
#' @param num_steps Number of steps for the random walk. Default is 10. 
#' If -1 and restart_prob is set, it is calculated automatically.
#' @param seed Integer, random seed for reproducibility. Default is 42.
#' @param restart_prob Probability of restarting the random walk. Default is 0.0.
#'
#' @return A data frame or matrix (depending on the input) with imputed values.
#' @export
#' @examples
#' data(iris)
#' iris[1:5, 1] <- NA
#' imputed_data <- rwimpute(iris)
#' @useDynLib rwimputation, .registration = TRUE
#' @importFrom cluster daisy
rwimpute <- function(data, num_steps = 10L, seed=42L, restart_prob = 0.0) {
    
    if(restart_prob < 0.0 || restart_prob >= 1) stop("restart_prob must be in range [0, 1)")
    is_matrix <- is.matrix(data)
    if (is.null(dim(data))) {
        stop("Povided object has no dimensions. Data must be a matrix or a data frame.")
    }
    if (ncol(data) < 2L) {
        stop("Data must have at least 2 columns to perform random walk imputation.")
    }

    if (!is.data.frame(data)) data <- as.data.frame(data)
    p_matrix <- create_p_matrix(data, 0.05)

    if(restart_prob <= 0.0){
        if(num_steps == -1L){
            num_steps <- 10L 
        }
    } else{
        if(num_steps == -1L) {
            est_steps <- ceiling(40 / restart_prob)
            num_steps <- as.integer(max(50, min(est_steps, 2000)))   
        } else if (num_steps > 5000L) {
            warning("num_steps limited to 5000")
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
    params <- as.double(c(num_steps, restart_prob))

    raw_result <- .Call("r_rwimpute", data_matrix, as.integer(p_matrix$rows), as.integer(p_matrix$col_ind),as.double(p_matrix$wght),
    as.integer(col_types), params, as.integer(seed))
    result_df <- data
    for (j in 1:cols) {
        if (col_types[j] == 1) {
            indices <- round(raw_result[, j])
            result_df[[j]] <- factor(factor_levels[[j]][indices], levels = factor_levels[[j]])
        } else {
            result_df[[j]] <- raw_result[, j]
        }
    }
    if (is_matrix) {
        return(as.matrix(result_df))
    } else {
        return(result_df)
    }

}