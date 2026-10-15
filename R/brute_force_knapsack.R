#' Brute Force Knapsack Solver
#'
#' @param x A data frame with columns `w` (weight) and `v` (value)
#' @param W Maximum weight capacity of the knapsack
#' @param parallel Logical. If TRUE, uses parallel processing across available cores. Default is FALSE.
#' @return A list containing the maximum total value and the selected row indices
#' @export
brute_force_knapsack <- function(x, W, parallel = FALSE) {
  if (!is.data.frame(x) || any(x$w <= 0) || any(x$v <= 0) || W <= 0) {
    stop("Invalid input parameters.")
  }

  n <- nrow(x)
  total_combos <- 2^n

  if (parallel) {

    num_cores <- max(1, parallel::detectCores() - 1)

    cl <- parallel::makeCluster(num_cores)

    parallel::clusterExport(cl, varlist = c("x", "W", "n", "total_combos"), envir = environment())

    chunks <- split(0:(total_combos - 1), cut(0:(total_combos - 1), num_cores, labels = FALSE))

    results <- parallel::parLapply(cl, chunks, function(chunk) {
      max_val <- 0
      best_elements <- c()

      for (i in chunk) {
        bits <- as.numeric(intToBits(i))[1:n]
        elements <- which(bits == 1)

        if (length(elements) > 0) {
          total_weight <- sum(x$w[elements])
          total_val <- sum(x$v[elements])

          if (total_weight <= W && total_val > max_val) {
            max_val <- total_val
            best_elements <- elements
          }
        }
      }
      return(list(value = max_val, elements = best_elements))
    })

    parallel::stopCluster(cl)

    best_val <- 0
    best_elements <- c()
    for (res in results) {
      if (res$value > best_val) {
        best_val <- res$value
        best_elements <- res$elements
      }
    }
    return(list(value = best_val, elements = best_elements))
  }

  max_val <- 0
  best_elements <- c()

  for (i in 0:(total_combos - 1)) {
    bits <- as.numeric(intToBits(i))[1:n]
    elements <- which(bits == 1)

    if (length(elements) > 0) {
      total_weight <- sum(x$w[elements])
      total_val <- sum(x$v[elements])

      if (total_weight <= W && total_val > max_val) {
        max_val <- total_val
        best_elements <- elements
      }
    }
  }

  return(list(value = max_val, elements = best_elements))
}
