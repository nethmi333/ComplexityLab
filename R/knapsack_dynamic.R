#' Dynamic Programming Knapsack Solver
#'
#' @param x A data frame with columns `w` (weight) and `v` (value)
#' @param W Maximum weight capacity of the knapsack
#' @return A list containing the maximum total value and the selected row indices
#' @export
knapsack_dynamic <- function(x, W) {

  if (!is.data.frame(x) || !all(c("w", "v") %in% colnames(x))) {
    stop("x must be a data.frame containing columns 'w' and 'v'.")
  }
  if (any(x$w <= 0) || any(x$v <= 0) || W <= 0) {
    stop("All weights, values, and capacity W must be positive numbers.")
  }

  n <- nrow(x)


  m <- matrix(0, nrow = n + 1, ncol = W + 1)


  for (i in seq_len(n)) {
    weight_i <- x$w[i]
    value_i <- x$v[i]

    for (w in 0:W) {
      if (weight_i <= w) {

        m[i + 1, w + 1] <- max(
          m[i, w + 1],
          m[i, w - weight_i + 1] + value_i
        )
      } else {

        m[i + 1, w + 1] <- m[i, w + 1]
      }
    }
  }


  max_val <- m[n + 1, W + 1]
  elements <- c()
  curr_w <- W

  for (i in n:1) {
    if (m[i + 1, curr_w + 1] != m[i, curr_w + 1]) {
      elements <- c(elements, i)
      curr_w <- curr_w - x$w[i]
    }
  }


  elements <- sort(elements)

  return(list(value = max_val, elements = elements))
}
