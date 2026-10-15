#' Greedy Knapsack Solver
#'
#' @param x A data frame with columns `w` (weight) and `v` (value)
#' @param W Maximum weight capacity of the knapsack
#' @return A list containing the approximate maximum total value and the selected row indices
#' @export
greedy_knapsack <- function(x, W) {

  if (!is.data.frame(x) || !all(c("w", "v") %in% colnames(x))) {
    stop("x must be a data.frame containing columns 'w' and 'v'.")
  }
  if (any(x$w <= 0) || any(x$v <= 0) || W <= 0) {
    stop("All weights, values, and capacity W must be positive numbers.")
  }

  n <- nrow(x)
  ratio <- x$v / x$w

  sorted_indices <- order(ratio, decreasing = TRUE)

  current_weight <- 0
  total_value <- 0
  selected_elements <- c()

  for (i in sorted_indices) {
    if (current_weight + x$w[i] <= W) {
      current_weight <- current_weight + x$w[i]
      total_value <- total_value + x$v[i]
      selected_elements <- c(selected_elements, i)
    }
  }

  selected_elements <- sort(selected_elements)

  return(list(value = total_value, elements = selected_elements))
}
