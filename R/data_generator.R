#' Generate Knapsack Objects
#'
#' @param n Number of items to generate
#' @param seed Seed for reproducibility
#' @return A data frame with columns w (weight) and v (value)
#' @export
generate_knapsack_objects <- function(n = 2000, seed = 42) {
  suppressWarnings({
    RNGversion(min(as.character(getRversion()), "3.5.3"))
    set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion")
  })

  data.frame(
    w = sample(1:4000, size = n, replace = TRUE),
    v = runif(n = n, 0, 10000)
  )
}
