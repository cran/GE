#' @title Convert a rate vector to a beta vector
#' @aliases rate_to_beta
#' @description Convert a rate vector to a beta vector.
#' @param rate a rate vector.
#' @return a beta vector.
#' @examples
#' \donttest{
#' rate_to_beta(c(1, 1 / 3, 1 / 4))
#' }
#'
rate_to_beta <- function(rate) {
  result <- c(rate[1], rate[-1] * rate[1])
  prop.table(result)
}

#' @title Convert a beta vector to a rate vector
#' @aliases beta_to_rate
#' @description Convert a beta vector to a rate vector.
#' @param beta a beta vector.
#' @return a rate vector.
#' @examples
#' \donttest{
#' x <- beta_to_rate(c(0.7, 0.1, 0.2))
#' rate_to_beta(x)
#' }
#'
beta_to_rate <- function(beta) {
  c(beta[1], beta[-1] / beta[1])
}
