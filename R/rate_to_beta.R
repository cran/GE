#' @export
#' @title Conversion between a Rate Vector and a Beta Vector
#' @aliases rate_to_beta
#' @aliases beta_to_rate
#' @description Conversion between an expenditure rate vector and a beta vector (i.e. an expenditure proportion vector).
#' For an economic agent, the rate vector indicates the ratios between expenditures on financial instruments and the physical commodity.
#' The first element of the rate vector indicates the quantity of the physical commodity needed to obtain a unit of output.
#' Other elements indicate the ratio of expenditures on various financial instruments to that of the physical commodity,
#' which may be equal to the interest rate, the tax rate, the dividend rate, etc.
#' The beta vector indicates the proportions of expenditures on various commodities.
#' @describeIn rate_to_beta Convert a rate vector to a beta vector.
#' @param x a vector.
#' @return A vector.
#' @examples
#' \donttest{
#' rate_to_beta(c(1, 1 / 3, 1 / 4))
#' rate_to_beta(c(0.5, 1 / 3, 1 / 4))
#'
#' x <- beta_to_rate(c(0.7, 0.1, 0.2))
#' rate_to_beta(x)
#' }
#' @seealso \cite{\link{demand_coefficient}}
rate_to_beta <- function(x) {
  result <- c(x[1], x[-1] * x[1])
  prop.table(result)
}

#' @export
#' @describeIn rate_to_beta Convert a beta vector to a rate vector.
#' When converting the beta vector into a rate vector, it will be assumed that the first element of these two vectors is the same.
beta_to_rate <- function(x) {
  c(x[1], x[-1] / x[1])
}
