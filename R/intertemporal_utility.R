#' @export

#' @title Intertemporal Utility Function
#' @aliases intertemporal_utility
#' @description An intertemporal utility function with constant relative risk aversion.
#'
#' @param x a vector consists of the instantaneous utility levels of some periods.
#' @param beta the discount factor.
#' @param gamma the coefficient (>=0) in CRRA instantaneous utility function.
#' @return A list consists of the intertemporal utility level and the certainty-equivalent instantaneous utility level (i.e. steady-state-equivalent instantaneous utility level).
#' @examples
#' \donttest{
#' intertemporal_utility(c(1, 2, 3), beta = 0.99, gamma = 0.5)
#' intertemporal_utility(c(1, 2, 3) / 10, beta = 0.99, gamma = 0.5)
#' intertemporal_utility(c(1, 2, 3) / 10, gamma = 0)
#' }
#'
intertemporal_utility <- function(x, beta = 1, gamma = 1) {
  n <- length(x)
  prop <- beta^(0:(n - 1))
  tmp <- CRRA(x, gamma = gamma, p = prop)
  u.intertemporal <- unname(tmp$u * sum(prop))
  list(u.intertemporal = u.intertemporal, CE = tmp$CE)
}
