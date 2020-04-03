#' @export

#' @title Intertemporal Utility Function
#' @aliases intertemporal_utility
#' @description This is an intertemporal utility function with constant relative risk aversion.
#'
#' @param x a vector consists of the instantaneous utility levels of some periods.
#' @param beta the discount factor.
#' @param gamma the coefficient (>=0) in CRRA instantaneous utility function.
#' @return A list consists of the intertemporal utility level and the certainty-equivalent instantaneous utility level (i.e. steady-state-equivalent instantaneous utility level).
#' @examples
#' \donttest{
#' intertemporal_utility(c(1, 2, 3), beta = 0.99, gamma = 0.5)
#' intertemporal_utility(c(1, 2, 3)/10, beta = 0.99, gamma = 0.5)
#' intertemporal_utility(c(1, 2, 3)/10, gamma = 0)
#' }
#'
intertemporal_utility <- function(x, beta = 1, gamma = 1) {
  n <- length(x)
  if (gamma < 0) stop("Li: gamma<0")
  if (gamma == 1) {
    utility <- sum(log(x) * (beta^(0:(n - 1))))
    CE <- exp(utility/sum((beta^(0:(n - 1)))))
  } else {
    utility <- sum(((x^(1 - gamma) - 1) / (1 - gamma)) * (beta^(0:(n - 1))))
    CE <- (utility/sum((beta^(0:(n - 1))))*(1-gamma)+1)^(1/(1-gamma))
  }

  list(utility=utility, CE=CE)
}




