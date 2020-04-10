#' @export
#' @title Constant Relative Risk Aversion (CRRA) Utility Function
#' @aliases CRRA
#' @description Compute the value and the certainty equivalent of the CRRA utility function.
#' @param x a vector of all possible states (e.g. returns).
#' @param theta the relative risk aversion coefficient.
#' @param p the probability vector. By default, the states are assumed to
#' occur with equal probability.
#' @return A 2-vector consisting of the value and the certainty equivalent.
#' @examples
#' csv <- 0.05 # coefficient of standard deviation
#' mu <- 90 # mu <- 100
#' sigma <- mu * csv
#' x <- seq(mu - 5 * sigma, mu + 5 * sigma, length.out = 10000)
#' pd <- dnorm(x, mean = mu, sd = sigma)
#' theta <- 0.8
#' # the ratio of risk premium to expected return (i.e. the relative risk premium).
#' (mu - CRRA(x, theta, pd)[2]) / mu
CRRA <- function(x, theta, p = rep(1 / length(x), length(x))) {
  if (theta == 1) {
    u <- weighted.mean(log(x), p)
    CE <- exp(u)
  } else {
    u <- weighted.mean((x^(1 - theta) - 1) / (1 - theta), p)
    CE <- (u * (1 - theta) + 1)^(1 / (1 - theta))
  }

  return(c(u = u, CE = CE))
}
