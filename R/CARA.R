#' @export
#' @title Constant Absolute Risk Aversion (CARA) Utility Function
#' @aliases CARA
#' @description Compute the value and the certainty equivalent of the CARA utility function, i.e. -exp(-gamma*x). In general equilibrium analysis, the CARA utility function has an interval scale like temperature.
#' @param x a payoff k-vector.
#' @param gamma the Arrow-Pratt measure of absolute risk aversion.
#' @param prob a probability k-vector. By default, the states are assumed to
#' occur with equal probability.
#' @return A list containing the following components:
#' \itemize{
#' \item u: the utility level.
#' \item CE: the certainty equivalent.
#' }
#' @examples
#' mu <- 5 # mu <- 8
#' a <- 1
#' x <- c(mu - a, mu + a)
#' gamma <- 0.8
#' mu - CARA(x, gamma)$CE
#'
#' ####
#' gamma <- 0.8
#' mu <- 2
#' sigma <- 2
#' x <- seq(mu - 5 * sigma, mu + 5 * sigma, length.out = 10000)
#' # two CE calculation methods for random variables of normal distribution
#' CARA(x, gamma, dnorm(x, mean = mu, sd = sigma))
#' mu - gamma * sigma^2 / 2
CARA <- function(x, gamma, prob = rep(1 / length(x), length(x))) {
  u <- weighted.mean(-exp(-x * gamma), prob)
  CE <- log(-u) / (-gamma)
  return(list(u = u, CE = CE))
}
