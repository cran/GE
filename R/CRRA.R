#' @export
#' @title Constant Relative Risk Aversion (CRRA) Utility Function
#' @aliases CRRA
#' @description Compute the value and the certainty equivalent of the CRRA utility function.
#' @param x a vector of all possible states (e.g. returns).
#' @param gamma the relative risk aversion coefficient.
#' @param p the probability vector. By default, the states are assumed to
#' occur with equal probability.
#' @return A list containing the following components:
#' \itemize{
#' \item u: the utility level.
#' \item CE: the certainty equivalent.
#' }
#' @examples
#' csv <- 0.05 # coefficient of standard deviation
#' mu <- 90 # mu <- 100
#' sigma <- mu * csv
#' x <- seq(mu - 5 * sigma, mu + 5 * sigma, length.out = 10000)
#' pd <- dnorm(x, mean = mu, sd = sigma)
#' gamma <- 0.8
#' # the ratio of risk premium to expected return (i.e. the relative risk premium).
#' (mu - CRRA(x, gamma, pd)$CE) / mu
#'
#' ####
#' df <- apply_expand.grid(
#'   function(arg) {
#'     CRRA(arg["x"], arg["gamma"])$u
#'   },
#'   x = seq(0.5, 3, 0.1),
#'   gamma = c(0.5, 1, 2, 3)
#' )
#' coplot(result ~ x | as.factor(gamma), data = df)


CRRA <- function(x, gamma, p = rep(1 / length(x), length(x))) {
  if (gamma == 1) {
    u <- weighted.mean(log(x), p)
    CE <- exp(u)
  } else {
    u <- weighted.mean((x^(1 - gamma) - 1) / (1 - gamma), p)
    CE <- (u * (1 - gamma) + 1)^(1 / (1 - gamma))
  }

  return(list(u = u, CE = CE))
}
