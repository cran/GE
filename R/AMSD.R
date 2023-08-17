#' @export
#' @title Additive-Mean-Variance Utility Function and Additive-Mean-Standard-Deviation Utility Function
#' @aliases AMSD
#' @aliases AMV
#' @description Compute the utility function mean(x) - (gamma * sd.p(x))^theta / theta or
#' weighted.mean(x, wt) - (gamma * sd.p(x, wt))^theta / theta.
#' @describeIn AMSD
#' Computes the utility function mean(x) - (gamma * sd.p(x))^theta / theta or
#' weighted.mean(x, wt) - (gamma * sd.p(x, wt))^theta / theta. When theta == 2, it is the additive mean-variance utility function (i.e.
#' the function AMV).
#' When theta == 1 (the default value), it is the additive mean and standard deviation utility function.
#' @param x a numeric n-vector.
#' @param gamma a non-negative scalar representing the risk aversion coefficient with a default value of 1.
#' @param theta a non-negative scalar with a default value of 1.
#' @param wt a numeric n-vector of weights (or probability).
#' If wt is NULL, all elements of x are given the same weight.
#' @return A scalar indicating the utility level.
#' @references Nakamura, Yutaka (2015). Mean-Variance Utility. Journal of Economic Theory, 160: 536-556.
#' @examples
#' \donttest{
#' AMSD(1:2, gamma = 0.05)
#' AMSD(1:2, gamma = 1, theta = 2)
#'
#' marginal_utility(
#'   c(1, 1.001),
#'   c(0, 1), function(x) AMSD(x, gamma = 0.5)
#' )
#' marginal_utility(
#'   c(1.001, 1),
#'   c(0, 1), function(x) AMSD(x, gamma = 0.5)
#' )
#' }
#'
AMSD <- function(x, gamma = 1, wt = NULL, theta = 1) {
  if (is.null(wt)) wt <- rep(1, length(x))
  weighted.mean(x, wt) - (gamma * sd.p(x, wt))^theta / theta
}


#' @export
#' @describeIn AMSD
#' Compute the additive mean-variance utility function mean(x) - 0.5 * gamma * var.p(x) or
#' weighted.mean(x, wt) - 0.5 * gamma * var.p(x, wt).
AMV <- function(x, gamma = 1, wt = NULL) {
  if (is.null(wt)) wt <- rep(1, length(x))
  weighted.mean(x, wt) - 0.5 * gamma^2 * var.p(x, wt)
}
