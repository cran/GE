#' @export
#' @title Additive-Mean-Standard-Deviation Portfolio Utility Function
#' @aliases AMSDP
#' @description Compute the utility function x \%*\% mp - gamma^theta * (t(x) \%*\% Cov \%*\% x)^(0.5 * theta) / theta for a portfolio x.
#' @param x a numeric n-vector representing a portfolio.
#' @param mp a numeric n-vector representing the mean payoff of each of the n assets.
#' @param Cov the n-by-n covariance matrix of the payoff vectors of n assets.
#' @param gamma a non-negative scalar representing the risk aversion coefficient with a default value of 1.
#' @param theta a non-negative scalar with a default value of 1.
#' @return A scalar indicating the utility level.
#' @references Danthine, J. P., Donaldson, J. (2005, ISBN: 9780123693808) Intermediate Financial Theory. Elsevier Academic Press.
#' @references Nakamura, Yutaka (2015) Mean-Variance Utility. Journal of Economic Theory, 160: 536-556.
#' @references Sharpe, William F (2008, ISBN: 9780691138503) Investors and Markets: Portfolio Choices, Asset Prices, and Investment Advice. Princeton University Press.
#' @references Xu Gao (2018, ISBN: 9787300258232) Twenty-five Lectures on Financial Economics. Beijing: China Renmin University Press. (In Chinese)
#' @seealso \code{\link{AMSD}}
#' @examples
#' \donttest{
#' UAP <- matrix(c(
#'   0, 1, 1,
#'   0, 2, 1,
#'   1, 1, 1,
#'   1, 2, 1,
#'   2, 0, 1
#' ), nrow = 5, byrow = TRUE)
#'
#' portfolio <- c(1.977, 1.183, 3.820)
#'
#' AMSDP(portfolio, colMeans(UAP),
#'   cov.wt(UAP, method = "ML")$cov,
#'   gamma = 1, theta = 1
#' )
#'
#' AMSD(UAP %*% portfolio, gamma = 1, theta = 1)
#' }
AMSDP <- function(x, mp, Cov, gamma = 1, theta = 1) {
  x <- c(x)
  result <- x %*% mp - gamma^theta * (t(x) %*% Cov %*% x)^(0.5 * theta) / theta
  c(result)
}
