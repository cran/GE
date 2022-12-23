#' @export
#' @title Additive-Mean-Standard-Deviation Portfolio Utility Function
#' @aliases AMSDP
#' @description Compute the utility function x \%*\% mp - gamma^theta * (t(x) \%*\% Cov \%*\% x)^(0.5 * theta) / theta for a portfolio x.
#' @param x a numeric n-vector representing a portfolio.
#' @param mp a numeric n-vector representing the mean payoff of each of the n securities.
#' @param Cov the n-by-n covariance matrix of the payoff vectors of n securities.
#' @param gamma the risk aversion coefficient.
#' @param theta a non-negative scalar with a default value of 1.
#' @references Danthine, J. P., Donaldson, J. (2005, ISBN: 9780123693808) Intermediate Financial Theory. Elsevier Academic Press.
#' @references Nakamura, Yutaka (2015) Mean-Variance Utility. Journal of Economic Theory, 160: 536-556.
#' @references Sharpe, William F (2008, ISBN: 9780691138503) Investors and Markets: Portfolio Choices, Asset Prices, and Investment Advice. Princeton University Press.
#' @references Xu Gao (2018, ISBN: 9787300258232) Twenty-five Lectures on Financial Economics. Beijing: China Renmin University Press. (In Chinese)
#' @seealso \code{\link{AMSD}}

AMSDP <- function(x, mp, Cov, gamma = 1, theta = 1) {
  x <- c(x)
  x %*% mp - gamma^theta * (t(x) %*% Cov %*% x)^(0.5 * theta) / theta
}
