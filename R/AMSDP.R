#' @export
#' @title Additive-Mean-Standard-Deviation Portfolio Utility Function
#' @aliases AMSDP
#' @description Compute the utility function x \%*\% mp - 0.5 * gamma * (t(x) \%*\% Cov \%*\% x)^(0.5 * theta) for a portfolio x.
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
#' @examples
#' \donttest{
#' #### an example of security pricing with two heterogeneous agents who have
#' ## different beliefs and predict different payoff vectors.
#'
#' ## the predicted payoff vectors of agent 1 on the two securities.
#' secy1.1 <- c(1, 2, 2, 0)
#' secy2.1 <- c(2, 2, 0, 2)
#'
#' ## the predicted payoff vectors of agent 2 on the two securities.
#' secy1.2 <- c(1, 0, 2, 0)
#' secy2.2 <- c(2, 1, 0, 2)
#'
#' secy3 <- c(1, 1, 1, 1)
#'
#' ## the unit security payoff matrix of agent 1.
#' USP1 <- cbind(secy1.1, secy2.1, secy3)
#'
#' ## the unit security payoff matrix of agent 2.
#' USP2 <- cbind(secy1.2, secy2.2, secy3)
#'
#' mp1 <- colMeans(USP1)
#' Cov1 <- cov.wt(USP1, method = "ML")$cov
#'
#' mp2 <- colMeans(USP2)
#' Cov2 <- cov.wt(USP2, method = "ML")$cov
#'
#' ## the utility function of agent 1.
#' uf1 <- function(x) AMSDP(x, mp1, Cov1, gamma = 2)
#'
#' ## the utility function of agent 2.
#' uf2 <- function(x) AMSDP(x, mp2, Cov2, gamma = 2)
#'
#'
#' ge <- sdm2(
#'   A = function(state) {
#'     Portfolio <- state$last.A %*% dg(state$last.z)
#'
#'     VMU <- marginal_utility(Portfolio, diag(3), list(uf1, uf2), state$p)
#'
#'     Ratio <- sweep(VMU, 2, colMeans(VMU), "/")
#'
#'     A <- state$last.A * ratio_adjust(Ratio, coef = 0.15, method = "linear")
#'     prop.table(A, 2)
#'   },
#'   B = matrix(0, 3, 2),
#'   S0Exg = matrix(c(
#'     1, 5,
#'     2, 5,
#'     3, 5
#'   ), 3, 2, TRUE),
#'   names.commodity = c("secy1", "secy2", "secy3"),
#'   names.agent = c("agt1", "agt2"),
#'   numeraire = "secy3",
#'   maxIteration = 1,
#'   numberOfPeriods = 1000,
#'   ts = TRUE
#' )
#'
#' ge$p
#' ge$D
#' marginal_utility(ge$D[, 1], diag(3), uf1) / ge$p
#' marginal_utility(ge$D[, 2], diag(3), uf2) / ge$p
#' }

AMSDP <- function(x, mp, Cov, gamma = 1, theta = 1) {
  x %*% mp - 0.5 * gamma * (t(x) %*% Cov %*% x)^(0.5 * theta)
}