#' @export
#' @title An Example Illustrating the Matthew Effect of Asset Exchange
#' @aliases gemAssetExchange_MatthewEffect_2_2
#' @description This is an example that illustrates the Matthew effect of asset exchange,
#' wherein the wealth gap between two traders widens after the exchange process.
#' Initially, these traders had a relatively small wealth (i.e. expected average payoff) gap.
#' However, the exchange leads to an expansion of the wealth gap.
#' This outcome can be attributed to the fact that a trader's risk aversion coefficient is affected by his level of wealth.
#' When traders have less wealth their risk aversion coefficient is higher.
#' Consequently, a trader with less wealth tends to acquire more low-risk, low-average-payoff assets through trading.
#' As a result, the expected average payoff of a trader with less wealth may decrease after the exchange.
#' Conversely, a trader with more wealth may hold more high-risk, high-average-payoff assets after trading.
#' @param ... arguments to be passed to the function sdm2.
#' @seealso \code{\link{gemAssetPricing_PUF}}.
#' @examples
#' \donttest{
#' #### Matthew effect
#' asset1 <- c(40, 200)
#' asset2 <- c(100, 100)
#'
#' # unit asset payoff matrix.
#' UAP <- cbind(asset1, asset2)
#'
#' S <- matrix(c(
#'   0.49, 0.51,
#'   0.49, 0.51
#' ), 2, 2, TRUE)
#'
#' ge <- sdm2(
#'   A = function(state) {
#'     Portfolio <- state$last.A %*% dg(state$last.z)
#'
#'     Payoff <- UAP %*% Portfolio
#'     payoff.average <- colMeans(Payoff)
#'
#'     # the risk aversion coefficients.
#'     rac <- ifelse(payoff.average > mean(UAP) * 1.02, 0.5, 1)
#'     rac <- ifelse(payoff.average < mean(UAP) / 1.02, 2, rac)
#'
#'     uf1 <- function(portfolio) {
#'       payoff <- UAP %*% portfolio
#'       CES(alpha = 1, beta = c(0.5, 0.5), x = payoff, es = 1 / rac[1])
#'     }
#'
#'     uf2 <- function(portfolio) {
#'       payoff <- UAP %*% portfolio
#'       CES(alpha = 1, beta = c(0.5, 0.5), x = payoff, es = 1 / rac[2])
#'     }
#'
#'     VMU <- marginal_utility(Portfolio, diag(2), list(uf1, uf2), state$p)
#'     VMU <- pmax(VMU, 1e-10)
#'
#'     Ratio <- sweep(VMU, 2, colMeans(VMU), "/")
#'     A <- state$last.A * ratio_adjust(Ratio, coef = 0.1, method = "linear")
#'
#'     prop.table(A, 2)
#'   },
#'   B = matrix(0, 2, 2),
#'   S0Exg = S,
#'   names.commodity = c("asset1", "asset2"),
#'   numeraire = 2,
#'   maxIteration = 1,
#'   numberOfPeriods = 1000,
#'   policy = makePolicyMeanValue(50),
#'   ts = TRUE
#' )
#'
#' matplot(ge$ts.p, type = "l")
#' ge$p
#' ge$z
#' ge$D
#'
#' (Payoff.S <- UAP %*% S)
#' colMeans(Payoff.S)
#'
#' (Payoff.D <- UAP %*% ge$D)
#' colMeans(Payoff.D)
#'
#' ## Calculate the equilibrium under the fixed risk aversion coefficients.
#' rac <- c(2, 0.5)
#'
#' uf <- list()
#' uf[[1]] <- function(portfolio) {
#'   payoff <- UAP %*% portfolio
#'   CES(alpha = 1, beta = c(0.5, 0.5), x = payoff, es = 1 / rac[1])
#' }
#'
#' uf[[2]] <- function(portfolio) {
#'   payoff <- UAP %*% portfolio
#'   CES(alpha = 1, beta = c(0.5, 0.5), x = payoff, es = 1 / rac[2])
#' }
#'
#' ge <- gemAssetPricing_PUF(
#'   S = S,
#'   uf = uf,
#'   policy = makePolicyMeanValue(50)
#' )
#'
#' ge$p
#' addmargins(ge$D, 2)
#' addmargins(ge$S, 2)
#' ge$VMU
#'
#' (Payoff <- UAP %*% ge$D)
#' colMeans(Payoff)
#' }

gemAssetExchange_MatthewEffect_2_2 <- function(...) sdm2(...)
