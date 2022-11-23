#' @export
#' @title Compute Asset Market Equilibria for Some Simple Cases
#' @aliases gemAssetPricing
#' @description Compute the equilibrium of an asset market by the function sdm2 and by computing marginal utility of assets.
#' This function is similar to the function gemSecurityPricing.
#' The difference is that this function uses portfolio utility functions,
#' while the function gemSecurityPricing uses payoff utility functions.
#' @param S an n-by-m supply matrix of assets.
#' @param uf a portfolio utility function or a portfolio utility function list.
#' @param numeraire	the name, index or price of the numeraire commodity. See \code{\link{sdm2}}.
#' @param ratio_adjust_coef a scalar indicating the adjustment velocity of demand structure.
#' @param ... arguments to be passed to the function sdm2.
#' @return  A general equilibrium containing a value marginal utility matrix (VMU).
#' @references Danthine, J. P., Donaldson, J. (2005, ISBN: 9780123693808) Intermediate Financial Theory. Elsevier Academic Press.
#' @references Sharpe, William F. (2008, ISBN: 9780691138503) Investors and Markets: Portfolio Choices, Asset Prices, and Investment Advice. Princeton University Press.
#' @references https://web.stanford.edu/~wfsharpe/apsim/index.html
#' @seealso \code{\link{gemSecurityPricing}}.
#' @examples
#' \donttest{
#' #### an example with AMSDP utility function.
#' set.seed(1)
#' n <- 5
#' m <- 3
#' Supply <- matrix(runif(n * m, 10, 100), n, m)
#' # the risk aversion coefficient.
#' gamma <- runif(m, 0.5, 2)
#'
#' # predicted mean payoff, which may be gross return rates, price indices or prices.
#' Pmp <- matrix(runif(n * m, min = 0.8, max = 1.5), n, m)
#' # predicted standard deviation of payoff
#' Psd <- matrix(runif(n * m, min = 0.01, max = 0.2), n, m)
#' Psd[n, ] <- 0
#'
#' # Suppose the predicted payoff correlation matrices of agents are the same.
#' Cor <- cor(matrix(runif(n * n), n, n))
#' Cor[, n] <- Cor[n, ] <- 0
#' Cor[n, n] <- 1
#'
#' # the list of utility function.
#' lst.uf <- list()
#'
#' make.uf <- function(mp, Cov, gamma) {
#'   force(mp); force(Cov); force(gamma)
#'   function(x) {
#'     AMSDP(x, mp = mp, Cov = Cov, gamma = gamma, theta = 1)
#'   }
#' }
#'
#' for (k in 1:m) {
#'   sigma <- Psd[, k]
#'   if (is.matrix(Cor)) {
#'     Cov <- dg(sigma) %*% Cor %*% dg(sigma)
#'   } else {
#'     Cov <- dg(sigma) %*% Cor[[k]] %*% dg(sigma)
#'   }
#'
#'   lst.uf[[k]] <- make.uf(mp = Pmp[, k], Cov = Cov, gamma = gamma[k])
#' }
#'
#' ge <- gemAssetPricing(
#'   S = Supply, uf = lst.uf,
#'   maxIteration = 1,
#'   numberOfPeriods = 2000,
#'   policy = makePolicyMeanValue(150),
#'   ts = TRUE
#' )
#'
#' matplot(ge$ts.p, type = "l")
#' ge$p
#' ge$D
#' ge$VMU
#'
#' #### an example of Danthine and Donaldson (2005, section 8.3).
#' ge <- gemAssetPricing(
#'   S = matrix(c(
#'     10, 5,
#'     1, 4,
#'     2, 6
#'   ), 3, 2, TRUE),
#'   uf = function(x) 0.5 * x[1] + 0.9 * (1 / 3 * log(x[2]) + 2 / 3 * log(x[3])),
#'   maxIteration = 1,
#'   numberOfPeriods = 500,
#'   ts = TRUE
#' )
#' matplot(ge$ts.p, type = "l")
#' ge$p
#'
#' #### an example of Sharpe (2008, chapter 2, case 1)
#' secy1 <- c(1, 0, 0, 0, 0)
#' secy2 <- c(0, 1, 1, 1, 1)
#' secy3 <- c(0, 5, 3, 8, 4) - 3 * secy2
#' secy4 <- c(0, 3, 5, 4, 8) - 3 * secy2
#' # unit security payoff matrix
#' UP <- cbind(secy1, secy2, secy3, secy4)
#'
#' prob <- c(0.15, 0.25, 0.25, 0.35)
#' wt <- prop.table(c(1, 0.96 * prob)) # weights
#'
#' ge <- gemAssetPricing(
#'   S = matrix(c(
#'     49, 49,
#'     30, 30,
#'     10, 0,
#'     0, 10
#'   ), 4, 2, TRUE),
#'   uf = list(
#'     function(portfolio) CES(alpha = 1, beta = wt, x = UP %*% portfolio, es = 1 / 1.5),
#'     function(portfolio) CES(alpha = 1, beta = wt, x = UP %*% portfolio, es = 1 / 2.5)
#'   ),
#'   maxIteration = 1,
#'   numberOfPeriods = 1000,
#'   numeraire = 1,
#'   ts = TRUE
#' )
#' matplot(ge$ts.p, type = "l")
#' ge$p
#' ge$p[3:4] + 3 * ge$p[2]
#'
#' }


gemAssetPricing <- function(S, uf,
                            numeraire = nrow(S),
                            ratio_adjust_coef = 0.1,
                            ...) {
  n <- nrow(S)
  m <- ncol(S)

  ge <- sdm2(
    A = function(state) {
      Portfolio <- state$last.A %*% dg(state$last.z)
      VMU <- marginal_utility(Portfolio, diag(n), uf, state$p)

      VMU <- pmax(VMU, 1e-10)

      Ratio <- sweep(VMU, 2, colMeans(VMU), "/")

      A <- state$last.A * ratio_adjust(Ratio, coef = ratio_adjust_coef, method = "linear")

      prop.table(A, 2)
      A
    },
    B = matrix(0, n, m),
    S0Exg = S,
    names.commodity = paste0("asset", 1:n),
    names.agent = paste0("agt", 1:m),
    numeraire = numeraire,
    ...
  )

  ge$VMU <- marginal_utility(ge$D, diag(n), uf = uf, price = ge$p)

  ge
}

