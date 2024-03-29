#' @export
#' @title A Canonical Dynamic Macroeconomic General Equilibrium Model (see Torres, 2016)
#' @aliases gemCanonicalDynamicMacroeconomic_3_2
#' @description A canonical dynamic macroeconomic general equilibrium model (see Torres, 2016, Table 2.1 and 2.2).
#' @details A general equilibrium model with 3 commodities (i.e. product, labor and equity shares)
#' and 2 agents (i.e. a firm and a consumer). Labor is the numeraire.
#' @param discount.factor the intertemporal discount factor.
#' @param depreciation.rate the physical depreciation rate of capital stock.
#' @param beta.prod.firm  the share parameter of the product in the Cobb-Douglas production function.
#' @param beta.prod.consumer the share parameter of the product in the Cobb-Douglas period utility function.
#' This parameter represents the individual's preferences regarding consumption-leisure decisions.
#' @param policy.supply a policy function or a policy function list which adjusts the supplies.
#' @param policy.technology a policy function or a policy function list which adjusts the technology.
#' @param policy.price a policy function or a policy function list which adjusts the prices.
#' @param ... arguments to be to be passed to the function sdm2.
#' @return A general equilibrium (see \code{\link{sdm2}}).
#' @references Torres, Jose L. (2016, ISBN: 9781622730452) Introduction to Dynamic Macroeconomic General Equilibrium Models (Second Edition). Vernon Press.
#' @references Li Xiangyang (2018, ISBN: 9787302497745) Dynamic Stochastic General Equilibrium (DSGE) Model: Theory, Methodology, and Dynare Practice. Tsinghua University Press. (In Chinese)
#' @seealso The spot market clearing path (alias instantaneous equilibrium path) can be computed with the function \code{\link{policyMarketClearingPrice}}.
#' @examples
#' \donttest{
#' gemCanonicalDynamicMacroeconomic_3_2()
#'
#' #### a spot market clearing path (alias instantaneous equilibrium path)
#' ge <- gemCanonicalDynamicMacroeconomic_3_2(
#'   policy.price = policyMarketClearingPrice,
#'   ts = TRUE,
#'   maxIteration = 1,
#'   numberOfPeriods = 100,
#'   z0 = c(50, 100)
#' )
#'
#' par(mfrow = c(1, 2))
#' matplot(ge$ts.z, type = "o", pch = 20)
#' matplot(ge$ts.p, type = "o", pch = 20)
#'
#' #### technology change in a market-clearing path
#' policyTechnologyChange <- function(time, A) {
#'   alpha <- 1.2 # The original value is 1.
#'   time.win <- c(50, 50)
#'   discount.factor <- 0.97
#'   depreciation.rate <- 0.06
#'   beta.prod.firm  <- 0.35
#'   return.rate <- 1 / discount.factor - 1
#'
#'   if (time >= time.win[1] && time <= time.win[2]) {
#'     A[[1]]$func <- function(p) {
#'       result <- CD_A(
#'         alpha, rbind(beta.prod.firm , 1 - beta.prod.firm , 0),
#'         c(p[1] * (return.rate + depreciation.rate), p[2:3])
#'       )
#'       result[3] <- p[1] * result[1] * return.rate / p[3]
#'       result
#'     }
#'   }
#' }
#'
#' ge <- gemCanonicalDynamicMacroeconomic_3_2(
#'   policy.technology = policyTechnologyChange,
#'   policy.price = policyMarketClearingPrice,
#'   ts = TRUE,
#'   maxIteration = 1,
#'   numberOfPeriods = 100,
#'   z0 = c(50, 100)
#' )
#'
#' par(mfrow = c(1, 2))
#' matplot(ge$ts.z, type = "o", pch = 20)
#' matplot(ge$ts.p, type = "o", pch = 20)
#'
#' #### an example on page 46 of Li Xiangyang (2018)
#' ge <- gemCanonicalDynamicMacroeconomic_3_2(
#'   discount.factor = 0.99,
#'   depreciation.rate = 0.025,
#'   beta.prod.firm  = 0.36,
#'   beta.prod.consumer = 1
#' )
#' }

gemCanonicalDynamicMacroeconomic_3_2 <- function(discount.factor = 0.97,
                                                 depreciation.rate = 0.06,
                                                 beta.prod.firm  = 0.35,
                                                 beta.prod.consumer = 0.4,
                                                 policy.supply = NULL,
                                                 policy.technology = NULL,
                                                 policy.price = NULL,
                                                 ...) {
  return.rate <- 1 / discount.factor - 1

  dst.firm <- node_new("output",
    type = "FUNC",
    func = function(p) {
      result <- CD_A(
        1, rbind(beta.prod.firm , 1 - beta.prod.firm , 0),
        c(p[1] * (return.rate + depreciation.rate), p[2:3])
      )
      result[3] <- p[1] * result[1] * return.rate / p[3]
      result
    },
    "prod", "lab", "equity.share"
  )

  dst.consumer <- node_new("util",
    type = "CD", alpha = 1, beta = c(beta.prod.consumer, 1 - beta.prod.consumer),
    "prod", "lab"
  )

  policy.B <- function(time, state, A) {
    a.firm.current <- demand_coefficient(A[[1]], state$p)
    state$B[1, 1] <- 1 + a.firm.current[1] * (1 - depreciation.rate)
    state
  }
  policy <- c(policy.supply, policy.technology, policy.price, policy.B)
  policy <- policy[!sapply(policy, is.null)]

  sdm2(
    A = list(dst.firm, dst.consumer),
    B = {
      B <- matrix(0, 3, 2)
      B[1, 1] <- 1
      B
    },
    S0Exg = matrix(c(
      NA, NA,
      NA, 100,
      NA, 100
    ), 3, 2, TRUE),
    names.commodity = c("prod", "lab", "equity.share"),
    names.agent = c("firm", "consumer"),
    numeraire = "lab",
    policy = policy,
    ...
  )
}
