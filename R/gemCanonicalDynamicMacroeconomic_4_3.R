#' @export
#' @title A Canonical Dynamic Macroeconomic General Equilibrium Model (see Torres, 2016)
#' @aliases gemCanonicalDynamicMacroeconomic_4_3
#' @description A canonical dynamic macroeconomic general equilibrium model (see Torres, 2016, Table 2.1 and 2.2).
#' @details A general equilibrium model with 4 commodities (i.e. product, labor, capital and equity shares)
#' and 3 agents (i.e. a production firm, a consumer and a capital-leasing firm).
#' @param discount.factor the intertemporal discount factor.
#' @param depreciation.rate the physical depreciation rate of capital stock.
#' @param beta.lab.firm1 the share parameter for labor of the Cobb-Douglas production function of the production firm.
#' @param beta.lab.consumer the share parameter for labor of the Cobb-Douglas utility function of the consumer.
#' @param ... arguments to be to be passed to the function sdm2.
#' @return A general equilibrium (see \code{\link{sdm2}})
#' @references Torres, Jose L. (2016, ISBN: 9781622730452) Introduction to Dynamic Macroeconomic General Equilibrium Models (Second Edition). Vernon Press.
#' @examples
#' \donttest{
#' #### a market-clearing path that converges to the steady-state equilibrium
#' ge <- gemCanonicalDynamicMacroeconomic_4_3(
#'   numberOfPeriods = 100,
#'   policy = policyMarketClearingPrice,
#'   z0 = c(1, 1, 1)
#' )
#'
#' matplot(ge$ts.z, type = "o", pch = 20)
#' matplot(ge$ts.p, type = "o", pch = 20)
#'
#' ## population growth: a market-clearing path
#' ## that converges a balanced growth path
#' ge <- gemCanonicalDynamicMacroeconomic_4_3(
#'   numberOfPeriods = 100,
#'   GRExg = 0.01,
#'   z0 = c(1, 1, 1),
#'   policy = policyMarketClearingPrice
#' )
#'
#' matplot((ge$ts.p), type = "l")
#' matplot((ge$ts.z), type = "l")
#' matplot(growth_rate(ge$ts.z), type = "l")
#'
#' #### a disequilibrium path and the steady-state equilibrium
#' ge <- gemCanonicalDynamicMacroeconomic_4_3(
#'   numberOfPeriods = 5000,
#'   priceAdjustmentVelocity = 0.03,
#'   z0 = c(1, 1, 1)
#' )
#'
#' ge$p
#' ge$z
#' matplot(ge$ts.z, type = "l")
#' node_plot(ge$dstl[[3]], param = TRUE)
#'
#' ## a small disturbance to the product supply
#' ge <- gemCanonicalDynamicMacroeconomic_4_3(
#'   numberOfPeriods = 4000,
#'   priceAdjustmentVelocity = 0.03,
#'   z0 = c(1, 1, 1),
#'   policy = function(time, state) {
#'     if (time == 1500) {
#'       state$S[1, 1] <- state$S[1, 1] * 0.999
#'     }
#'     state
#'   }
#' )
#'
#' #### business cycle
#' de <- gemCanonicalDynamicMacroeconomic_4_3(
#'   numberOfPeriods = 1000,
#'   priceAdjustmentVelocity = 0.15
#' )
#'
#' ## A tax rate policy is implemented from the 600th period to stabilize the economy.
#' ge <- gemCanonicalDynamicMacroeconomic_4_3(
#'   numberOfPeriods = 1500,
#'   priceAdjustmentVelocity = 0.15,
#'   policy = Example9.10.policy.tax
#' )
#'
#' matplot(ge$ts.z, type = "l")
#' plot(ge$policy.data, type = "l") # tax rates
#'
#' #### a market-clearing path with a productivity shock
#' nPeriod <- 100 # the number of periods of the market-clearing path
#' set.seed(1)
#' alpha.shock <- rep(1, nPeriod)
#' alpha.shock[11] <- exp(0.01)
#' for (t in 12:nPeriod) {
#'   alpha.shock[t] <- exp(0.95 * log(alpha.shock[t - 1]))
#' }
#' plot(alpha.shock)
#'
#' ge <- gemCanonicalDynamicMacroeconomic_4_3(
#'   numberOfPeriods = nPeriod,
#'   p0 = c(1, 1.34312, 0.09093, 0.08865),
#'   z0 = c(0.7447, 0.6120, 2.8665),
#'   policy = list(
#'     function(time, A) {
#'       A[[1]]$alpha <- alpha.shock[time]
#'     },
#'     policyMarketClearingPrice
#'   )
#' )
#'
#' matplot(ge$ts.z[, 1], type = "o", pch = 20)
#' }

gemCanonicalDynamicMacroeconomic_4_3 <- function(discount.factor = 0.97,
                                                 depreciation.rate = 0.06,
                                                 beta.lab.firm1 =  0.65,
                                                 beta.lab.consumer =  0.6,
                                                 ...) {
  return.rate <- 1 / discount.factor - 1
  depreciation.rate <- 0.06

  dst.firm1 <- node_new("prod",
                        type = "CD", alpha = 1,
                        beta = c(beta.lab.firm1, 1 - beta.lab.firm1),
                        "lab", "cap"
  )

  dst.firm2 <- node_new("cap",
                        type = "FIN", rate = c(1, return.rate),
                        "cc1", "equity.share"
  )
  node_set(dst.firm2, "cc1",
           type = "Leontief", a = 1,
           "prod"
  )

  dst.consumer <- node_new("util",
                           type = "CD", alpha = 1,
                           beta = c(1 - beta.lab.consumer, beta.lab.consumer),
                           "prod", "lab"
  )

  dstl <- list(dst.firm1, dst.consumer, dst.firm2)
  ge <- sdm2(
    A = dstl,
    B = matrix(c(
      1, 0, 1 - depreciation.rate,
      0, 0, 0,
      0, 0, 1,
      0, 0, 0
    ), 4, 3, TRUE),
    S0Exg = {
      S0Exg <- matrix(NA, 4, 3)
      S0Exg[2, 2] <- S0Exg[4, 2] <- 1
      S0Exg
    },
    names.commodity = c("prod", "lab", "cap", "equity.share"),
    names.agent = c("firm1", "consumer", "firm2"),
    numeraire = "prod",
    maxIteration = 1,
    ts = TRUE,
    ...
  )

  plot(ge$ts.z[, 1], type = "l")
  ge$dstl <- dstl
  ge
}
