#' @export
#' @title A Canonical Dynamic Macroeconomic General Equilibrium Model (see Torres, 2016)
#' @aliases gemCanonicalDynamicMacroeconomic_4_3
#' @description A canonical dynamic macroeconomic general equilibrium model (see Torres, 2016, Table 2.1 and 2.2).
#' @details A general equilibrium model with 4 commodities (i.e. product, labor, capital and equity shares)
#' and 3 agents (i.e. a production firm, a consumer and a capital-leasing firm).
#' @param discount.factor the intertemporal discount factor.
#' @param depreciation.rate the physical depreciation rate of capital stock.
#' @param beta.lab.firm the share parameter for labor of the Cobb-Douglas production function of the production firm.
#' @param beta.lab.consumer the share parameter for labor of the Cobb-Douglas utility function of the consumer.
#' @param ... arguments to be to be passed to the function sdm2.
#' @return A general equilibrium (see \code{\link{sdm2}})
#' @references Torres, Jose L. (2016, ISBN: 9781622730452) Introduction to Dynamic Macroeconomic General Equilibrium Models (Second Edition). Vernon Press.
#' @examples
#' \donttest{
#' ge <- gemCanonicalDynamicMacroeconomic_4_3(
#'   numberOfPeriods = 5000,
#'   priceAdjustmentVelocity = 0.03
#' )
#'
#' ## business cycle
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
#' }

gemCanonicalDynamicMacroeconomic_4_3 <- function(discount.factor = 0.97,
                                                 depreciation.rate = 0.06,
                                                 beta.lab.firm =  0.65,
                                                 beta.lab.consumer =  0.6,
                                                 ...) {
  return.rate <- 1 / discount.factor - 1
  depreciation.rate <- 0.06



  dst.firm1 <- node_new("prod",
                        type = "CD", alpha = 1,
                        beta = c(beta.lab.firm, 1 - beta.lab.firm),
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
  node_plot(dst.firm2)

  dst.consumer <- node_new("util",
                           type = "CD", alpha = 1,
                           beta = c(1 - beta.lab.consumer, beta.lab.consumer),
                           "prod", "lab"
  )

  ge <- sdm2(
    A = list(dst.firm1, dst.consumer, dst.firm2),
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
    names.agent = c("firm1", "firm2", "consumer"),
    numeraire = "prod",
    maxIteration = 1,
    ts = TRUE,
    ...
  )

  plot(ge$ts.z[, 1], type = "l")
  ge
}
