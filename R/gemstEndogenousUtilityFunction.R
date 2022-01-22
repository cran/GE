#' @export
#' @title An Endogenous Utility Function
#' @aliases gemstEndogenousUtilityFunction
#' @description This is an example of the market-clearing path with an endogenous utility function.
#' The parameters of the utility function will change with the utility level.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' dst.firm <- node_new(
#'   "output",
#'   type = "CD", alpha = 5, beta = c(0.5, 0.5),
#'   "prod", "lab"
#' )
#'
#' dst.consumer <- node_new(
#'   "utility",
#'   type = "CD", alpha = 1, beta = c(0.5, 0.5),
#'   "prod", "lab"
#' )
#'
#' ge <- sdm2(
#'   A = list(dst.firm, dst.consumer),
#'   B = matrix(c(
#'     1, 0,
#'     0, 1
#'   ), 2, 2, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA,
#'     NA, 1
#'   ), 2, 2, TRUE),
#'   names.commodity = c("prod", "lab"),
#'   names.agent = c("firm", "consumer"),
#'   numeraire = "lab",
#'   z0 = c(0.01, 1),
#'   p0 = c(1, 1),
#'   ts = TRUE,
#'   policy = list(
#'     function(A, state) {
#'       util <- state$last.z[2]
#'       beta2 <- 0.95 * plogis(util, location = 2, scale = 2)
#'       A[[2]]$beta <- c(1 - beta2, beta2)
#'     },
#'     policyMarketClearingPrice
#'   ),
#'   numberOfPeriods = 20,
#'   maxIteration = 1
#' )
#'
#' matplot(ge$ts.z, type = "b", pch = 20)
#' ge$z
#' dst.consumer$beta
#' }
#'

gemstEndogenousUtilityFunction <- function(...) sdm2(...)