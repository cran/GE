#' @export
#' @title Some General Equilibrium Models with Endogenous Utility Function
#' @aliases gemstEndogenousUtilityFunction
#' @description Some examples of the market-clearing path with an endogenous utility function.
#' The parameters of the utility function will change with the utility level.
#'
#' To deal with non-homothetic preferences, we can simply use an endogenous CES-type utility function instead of a utility function with a more complex form.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' #### a 2-by-2 example
#' dst.firm <- node_new(
#'   "output",
#'   type = "CD", alpha = 5, beta = c(0.5, 0.5),
#'   "prod", "lab"
#' )
#'
#' dst.consumer <- node_new(
#'   "util",
#'   type = "CD", alpha = 1, beta = c(0.5, 0.5),
#'   "prod", "lab"
#' )
#'
#' ge <- sdm2(
#'   A = list(dst.firm, dst.consumer),
#'   B = matrix(c(
#'     1, 0,
#'     0, 0
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
#' matplot(ge$ts.z, type = "o", pch = 20)
#' ge$z
#' dst.consumer$beta
#'
#' #### a 3-by-3 example with 100 laborers
#' #### Assume that each laborer desires to consume one unit of
#' #### corn per period, regardless of her level of utility.
#' dst.firm.corn <- node_new(
#'   "corn",
#'   type = "CD", alpha = 1, beta = c(0.5, 0.5),
#'   "iron", "lab"
#' )
#'
#' dst.firm.iron <- node_new(
#'   "iron",
#'   type = "CD", alpha = 5, beta = c(0.5, 0.5),
#'   "iron", "lab"
#' )
#'
#' dst.consumer <- node_new(
#'   "util",
#'   type = "Leontief",
#'   a = c(0.5, 0.5),
#'   "corn", "iron"
#' )
#'
#' ge <- sdm2(
#'   A = list(dst.firm.corn, dst.firm.iron, dst.consumer),
#'   B = matrix(c(
#'     1, 0, 0,
#'     0, 1, 0,
#'     0, 0, 0
#'   ), 3, 3, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA, NA,
#'     NA, NA, NA,
#'     NA, NA, 100
#'   ), 3, 3, TRUE),
#'   names.commodity = c("corn", "iron", "lab"),
#'   names.agent = c("firm.corn", "firm.iron", "consumer"),
#'   numeraire = "lab",
#'   ts = TRUE,
#'   policy = list(
#'     function(A, state) {
#'       last.util <- state$last.z[3] / 100 # the previous utility level of each laborer
#'       a1 <- min(1 / last.util, 1)
#'       A[[3]]$a <- c(a1, 1 - a1)
#'     },
#'     policyMarketClearingPrice
#'   ),
#'   numberOfPeriods = 40,
#'   maxIteration = 1
#' )
#'
#' matplot(ge$ts.z, type = "o", pch = 20)
#' ge$z
#' ge$A
#' ge$D
#'
#' #### a 4-by-4 example with 100 laborers
#' dst.agri <- node_new(
#'   "output",
#'   type = "SCES",
#'   es = 0.5, alpha = 2, beta = c(0.2, 0.8),
#'   "manu", "lab"
#' )
#'
#' dst.manu <- node_new(
#'   "output",
#'   type = "SCES",
#'   es = 0.5, alpha = 3, beta = c(0.6, 0.4),
#'   "manu", "lab"
#' )
#'
#' dst.serv <- node_new(
#'   "output",
#'   type = "SCES",
#'   es = 0.5, alpha = 2, beta = c(0.4, 0.6),
#'   "manu", "lab"
#' )
#'
#' dst.consumer <- node_new(
#'   "util",
#'   type = "CD", alpha = 1, beta = c(0.6, 0.3, 0.1),
#'   "agri", "manu", "serv"
#' )
#'
#' ge <- sdm2(
#'   A = list(dst.agri, dst.manu, dst.serv, dst.consumer),
#'   B = diag(c(1, 1, 1, 0)),
#'   S0Exg = {
#'     tmp <- matrix(NA, 4, 4)
#'     tmp[4, 4] <- 100
#'     tmp
#'   },
#'   names.commodity = c("agri", "manu", "serv", "lab"),
#'   names.agent = c("agri", "manu", "serv", "consumer"),
#'   numeraire = "lab",
#'   ts = TRUE,
#'   policy = list(
#'     function(A, state) {
#'       util <- state$last.z[4] / 100 #
#'       beta1 <- structural_function(util, c(1, 6), 0.6, 0.1)
#'       beta3 <- structural_function(util, c(1, 6), 0.1, 0.5)
#'       beta2 <- 1 - beta1 - beta3
#'       A[[4]]$beta <- c(beta1, beta2, beta3)
#'     },
#'     policyMarketClearingPrice
#'   ),
#'   numberOfPeriods = 20,
#'   maxIteration = 1
#' )
#'
#' matplot(ge$ts.z, type = "o", pch = 20)
#' ge$z
#' dst.consumer$beta
#' }
#'

gemstEndogenousUtilityFunction <- function(...) sdm2(...)
