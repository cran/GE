#' @export
#' @title Instantaneous Equilibrium Paths with Heterogeneous Firms
#' @aliases gemHeterogeneousFirms_2_3
#' @description This is an example of instantaneous equilibrium paths with heterogeneous firms.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' dst.firm1 <- node_new(
#'   "output",
#'   type = "CD", alpha = 1, beta = c(0.35, 0.65),
#'   "prod", "lab"
#' )
#'
#' dst.firm2 <- node_new(
#'   "output",
#'   type = "CD", alpha = 1.3, beta = c(0.9, 0.1),
#'   "prod", "lab"
#' )
#'
#' dst.consumer <- node_new(
#'   "util",
#'   type = "Leontief", a = 1,
#'   "prod"
#' )
#'
#' ge <- sdm2(
#'   A = list(dst.firm1, dst.firm2, dst.consumer),
#'   B = matrix(c(
#'     1, 1, 0,
#'     0, 0, 1
#'   ), 2, 3, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA, NA,
#'     NA, NA, 1
#'   ), 2, 3, TRUE),
#'   names.commodity = c("prod", "lab"),
#'   names.agent = c("firm1", "firm2", "consumer"),
#'   numeraire = "lab",
#'   z0 = c(0.01, 0.01, 1),
#'   ts = TRUE,
#'   policy = policyMarketClearingPrice,
#'   numberOfPeriods = 200,
#'   maxIteration = 1
#' )
#'
#' matplot(ge$ts.z, type = "l")
#' }

gemHeterogeneousFirms_2_3 <- function(...) sdm2(...)
