#' @export
#' @title Some Examples with Exogenous Utility Level and Endogenous Labor Supply
#' @aliases gemExogenousUtilityLevel_EndogenousLaborSupply_3_3
#' @description Some examples with exogenous utility level and endogenous labor supply.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' ## a spot market clearing path (alias instantaneous equilibrium path)
#' utility.level.laborer <- 0.625
#'
#' dst.firm <- node_new("output",
#'   type = "CD",
#'   alpha = 1,
#'   beta = c(0.5, 0.5),
#'   "land", "lab"
#' )
#'
#' dst.landowner <- node_new(
#'   "util",
#'   type = "Leontief", a = 1,
#'   "prod"
#' )
#'
#' dst.laborer <- Clone(dst.landowner)
#'
#' dstl <- list(dst.firm, dst.landowner, dst.laborer)
#'
#' ge <- sdm2(
#'   A = dstl,
#'   B = diag(c(1, 0, 0)),
#'   S0Exg = matrix(c(
#'     NA, NA, NA,
#'     NA, 100, NA,
#'     NA, NA, 100
#'   ), 3, 3, TRUE),
#'   GRExg = 0,
#'   names.commodity = c("prod", "land", "lab"),
#'   names.agent = c("firm", "landowner", "laborer"),
#'   maxIteration = 1,
#'   numberOfPeriods = 30,
#'   numeraire = "prod",
#'   ts = TRUE,
#'   policy = list(
#'     function(time, state, state.history) {
#'       if (time > 1) {
#'         last.labor.supply <- state.history$S[3, 3, time - 1]
#'
#'         ratio <- state$last.z[3] / last.labor.supply / utility.level.laborer
#'         state$S[3, 3] <- last.labor.supply * ratio
#'       }
#'       state
#'     },
#'     policyMarketClearingPrice
#'   )
#' )
#'
#' matplot(ge$ts.p, type = "l")
#' plot(ge$ts.S[3, 3, ], type = "l")
#' ge$S
#'
#'
#' ## Regard the laborer as a firm.
#' dstl[[3]] <- node_new(
#'   "lab",
#'   type = "Leontief", a = utility.level.laborer,
#'   "prod"
#' )
#'
#' ge <- sdm2(
#'   A = dstl,
#'   B = diag(c(1, 0, 1)),
#'   S0Exg = matrix(c(
#'     NA, NA, NA,
#'     NA, 100, NA,
#'     NA, NA, NA
#'   ), 3, 3, TRUE),
#'   names.commodity = c("prod", "land", "lab"),
#'   names.agent = c("firm", "landowner", "laborer"),
#'   maxIteration = 1,
#'   numberOfPeriods = 30,
#'   numeraire = "prod",
#'   ts = TRUE,
#'   policy = policyMarketClearingPrice
#' )
#'
#' matplot(ge$ts.p, type = "l")
#' plot(ge$ts.S[3, 3, ], type = "l")
#' ge$p
#' ge$S
#' }

gemExogenousUtilityLevel_EndogenousLaborSupply_3_3 <- function(...) sdm2(...)
