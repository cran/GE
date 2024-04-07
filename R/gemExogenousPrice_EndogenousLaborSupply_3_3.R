#' @export
#' @title An Example of Price Regulation and Endogenous Labor Supply (Example 9.5 of Li, 2019)
#' @aliases gemExogenousPrice_EndogenousLaborSupply_3_3
#' @description This is an example of price regulation and endogenous labor supply.
#' See CGE::Example9.5.
#' @param ... arguments to be passed to the function sdm2.
#' @seealso \code{\link{gemExogenousPrice}}
#' @examples
#' \donttest{
#' ## the exogenous labor price with product as numeraire.
#' p.labor <- 0.625
#'
#' dst.firm <- node_new("output",
#'   type = "CD",
#'   alpha = 1,  beta = c(0.5, 0.5),
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
#' ge <- sdm2(
#'   A = list(
#'     dst.firm,
#'     dst.landowner,
#'     dst.laborer
#'   ),
#'   B = diag(3),
#'   S0Exg = matrix(c(
#'     NA, NA, NA,
#'     NA, 100, NA,
#'     NA, NA, 100
#'   ), 3, 3, TRUE),
#'   GRExg = 0,
#'   names.commodity = c("prod", "land", "lab"),
#'   names.agent = c("firm", "landowner", "laborer"),
#'   maxIteration = 1,
#'   numberOfPeriods = 200,
#'   depreciationCoef = 0,
#'   numeraire = "prod",
#'   ts = TRUE,
#'   policy = function(time, state, state.history) {
#'     if (time > 1) {
#'       ratio <- state$p[3] / state$p[1] / p.labor
#'       last.labor.supply <- state.history$S[3, 3, time - 1]
#'       state$S[3, 3] <- last.labor.supply * ratio
#'     }
#'
#'     state
#'   }
#' )
#'
#' matplot(ge$ts.p, type = "l")
#' tail(ge$ts.S[3, 3, ])
#' plot(ge$ts.S[3, 3, ], type = "l")
#' }

gemExogenousPrice_EndogenousLaborSupply_3_3 <- function(...) sdm2(...)
