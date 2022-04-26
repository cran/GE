#' @export
#' @title Some Examples of Market Clearing Paths Involving Land and Labor
#' @aliases gemLand_Labor
#' @description Some examples of market clearing paths involving land and labor.
#' The labor supply may increase from the fifth period.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' #### a 3-by-3 economy
#' f <- function(GRLabor = 0,
#'               es.land.labor = 1) {
#'   dst.firm <- node_new("output",
#'                        type = "SCES",
#'                        es = es.land.labor, alpha = 1,
#'                        beta = c(0.5, 0.5),
#'                        "land", "lab"
#'   )
#'
#'   dst.land.owner <- node_new(
#'     "util",
#'     type = "Leontief", a = 1,
#'     "prod"
#'   )
#'
#'   dst.laborer <- Clone(dst.land.owner)
#'
#'   dstl <- list(dst.firm, dst.land.owner, dst.laborer)
#'
#'   ge <- sdm2(
#'     A = dstl,
#'     B = diag(c(1, 0, 0)),
#'     S0Exg = matrix(c(
#'       NA, NA, NA,
#'       NA, 100, NA,
#'       NA, NA, 100
#'     ), 3, 3, TRUE),
#'     names.commodity = c("prod", "land", "lab"),
#'     names.agent = c("firm", "land.owner", "laborer"),
#'     maxIteration = 1,
#'     numberOfPeriods = 30,
#'     numeraire = "lab",
#'     ts = TRUE,
#'     policy = list(
#'       function(time, state) {
#'         if (time >= 5) {
#'           state$S[3, 3] <- 100 * (1 + GRLabor)^(time - 4)
#'         }
#'         state
#'       },
#'       policyMarketClearingPrice
#'     ),
#'     z0 = c(200, 100, 100),
#'     p0 = c(1, 1, 1)
#'   )
#'
#'   par(mfrow = c(1, 2))
#'   matplot(growth_rate(ge$ts.p), type = "o", pch = 20)
#'   matplot(growth_rate(ge$ts.z), type = "o", pch = 20)
#'
#'   ge
#' }
#'
#' ge <- f()
#' ge$p
#' ge$z
#'
#'
#' f(GRLabor = 0.03)
#' f(GRLabor = -0.03)
#' f(GRLabor = 0.03, es.land.labor = 0.5)
#' f(GRLabor = 0.03, es.land.labor = 1.5)
#'
#' #### a 4-by-3 economy
#' GRLabor <- 0.03
#'
#' dst.agri <- node_new("agri",
#'                      type = "SCES", es = 0.5, alpha = 1,
#'                      beta = c(0.75, 0.25),
#'                      "land", "lab"
#' )
#'
#' dst.manu <- node_new("manu",
#'                      type = "SCES", es = 0.5, alpha = 1,
#'                      beta = c(0.25, 0.75),
#'                      "land", "lab"
#' )
#'
#' dst.consumer <- node_new("util",
#'                          type = "SCES", es = 0.5, alpha = 1,
#'                          beta = c(0.5, 0.5),
#'                          "agri", "manu"
#' )
#'
#' dstl <- list(dst.agri, dst.manu, dst.consumer)
#'
#' ge <- sdm2(
#'   A = dstl,
#'   B = matrix(c(
#'     1, 0, 0,
#'     0, 1, 0,
#'     0, 0, 0,
#'     0, 0, 0
#'   ), 4, 3, TRUE),
#'   S0Exg = {
#'     S0Exg <- matrix(NA, 4, 3)
#'     S0Exg[3:4, 3] <- 100
#'     S0Exg
#'   },
#'   names.commodity = c("agri", "manu", "land", "lab"),
#'   names.agent = c("agri", "manu", "consumer"),
#'   numeraire = c("manu"),
#'   ts = TRUE,
#'   policy = list(
#'     function(time, state) {
#'       if (time >= 5) {
#'         state$S[4, 3] <- 100 * (1 + GRLabor)^(time - 4)
#'       }
#'       state
#'     },
#'     policyMarketClearingPrice
#'   ),
#'   numberOfPeriods = 40,
#'   maxIteration = 1,
#'   z0 = c(100, 100, 200),
#'   p0 = c(1, 1, 1, 1)
#' )
#'
#' matplot(ge$ts.z, type = "o", pch = 20)
#' matplot(growth_rate(ge$ts.z), type = "o", pch = 20)
#' matplot(growth_rate(ge$ts.p), type = "o", pch = 20)
#' }

gemLand_Labor <- function(...) sdm2(...)
