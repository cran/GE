#' @export
#' @title Some Examples of Market Clearing Paths with Persistent Technological Progress
#' @aliases gemPersistentTechnologicalProgress
#' @description Some examples of market clearing paths with persistent technological progress.
#' From the fifth period, technological progress occurs.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' #### a 2-by-2 example with labor-saving technological progress
#' tpr <- 0.03 # technological progress rate
#'
#' dst.firm <- node_new(
#'   "prod",
#'   type = "SCES",
#'   es = 0.5, alpha = 1,
#'   beta = c(0.5, 0.5),
#'   "prod", "cc1"
#' )
#' node_set(dst.firm, "cc1",
#'          type = "Leontief", a = 1,
#'          "lab"
#' )
#'
#' dst.consumer <- node_new(
#'   "util",
#'   type = "Leontief", a = 1,
#'   "prod"
#' )
#'
#' dstl <- list(dst.firm, dst.consumer)
#'
#' ge <- sdm2(
#'   A = dstl,
#'   B = matrix(c(
#'     1, 0,
#'     0, 0
#'   ), 2, 2, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA,
#'     NA, 100
#'   ), 2, 2, TRUE),
#'   names.commodity = c("prod", "lab"),
#'   names.agent = c("firm", "consumer"),
#'   numeraire = "prod",
#'   ts = TRUE,
#'   policy = list(
#'     function(time, A) {
#'       if (time >= 5) {
#'         node_set(A[[1]], "cc1",
#'                  a = (1 + tpr)^-(time - 4)
#'         )
#'       }
#'     },
#'     policyMarketClearingPrice
#'   ),
#'   numberOfPeriods = 40,
#'   maxIteration = 1,
#'   z0 = c(200, 100),
#'   p0 = c(1, 1)
#' )
#'
#' matplot(growth_rate(ge$ts.z), type = "o", pch = 20)
#' matplot(growth_rate(ge$ts.p), type = "o", pch = 20)
#'
#' #### a 3-by-3 example with labor-saving technological progress
#' tpr <- 0.03 # technological progress rate
#'
#' dst.manu <- node_new("manu",
#'                      type = "SCES", es = 0.5, alpha = 1,
#'                      beta = c(0.6, 0.4),
#'                      "manu", "cc1"
#' )
#' node_set(dst.manu, "cc1",
#'          type = "Leontief", a = 1,
#'          "lab"
#' )
#'
#' dst.serv <- node_new("serv",
#'                      type = "SCES", es = 0.5, alpha = 1,
#'                      beta = c(0.4, 0.6),
#'                      "manu", "lab"
#' )
#'
#' dst.consumer <- node_new("util",
#'                          type = "SCES", es = 0.5, alpha = 1,
#'                          beta = c(0.4, 0.6),
#'                          "manu", "serv"
#' )
#'
#' dstl <- list(dst.manu, dst.serv, dst.consumer)
#'
#' ge <- sdm2(
#'   A = dstl,
#'   B = matrix(c(
#'     1, 0, 0,
#'     0, 1, 0,
#'     0, 0, 0
#'   ), 3, 3, TRUE),
#'   S0Exg = {
#'     S0Exg <- matrix(NA, 3, 3)
#'     S0Exg[3, 3] <- 100
#'     S0Exg
#'   },
#'   names.commodity = c("manu", "serv", "lab"),
#'   names.agent = c("manu", "serv", "consumer"),
#'   numeraire = c("manu"),
#'   ts = TRUE,
#'   policy = list(
#'     function(time, A) {
#'       if (time >= 5) {
#'         node_set(A[[1]], "cc1",
#'                  a = (1 + tpr)^-(time - 4)
#'         )
#'       }
#'     },
#'     policyMarketClearingPrice
#'   ),
#'   numberOfPeriods = 40,
#'   maxIteration = 1,
#'   z0 = c(160, 60, 100),
#'   p0 = c(1, 1, 1)
#' )
#'
#' matplot(ge$ts.z, type = "o", pch = 20)
#' matplot(growth_rate(ge$ts.z), type = "o", pch = 20)
#' matplot(growth_rate(ge$ts.p), type = "o", pch = 20)
#' }

gemPersistentTechnologicalProgress <- function(...) sdm2(...)
