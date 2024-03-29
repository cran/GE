#' @export
#' @title Some Examples of Balanced Growth Paths
#' @aliases gemBalancedGrowthPath
#' @description Some examples of spot market clearing paths (alias instantaneous equilibrium paths) which converge to balanced growth paths.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' #### an example with a firm and a laborer
#' dst.firm <- node_new(
#'   "prod",
#'   type = "CD", alpha = 5, beta = c(0.5, 0.5),
#'   "prod", "lab"
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
#'     NA, 1
#'   ), 2, 2, TRUE),
#'   names.commodity = c("prod", "lab"),
#'   names.agent = c("firm", "consumer"),
#'   numeraire = "lab",
#'   z0 = c(1, 1),
#'   ts = TRUE,
#'   policy = policyMarketClearingPrice,
#'   numberOfPeriods = 40,
#'   maxIteration = 1,
#'   GRExg = 0.03
#' )
#'
#' matplot(ge$ts.z, type = "o", pch = 20)
#' matplot(growth_rate(ge$ts.z), type = "o", pch = 20)
#'
#' #### an example with two firms and a laborer
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
#'   type = "Leontief", a = 1,
#'   "corn"
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
#'   policy = policyMarketClearingPrice,
#'   numberOfPeriods = 30,
#'   maxIteration = 1,
#'   GRExg = 0.03
#' )
#'
#' matplot(ge$ts.z, type = "o", pch = 20)
#' matplot(growth_rate(ge$ts.z), type = "o", pch = 20)
#'
#' #### another example with two firms and a laborer
#' dst.manu <- node_new("manu",
#'                      type = "SCES", es = 1, alpha = 1,
#'                      beta = c(0.6, 0.4),
#'                      "manu", "lab"
#' )
#'
#' dst.serv <- node_new("serv",
#'                      type = "SCES", es = 1, alpha = 1,
#'                      beta = c(0.4, 0.6),
#'                      "manu", "lab"
#' )
#'
#' dst.consumer <- node_new("util",
#'                          type = "SCES", es = 1, alpha = 1,
#'                          beta = c(0.4, 0.6),
#'                          "manu", "serv"
#' )
#'
#' dstl <- list(dst.manu, dst.serv, dst.consumer)
#'
#' S0Exg <- matrix(NA, 3, 3)
#' S0Exg[3, 3] <- 100
#'
#' ge <- sdm2(
#'   A = dstl,
#'   B = matrix(c(
#'     1, 0, 0,
#'     0, 1, 0,
#'     0, 0, 0
#'   ), 3, 3, TRUE),
#'   S0Exg = S0Exg,
#'   names.commodity = c("manu", "serv", "lab"),
#'   names.agent = c("manu", "serv", "consumer"),
#'   numeraire = c("manu"),
#'   ts = TRUE,
#'   policy = list(
#'     function(time, state) {
#'       if (time >= 5) {
#'         state$S[3, 3] <- 100 * 1.03^(time - 4)
#'       }
#'       state
#'     },
#'     policyMarketClearingPrice
#'   ),
#'   numberOfPeriods = 20,
#'   maxIteration = 1,
#'   z0 = c(160, 60, 100),
#'   p0 = c(1, 1, 1)
#' )
#'
#' ge$p
#' ge$D
#' ge$S
#'
#' matplot(ge$ts.z, type = "o", pch = 20)
#' matplot(growth_rate(ge$ts.z), type = "o", pch = 20)
#' }

gemBalancedGrowthPath <- function(...) sdm2(...)
