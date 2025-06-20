#' @export
#' @title Some Examples of Temporary Equilibrium Paths
#' @aliases gemTemporaryEquilibriumPath
#' @description Some examples of temporary equilibrium paths.
#' The temporary equilibrium path consists of a series of temporary equilibria.
#' Each temporary equilibrium achieves market clearing and may involve futures markets in addition to spot markets.
#' A spot equilibrium path is a temporary equilibrium path that only involves spot market transactions.
#' @param ... arguments to be passed to the function sdm2.
#' @references Grandmont, J.M. (1977). Temporary General Equilibrium Theory. Econometrica 45, 535-572.
#' @seealso {
#' \code{\link{policyMarketClearingPrice}}
#' }
#' @examples
#' \donttest{
#' #### A pure exchange economy.
#' ## Consumers 1 and 2 each supply 50 units of payoff each period.
#' ## Consumers have limited foresight into the future and always expect
#' ## that the supply of consumer 1 will be 50 and the supply of
#' ## consumer 2 will be 40 in the next period.
#' dst.consumer1 <- node_new("util",
#'   type = "SCES", es = 2,
#'   alpha = 1, beta = c(0.5, 0.5),
#'   "payoff1", "payoff2"
#' )
#'
#' dst.consumer2 <- node_new("util",
#'   type = "SCES", es = 1,
#'   alpha = 1, beta = c(0.5, 0.5),
#'   "payoff1", "payoff2"
#' )
#'
#' result <- list()
#' for (time in 1:20) {
#'   if (time == 1) {
#'     S0Exg <- matrix(c(
#'       50, 50,
#'       50, 40
#'     ), 2, 2, TRUE)
#'   } else {
#'     S0Exg <- rbind(ge$D[2, ] + c(0, 10), c(50, 40))
#'   }
#'
#'   ge <- sdm2(
#'     A = list(dst.consumer1, dst.consumer2),
#'     B = matrix(0, 2, 2),
#'     S0Exg = S0Exg,
#'     names.commodity = c("payoff1", "payoff2"),
#'     names.agent = c("consumer1", "consumer2"),
#'     numeraire = "payoff1"
#'   )
#'
#'   result[[time]] <- ge
#' }
#'
#' sapply(result, function(x) x$p)
#' sapply(result, function(x) x$z)
#' # lapply(result, function(x) x$D)
#' # lapply(result, function(x) x$S)
#'
#' #### An economy with production.
#' dst.consumer <- node_new("util",
#'   type = "CD",
#'   alpha = 1,
#'   beta = c(1 / 3, 2 / 3), # beta = c(1/2, 1/2)
#'   "prod1", "prod2"
#' )
#'
#' dst.firm <- node_new("prod2",
#'   type = "CD", alpha = 2, beta = c(0.5, 0.5),
#'   "prod1", "lab"
#' )
#'
#' result <- list()
#' for (time in 1:20) {
#'   if (time == 1) {
#'     S0Exg <- matrix(c(
#'       10, 0,
#'       0, NA,
#'       100, 0
#'     ), 3, 2, TRUE)
#'   } else {
#'     S0Exg <- matrix(c(
#'       ge$D[2, 1], 0,
#'       0, NA,
#'       100, 0
#'     ), 3, 2, TRUE)
#'   }
#'
#'   ge <- sdm2(
#'     A = list(dst.consumer, dst.firm),
#'     B = matrix(c(
#'       0, 0,
#'       0, 1,
#'       0, 0
#'     ), 3, 2, TRUE),
#'     S0Exg = S0Exg,
#'     names.commodity = c("prod1", "prod2", "lab"),
#'     names.agent = c("consumer", "firm"),
#'     numeraire = "prod1"
#'   )
#'
#'   result[[time]] <- ge
#' }
#'
#'
#' sapply(result, function(x) x$p)
#' sapply(result, function(x) x$z)
#' # lapply(result, function(x) x$D)
#' # lapply(result, function(x) x$S)
#'
#' ##
#' result <- list()
#' last.output <- 10
#' for (time in 1:30) {
#'   if (time == 1) {
#'     S0Exg <- matrix(c(
#'       10, 0,
#'       0, last.output,
#'       100, 0
#'     ), 3, 2, TRUE)
#'   } else {
#'     S0Exg <- rbind(
#'       c(ge$D[2, 1], max(ge$z[2] - last.output, 0)),
#'       c(0, last.output),
#'       c(100, 0)
#'     )
#'
#'     last.output <- ge$z[2]
#'   }
#'
#'   ge <- sdm2(
#'     A = list(dst.consumer, dst.firm),
#'     B = matrix(c(
#'       0, 0,
#'       0, 1,
#'       0, 0
#'     ), 3, 2, TRUE),
#'     # B = matrix(0, 3, 2),
#'     S0Exg = S0Exg,
#'     names.commodity = c("prod1", "prod2", "lab"),
#'     names.agent = c("consumer", "firm"),
#'     numeraire = "prod1"
#'   )
#'
#'   result[[time]] <- ge
#' }

#' sapply(result, function(x) x$p)
#' sapply(result, function(x) x$z)
#' # lapply(result, function(x) x$D)
#' # lapply(result, function(x) x$S)
#' }

gemTemporaryEquilibriumPath <- function(...) sdm2(...)
