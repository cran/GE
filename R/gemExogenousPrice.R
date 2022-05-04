#' @export
#' @title Some Examples with Exogenous Price (Price Regulation)
#' @aliases gemExogenousPrice
#' @description Some examples with exogenous price (price regulation).
#' When a price regulation policy is imposed in a structural dynamic model,
#' the economy may converge to a fixed point (i.e. a price regulation equilibrium)
#' where the market does not clear.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' dst.firm <- node_new("output",
#'   type = "CD", alpha = 5,
#'   beta = c(0.5, 0.5),
#'   "prod", "lab"
#' )
#'
#' dst.consumer <- node_new("utility",
#'   type = "CD", alpha = 1,
#'   beta = c(0.5, 0.5),
#'   "prod", "lab"
#' )
#'
#' f <- function(pExg = NULL, policy = NULL) {
#'   ge <- sdm2(
#'     A = list(dst.firm, dst.consumer),
#'     B = diag(c(1, 0)),
#'     S0Exg = {
#'       S0Exg <- matrix(NA, 2, 2)
#'       S0Exg[2, 2] <- 100
#'       S0Exg
#'     },
#'     names.commodity = c("prod", "lab"),
#'     names.agent = c("firm", "consumer"),
#'     numeraire = "lab",
#'     maxIteration = 1,
#'     numberOfPeriods = 100,
#'     p0 = c(0.16, 1),
#'     ts = TRUE,
#'     pExg = pExg,
#'     policy = policy
#'   )
#'
#'   print(ge$p)
#'   print(ge$z)
#'   par(mfrow = c(1, 2))
#'   matplot(ge$ts.p, type = "l")
#'   matplot(ge$ts.z, type = "l")
#'   ge
#' }
#'
#' ## No price regulation policy.
#' f()
#'
#' ## Set the market prices to the steady equilibrium prices from the beginning.
#' ## The labor market keeps oversupplied.
#' result <- f(pExg = c(0.16, 1))
#' matplot(result$ts.q, type = "l") # sale rates
#'
#' ## the same as above
#' f(policy = function(state) {
#'   state$p <- c(0.16, 1)
#'   state
#' })
#'
#' ## The price regulation policy is implemented from the 10th period.
#' f(policy = function(time, state) {
#'   if (time >= 10) state$p <- c(0.16, 1)
#'   state
#' })
#'
#' ## The price regulation policy is implemented from the 30th period.
#' f(policy = function(time, state) {
#'   if (time >= 30) state$p <- c(0.16, 1)
#'   state
#' })
#'
#' ## price ceil
#' f(policy = function(time, state) {
#'   if (time >= 30) {
#'     state$p <- state$p / state$p[2]
#'     if (state$p[1] > 0.15) state$p[1] <- 0.15
#'   }
#'   state
#' })
#'
#' ##
#' ge <- f(policy = function(time, state) {
#'   if (time >= 30) state$p <- c(0.17, 1)
#'   state
#' })
#'
#' tail(ge$ts.q)
#' }


gemExogenousPrice <- function(...) sdm2(...)