#' @export
#' @title Some Examples with Exogenous Price (Price Control)
#' @aliases gemExogenousPrice
#' @description Some examples with exogenous price (i.e. price control, price regulation).
#' When a price control policy is imposed in a structural dynamic model,
#' the economy may converge to a steady state where the market does not clear.
#' @param ... arguments to be passed to the function sdm2.
#' @references LI Wu (2019, ISBN: 9787521804225) General Equilibrium and Structural Dynamics: Perspectives of New Structural Economics. Beijing: Economic Science Press. (In Chinese)
#' @seealso \code{\link{gemExogenousPrice_EndogenousLaborSupply_3_3}}
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
#'   pcss <- sdm2(
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
#'   print(pcss$p)
#'   print(pcss$z)
#'   par(mfrow = c(1, 2))
#'   matplot(pcss$ts.p, type = "l")
#'   matplot(pcss$ts.z, type = "l")
#'   invisible(pcss)
#' }
#'
#' ## No price control policy.
#' f()
#'
#' ## Set the market prices to the steady-state equilibrium prices from the beginning.
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
#' ## The price control policy is implemented from the 10th period.
#' f(policy = function(time, state) {
#'   if (time >= 10) state$p <- c(0.16, 1)
#'   state
#' })
#'
#' ## The price control policy is implemented from the 30th period.
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
#' pcss <- f(policy = function(time, state) {
#'   if (time >= 30) state$p <- c(0.17, 1)
#'   state
#' })
#'
#' tail(pcss$ts.q)
#'
#' #### another 2-by-2 example.
#' f <- function(GRExg = 0, pExg = c(2, 1)) {
#'   pcss <- sdm(
#'     A = matrix(c(
#'       0, 1,
#'       1, 0
#'     ), 2, 2, TRUE),
#'     B = matrix(c(
#'       1, 0,
#'       0, 0
#'     ), 2, 2, TRUE),
#'     S0Exg = matrix(c(
#'       NA, NA,
#'       NA, 100
#'     ), 2, 2, TRUE),
#'     GRExg = GRExg,
#'     pExg = pExg,
#'     maxIteration = 1,
#'     numberOfPeriods = 300,
#'     depreciationCoef = 0,
#'     z0 = c(100, 0),
#'     ts = TRUE
#'   )
#'   matplot(pcss$ts.z, type = "l")
#'   print("pcss$z:")
#'   pcss$z
#'   print("tail(pcss$ts.q, 3)")
#'   print(tail(pcss$ts.q, 3))
#'   invisible(pcss)
#' }
#'
#' f()
#' f(GRExg = 0.01)
#' f(pExg = c(1, 2))
#'
#' #### Example 9.5 in Li (2019).
#' f <- function(GRExg = 0, pExg = c(1, NA, 0.625)) {
#'   pcss <- sdm(
#'     A = function(state) {
#'       alpha <- rbind(1, 1, 1)
#'       Beta <- matrix(c(
#'         0, 1, 1,
#'         0.5, 0, 0,
#'         0.5, 0, 0
#'       ), 3, 3, TRUE)
#'       CD_A(alpha, Beta, state$p)
#'     },
#'     B = diag(c(1, 0, 0)),
#'     S0Exg = matrix(c(
#'       NA, NA, NA,
#'       NA, 100, NA,
#'       NA, NA, 100
#'     ), 3, 3, TRUE),
#'     GRExg = GRExg,
#'     pExg = pExg,
#'     maxIteration = 1,
#'     numberOfPeriods = 300,
#'     depreciationCoef = 0,
#'     z0 = c(100, 0, 0),
#'     ts = TRUE
#'   )
#'   matplot(pcss$ts.z, type = "l")
#'   print("pcss$z:")
#'   pcss$z
#'   print("tail(pcss$ts.q, 3)")
#'   print(tail(pcss$ts.q, 3))
#'   invisible(pcss)
#' }
#'
#' f()
#' f(GRExg = 0.01)
#' f(pExg = c(1, 0.25, 0.25))
#' f(pExg = c(1, 0.2, 0.25))
#' }


gemExogenousPrice <- function(...) sdm2(...)
