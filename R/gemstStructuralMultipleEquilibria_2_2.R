#' @export
#' @title Structural Multiple Equilibria and Structural Transition Policy
#' @aliases gemstStructuralMultipleEquilibria_2_2
#' @description Some examples of structural multiple equilibria and structural transition policy.
#' In these examples it is assumed that the firm has a structural production function (see Li, Fu, 2020), e.g.
#'
#' structural_function(last.output, c(0.3, 0.4), 1, 2) * x1^0.35 * x2^0.65
#'
#' wherein last.output is the output of the firm in the previous period.
#' @param ... arguments to be passed to the function sdm2.
#' @references Li Wu, Fu Caihui (2020) A Simulation Study on the Economic Structure Transition Policy. Journal of Shanghai University (Social Sciences). 37(2), pp: 33-45. (In Chinese)
#' @examples
#' \donttest{
#' dst.firm <- node_new("output",
#'                      type = "CD", alpha = 1,
#'                      beta = c(0.35, 0.65),
#'                      "prod", "lab"
#' )
#'
#' dst.consumer <- node_new("utility",
#'                          type = "CD", alpha = 1,
#'                          beta = c(0.4, 0.6),
#'                          "prod", "lab"
#' )
#'
#' policy.technology <- function(time, state, A) {
#'   # state$last.z[1] is the previous output.
#'   A[[1]]$alpha <- structural_function(state$last.z[1], c(0.3, 0.4), 1, 2)
#' }
#'
#' policy.tax <- function(time, state) {
#'   if ((time >= 15) && state$last.z < 0.4) {
#'     state$S[2, 2] <- 0.8
#'     state$S[2, 1] <- 0.2
#'   } else {
#'     state$S[2, 2] <- 1
#'     state$S[2, 1] <- 0
#'   }
#'
#'   state
#' }
#'
#' f <- function(z0 = c(0.1, 1),
#'               policy = list(
#'                 policy.technology,
#'                 policyMarketClearingPrice
#'               )) {
#'   ge <- sdm2(
#'     A = list(dst.firm, dst.consumer),
#'     B = matrix(c(
#'       1, 0,
#'       0, 1
#'     ), 2, 2, TRUE),
#'     S0Exg = matrix(c(
#'       NA, NA,
#'       NA, 1
#'     ), 2, 2, TRUE),
#'     names.commodity = c("prod", "lab"),
#'     names.agent = c("firm", "consumer"),
#'     numeraire = "lab",
#'     z0 = z0,
#'     p0 = c(1, 1),
#'     maxIteration = 1,
#'     numberOfPeriods = 30,
#'     policy = policy,
#'     ts = TRUE
#'   )
#'
#'   matplot(ge$ts.z, type = "b", pch = 20)
#'   ge
#' }
#'
#' geLow <- f()
#' geLow$z
#'
#' geHigh <- f(z0 = c(0.5, 1))
#' geHigh$z
#'
#' f(policy = list(
#'   policy.technology,
#'   policy.tax,
#'   policyMarketClearingPrice
#' ))
#'
#' #### structural transition: disequilibrium path and
#' ## market-clearing path (instantaneous equilibrium path)
#' dst.firm <- node_new("output",
#'                      type = "CD", alpha = 5,
#'                      beta = c(0.5, 0.5),
#'                      "prod", "lab"
#' )
#'
#' dst.consumer <- node_new("utility",
#'                          type = "Leontief", a = 1,
#'                          "prod"
#' )
#'
#' policy.technology <- function(time, state, A) {
#'   # state$last.z[1] is last output.
#'   A[[1]]$alpha <- structural_function(state$last.z[1], c(15, 20), 5, 15)
#'   return(NULL)
#' }
#'
#' policy.tax <- function(time, state) {
#'   if ((time >= 100) && (time <= 109)) {
#'     state$S[2, 2] <- 0.6
#'     state$S[2, 1] <- 0.4
#'   } else {
#'     state$S[2, 2] <- 1
#'     state$S[2, 1] <- 0
#'   }
#'
#'   state
#' }
#'
#' f <- function(z0 = c(1, 1),
#'               p0 = c(1, 1),
#'               policy = policy.technology) {
#'   ge <- sdm2(
#'     A = list(dst.firm, dst.consumer),
#'     B = matrix(c(
#'       1, 0,
#'       0, 1
#'     ), 2, 2, TRUE),
#'     S0Exg = matrix(c(
#'       NA, NA,
#'       NA, 1
#'     ), 2, 2, TRUE),
#'     names.commodity = c("prod", "lab"),
#'     names.agent = c("firm", "consumer"),
#'     numeraire = "lab",
#'     z0 = z0,
#'     p0 = p0,
#'     maxIteration = 1,
#'     numberOfPeriods = 200,
#'     policy = policy,
#'     priceAdjustmentVelocity = 0.4,
#'     ts = TRUE
#'   )
#'
#'   matplot(ge$ts.z, type = "l", pch = 20)
#'   ge
#' }
#'
#' geLow <- f()
#' geLow$z
#'
#' geHigh <- f(z0 = c(18, 1), p0 = c(1, 9))
#' geHigh$z
#'
#' ## structural transition: disequilibrium path
#' f(policy = list(
#'   policy.technology,
#'   policy.tax
#' ))$z
#'
#'
#' ## structural transition: market-clearing path
#' f(policy = list(
#'   policy.technology,
#'   policy.tax,
#'   policyMarketClearingPrice
#' ))$z
#'
#' ## structural transition through foreign aid
#' policy.foreign_aid <- function(time, state) {
#'   if ((time >= 100) && (time <= 109)) {
#'     state$S[2, 2] <- 3
#'   } else {
#'     state$S[2, 2] <- 1
#'   }
#'
#'   state
#' }
#'
#' f(policy = list(
#'   function(time, state, A) { # technology policy
#'     # state$last.z[1] is last output.
#'     A[[1]]$alpha <- structural_function(state$last.z[1], c(30, 35), 5, 15)
#'   },
#'   policy.foreign_aid
#' ))
#' }

gemstStructuralMultipleEquilibria_2_2 <- function(...) sdm2(...)

