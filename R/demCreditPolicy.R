#' @export
#' @title A Disequilibrium Model with Credit
#' @aliases demCreditPolicy
#' @description This is an example to illustrate that credit policies may lead to business cycles.
#' When the firm's profit rate is high, the laborer lends labor or labor income to the firm; when the firm's profit rate is low, the firm repays the loan with products.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' dst.firm <- node_new("output",
#'   type = "CD", alpha = 1.2,
#'   beta = c(0.5, 0.5),
#'   "prod", "lab"
#' )
#'
#' dst.consumer <- node_new("utility",
#'   type = "Leontief", a = 1,
#'   "prod"
#' )
#'
#' f <- function(policy = NULL) {
#'   ge <- sdm2(
#'     A = list(dst.firm, dst.consumer),
#'     B = matrix(c(
#'       1, 0,
#'       0, 1
#'     ), 2, 2, TRUE),
#'     S0Exg = matrix(c(
#'       NA, NA,
#'       NA, 100
#'     ), 2, 2, TRUE),
#'     names.commodity = c("prod", "lab"),
#'     names.agent = c("firm", "consumer"),
#'     ts = TRUE,
#'     policy = policy,
#'     numberOfPeriods = 200,
#'     maxIteration = 1,
#'     priceAdjustmentVelocity = 0.05
#'   )
#'
#'   matplot(ge$ts.z, type = "o", pch = 20)
#'   ge
#' }
#'
#' ## no credit policy
#' f()
#'
#' ## credit policy
#' policy.credit <- function(time, state) {
#'   profit.rate <- state$p[1] / sum(state$last.A[, 1] * state$p) - 1
#'
#'   if (profit.rate > 0.01) {
#'     state$S[2, 2] <- 50
#'     state$S[2, 1] <- 50
#'   } else if (profit.rate < -0.01) {
#'     state$S[1, 2] <- state$S[1, 1] * 0.5
#'     state$S[1, 1] <- state$S[1, 1] * 0.5
#'   } else {
#'     state$S[2, 2] <- 100
#'     state$S[2, 1] <- 0
#'   }
#'
#'   state
#' }
#'
#' f(policy = policy.credit)
#'
#' }

demCreditPolicy <- function(...) sdm2(...)
