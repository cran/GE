#' @export
#' @title Some 2-by-2 Time Circle Models with Uncertainty
#' @aliases gemIntertemporal_TimeCircle_Stochastic_2_2
#' @description Some time circle models with uncertainty.
#' In these models, there is a consumer who will live for two periods and has a von Neumann-Morgenstern expected utility function.
#' There is one natural state in the first period,
#' and two natural states in the second period.
#' In the economy, there are two types of commodities: product and labor.
#' In the first period, the economy can borrow a certain amount of product from an external source,
#' such as a bank, and repay it after the economic operation is complete.
#' The amount of product to be repaid is zeta times the amount borrowed. zeta is an exogenous variable.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' #### Assume that the consumer supplies labor only in the first period,
#' ## and the firm produces only in the first period.
#' zeta <- 1.25 # the ratio of repayments to loans
#' dst.firm <- node_new(
#'   "prod2",
#'   type = "CD", alpha = 1,
#'   beta = c(0.5, 0.5),
#'   "prod1", "lab1"
#' )
#'
#' dst.bank <- node_new(
#'   "prod1",
#'   type = "Leontief",
#'   a = c(1, 1) * zeta,
#'   "prod2.1", "prod2.2"
#' )
#'
#' dst.consumer <- node_new(
#'   "util",
#'   type = "CD", alpha = 1,
#'   beta = c(0.5, 0.25, 0.25),
#'   "prod1", "prod2.1", "prod2.2"
#' )
#'
#' ge <- sdm2(
#'   A = c(dst.firm, dst.bank, dst.consumer),
#'   B = matrix(c(
#'     0, 1, 0,
#'     2, 0, 0,
#'     1, 0, 0,
#'     0, 0, 0
#'   ), 4, 3, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA, NA,
#'     NA, NA, NA,
#'     NA, NA, NA,
#'     NA, NA, 100
#'   ), 4, 3, TRUE),
#'   names.commodity = c("prod1", "prod2.1", "prod2.2", "lab1"),
#'   names.agent = c("firm", "bank", "consumer"),
#'   numeraire = "lab1",
#'   policy = makePolicyMeanValue(30),
#'   ts = TRUE
#' )
#'
#' ge$p
#' ge$z
#' addmargins(ge$D, 2)
#' addmargins(ge$S, 2)
#'
#' #### Assume that the consumer supplies labor in both periods and
#' ## firms produce in both periods.
#' zeta <- 1.25 # the ratio of repayments to loans
#' dst.firm1 <- node_new(
#'   "prod2",
#'   type = "CD", alpha = 2,
#'   beta = c(0.5, 0.5),
#'   "lab1", "prod1"
#' )
#'
#' dst.firm2.1 <- node_new(
#'   "prod3.1",
#'   type = "CD", alpha = 2,
#'   beta = c(0.5, 0.5),
#'   "lab2.1", "prod2.1"
#' )
#'
#' dst.firm2.2 <- node_new(
#'   "prod3.2",
#'   type = "CD", alpha = 1,
#'   beta = c(0.5, 0.5),
#'   "lab2.2", "prod2.2"
#' )
#'
#' dst.bank <- node_new(
#'   "prod1",
#'   type = "Leontief",
#'   a = c(1, 1) * zeta,
#'   "prod3.1", "prod3.2"
#' )
#'
#' dst.consumer <- node_new(
#'   "util",
#'   type = "CD", alpha = 1,
#'   beta = c(1 / 3, 1 / 3, 1 / 3),
#'   "prod1", "prod2.1", "prod2.2"
#' )
#'
#' ge <- sdm2(
#'   A = c(
#'     dst.firm1, dst.firm2.1, dst.firm2.2,
#'     dst.bank, dst.consumer
#'   ),
#'   B = matrix(c(
#'     0, 0, 0, 1, 0,
#'     1, 0, 0, 0, 0,
#'     1, 0, 0, 0, 0,
#'     0, 1, 0, 0, 0,
#'     0, 0, 1, 0, 0,
#'     0, 0, 0, 0, 0,
#'     0, 0, 0, 0, 0,
#'     0, 0, 0, 0, 0
#'   ), 8, 5, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA, NA, NA, NA,
#'     NA, NA, NA, NA, NA,
#'     NA, NA, NA, NA, NA,
#'     NA, NA, NA, NA, NA,
#'     NA, NA, NA, NA, NA,
#'     NA, NA, NA, NA, 100,
#'     NA, NA, NA, NA, 100,
#'     NA, NA, NA, NA, 100
#'   ), 8, 5, TRUE),
#'   names.commodity = c(
#'     "prod1", "prod2.1", "prod2.2",
#'     "prod3.1", "prod3.2",
#'     "lab1", "lab2.1", "lab2.2"
#'   ),
#'   names.agent = c(
#'     "firm1", "firm2.1", "firm2.2",
#'     "bank", "consumer"
#'   ),
#'   numeraire = "lab1",
#'   policy = makePolicyMeanValue(30),
#'   ts = TRUE
#' )
#'
#' ge$p
#' ge$z
#' ge$D
#' ge$S
#' ge$DV
#' ge$SV
#' }

gemIntertemporal_TimeCircle_Stochastic_2_2 <- function(...) sdm2(...)
