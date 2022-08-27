#' @export
#' @title An Intertemporal Stochastic Model with a Consumer and Some Banks
#' @aliases gemIntertemporalStochastic_Bank_3Periods
#' @description An intertemporal stochastic model with a consumer and some banks.
#' In the model the consumer will live for three periods.
#' There is one natural state in the first period,
#' and two natural states in the second and third period.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' dst.bank1a <- node_new(
#'   "payoff2.1&2.2",
#'   type = "Leontief", a = 1,
#'   "payoff1"
#' )
#'
#' dst.bank1b <- node_new(
#'   "payoff3.1&3.2",
#'   type = "Leontief", a = 1,
#'   "payoff1"
#' )
#'
#' dst.bank2.1 <- node_new(
#'   "payoff3.1",
#'   type = "Leontief", a = 1,
#'   "payoff2.1"
#' )
#'
#' dst.bank2.2 <- node_new(
#'   "payoff3.2",
#'   type = "Leontief", a = 1,
#'   "payoff2.2"
#' )
#'
#' dst.consumer <- node_new(
#'   "util",
#'   type = "CD", alpha = 1, beta = c(1/3, 1/6, 1/6, 1/6, 1/6),
#'   "payoff1", "payoff2.1", "payoff2.2", "payoff3.1", "payoff3.2"
#' )
#'
#' ge <- sdm2(
#'   A = list(dst.bank1a, dst.bank1b,dst.bank2.1,dst.bank2.2,dst.consumer),
#'   B = matrix(c(
#'     0,   0,0, 0,0,
#'     1.1, 0,0, 0,0,
#'     1.1, 0,0, 0, 0,
#'     0,   1.5, 1.1, 0,0,
#'     0,   1.5, 0, 1.1,0
#'   ), 5, 5, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA, NA, NA, 1,
#'     NA, NA, NA, NA, 1,
#'     NA, NA, NA, NA, 0,
#'     NA, NA, NA, NA, 0,
#'     NA, NA, NA, NA, 0
#'   ), 5, 5, TRUE),
#'   names.commodity = c("payoff1", "payoff2.1", "payoff2.2", "payoff3.1", "payoff3.2"),
#'   names.agent = c("bank1a","bank1b","bank2.1","bank2.2", "consumer"),
#'   numeraire = "payoff1"
#' )
#'
#' ge$p
#' round(ge$D, 4)
#' round(ge$S, 4)
#'
#' #### the general equilibrium in the first natural state in period 2
#' dst.bank2.1 <- node_new(
#'   "payoff3.1",
#'   type = "Leontief", a = 1,
#'   "payoff2.1"
#' )
#'
#' dst.consumer <- node_new(
#'   "util",
#'   type = "CD", alpha = 1, beta = c(0.5, 0.5),
#'   "payoff2.1", "payoff3.1"
#' )
#'
#' ge2.1 <- sdm2(
#'   A = list(dst.bank2.1,
#'            dst.consumer),
#'   B = matrix(c(
#'     0,   0,
#'     1.1, 0
#'   ), 2, 2, TRUE),
#'   S0Exg = matrix(c(
#'     NA, 1.3084,
#'     NA, 0.4599
#'   ), 2, 2, TRUE),
#'   names.commodity = c("payoff2.1", "payoff3.1"),
#'   names.agent = c("bank2.1", "consumer"),
#'   numeraire = "payoff2.1"
#' )
#'
#' ge2.1$p
#' round(ge2.1$D, 4)
#' round(ge2.1$S, 4)
#'
#' ## the general equilibrium in an unanticipated natural state in period 2
#' ge2.3 <- sdm2(
#'   A = list(dst.bank2.1,
#'            dst.consumer),
#'   B = matrix(c(
#'     0,   0,
#'     1.1, 0
#'   ), 2, 2, TRUE),
#'   S0Exg = matrix(c(
#'     NA, 0.4,
#'     NA, 0.4599
#'   ), 2, 2, TRUE),
#'   names.commodity = c("payoff2.1", "payoff3.1"),
#'   names.agent = c("bank2.1", "consumer"),
#'   numeraire = "payoff2.1"
#' )
#'
#' ge2.3$p
#' round(ge2.3$D, 4)
#' round(ge2.3$S, 4)
#' }

gemIntertemporalStochastic_Bank_3Periods <- function(...) sdm2(...)
