#' @export
#' @title An Intertemporal Stochastic Model with a Consumer and a Bank
#' @aliases gemIntertemporalStochastic_Bank_TwoPeriods
#' @description An intertemporal stochastic model with a consumer and a bank.
#' In the model the consumer will live for two periods.
#' There is one natural state in the first period,
#' and two natural states in the second period.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' #### a savings bank
#' Ra <- 1.2 # the interest rate coefficient in the first natural state in the future
#' Rb <- 1.1 # the interest rate coefficient in the second natural state in the future
#'
#' dst.bank <- node_new(
#'   "output",
#'   type = "Leontief", a = 1,
#'   "payoff1"
#' )
#'
#' dst.consumer <- node_new(
#'   "util",
#'   type = "CD", alpha = 1, beta = c(1 / 2, 1 / 6, 1 / 3),
#'   "payoff1", "payoff2", "payoff3"
#' )
#'
#' ge <- sdm2(
#'   A = list(dst.bank, dst.consumer),
#'   B = matrix(c(
#'     0, 0,
#'     Ra, 0,
#'     Rb, 0
#'   ), 3, 2, TRUE),
#'   S0Exg = matrix(c(
#'     NA, 1,
#'     NA, 0,
#'     NA, 2
#'   ), 3, 2, TRUE),
#'   names.commodity = c("payoff1", "payoff2", "payoff3"),
#'   names.agent = c("bank", "consumer"),
#'   numeraire = "payoff1",
#' )
#'
#' ge$p
#' addmargins(ge$D, 2)
#' addmargins(ge$S, 2)
#'
#' #### a credit bank
#' Ra <- 1.2
#' Rb <- 1.1
#'
#' dst.bank <- node_new(
#'   "payoff1",
#'   type = "Leontief", a = c(Ra, Rb),
#'   "payoff2", "payoff3"
#' )
#'
#' dst.consumer <- node_new(
#'   "util",
#'   type = "CD", alpha = 1, beta = c(1 / 2, 1 / 6, 1 / 3),
#'   "payoff1", "payoff2", "payoff3"
#' )
#'
#' ge <- sdm2(
#'   A = list(dst.bank, dst.consumer),
#'   B = matrix(c(
#'     1, 0,
#'     0, 0,
#'     0, 0
#'   ), 3, 2, TRUE),
#'   S0Exg = matrix(c(
#'     NA, 0,
#'     NA, 1,
#'     NA, 2
#'   ), 3, 2, TRUE),
#'   names.commodity = c("payoff1", "payoff2", "payoff3"),
#'   names.agent = c("bank", "consumer"),
#'   numeraire = "payoff1"
#' )
#'
#' ge$p
#' addmargins(ge$D, 2)
#' addmargins(ge$S, 2)
#' }

gemIntertemporalStochastic_Bank_TwoPeriods <- function(...) sdm2(...)
