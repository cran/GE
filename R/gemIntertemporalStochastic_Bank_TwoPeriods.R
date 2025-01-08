#' @export
#' @title An Intertemporal Stochastic Model with a Consumer and a Bank
#' @aliases gemIntertemporalStochastic_Bank_TwoPeriods
#' @description An intertemporal stochastic model with a consumer and a bank.
#' In this model the consumer will live for two periods.
#' There is one natural state in the first period, and two natural states in the second period.
#'
#' The consumer has an intertemporal stochastic utility function of the Cobb-Douglas (CD) type,
#' \eqn{x_1^{1/2} x_2^{1/6} x_3^{1/3}}, where \eqn{x_1}, \eqn{x_2}, and \eqn{x_3} represent the payoffs in three different states of nature, respectively.
#' The ratio of the share coefficients for the two future states of nature is equal to the ratio of their corresponding probabilities.
#' The share coefficient for the present is the same as that for the future.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' #### (A) A savings bank.
#' Ra <- 1.2 # the interest rate coefficient in the first natural state in the future
#' Rb <- 1.1 # the interest rate coefficient in the second natural state in the future
#'
#' # When the savings bank invests one unit of payoff 1, it can
#' # produce Ra units of payoff 2 and Rb units of payoff 3.
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
#' unname(Ra * ge$p[2] + Rb * ge$p[3])
#' # The amount of savings in equilibrium is 0.2995.
#' addmargins(ge$D, 2)
#' addmargins(ge$S, 2)
#'
#' ## Solve with the optimization method and Rsolnp package.
#' # library(Rsolnp)
#' # Ra <- 1.2
#' # Rb <- 1.1
#' # payoff <- c(1, 0, 2)
#' # # The loss function is used to calculate the loss (i.e.,
#' # # the negative utility) for a given amount of savings.
#' # loss <- function(savings) {
#' #   utility <- prod(c(payoff[1] - savings, payoff[2] +
#' #                       Ra * savings, payoff[3] + Rb * savings)^wt)
#' #   return(-utility)
#' # }
#' #
#' # result <- solnp(0.5, loss, LB = 0, UB = 1)
#' # result$pars
#' #
#' # x <- rbind(payoff[1] - result$pars, payoff[2] + Ra * result$pars, payoff[3] + Rb * result$pars)
#' # mu <- marginal_utility(x, diag(3), uf = function(x) prod(x^wt))
#' # mu / mu[1]
#'
#' #### (B) A lending bank.
#' Ra <- 1.2
#' Rb <- 1.1
#'
#' # To produce one unit of payoff 1, the lending bank needs
#' # to invest Ra units of payoff 2 and Rb units of payoff 3.
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
#' unname(Ra * ge$p[2] + Rb * ge$p[3])
#' addmargins(ge$D, 2)
#' # In equilibrium, the bank lends out 0.5645 units of payoff in period 1.
#' addmargins(ge$S, 2)
#'
#' ## Solve with the optimization method and Rsolnp package.
#' # library(Rsolnp)
#' # Ra <- 1.2
#' # Rb <- 1.1
#' # payoff <- c(0, 1, 2)
#' # loss <- function(savings) {
#' #   utility <- prod(c(payoff[1] - savings, payoff[2] +
#' #                       Ra * savings, payoff[3] + Rb * savings)^wt)
#' #   return(-utility)
#' # }
#' #
#' # result <- solnp(-0.5, loss, LB = -2, UB = 0)
#' # result$pars
#' #
#' # x <- rbind(payoff[1] - result$pars, payoff[2] + Ra * result$pars, payoff[3] + Rb * result$pars)
#' # mu <- marginal_utility(x, diag(3), uf = function(x) prod(x^wt))
#' # mu / mu[1]
#' }

gemIntertemporalStochastic_Bank_TwoPeriods <- function(...) sdm2(...)