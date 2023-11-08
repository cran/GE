#' @export
#' @title The Basic Overlapping Generations Pure Exchange Model (see Samuelson, 1958)
#' @aliases gemOLG_PureExchange
#' @description This is the basic overlapping generations pure exchange model.
#' @param ... arguments to be passed to the function sdm2.
#' @details As Samuelson (1958) wrote, break each life up into thirds.
#' Agents get 100 units of payoff in period 1 and one unit in period 2; in period 3 they retire and get nothing.
#' Suppose there are three agents in each period, namely age1, age2 and age3.
#' In the next period, the present age1 will become age2, the present age2 will become age3,
#' the present age3 will disappear and a new age1 will appear.
#' Let c1, c2 and c3 denote the consumption of an agent in each period.
#' Suppose the utility function is (c1 * c2 * c3)^(1 / 3), which is actually the same as log(c1) + log(c2) + log(c3).
#' In each period, age1 and age2 will exchange their payoffs of the present period and the next period.
#' Age2 will sell some present payoff and buy some future payoff as pension, and for age1, it's the opposite.
#' Age3 simply receives the pension and need not take part in the exchange.
#' Hence only two agents participate in the pure exchange economy.
#' In the exchange process, the utility function of age1 is c1^(1 / 3) * x2^(2 / 3), wherein x2 is the revenue of the next period,
#' and the utility function of age2 is c2^(1 / 2) * c3^(1 / 2).
#' @note We can also suppose only age2 gets payoff and age1 does not.
#' @references Samuelson, P. A. (1958) An Exact Consumption-Loan Model of Interest with or without the Social Contrivance of Money. Journal of Political Economy, vol. 66(6): 467-482.
#' @seealso {
#' \code{\link{gemOLG_TimeCircle}}
#' }
#' @examples
#' \donttest{
#' #### the basic overlapping generations (inefficient) exchange model in sequential form.
#' dst.age1 <- node_new(
#'   "util",
#'   type = "CD", alpha = 1, beta = c(1 / 3, 2 / 3),
#'   "payoff1", "payoff2"
#' )
#'
#' dst.age2 <- node_new(
#'   "util",
#'   type = "CD", alpha = 1, beta = c(1 / 2, 1 / 2),
#'   "payoff1", "payoff2"
#' )
#'
#' policy.supply <- function(time, state) {
#'   pension <- (state$last.A[, 2] * state$last.z[2])[2]
#'   if (time > 1) state$S[1, 2] <- 100 - pension
#'   state
#' }
#'
#' ge <- sdm2(
#'   A = list(dst.age1, dst.age2),
#'   B = matrix(0, 2, 2),
#'   S0Exg = matrix(c(
#'     100, 100,
#'     100, 0
#'   ), 2, 2, TRUE),
#'   names.commodity = c("payoff1", "payoff2"),
#'   names.agent = c("age1", "age2"),
#'   numeraire = "payoff1",
#'   policy = list(policy.supply, policyMarketClearingPrice),
#'   maxIteration = 1,
#'   numberOfPeriods = 20,
#'   ts = TRUE
#' )
#'
#' ge$p # c(1, 3 / 2 + sqrt(13) / 2)
#' ge$ts.p
#' ge$S
#' ge$D
#' ge$DV
#'
#' #### the basic overlapping generations exchange model in timeline form.
#' m <- 15 # the number of generations
#' n <- m + 1 # the number of commodity kinds
#'
#' names.commodity <- paste0("payoff", 1:n)
#' names.agent <- paste0("gen", 1:m)
#'
#' # the exogenous supply matrix.
#' S0Exg <- matrix(NA, n, m, dimnames = list(names.commodity, names.agent))
#' for (k in 1:m) {
#'   S0Exg[k:(k + 1), k] <- 100
#' }
#'
#' dstl.consumer <- list()
#' for (k in 1:(m - 1)) {
#'   dstl.consumer[[k]] <- node_new(
#'     "util",
#'     type = "CD", alpha = 1,
#'     beta = rep(1 / 3, 3),
#'     paste0("payoff", k:(k + 2))
#'   )
#' }
#'
#' dstl.consumer[[m]] <- node_new(
#'   "util",
#'   type = "CD", alpha = 1,
#'   beta = c(0.5, 0.5),
#'   paste0("payoff", m:(m + 1))
#' )
#'
#' ge <- sdm2(
#'   A = dstl.consumer,
#'   B = matrix(0, n, m),
#'   S0Exg = S0Exg,
#'   names.commodity = names.commodity,
#'   names.agent = names.agent,
#'   numeraire = "payoff1"
#' )
#'
#' round(addmargins(ge$D, 2), 2)
#' round(addmargins(ge$S, 2), 2)
#' growth_rate(ge$p) + 1 # 3 / 2 + sqrt(13) / 2
#'
#' #### Assume that in the timeline model, each consumer lives for four periods or more.
#' nl <- 4 # the number of life periods
#' payoff <- c(rep(100, nl - 1), 1e-10) # the lifetime payoffs
#' m <- 20 # the number of generations
#' n <- m + nl - 1 # the number of commodity kinds
#'
#' names.commodity <- paste0("payoff", 1:n)
#' names.agent <- paste0("gen", 1:m)
#'
#' # the exogenous supply matrix.
#' S0Exg <- matrix(NA, n, m, dimnames = list(names.commodity, names.agent))
#' for (k in 1:m) {
#'   S0Exg[k:(k + nl - 1), k] <- payoff
#' }
#'
#' dstl.consumer <- list()
#' for (k in 1:m) {
#'   dstl.consumer[[k]] <- node_new(
#'     "util",
#'     type = "CD", alpha = 1,
#'     beta = rep(1 / nl, nl),
#'     paste0("payoff", k:(k + nl - 1))
#'   )
#' }
#'
#' ge <- sdm2(
#'   A = dstl.consumer,
#'   B = matrix(0, n, m),
#'   S0Exg = S0Exg,
#'   names.commodity = names.commodity,
#'   names.agent = names.agent,
#'   numeraire = "payoff1"
#' )
#'
#' round(addmargins(ge$D, 2), 2)
#' round(addmargins(ge$S, 2), 2)
#' growth_rate(ge$p) + 1
#' }

gemOLG_PureExchange <- function(...) sdm2(...)
