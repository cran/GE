#' @export
#' @title Some Examples of an Intertemporal Model with Two Consumers and a Type of Bank
#' @aliases gemIntertemporal_Bank_1_3
#' @description Some examples of an intertemporal model with two consumers and a type of bank.
#' @param ... arguments to be passed to the function sdm2.
#' @seealso {
#' \code{\link{gemIntertemporal_Bank_1_2}}
#' }
#' @examples
#' \donttest{
#' #### an example with a 5-period-lived consumer
#' np <- 5 # the number of economic periods
#' interest.rate <- 0.1
#'
#' n <- np # the number of commodity kinds
#' m <- np + 1 # the number of agent kinds
#'
#' names.commodity <- paste0("payoff", 1:np)
#' names.agent <- c(paste0("bank", 1:(np - 1)), "consumer1", "consumer2")
#'
#' # the exogenous supply matrix.
#' S0Exg <- matrix(NA, n, m, dimnames = list(names.commodity, names.agent))
#' S0Exg[paste0("payoff", 1:np), "consumer1"] <- 100 / (np:1)
#' S0Exg[paste0("payoff", 1:np), "consumer2"] <- 100 / (1:np)
#'
#' # the output coefficient matrix.
#' B <- matrix(0, n, m, dimnames = list(names.commodity, names.agent))
#' for (k in 1:(np - 1)) {
#'   B[paste0("payoff", k + 1), paste0("bank", k)] <- 1
#' }
#'
#' dstl.bank <- list()
#' for (k in 1:(np - 1)) {
#'   dstl.bank[[k]] <- node_new(
#'     "output",
#'     type = "Leontief", a = 1 / (1 + interest.rate),
#'     paste0("payoff", k)
#'   )
#' }
#'
#' dst.consumer1 <- node_new(
#'   "util",
#'   type = "SCES",
#'   es = 1, alpha = 1, beta = prop.table(1:np),
#'   paste0("payoff", 1:np)
#' )
#'
#' dst.consumer2 <- node_new(
#'   "util",
#'   type = "SCES",
#'   es = 1, alpha = 1, beta = prop.table(np:1),
#'   paste0("payoff", 1:np)
#' )
#'
#' ge <- sdm2(
#'   A = c(dstl.bank, dst.consumer1, dst.consumer2),
#'   B = B,
#'   S0Exg = S0Exg,
#'   names.commodity = names.commodity,
#'   names.agent = names.agent,
#'   numeraire = "payoff1",
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
#' growth_rate(ge$p)
#'
#' ##
#' dst.consumer1$es <- 0
#' dst.consumer1$beta <- rep(1 / np, np)
#'
#' ge <- sdm2(
#'   A = c(dstl.bank, dst.consumer1, dst.consumer2),
#'   B = B,
#'   S0Exg = S0Exg,
#'   names.commodity = names.commodity,
#'   names.agent = names.agent,
#'   numeraire = "payoff1",
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
#' growth_rate(ge$p)
#' }

gemIntertemporal_Bank_1_3 <- function(...) sdm2(...)
