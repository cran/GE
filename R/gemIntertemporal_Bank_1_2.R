#' @export
#' @title Some Examples of an Intertemporal Model with a Consumer and a Type of Bank
#' @aliases gemIntertemporal_Bank_1_2
#' @description Some examples of an intertemporal model with a consumer and a type of bank.
#' These models can be used to solve some intertemporal savings problems.
#' Below is an example.
#'
#' A np-period-lived consumer has some payoff (or cash, exhaustible resource etc.) in each period.
#' In each period the consumer can use payoff for consumption or save payoff into bank.
#' The interest rate is given.
#' The consumer has a SCES intertemporal utility function and attempts to maximize intertemporal utility by saving.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' #### an example with a 5-period-lived consumer
#' np <- 5 # the number of periods
#'
#' interest.rate <- 0.1
#' S <- matrix(NA, np, np)
#' S[1:np, np] <- 100 / (np:1)
#'
#' B <- matrix(0, np, np)
#' B[2:np, 1:(np - 1)] <- diag(np - 1)
#'
#' dstl.bank <- list()
#' for (k in 1:(np - 1)) {
#'   dstl.bank[[k]] <- node_new("output",
#'                              type = "Leontief",
#'                              a = 1 / (1 + interest.rate),
#'                              paste0("payoff", k))
#' }
#'
#' dst.consumer <- node_new(
#'   "util",
#'   type = "SCES",
#'   es = 1,
#'   alpha = 1,
#'   beta = prop.table(1:np),
#'   paste0("payoff", 1:np)
#' )
#'
#' ge <- sdm2(
#'   A = c(dstl.bank, dst.consumer),
#'   B = B,
#'   S0Exg = S,
#'   names.commodity = paste0("payoff", 1:np),
#'   names.agent = c(paste0("bank", 1:(np - 1)), "consumer"),
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
#' dst.consumer$es <- 0
#' dst.consumer$beta <- rep(1 / np, np)
#' S[1:np, np] <- 100 / (1:np)
#' ge <- sdm2(
#'   A = c(dstl.bank, dst.consumer),
#'   B = B,
#'   S0Exg = S,
#'   names.commodity = paste0("payoff", 1:np),
#'   names.agent = c(paste0("bank", 1:(np - 1)), "consumer"),
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
#' }

gemIntertemporal_Bank_1_2 <- function(...) sdm2(...)
