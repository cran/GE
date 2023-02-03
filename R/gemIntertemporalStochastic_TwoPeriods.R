#' @export
#' @title Some Examples of a Two-Period Intertemporal Stochastic Equilibrium Model
#' @aliases gemIntertemporalStochastic_TwoPeriods
#' @description Some examples of a two-period intertemporal equilibrium model with two types of commodities (i.e. product and labor)
#' and one firm.
#' In the second period there are two states of nature, in which the firm has different productivity.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' #### an examples with a consumer and a firm.
#' alpha1 <- 1
#' alpha2 <- 2
#'
#' supply.lab <- 100
#' supply.prod1 <- 30
#'
#' dst.firm <- node_new(
#'   "prod2",
#'   type = "CD",
#'   alpha = 1, beta = c(0.5, 0.5),
#'   "prod1", "lab1"
#' )
#'
#' dst.consumer <- node_new(
#'   "util",
#'   type = "CD",
#'   alpha = 1, beta = c(0.5, 0.25, 0.25),
#'   "prod1", "prod2.1", "prod2.2"
#' )
#'
#' ge <- sdm2(
#'   A = c(dst.firm, dst.consumer),
#'   B = matrix(c(
#'     0, 0,
#'     0, 0,
#'     alpha1, 0,
#'     alpha2, 0
#'   ), 4, 2, TRUE),
#'   S0Exg = matrix(c(
#'     NA, supply.prod1,
#'     NA, supply.lab,
#'     NA, NA,
#'     NA, NA
#'   ), 4, 2, TRUE),
#'   names.commodity = c("prod1", "lab1", "prod2.1", "prod2.2"),
#'   names.agent = c("firm", "consumer"),
#'   numeraire = "prod1"
#' )
#'
#' ge$p
#' ge$z
#' ge$D
#' ge$S
#' ge$DV
#' ge$SV
#'
#' #### an examples with two types of consumer and a firm.
#' dst.firm <- node_new(
#'   "prod2",
#'   type = "CD",
#'   alpha = 1, beta = c(0.5, 0.5),
#'   "prod1", "lab1"
#' )
#'
#' dst.consumer1 <- node_new(
#'   "util",
#'   type = "CD",
#'   alpha = 1, beta = c(0.5, 0.4, 0.1),
#'   "prod1", "prod2.1", "prod2.2"
#' )
#'
#' dst.consumer2 <- node_new(
#'   "util",
#'   type = "CD",
#'   alpha = 1, beta = c(0.5, 0.25, 0.25),
#'   "prod1", "prod2.1", "prod2.2"
#' )
#'
#' ge <- sdm2(
#'   A = c(dst.firm, dst.consumer1, dst.consumer2),
#'   B = matrix(c(
#'     0, 0, 0,
#'     0, 0, 0,
#'     1, 0, 0,
#'     2, 0, 0
#'   ), 4, 3, TRUE),
#'   S0Exg = matrix(c(
#'     NA, 30, 30,
#'     NA, 100, 100,
#'     NA, NA, NA,
#'     NA, NA, NA
#'   ), 4, 3, TRUE),
#'   names.commodity = c("prod1", "lab1", "prod2.1", "prod2.2"),
#'   names.agent = c("firm", "consumer1", "consumer2"),
#'   numeraire = "prod1"
#' )
#'
#' ge$p
#' ge$z
#' ge$D
#' ge$S
#' ge$DV
#' ge$SV
#' }


gemIntertemporalStochastic_TwoPeriods <- function(...) sdm2(...)
