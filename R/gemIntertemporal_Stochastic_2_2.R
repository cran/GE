#' @export
#' @title An Intertemporal Model with Uncertainty
#' @aliases gemIntertemporal_Stochastic_2_2
#' @description An intertemporal model with uncertainty.
#' In the model the consumer will live for three periods and has a von Neumann-Morgenstern expected utility function.
#' There is one natural state in the first period,
#' and two natural states in the second and third period.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
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
#'   "prod2.1","lab2.1"
#' )
#'
#' dst.firm2.2 <- node_new(
#'   "prod3.2",
#'   type = "CD", alpha = 1,
#'   beta = c(0.4, 0.6),
#'   "prod2.2","lab2.2"
#' )
#'
#' dst.consumer <- node_new(
#'   "util",
#'   type = "CD", alpha = 1,
#'   beta = rep(1/5, 5),
#'   "prod1","prod2.1","prod2.2",
#'   "prod3.1","prod3.2"
#' )
#'
#' ge <- sdm2(
#'   A = c(dst.firm1, dst.firm2.1,dst.firm2.2,
#'         dst.consumer),
#'   B = matrix(c(0, 0, 0, 0,
#'                1, 0, 0, 0,
#'                1, 0, 0, 0,
#'                0, 1, 0, 0,
#'                0, 0, 1, 0,
#'                0, 0, 0, 0,
#'                0, 0, 0, 0,
#'                0, 0, 0, 0),8,4,TRUE),
#'   S0Exg = matrix(c(NA, NA, NA, 50,
#'                    NA, NA, NA, NA,
#'                    NA, NA, NA, NA,
#'                    NA, NA, NA, NA,
#'                    NA, NA, NA, NA,
#'                    NA, NA, NA,100,
#'                    NA, NA, NA,100,
#'                    NA, NA, NA,100),8,4,TRUE),
#'   names.commodity = c("prod1", "prod2.1","prod2.2",
#'                       "prod3.1","prod3.2",
#'                       "lab1", "lab2.1","lab2.2"),
#'   names.agent = c("firm1", "firm2.1","firm2.2",
#'                   "consumer"),
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

gemIntertemporal_Stochastic_2_2 <- function(...) sdm2(...)
