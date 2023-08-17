#' @export
#' @title Some Simple Pure Exchange Equilibrium Models
#' @aliases gemPureExchange
#' @description Some simple pure exchange general equilibrium models.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' dst.consumer1 <- dst.consumer2 <- node_new("util",
#'   type = "CD", alpha = 1, beta = c(0.5, 0.25, 0.25),
#'   "payoff1", "payoff2", "payoff3"
#' )
#'
#' ge <- sdm2(
#'   A = list(dst.consumer1, dst.consumer2),
#'   B = matrix(0, 3, 2),
#'   S0Exg = matrix(c(
#'     1, 1,
#'     0, 2,
#'     2, 2
#'   ), 3, 2, TRUE),
#'   names.commodity = c("payoff1", "payoff2", "payoff3"),
#'   names.agent = c("consumer1", "consumer2"),
#'   numeraire = "payoff1"
#' )
#'
#' ge$p
#' ge$D
#'
#' ##
#' dst.consumer2 <- node_new("util",
#'   type = "CD", alpha = 1, beta = c(0.5, 0.1, 0.4),
#'   "payoff1", "payoff2", "payoff3"
#' )
#'
#' ge <- sdm2(
#'   A = list(dst.consumer1, dst.consumer2),
#'   B = matrix(0, 3, 2),
#'   S0Exg = matrix(c(
#'     1, 1,
#'     0, 2,
#'     2, 2
#'   ), 3, 2, TRUE),
#'   names.commodity = c("payoff1", "payoff2", "payoff3"),
#'   names.agent = c("consumer1", "consumer2"),
#'   numeraire = "payoff1"
#' )
#'
#' ge$p
#' ge$D
#'
#' ####
#' dst.consumer1 <- node_new("util",
#'   type = "CD", alpha = 1,
#'   beta = c(0.5, 0.5),
#'   "cc1", "cc2"
#' )
#' node_set(dst.consumer1, "cc2",
#'   type = "CD", alpha = 1,
#'   beta = c(0.2, 0.8),
#'   "cc2.1", "cc2.2"
#' )
#' node_set(dst.consumer1, "cc1",
#'   type = "Leontief",
#'   a = c(0.5, 0.5),
#'   "corn1", "iron1"
#' )
#'
#' node_set(dst.consumer1, "cc2.1",
#'   type = "Leontief", a = c(0.5, 0.5),
#'   "corn2.1", "iron2.1"
#' )
#' node_set(dst.consumer1, "cc2.2",
#'   type = "Leontief", a = c(0.5, 0.5),
#'   "corn2.2", "iron2.2"
#' )
#'
#' dst.consumer2 <- node_new("util",
#'   type = "CD", alpha = 1,
#'   beta = prop.table(c(
#'     0.5 * c(1, 1),
#'     0.5 * 0.2 * c(1, 1), 0.5 * 0.8 * c(1, 1)
#'   )),
#'   "corn1", "iron1", "corn2.1",
#'   "iron2.1", "corn2.2", "iron2.2"
#' )
#'
#' node_plot(dst.consumer1, TRUE)
#'
#' ge <- sdm2(
#'   A = list(dst.consumer1, dst.consumer2),
#'   B = matrix(0, 6, 2),
#'   S0Exg = matrix(c(
#'     2, 2,
#'     2, 2,
#'     3, 2,
#'     1, 2,
#'     1, 2,
#'     3, 2
#'   ), 6, 2, TRUE),
#'   names.commodity = c(
#'     "corn1", "iron1", "corn2.1",
#'     "iron2.1", "corn2.2", "iron2.2"
#'   ),
#'   names.agent = c("consumer1", "consumer2"),
#'   numeraire = "corn1"
#' )
#'
#' ge$D
#' ge$DV
#' }

gemPureExchange <- function(...) sdm2(...)
