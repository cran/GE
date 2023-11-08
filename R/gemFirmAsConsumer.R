#' @export
#' @title Some Examples of Treating Firms as Consumer-Type Agents
#' @aliases gemFirmAsConsumer
#' @description Some examples of equilibrium models wherein firms are treated as
#' consumer-type agents instead of producer-type agents.
#' @param ... arguments to be passed to the function sdm2.
#' @seealso {
#' \code{\link{gemIntertemporal_2_2}}
#' }
#' @examples
#' \donttest{
#' #### an intertemporal model with firm
#' ## (see gemIntertemporal_2_2)
#' np <- 3 # the number of economic periods
#'
#' S0Exg <- matrix(c(
#'   0, 0, 150,
#'   1000, 0, 0,
#'   0, 1000, 0,
#'   0, 0, 100,
#'   0, 0, 100
#' ), 5, 3, TRUE)
#'
#' B <- matrix(0, 5, 3, TRUE)
#'
#' dst.firm1 <- node_new("util",
#'   type = "StickyLinear",
#'   beta = c(1, 1),
#'   "prod2", "cc1"
#' )
#' node_set(dst.firm1, "cc1",
#'   type = "CD",
#'   alpha = 2, beta = c(0.5, 0.5),
#'   "prod1", "lab1"
#' )
#'
#' dst.firm2 <- node_new("util",
#'   type = "StickyLinear",
#'   beta = c(1, 1),
#'   "prod3", "cc1"
#' )
#' node_set(dst.firm2, "cc1",
#'   type = "CD",
#'   alpha = 2, beta = c(0.5, 0.5),
#'   "prod2", "lab2"
#' )
#'
#' dst.consumer <- node_new(
#'   "util",
#'   type = "CD",
#'   alpha = 1, beta = prop.table(rep(1, np)),
#'   paste0("prod", 1:np)
#' )
#'
#' ge <- sdm2(
#'   A = list(dst.firm1, dst.firm2, dst.consumer),
#'   B = B,
#'   S0Exg = S0Exg,
#'   names.commodity = c(paste0("prod", 1:np), paste0("lab", 1:(np - 1))),
#'   names.agent = c(paste0("firm", 1:(np - 1)), "consumer"),
#'   numeraire = "prod1",
#'   ts = TRUE
#' )
#'
#' #### an intertemporal model with bank
#' igr <- 1.1
#' beta.bank <- c(1, 1 / igr, 1 / igr^2)
#'
#' dst.bank <- node_new(
#'   "output",
#'   type = "StickyLinear",
#'   beta = beta.bank,
#'   "payoff1", "payoff2", "payoff3"
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
#'   B = matrix(0, 3, 2, TRUE),
#'   S0Exg = matrix(c(
#'     100, 0,
#'     100, 2,
#'     100, 1
#'   ), 3, 2, TRUE),
#'   names.commodity = c("payoff1", "payoff2", "payoff3"),
#'   names.agent = c("bank", "consumer"),
#'   numeraire = "payoff1"
#' )
#'
#' #### an instantaneous sequential model
#' dst.firm <- node_new("output",
#'   type = "StickyLinear",
#'   beta = c(1, 1),
#'   "prod", "cc1"
#' )
#' node_set(dst.firm, "cc1",
#'   type = "CD",
#'   alpha = 2, beta = c(0.5, 0.5),
#'   "cap", "lab"
#' )
#'
#' dst.consumer <- node_new("util",
#'   type = "Leontief",
#'   a = 1,
#'   "prod"
#' )
#'
#' ge <- sdm2(
#'   A = list(dst.firm, dst.consumer),
#'   B = matrix(0, 3, 2, TRUE),
#'   S0Exg = matrix(c(
#'     1000, 0,
#'     0, 50,
#'     0, 100
#'   ), 3, 2, TRUE),
#'   names.commodity = c("prod", "cap", "lab"),
#'   names.agent = c("firm", "consumer"),
#'   numeraire = "prod"
#' )
#' ge$p
#' ge$z
#' ge$D
#'
#' ## the corresponding model treating a firm as a producer-type agent
#' ge <- sdm2(
#'   A = function(state) {
#'     a1 <- CD_A(alpha = 2, Beta = c(0, 0.5, 0.5), p = state$p)
#'     a2 <- c(1, 0, 0)
#'     cbind(a1, a2)
#'   },
#'   B = diag(c(1, 0), 3, 2),
#'   S0Exg = matrix(c(
#'     NA, NA,
#'     NA, 50,
#'     NA, 100
#'   ), 3, 2, TRUE),
#'   names.commodity = c("prod", "cap", "lab"),
#'   names.agent = c("firm", "consumer"),
#'   numeraire = "prod"
#' )
#' ge$p
#' ge$z
#' ge$D
#' }
gemFirmAsConsumer <- function(...) sdm2(...)
