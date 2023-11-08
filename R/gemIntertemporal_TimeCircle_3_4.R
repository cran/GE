#' @export
#' @title A Time Circle Model with Two Consumers and Two Types of Firms
#' @aliases gemIntertemporal_TimeCircle_3_4
#' @description An (intertemporal) time circle model with two consumers and two types of firms.
#' There are three commodities in the model, namely wheat, iron, and labor.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' #### an example with a Cobb-Douglas intertemporal utility function
#' np <- 5 # the number of economic periods, firms.
#' zeta <- 1.25 # the ratio of repayments to loans
#'
#' n <- 3 * np # the number of commodity kinds
#' m <- 2 * np + 2 # the number of agent kinds
#'
#' names.commodity <- c(
#'   paste0("wheat", 1:np),
#'   paste0("iron", 1:np),
#'   paste0("lab", 1:np)
#' )
#' names.agent <- c(
#'   paste0("firm.wheat", 1:np), paste0("firm.iron", 1:np),
#'   "consumer1", "consumer2"
#' )
#'
#' # the exogenous supply matrix.
#' S0Exg <- matrix(NA, n, m, dimnames = list(names.commodity, names.agent))
#' S0Exg[paste0("lab", 1:np), c("consumer1", "consumer2")] <- 100 # the supply of labor
#'
#' # the output coefficient matrix.
#' B <- matrix(0, n, m, dimnames = list(names.commodity, names.agent))
#' for (k in 1:(np - 1)) {
#'   B[paste0("wheat", k + 1), paste0("firm.wheat", k)] <- 1
#'   B[paste0("iron", k + 1), paste0("firm.iron", k)] <- 1
#' }
#' B["wheat1", paste0("firm.wheat", np)] <- 1 / zeta
#' B["iron1", paste0("firm.iron", np)] <- 1 / zeta
#'
#' dstl.firm.wheat <- dstl.firm.iron <- list()
#' for (k in 1:np) {
#'   dstl.firm.wheat[[k]] <- node_new(
#'     "prod",
#'     type = "CD", alpha = 1, beta = c(0.5, 0.5),
#'     paste0("iron", k), paste0("lab", k)
#'   )
#'
#'   dstl.firm.iron[[k]] <- node_new(
#'     "prod",
#'     type = "CD", alpha = 2, beta = c(0.5, 0.5),
#'     paste0("iron", k), paste0("lab", k)
#'   )
#' }
#'
#' dst.consumer1 <- node_new(
#'   "util",
#'   type = "CD", alpha = 1, beta = prop.table(rep(1, np)),
#'   paste0("wheat", 1:np)
#' )
#'
#' dst.consumer2 <- node_new(
#'   "util",
#'   type = "CD", alpha = 1, beta = prop.table(rep(1, np)),
#'   paste0("cc", 1:np)
#' )
#' for (k in 1:np) {
#'   node_set(
#'     dst.consumer2, paste0("cc", k),
#'     type = "CD", alpha = 1, beta = c(0.5, 0.5),
#'     paste0("wheat", k), paste0("iron", k)
#'   )
#' }
#'
#' ge <- sdm2(
#'   A = c(dstl.firm.wheat, dstl.firm.iron, dst.consumer1, dst.consumer2),
#'   B = B,
#'   S0Exg = S0Exg,
#'   names.commodity = names.commodity,
#'   names.agent = names.agent,
#'   numeraire = "lab1",
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

gemIntertemporal_TimeCircle_3_4 <- function(...) sdm2(...)
