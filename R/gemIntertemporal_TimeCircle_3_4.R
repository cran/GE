#' @export
#' @title A Time Circle Model with Two Consumers and Two Types of Firms
#' @aliases gemIntertemporal_TimeCircle_3_4
#' @description An (intertemporal) time circle model with two consumers and two types of firms.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' #### an example with a Cobb-Douglas intertemporal utility function
#' np <- 5 # the number of internal periods, firms.
#' n <- 3 * np
#' m <- 2 * np + 2
#'
#' zeta <- 1.25 # the ratio of repayments to loans
#' S <- matrix(NA, n, m)
#' S[(n - np + 1):n, (m - 1):m] <- 100
#'
#' B <- matrix(0, n, m)
#' B[1:np, 1:np] <-
#'   B[(np + 1):(2 * np), (np + 1):(2 * np)] <- diag(np)[, c(2:np, 1)]
#' B[1, np] <-  B[np + 1, 2 * np] <- 1 / zeta
#'
#' dstl.firm.corn <- dstl.firm.iron  <- list()
#' for (k in 1:np) {
#'   dstl.firm.corn[[k]] <- node_new(
#'     "prod",
#'     type = "CD",
#'     alpha = 1,
#'     beta = c(0.5, 0.5),
#'     paste0("iron", k),
#'     paste0("lab", k)
#'   )
#'
#'   dstl.firm.iron[[k]] <- node_new(
#'     "prod",
#'     type = "CD",
#'     alpha = 2,
#'     beta = c(0.5, 0.5),
#'     paste0("iron", k),
#'     paste0("lab", k)
#'   )
#' }
#'
#' dst.consumer1 <- node_new(
#'   "util",
#'   type = "CD",
#'   alpha = 1,
#'   beta = prop.table(rep(1, np)),
#'   paste0("corn", 1:np)
#' )
#'
#' dst.consumer2 <- node_new(
#'   "util",
#'   type = "CD",
#'   alpha = 1,
#'   beta = prop.table(rep(1, np)),
#'   paste0("cc", 1:np)
#' )
#' for (k in 1:np) {
#'   node_set(
#'     dst.consumer2,
#'     paste0("cc", k),
#'     type = "CD",
#'     alpha = 1,
#'     beta = c(0.5, 0.5),
#'     paste0("corn", k),
#'     paste0("iron", k)
#'   )
#' }
#'
#' ge <- sdm2(
#'   A = c(dstl.firm.corn, dstl.firm.iron, dst.consumer1, dst.consumer2),
#'   B = B,
#'   S0Exg = S,
#'   names.commodity = c(paste0("corn", 1:np),
#'                       paste0("iron", 1:np),
#'                       paste0("lab", 1:np)),
#'   names.agent = c(
#'     paste0("firm.corn", 1:np),
#'     paste0("firm.iron", 1:np),
#'     "consumer1",
#'     "consumer2"
#'   ),
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
