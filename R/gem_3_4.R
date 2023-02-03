#' @export
#' @title Some Simple 3-by-4 General Equilibrium Models
#' @aliases gem_3_4
#' @description Some simple 3-by-4 general equilibrium models with two firms and two consumer.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' ####
#' ge <- sdm2(
#'   A = function(state) {
#'     a.firm.corn <- CD_A(alpha = 1, Beta = c(0, 0.5, 0.5), state$p)
#'     a.firm.iron <- CD_A(alpha = 2, Beta = c(0, 0.5, 0.5), state$p)
#'     a.consumer1 <- c(1, 0, 0)
#'     a.consumer2 <- CD_A(alpha = 1, Beta = c(0.5, 0.5, 0), state$p)
#'
#'     cbind(a.firm.corn, a.firm.iron, a.consumer1, a.consumer2)
#'   },
#'   B = diag(c(1, 1, 0), 3, 4),
#'   S0Exg = {
#'     tmp <- matrix(NA, 3, 4)
#'     tmp[3, 3:4] <- 100
#'     tmp
#'   },
#'   names.commodity = c("corn", "iron", "lab"),
#'   names.agent = c("firm.corn", "firm.iron", "consumer1", "consumer2"),
#'   numeraire = "lab"
#' )
#'
#' ge$p
#' ge$z
#' ge$D
#' ge$S
#'
#' ####
#' ge <- sdm2(
#'   A = function(state) {
#'     a.firm.corn <-
#'       SCES_A(
#'         es = 1,
#'         alpha = 1,
#'         Beta = c(0, 0.5, 0.5),
#'         p = state$p
#'       )
#'     a.firm.iron <-
#'       SCES_A(
#'         es = 1,
#'         alpha = 2,
#'         Beta = c(0, 0.5, 0.5),
#'         p = state$p
#'       )
#'     a.consumer1 <- c(1, 0, 0)
#'     a.consumer2 <- CD_A(alpha = 1, Beta = c(0.5, 0.5, 0), state$p)
#'
#'     cbind(a.firm.corn, a.firm.iron, a.consumer1, a.consumer2)
#'   },
#'   B = diag(c(1, 1, 0), 3, 4),
#'   S0Exg = {
#'     tmp <- matrix(NA, 3, 4)
#'     tmp[3, 3:4] <- 100
#'     tmp
#'   },
#'   names.commodity = c("corn", "iron", "lab"),
#'   names.agent = c("firm.corn", "firm.iron", "consumer1", "consumer2"),
#'   numeraire = "lab"
#' )
#'
#' ge$p
#' ge$z
#' ge$D
#' ge$S
#'
#' #### an example at
#' ## https://web.stanford.edu/~jdlevin/Econ%20202/General%20Equilibrium.pdf
#' ge <- sdm2(
#'   A = function(state) {
#'     a.firm.1 <- c(0, 1, 0)
#'     a.firm.2 <- c(0, 0, 1)
#'     a.consumer1 <- CD_A(alpha = 1, Beta = c(1 / 3, 1 / 3, 1 / 3), state$p)
#'     a.consumer2 <- CD_A(alpha = 1, Beta = c(1 / 3, 1 / 3, 1 / 3), state$p)
#'
#'     cbind(a.firm.1, a.firm.2, a.consumer1, a.consumer2)
#'   },
#'   B = matrix(c(
#'     2, 0, 0, 0,
#'     0, 1, 0, 0,
#'     0.5, 0, 0, 0
#'   ), 3, 4, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA, 1, 2,
#'     NA, NA, 2, 2,
#'     NA, NA, 3, 2
#'   ), 3, 4, TRUE),
#'   names.agent = c("firm1", "firm2", "consumer1", "consumer2"),
#'   numeraire = 3
#' )
#'
#' ge$p
#' ge$z
#' ge$D
#' ge$S
#' }

gem_3_4 <- function(...) sdm2(...)
