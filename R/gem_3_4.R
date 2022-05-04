#' @export
#' @title Some Simple 3-by-4 General Equilibrium Models
#' @aliases gem_3_4
#' @description Some simple 3-by-4 general equilibrium models with two firm and two consumer.
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
#' }

gem_3_4 <- function(...) sdm2(...)