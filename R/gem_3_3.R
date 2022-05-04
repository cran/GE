#' @export
#' @title Some Simple 3-by-3 General Equilibrium Models
#' @aliases gem_3_3
#' @description Some simple 3-by-3 general equilibrium models with two firm and a consumer.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' ####
#' ge <- sdm2(
#'   A = function(state) {
#'     ## the vector of demand coefficients of the firm
#'     a.firm.corn <- CD_A(alpha = 1, Beta = c(0, 0.5, 0.5), p = state$p)
#'     a.firm.iron <- CD_A(alpha = 2, Beta = c(0, 0.5, 0.5), p = state$p)
#'     ## the vector of demand coefficients of the consumer
#'     a.consumer <- CD_A(alpha = 1, Beta = c(0.5, 0.5, 0), p = state$p)
#'     cbind(a.firm.corn, a.firm.iron, a.consumer)
#'   },
#'   B = diag(c(1, 1), 3),
#'   S0Exg = {
#'     tmp <- matrix(NA, 3, 3)
#'     tmp[3, 3] <- 100
#'     tmp
#'   },
#'   names.commodity = c("corn", "iron", "lab"),
#'   names.agent = c("firm.corn", "firm.iron", "consumer"),
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
#'     ## the vector of demand coefficients of the firm
#'     a.firm.corn <-
#'       SCES_A(es = 1,
#'              alpha = 1,
#'              Beta = c(0, 0.5, 0.5),
#'              p = state$p)
#'     a.firm.iron <-
#'       SCES_A(es = 1,
#'              alpha = 2,
#'              Beta = c(0, 0.5, 0.5),
#'              p = state$p)
#'     ## the vector of demand coefficients of the consumer
#'     a.consumer <- CD_A(alpha = 1, Beta = c(0.5, 0.5, 0), p = state$p)
#'     cbind(a.firm.corn, a.firm.iron, a.consumer)
#'   },
#'   B = diag(c(1, 1), 3),
#'   S0Exg = {
#'     tmp <- matrix(NA, 3, 3)
#'     tmp[3, 3] <- 100
#'     tmp
#'   },
#'   names.commodity = c("corn", "iron", "lab"),
#'   names.agent = c("firm.corn", "firm.iron", "consumer"),
#'   numeraire = "lab"
#' )
#'
#' ge$p
#' ge$z
#' ge$D
#' ge$S
#' }

gem_3_3 <- function(...) sdm2(...)