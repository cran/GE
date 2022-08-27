#' @export
#' @title Some Simple 3-by-3 General Equilibrium Models
#' @aliases gem_3_3
#' @description Some simple 3-by-3 general equilibrium models with two firms and a consumer.
#' @param ... arguments to be passed to the function sdm2.
#' @seealso {
#' \code{\link{gemCapitalAccumulation}}
#' }
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
#'
#' #### a general equilibrium model containing a production firm
#' #### and a capital-goods-leasing firm
#' ge <- sdm2(
#'   A = function(state) {
#'     a.firm1 <- CD_A(alpha = 2, Beta = c(0, 0.5, 0.5), state$p)
#'     a.consumer <- c(1, 0, 0)
#'     a.firm2 <- c(1, 0, 0)
#'     cbind(a.firm1, a.consumer, a.firm2)
#'   },
#'   B = matrix(c(
#'     1, 0, 0.5,
#'     0, 0, 1,
#'     0, 0, 0
#'   ), 3, 3, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA, NA,
#'     NA, NA, NA,
#'     NA, 100,NA
#'   ), 3, 3, TRUE),
#'   names.commodity = c("prod", "cap", "lab"),
#'   names.agent = c("firm1", "consumer","firm2"),
#'   numeraire = "prod",
#'   priceAdjustmentVelocity = 0.05
#' )
#' ge$p
#' ge$z
#' ge$D
#' }

gem_3_3 <- function(...) sdm2(...)
