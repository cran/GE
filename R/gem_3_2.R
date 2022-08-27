#' @export
#' @title Some Simple 3-by-2 General Equilibrium Models
#' @aliases gem_3_2
#' @description Some simple 3-by-2 general equilibrium models with a firm and a consumer.
#' @param ... arguments to be passed to the function sdm2.
#' @references http://www.econ.ucla.edu/riley/MAE/Course/SolvingForTheWE.pdf
#' @examples
#' \donttest{
#' ge.CD <- sdm2(
#'   A = function(state) {
#'     ## the vector of demand coefficients of the firm
#'     a1 <- CD_A(alpha = 2, Beta = c(0, 0.5, 0.5), state$p)
#'     ## the vector of demand coefficients of the consumer
#'     a2 <- c(1, 0, 0)
#'     cbind(a1, a2)
#'   },
#'   B = matrix(c(
#'     1, 0,
#'     0, 0,
#'     0, 0
#'   ), 3, 2, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA,
#'     NA, 100,
#'     NA, 100
#'   ), 3, 2, TRUE),
#'   names.commodity = c("prod", "cap", "lab"),
#'   names.agent = c("firm", "consumer"),
#'   numeraire = "prod"
#' )
#'
#' ge.CD$p
#' ge.CD$z
#' ge.CD$D
#' ge.CD$S
#'
#' #### Example 2 in the ucla reference
#' ## By introducing a new factor of production (called land here)
#' ## a firm with diminishing returns to scale can be converted into
#' ## a firm with constant returns to scale.
#' ge2.CD <- sdm2(
#'   A = function(state) {
#'     a.firm <- CD_A(alpha = 6, Beta = c(0, 0.5, 0.5), state$p)
#'     a.consumer <- CD_A(alpha = 1, Beta = c(0.2, 0.8, 0), state$p)
#'     cbind(a.firm, a.consumer)
#'   },
#'   B = matrix(c(
#'     1, 0,
#'     0, 0,
#'     0, 0
#'   ), 3, 2, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA,
#'     NA, 81,
#'     NA, 1
#'   ), 3, 2, TRUE),
#'   names.commodity = c("prod", "lab", "land"),
#'   names.agent = c("firm", "consumer"),
#'   numeraire = "prod"
#' )
#'
#' ge2.CD$p
#' ge2.CD$z
#' ge2.CD$D
#' ge2.CD$S
#'
#' ####
#' ge.SCES <- sdm2(
#'   A = function(state) {
#'     a1 <- SCES_A(es = 0.5, alpha = 1, Beta = c(0, 0.5, 0.5), p = state$p)
#'     a2 <- c(1, 0, 0)
#'     cbind(a1, a2)
#'   },
#'   B = matrix(c(
#'     1, 0,
#'     0, 0,
#'     0, 0
#'   ), 3, 2, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA,
#'     NA, 100,
#'     NA, 100
#'   ), 3, 2, TRUE),
#'   names.commodity = c("prod", "cap", "lab"),
#'   names.agent = c("firm", "consumer"),
#'   numeraire = "prod"
#' )
#'
#' ge.SCES$p
#' ge.SCES$z
#' ge.SCES$D
#' ge.SCES$S
#'
#' ####
#' ge2.SCES <- sdm2(
#'   A = function(state) {
#'     a1 <- SCES_A(es = 0.5, alpha = 1, Beta = c(0.2, 0.4, 0.4), p = state$p)
#'     a2 <- c(1, 0, 0)
#'     cbind(a1, a2)
#'   },
#'   B = matrix(c(
#'     1, 0,
#'     0, 0,
#'     0, 0
#'   ), 3, 2, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA,
#'     NA, 100,
#'     NA, 100
#'   ), 3, 2, TRUE),
#'   names.commodity = c("prod", "cap", "lab"),
#'   names.agent = c("firm", "consumer"),
#'   numeraire = "prod"
#' )
#'
#' ge2.SCES$p
#' ge2.SCES$z
#' ge2.SCES$D
#' ge2.SCES$S
#'
#' #### nested production function
#' dst.firm <- node_new(
#'   "prod",
#'   type = "Leontief",
#'   a = c(0.2, 0.8),
#'   "prod", "cc1"
#' )
#' node_set(dst.firm, "cc1",
#'          type = "SCES",
#'          es = 0.5, alpha = 1, beta = c(0.5, 0.5),
#'          "cap", "lab"
#' )
#'
#' dst.consumer <- node_new(
#'   "util",
#'   type = "Leontief", a = 1,
#'   "prod"
#' )
#'
#' ge3.SCES <- sdm2(
#'   A = list(dst.firm, dst.consumer),
#'   B = matrix(c(
#'     1, 0,
#'     0, 0,
#'     0, 0
#'   ), 3, 2, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA,
#'     NA, 100,
#'     NA, 100
#'   ), 3, 2, TRUE),
#'   names.commodity = c("prod", "cap", "lab"),
#'   names.agent = c("firm", "consumer"),
#'   numeraire = "prod"
#' )
#'
#' ge3.SCES$p
#' ge3.SCES$z
#' ge3.SCES$D
#' ge3.SCES$S
#' }

gem_3_2 <- function(...) sdm2(...)
