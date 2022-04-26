#' @export
#' @title Some Simple 2-by-2 General Equilibrium Models
#' @aliases gem_2_2
#' @description Some simple 2-by-2 general equilibrium models with a firm and a laborer.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' ####
#' ge.Leontief <- sdm2(
#'   A = matrix(c(
#'     0.5, 1,
#'     0.5, 0
#'   ), 2, 2, TRUE),
#'   B = matrix(c(
#'     1, 0,
#'     0, 0
#'   ), 2, 2, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA,
#'     NA, 100
#'   ), 2, 2, TRUE),
#'   names.commodity = c("prod", "lab"),
#'   names.agent = c("firm", "laborer"),
#'   numeraire = "prod"
#' )
#'
#' ge.Leontief$p
#' ge.Leontief$z
#' ge.Leontief$D
#' ge.Leontief$S
#'
#' ## the same as above
#' ge2.Leontief <- sdm2(
#'   A = list(
#'     dst.firm = node_new(
#'       "output",
#'       type = "Leontief",
#'       a = c(0.5, 0.5),
#'       "prod", "lab"
#'     ),
#'     dst.consumer = node_new(
#'       "util",
#'       type = "Leontief", a = 1,
#'       "prod"
#'     )
#'   ),
#'   B = matrix(c(
#'     1, 0,
#'     0, 0
#'   ), 2, 2, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA,
#'     NA, 100
#'   ), 2, 2, TRUE),
#'   names.commodity = c("prod", "lab"),
#'   names.agent = c("firm", "laborer"),
#'   numeraire = "prod"
#' )
#'
#' ge2.Leontief$p
#' ge2.Leontief$z
#' ge2.Leontief$D
#' ge2.Leontief$S
#'
#' ####
#' ge.CD <- sdm2(
#'   A = function(state) {
#'     ## the vector of demand coefficients of the firm
#'     a1 <- CD_A(alpha = 2, Beta = c(0.5, 0.5), state$p)
#'     ## the vector of demand coefficients of the laborer
#'     a2 <- c(1, 0)
#'     cbind(a1, a2)
#'   },
#'   B = matrix(c(
#'     1, 0,
#'     0, 0
#'   ), 2, 2, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA,
#'     NA, 100
#'   ), 2, 2, TRUE),
#'   names.commodity = c("prod", "lab"),
#'   names.agent = c("firm", "laborer"),
#'   numeraire = "prod"
#' )
#'
#' ge.CD$p
#' ge.CD$z
#' ge.CD$D
#' ge.CD$S
#'
#' ## the same as above
#' ge2.CD <- sdm2(
#'   A = list(
#'     dst.firm = node_new(
#'       "output",
#'       type = "CD", alpha = 2, beta = c(0.5, 0.5),
#'       "prod", "lab"
#'     ),
#'     dst.consumer = node_new(
#'       "util",
#'       type = "Leontief", a = 1,
#'       "prod"
#'     )
#'   ),
#'   B = matrix(c(
#'     1, 0,
#'     0, 0
#'   ), 2, 2, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA,
#'     NA, 100
#'   ), 2, 2, TRUE),
#'   names.commodity = c("prod", "lab"),
#'   names.agent = c("firm", "laborer"),
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
#'     a1 <- SCES_A(es = 0.5, alpha = 1, Beta = c(0.5, 0.5), p = state$p)
#'     a2 <- c(1, 0)
#'     cbind(a1, a2)
#'   },
#'   B = matrix(c(
#'     1, 0,
#'     0, 0
#'   ), 2, 2, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA,
#'     NA, 100
#'   ), 2, 2, TRUE),
#'   names.commodity = c("prod", "lab"),
#'   names.agent = c("firm", "laborer"),
#'   numeraire = "prod"
#' )
#'
#' ge.SCES$p
#' ge.SCES$z
#' ge.SCES$D
#' ge.SCES$S
#'
#' ## the same as above
#' ge2.SCES <- sdm2(
#'   A = list(
#'     dst.firm = node_new(
#'       "output",
#'       type = "SCES",
#'       es = 0.5, alpha = 1, beta = c(0.5, 0.5),
#'       "prod", "lab"
#'     ),
#'     dst.consumer = node_new(
#'       "util",
#'       type = "Leontief", a = 1,
#'       "prod"
#'     )
#'   ),
#'   B = matrix(c(
#'     1, 0,
#'     0, 0
#'   ), 2, 2, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA,
#'     NA, 100
#'   ), 2, 2, TRUE),
#'   names.commodity = c("prod", "lab"),
#'   names.agent = c("firm", "laborer"),
#'   numeraire = "prod"
#' )
#'
#' ge2.SCES$p
#' ge2.SCES$z
#' ge2.SCES$D
#' ge2.SCES$S
#' }

gem_2_2 <- function(...) sdm2(...)
