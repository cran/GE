#' @export
#' @title Some Simple 2-by-2 General Equilibrium Models
#' @aliases gem_2_2
#' @description Some simple 2-by-2 general equilibrium models with a firm and a laborer.
#' @param ... arguments to be passed to the function sdm2.
#' @references http://www.econ.ucla.edu/riley/MAE/Course/SolvingForTheWE.pdf
#' @examples
#' \donttest{
#' #### a 2-by-2 general equilibrium model with a Leontief production function.
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
#' addmargins(ge.Leontief$D, 2)
#' addmargins(ge.Leontief$S, 2)
#'
#' ## the same as above.
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
#' addmargins(ge2.Leontief$D, 2)
#' addmargins(ge2.Leontief$S, 2)
#'
#' #### a 2-by-2 general equilibrium model with a CD production function.
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
#' addmargins(ge.CD$D, 2)
#' addmargins(ge.CD$S, 2)
#'
#' ## the same as above.
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
#' addmargins(ge2.CD$D, 2)
#' addmargins(ge2.CD$S, 2)
#'
#' #### a 2-by-2 general equilibrium model with a SCES production function.
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
#' addmargins(ge.SCES$D, 2)
#' addmargins(ge.SCES$S, 2)
#'
#' ## the same as above.
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
#' addmargins(ge2.SCES$D, 2)
#' addmargins(ge2.SCES$S, 2)
#'
#' #### a 2-by-2 general equilibrium model with a CESAK production function.
#' ge.CESAK <- sdm2(
#'   A = function(state) {
#'     a.firm <- CESAK_dc(alpha = 2, betaK = 0.5, alphaK = 0.5, p = state$p, es = 1)
#'     a.consumer <- c(1, 0)
#'     cbind(a.firm, a.consumer)
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
#'   names.agent = c("firm", "consumer"),
#'   numeraire = "prod"
#' )
#'
#' ge.CESAK$p
#' ge.CESAK$z
#' addmargins(ge.CESAK$D, 2)
#' addmargins(ge.CESAK$S, 2)
#'
#' ## the same as above.
#' ge2.CESAK <- sdm2(
#'   A = list(
#'     dst.firm = node_new(
#'       "output",
#'       type = "CESAK", es = 1,
#'       alpha = 2, betaK = 0.5, alphaK = 0.5,
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
#'   names.agent = c("firm", "consumer"),
#'   numeraire = "prod"
#' )
#'
#' ge2.CESAK$p
#' ge2.CESAK$z
#' addmargins(ge2.CESAK$D, 2)
#' addmargins(ge2.CESAK$S, 2)
#'
#' #### Example 1 in the ucla reference.
#' ge3.SCES <- sdm2(
#'   A = function(state) {
#'     a.firm <- c(0, 0.25)
#'     a.consumer <- SCES_A(es = 0.5, alpha = 1, Beta = c(0.5, 0.5), p = state$p)
#'     cbind(a.firm, a.consumer)
#'   },
#'   B = matrix(c(
#'     1, 0,
#'     0, 0
#'   ), 2, 2, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA,
#'     NA, 30
#'   ), 2, 2, TRUE),
#'   names.commodity = c("prod", "lab"),
#'   names.agent = c("firm", "laborer"),
#'   numeraire = "prod"
#' )
#'
#' ge3.SCES$p
#' ge3.SCES$z
#' ge3.SCES$D
#' ge3.SCES$S
#'
#' #### The laborer has some product.
#' ge <- sdm2(
#'   A = function(state) {
#'     a.firm <- c(0, 1) # c(0, 2)
#'     a.consumer <- SCES_A(es = 0.5, alpha = 1, Beta = c(0.5, 0.5), p = state$p)
#'     cbind(a.firm, a.consumer)
#'   },
#'   B = matrix(c(
#'     1, 0,
#'     0, 0
#'   ), 2, 2, TRUE),
#'   S0Exg = matrix(c(
#'     NA, 50, # 500
#'     NA, 100
#'   ), 2, 2, TRUE),
#'   names.commodity = c("prod", "lab"),
#'   names.agent = c("firm", "laborer"),
#'   numeraire = "prod"
#' )
#'
#' ge$p
#' ge$z
#' ge$D
#' ge$S
#' }

gem_2_2 <- function(...) sdm2(...)
