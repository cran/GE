#' @export
#' @title Some Examples Illustrating Positive Externality
#' @aliases gemExternality_Positive
#' @description Some examples illustrating positive externality.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' #### positive externality of consumption to consumption
#' dst.consumer1 <- node_new("util",
#'   type = "Leontief", a = 1,
#'   "lab"
#' )
#'
#' dst.consumer2 <- node_new(
#'   "util",
#'   type = "CD",
#'   alpha = 1, beta = c(0.2, 0.8), # c(0.8, 0.2),
#'   "lab", "byproduct"
#' )
#'
#' ge.externality <- sdm2(
#'   A = list(dst.consumer1, dst.consumer2),
#'   B = matrix(0, 2, 2),
#'   S0Exg = matrix(c(
#'     50, 50,
#'     0, 0
#'   ), 2, 2, TRUE),
#'   names.commodity = c("lab", "byproduct"),
#'   names.agent = c("consumer1", "consumer2"),
#'   numeraire = "lab",
#'   policy = function(state) {
#'     state$S[2, 2] <- state$last.z[1]
#'     state
#'   }
#' )
#'
#' ge.externality$p
#' ge.externality$z
#' addmargins(ge.externality$D, 2)
#' addmargins(ge.externality$S, 2)
#'
#' ge <- sdm2(
#'   A = list(dst.consumer1, dst.consumer2),
#'   B = matrix(0, 2, 2),
#'   S0Exg = matrix(c(
#'     50, 50,
#'     0, 0
#'   ), 2, 2, TRUE),
#'   names.commodity = c("lab", "byproduct"),
#'   names.agent = c("consumer1", "consumer2"),
#'   numeraire = "lab",
#'   policy = function(state) {
#'     state$S[2, 1] <- state$last.z[1]
#'     state
#'   }
#' )
#'
#' ge$p
#' ge$z
#' addmargins(ge$D, 2)
#' addmargins(ge$S, 2)
#'
#' ## positive externality of production to consumption
#' dst.firm1 <- node_new("prod1",
#'   type = "Leontief", a = 1,
#'   "lab"
#' )
#'
#' dst.consumer3 <- node_new("util",
#'   type = "Leontief", a = 1,
#'   "prod1"
#' )
#'
#' dst.consumer2 <- node_new(
#'   "util",
#'   type = "CD",
#'   alpha = 1, beta = c(0.2, 0.8),
#'   "lab", "byproduct"
#' )
#'
#' ge.externality <- sdm2(
#'   A = list(dst.firm1, dst.consumer3, dst.consumer2),
#'   B = matrix(c(
#'     1, 0, 0,
#'     0, 0, 0,
#'     0, 0, 0
#'   ), 3, 3, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA, NA,
#'     NA, 50, 50,
#'     NA, NA, NA
#'   ), 3, 3, TRUE),
#'   names.commodity = c("prod1", "lab", "byproduct"),
#'   names.agent = c("firm1", "consumer3", "consumer2"),
#'   numeraire = "lab",
#'   policy = function(state) {
#'     state$S[3, 3] <- state$last.z[1]
#'     state
#'   }
#' )
#'
#' ge.externality$p
#' ge.externality$z
#' addmargins(ge.externality$D, 2)
#' addmargins(ge.externality$S, 2)
#'
#' ge <- sdm2(
#'   A = list(dst.firm1, dst.consumer3, dst.consumer2),
#'   B = matrix(c(
#'     1, 0, 0,
#'     0, 0, 0,
#'     1, 0, 0
#'   ), 3, 3, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA, NA,
#'     NA, 50, 50,
#'     NA, NA, NA
#'   ), 3, 3, TRUE),
#'   names.commodity = c("prod1", "lab", "byproduct"),
#'   names.agent = c("firm1", "consumer3", "consumer2"),
#'   numeraire = "lab"
#' )
#'
#' ge$p
#' ge$z
#' addmargins(ge$D, 2)
#' addmargins(ge$S, 2)
#'
#' ## positive externality of consumption to production
#' dst.consumer1 <- node_new("util",
#'   type = "Leontief", a = 1,
#'   "lab"
#' )
#'
#' dst.firm2.distorted <- node_new(
#'   "prod2",
#'   type = "Leontief", a = 1,
#'   "lab"
#' )
#'
#' dst.consumer2.Leontief <- node_new("util",
#'   type = "Leontief", a = 1,
#'   "prod2"
#' )
#'
#' ge.externality <- sdm2(
#'   A = list(dst.firm2.distorted, dst.consumer1, dst.consumer2.Leontief),
#'   B = matrix(c(
#'     1, 0, 0,
#'     0, 0, 0
#'   ), 2, 3, TRUE),
#'   S0Exg = matrix(c(
#'     0, 0, 0,
#'     0, 50, 50
#'   ), 2, 3, TRUE),
#'   names.commodity = c("prod2", "lab"),
#'   names.agent = c("firm2.distorted", "consumer1", "dst.consumer2.Leontief"),
#'   numeraire = "lab",
#'   policy = function(state) {
#'     state$S[1, 1] <- state$last.z[1]^0.2 * state$last.z[2]^0.8
#'     state
#'   }
#' )
#'
#' ge.externality$p
#' ge.externality$z
#' addmargins(ge.externality$D, 2)
#' addmargins(ge.externality$S, 2)
#' }

gemExternality_Positive <- function(...) sdm2(...)
