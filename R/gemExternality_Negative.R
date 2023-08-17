#' @export
#' @title Some Examples Illustrating Negative Externality
#' @aliases gemExternality_Negative
#' @description Some examples illustrating negative externality.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' #### negative externality of consumption to consumption
#' # Here the distortion means that an agent
#' # will use environmental resources for free.
#' dst.consumer1.distorted <- node_new("util",
#'   type = "CD",
#'   alpha = 1, beta = c(0.5, 0.5),
#'   "lab", "land"
#' )
#'
#' dst.consumer1 <- node_new("util",
#'   type = "CD",
#'   alpha = 1, beta = c(0.5, 0.5),
#'   "lab", "cc1"
#' )
#' node_set(dst.consumer1, "cc1",
#'   type = "Leontief",
#'   a = c(1, 1),
#'   "land", "env"
#' )
#'
#' dst.consumer2 <- node_new(
#'   "util",
#'   type = "CD",
#'   alpha = 1, beta = c(0.4, 0.4, 0.2),
#'   "lab", "land", "env"
#' )
#'
#' ge.externality <- sdm2(
#'   A = list(dst.consumer1.distorted, dst.consumer2),
#'   B = matrix(0, 3, 2),
#'   S0Exg = matrix(c(
#'     50, 0,
#'     0, 50,
#'     0, 0
#'   ), 3, 2, TRUE),
#'   names.commodity = c("lab", "land", "env"),
#'   names.agent = c("consumer1.distorted", "consumer2"),
#'   numeraire = "lab",
#'   policy = function(state) {
#'     last.D <- state$last.A %*% dg(state$last.z)
#'     state$S[3, 2] <- 100 - last.D[2, 1]
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
#'   B = matrix(0, 3, 2),
#'   S0Exg = matrix(c(
#'     50, 0,
#'     0, 50,
#'     13, 87
#'   ), 3, 2, TRUE),
#'   names.commodity = c("lab", "land", "env"),
#'   names.agent = c("consumer1", "consumer2"),
#'   numeraire = "lab"
#' )
#'
#' ge$p
#' ge$z
#' addmargins(ge$D, 2)
#' addmargins(ge$S, 2)
#'
#' ## A corrective tax is imposed on distorted consumer 1.
#' # 54% of tax revenue is allocated to distorted consumers 1, 46% to consumers 2.
#' dst.consumer1.distorted.taxed <- node_new("util",
#'                                           type = "CD",
#'                                           alpha = 1, beta = c(0.5, 0.5),
#'                                           "lab", "cc1"
#' )
#' node_set(dst.consumer1.distorted.taxed, "cc1",
#'          type = "FIN", rate = c(1, ge$DV[3, 1] / ge$DV[2, 1]),
#'          "land", "tax"
#' )
#'
#' ge.corrective.tax <- sdm2(
#'   A = list(dst.consumer1.distorted.taxed, dst.consumer2),
#'   B = matrix(0, 4, 2),
#'   S0Exg = matrix(c(
#'     50, 0,
#'     0, 50,
#'     0, 0,
#'     54, 46
#'   ), 4, 2, TRUE),
#'   names.commodity = c("lab", "land", "env", "tax"),
#'   names.agent = c("consumer1.distorted", "consumer2"),
#'   numeraire = "lab",
#'   policy = function(state) {
#'     last.D <- state$last.A %*% dg(state$last.z)
#'     state$S[3, 2] <- 100 - last.D[2, 1]
#'     state
#'   }
#' )
#'
#' ge.corrective.tax$z
#' addmargins(ge.corrective.tax$D, 2)
#' addmargins(ge.corrective.tax$S, 2)
#'
#' ## negative externality of production to consumption
#' dst.firm1.distorted <- dst.consumer1.distorted
#' dst.firm1 <- dst.consumer1
#'
#' dst.consumer1.Leontief <- node_new(
#'   "util",
#'   type = "Leontief",
#'   a = 1,
#'   "prod1"
#' )
#'
#' ge.externality <- sdm2(
#'   A = list(dst.firm1.distorted, dst.consumer1.Leontief, dst.consumer2),
#'   B = diag(c(1, 0, 0), 4, 3),
#'   S0Exg = matrix(c(
#'     NA, NA, NA,
#'     NA, 50, NA,
#'     NA, NA, 50,
#'     NA, NA, NA
#'   ), 4, 3, TRUE),
#'   names.commodity = c("prod1", "lab", "land", "env"),
#'   names.agent = c("firm1", "consumer1.Leontief", "consumer2"),
#'   numeraire = "lab",
#'   policy = function(state) {
#'     last.D <- state$last.A %*% dg(state$last.z)
#'     state$S[4, 3] <- 100 - last.D[3, 1]
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
#'   A = list(dst.firm1, dst.consumer1.Leontief, dst.consumer2),
#'   B = diag(c(1, 0, 0), 4, 3),
#'   S0Exg = matrix(c(
#'     NA, NA, NA,
#'     NA, 50, NA,
#'     NA, NA, 50,
#'     NA, 13, 87
#'   ), 4, 3, TRUE),
#'   names.commodity = c("prod1", "lab", "land", "env"),
#'   names.agent = c("firm1", "consumer1.Leontief", "consumer2"),
#'   numeraire = "lab"
#' )
#'
#' ge$p
#' ge$z
#' addmargins(ge$D, 2)
#' addmargins(ge$S, 2)
#'
#' #### negative externality of consumption to production and consumption
#' dst.firm2 <- dst.consumer2
#' dst.consumer2.Leontief <- node_new(
#'   "util",
#'   type = "Leontief",
#'   a = 1,
#'   "prod2"
#' )
#'
#' ge.externality <- sdm2(
#'   A = list(dst.firm2, dst.consumer1.distorted, dst.consumer2.Leontief),
#'   B = diag(c(1, 0, 0), 4, 3),
#'   S0Exg = matrix(c(
#'     NA, NA, NA,
#'     NA, 50, NA,
#'     NA, NA, 50,
#'     NA, NA, NA
#'   ), 4, 3, TRUE),
#'   names.commodity = c("prod2", "lab", "land", "env"),
#'   names.agent = c("firm2", "consumer1", "consumer2.Leontief"),
#'   numeraire = "lab",
#'   policy = function(state) {
#'     last.D <- state$last.A %*% dg(state$last.z)
#'     state$S[4, 3] <- 100 - last.D[3, 2]
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
#'   A = list(dst.firm2, dst.consumer1, dst.consumer2.Leontief),
#'   B = diag(c(1, 0, 0), 4, 3),
#'   S0Exg = matrix(c(
#'     NA, NA, NA,
#'     NA, 50, NA,
#'     NA, NA, 50,
#'     NA, 13, 87
#'   ), 4, 3, TRUE),
#'   names.commodity = c("prod2", "lab", "land", "env"),
#'   names.agent = c("firm2", "consumer1", "consumer2.Leontief"),
#'   numeraire = "lab"
#' )
#'
#' ge$p
#' ge$z
#' addmargins(ge$D, 2)
#' addmargins(ge$S, 2)
#'
#' ## negative externality of production to production and consumption
#' ge.externality <- sdm2(
#'   A = list(dst.firm1.distorted, dst.firm2, dst.consumer1.Leontief, dst.consumer2.Leontief),
#'   B = diag(c(1, 1, 0, 0), 5, 4),
#'   S0Exg = matrix(c(
#'     NA, NA, NA, NA,
#'     NA, NA, NA, NA,
#'     NA, NA, 50, NA,
#'     NA, NA, NA, 50,
#'     NA, NA, NA, NA
#'   ), 5, 4, TRUE),
#'   names.commodity = c("prod1", "prod2", "lab", "land", "env"),
#'   names.agent = c("firm1", "firm2", "consumer1.Leontief", "consumer2.Leontief"),
#'   numeraire = "lab",
#'   policy = function(state) {
#'     last.D <- state$last.A %*% dg(state$last.z)
#'     state$S[5, 4] <- 100 - last.D[4, 1]
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
#'   A = list(dst.firm1, dst.firm2, dst.consumer1.Leontief, dst.consumer2.Leontief),
#'   B = diag(c(1, 1, 0, 0), 5, 4),
#'   S0Exg = matrix(c(
#'     NA, NA, NA, NA,
#'     NA, NA, NA, NA,
#'     NA, NA, 50, NA,
#'     NA, NA, NA, 50,
#'     NA, NA, 13, 87
#'   ), 5, 4, TRUE),
#'   names.commodity = c("prod1", "prod2", "lab", "land", "env"),
#'   names.agent = c("firm1", "firm2", "consumer1.Leontief", "consumer2.Leontief"),
#'   numeraire = "lab"
#' )
#'
#' ge$p
#' ge$z
#' addmargins(ge$D, 2)
#' addmargins(ge$S, 2)
#'
#' ## negative externality of consumption to production
#' dst.firm2.distorted <- node_new(
#'   "util",
#'   type = "CD",
#'   alpha = 1, beta = c(0.5, 0.5),
#'   "lab", "land"
#' )
#'
#' ge.externality <- sdm2(
#'   A = list(dst.firm2.distorted, dst.consumer1.distorted, dst.consumer2.Leontief),
#'   B = diag(c(1, 0, 0)),
#'   S0Exg = matrix(c(
#'     NA, NA, NA,
#'     NA, 50, NA,
#'     NA, NA, 50
#'   ), 3, 3, TRUE),
#'   names.commodity = c("prod2", "lab", "land"),
#'   names.agent = c("firm2.distorted", "consumer1", "consumer2.Leontief"),
#'   numeraire = "lab",
#'   policy = function(A, state) {
#'     last.D <- state$last.A %*% dg(state$last.z)
#'     state$S[1, 1] <- (100 - last.D[3, 2])^0.2 * state$S[1, 1]^0.8
#'     state
#'   }
#' )
#'
#' ge.externality$p
#' ge.externality$z
#' addmargins(ge.externality$D, 2)
#' addmargins(ge.externality$S, 2)
#' }

gemExternality_Negative <- function(...) sdm2(...)
