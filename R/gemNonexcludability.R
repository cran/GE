#' @export
#' @title Some Examples Illustrating Non-excludability
#' @aliases gemNonexcludability
#' @description Some examples illustrating non-rival goods with non-excludability.
#' @param ... arguments to be passed to the function sdm2.
#' @seealso \cite{\link{gemNonrivalry_Uncongestiblity}}
#' @examples
#' \donttest{
#' dst.firm0 <- node_new(
#'   "non-rival services",
#'   type = "Leontief", a = 1,
#'   "labor"
#' )
#'
#' dst.consumer1 <- node_new(
#'   "util",
#'   type = "SCES", es = 1, # es = 0
#'   alpha = 1, beta = c(0.75, 0.25),
#'   "serv1", "labor"
#' )
#'
#' dst.consumer2 <- node_new(
#'   "util",
#'   type = "SCES", es = 1, # es = 0
#'   alpha = 1, beta = c(0.5, 0.5),
#'   "serv2", "labor"
#' )
#'
#' f.CD <- function(policy = NULL) {
#'   ge <- sdm2(
#'     A = list(dst.firm0, dst.consumer1, dst.consumer2),
#'     B = matrix(c(
#'       1, 0, 0,
#'       1, 0, 0,
#'       0, 0, 0
#'     ), 3, 3, TRUE),
#'     S0Exg = matrix(c(
#'       NA, NA, NA,
#'       NA, NA, NA,
#'       NA, 60, 60
#'     ), 3, 3, TRUE),
#'     names.commodity = c("serv1", "serv2", "labor"),
#'     names.agent = c("firm", "consumer1", "consumer2"),
#'     numeraire = "labor",
#'     policy = policy
#'   )
#'
#'   cat("ge$p:\n")
#'   print(round(ge$p, 5))
#'   cat("ge$z:\n")
#'   print(round(ge$z, 5))
#'   cat("ge$D:\n")
#'   print(addmargins(round(ge$D, 5), 2))
#'   cat("ge$S:\n")
#'   print(addmargins(round(ge$S, 5), 2))
#' }
#'
#' f.CD()
#'
#' # Suppose consumer 2 is a free rider.
#' policy.nonexcludability <- function(state) {
#'   state$S[2, 3] <- state$S[2, 1]
#'   state$S[2, 1] <- 0
#'   state
#' }
#' f.CD(policy.nonexcludability)
#'
#' ## Assume that both consumers have the same linear utility
#' # function x1 + 1.25 * x2, wherein x1 is the quantity
#' # of service and x2 is the quantity of labor.
#'
#' dst.firm1 <- node_new(
#'   "serv1",
#'   type = "Leontief", a = 0.8,
#'   "labor"
#' )
#'
#' dst.firm2 <- node_new(
#'   "serv2",
#'   type = "Leontief", a = 0.8,
#'   "labor"
#' )
#'
#' dst.consumer1 <- node_new(
#'   "util",
#'   type = "Leontief", a = 1,
#'   "serv1"
#' )
#'
#' dst.consumer2 <- node_new(
#'   "util",
#'   type = "Leontief", a = 1,
#'   "serv2"
#' )
#'
#' f.linear <- function(policy = NULL) {
#'   ge <- sdm2(
#'     A = list(
#'       dst.firm0, dst.consumer1, dst.consumer2,
#'       dst.firm1, dst.firm2
#'     ),
#'     B = matrix(c(
#'       1, 0, 0, 1, 0,
#'       1, 0, 0, 0, 1,
#'       0, 0, 0, 0, 0
#'     ), 3, 5, TRUE),
#'     S0Exg = matrix(c(
#'       NA, NA, NA, NA, NA,
#'       NA, NA, NA, NA, NA,
#'       NA, 60, 60, NA, NA
#'     ), 3, 5, TRUE),
#'     names.commodity = c("serv1", "serv2", "labor"),
#'     names.agent = c("firm.public", "consumer1", "consumer2", "firm1", "firm2"),
#'     numeraire = "labor",
#'     policy = policy
#'   )
#'
#'   cat("ge$p:\n")
#'   print(round(ge$p, 5))
#'   cat("ge$z:\n")
#'   print(round(ge$z, 5))
#'   cat("ge$D:\n")
#'   print(addmargins(round(ge$D, 5), 2))
#'   cat("ge$S:\n")
#'   print(addmargins(round(ge$S, 5), 2))
#' }
#'
#' f.linear()
#'
#' # Suppose consumer 2 is a free rider.
#' f.linear(policy.nonexcludability)
#' }

gemNonexcludability <- function(...) sdm2(...)
