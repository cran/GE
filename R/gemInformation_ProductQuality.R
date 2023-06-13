#' @export
#' @title An Example Illustrating Product Quality Information
#' @aliases gemInformation_ProductQuality
#' @description An examples illustrating product quality information.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' dst.firm.normal <- node_new("normal prod",
#'   type = "Leontief", a = 1,
#'   "lab"
#' )
#'
#' dst.firm.inferior <- node_new("inferior prod",
#'   type = "Leontief", a = 0.5,
#'   "lab"
#' )
#'
#' dst.quasifirm <- node_new("normal prod",
#'   type = "Leontief", a = 1,
#'   # a = 1 means that consumers cannot distinguish between normal and inferior products.
#'   # In this case, the calculated consumer utility is nominal.
#'   # The real utility of the consumer is lower than the nominal utility.
#'   # a = 10 is the opposite.
#'   "inferior prod"
#' )
#'
#' dst.consumer <- node_new("util",
#'   type = "CD",
#'   alpha = 1, beta = c(0.5, 0.5),
#'   "lab", "normal prod"
#' )
#'
#' ge <- sdm2(
#'   A = list(dst.firm.normal, dst.firm.inferior, dst.quasifirm, dst.consumer),
#'   B = matrix(c(
#'     1, 0, 1, 0,
#'     0, 1, 0, 0,
#'     0, 0, 0, 0
#'   ), 3, 4, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA,  NA, NA,
#'     NA, NA,  NA, NA,
#'     NA, NA,  NA, 100
#'   ), 3, 4, TRUE),
#'   names.commodity = c("normal prod", "inferior prod", "lab"),
#'   names.agent = c("normal firm", "inferior firm", "quasifirm", "consumer"),
#'   numeraire = "lab",
#'   maxIteration = 1,
#'   numberOfPeriods = 800
#' )
#'
#' ge$p
#' ge$z
#' addmargins(ge$D, 2)
#' addmargins(ge$S, 2)
#' }

gemInformation_ProductQuality <- function(...) sdm2(...)
