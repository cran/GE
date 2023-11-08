#' @export
#' @title Overlapping Generations Financial Models with Two Firms
#' @aliases gemOLGF_TwoFirms
#' @description Some examples of overlapping generations financial models with two firms.
#' @param ... arguments to be passed to the function sdm2.
#' @seealso {
#' \code{\link{gemOLGF_PureExchange}}
#' }
#' @examples
#' \donttest{
#' #### an example with two-period-lived consumers
#' dst.firm.corn <- node_new(
#'   "corn",
#'   type = "CD", alpha = 5,
#'   beta = c(1 / 2, 1 / 2),
#'   "iron", "lab"
#' )
#'
#' dst.firm.iron <- node_new(
#'   "iron",
#'   type = "CD", alpha = 5,
#'   beta = c(1 / 2, 1 / 2),
#'   "iron", "lab"
#' )
#'
#' dst.age1 <- node_new(
#'   "util",
#'   type = "FIN",
#'   rate = c(1, ratio.saving.consumption = 1),
#'   "corn", "secy" # security, the financial instrument
#' )
#'
#' dst.age2 <- node_new(
#'   "util",
#'   type = "Leontief", a = 1,
#'   "corn"
#' )
#'
#' ge <- sdm2(
#'   A = list(
#'     dst.firm.corn, dst.firm.iron, dst.age1, dst.age2
#'   ),
#'   B = matrix(c(
#'     1, 0, 0, 0,
#'     0, 1, 0, 0,
#'     0, 0, 0, 0,
#'     0, 0, 0, 0
#'   ), 4, 4, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA, NA, NA,
#'     NA, NA, NA, NA,
#'     NA, NA, 1, NA,
#'     NA, NA, NA, 1
#'   ), 4, 4, TRUE),
#'   names.commodity = c("corn", "iron", "lab", "secy"),
#'   names.agent = c("firm.corn", "firm.iron", "age1", "age2"),
#'   numeraire = "lab"
#' )
#'
#' ge$p
#' ge$D
#' ge$DV
#' ge$S
#'
#' ## an example with three-period-lived consumers
#' dst.age1$rate <- c(1, ratio.saving.consumption = 1 / 2)
#'
#' dst.age3 <- Clone(dst.age2)
#'
#' dst.age2 <- Clone(dst.age1)
#' dst.age2$rate <- c(1, ratio.saving.consumption = 1)
#'
#' ge <- sdm2(
#'   A = list(
#'     dst.firm.corn, dst.firm.iron, dst.age1, dst.age2, dst.age3
#'   ),
#'   B = matrix(c(
#'     1, 0, 0, 0, 0,
#'     0, 1, 0, 0, 0,
#'     0, 0, 0, 0, 0,
#'     0, 0, 0, 0, 0
#'   ), 4, 5, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA, NA, NA, NA,
#'     NA, NA, NA, NA, NA,
#'     NA, NA, 1, 1, NA,
#'     NA, NA, NA, 1, 1
#'   ), 4, 5, TRUE),
#'   names.commodity = c("corn", "iron", "lab", "secy"),
#'   names.agent = c("firm.corn", "firm.iron", "age1", "age2", "age3"),
#'   numeraire = "lab",
#'   policy = function(time, state) {
#'     # Assume that unsold security will be void.
#'     last.Demand <- state$last.A %*% dg(state$last.z)
#'     secy.holding <- prop.table(last.Demand[4, ])
#'     if (time > 1) {
#'       state$S[4, 4:5] <- secy.holding[3:4]
#'     }
#'     state
#'   }
#' )
#'
#' ge$p
#' ge$D
#' ge$DV
#' ge$S
#' }

gemOLGF_TwoFirms <- function(...) sdm2(...)
