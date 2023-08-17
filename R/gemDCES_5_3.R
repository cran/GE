#' @export
#' @title A Model with a Displaced CES Utility Function
#' @aliases gemDCES_5_3
#' @description A model with a displaced CES utility function (Zhang, 2008, page 134; Li, 2019, example 3.12, page 130).
#' @param ... arguments to be passed to the function sdm2.
#' @references LI Wu (2019, ISBN: 9787521804225) General Equilibrium and Structural Dynamics: Perspectives of New Structural Economics. Beijing: Economic Science Press. (In Chinese)
#' @references Zhang Jinshui (2008, ISBN: 9787040224818) Mathematical Economics. Beijing: Higher Education Press. (In Chinese)
#' @examples
#' \donttest{
#' ge <- sdm2(
#'   A = function(state) {
#'     a.firm1 <- CD_A(alpha = 1, Beta = c(0, 0, 0.5, 0.5, 0), state$p)
#'     a.firm2 <- CD_A(alpha = 2, Beta = c(0, 0, 0.5, 0, 0.5), state$p)
#'     a.consumer <- DCES_demand(
#'       es = 1,
#'       beta = c(1 / 3, 1 / 3, 1 / 3, 0, 0),
#'       xi = c(0, 0, 0.4, 0, 0),
#'       w = state$w[3] / 10^4,
#'       p = state$p
#'     )
#'     cbind(a.firm1, a.firm2, a.consumer)
#'   },
#'   B = matrix(c(
#'     1, 0, 0,
#'     0, 1, 0,
#'     0, 0, 0,
#'     0, 0, 0,
#'     0, 0, 0
#'   ), 5, 3, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA, NA,
#'     NA, NA, NA,
#'     NA, NA, 10000,
#'     NA, NA, 1,
#'     NA, NA, 1
#'   ), 5, 3, TRUE),
#'   names.commodity = c("prod1", "prod2", "lab", "land1", "land2"),
#'   names.agent = c("firm1", "firm2", "consumer"),
#'   numeraire = "lab"
#' )
#'
#' ge$p
#' ge$z
#' }

gemDCES_5_3 <- function(...) sdm2(...)
