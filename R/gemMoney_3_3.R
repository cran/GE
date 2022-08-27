#' @export
#' @title Some 3-by-3 General Equilibrium Models with Money
#' @aliases gemMoney_3_3
#' @description Some 3-by-3 general equilibrium models with money as a medium of exchange and a means of payment.
#' @param ... arguments to be passed to the function sdm2.
#' @references LI Wu (2019, ISBN: 9787521804225) General Equilibrium and Structural Dynamics: Perspectives of New Structural Economics. Beijing: Economic Science Press. (In Chinese)
#' @examples
#' \donttest{
#' interest.rate <- 0.25
#'
#' dst.firm <- node_new("prod",
#'                      type = "FIN", rate = c(1, interest.rate),
#'                      "cc1", "money"
#' )
#' node_set(dst.firm, "cc1",
#'          type = "CD", alpha = 2, beta = c(0.5, 0.5),
#'          "prod", "lab"
#' )
#'
#' dst.laborer <- dst.money.owner <-
#'   node_new("util",
#'            type = "FIN", rate = c(1, interest.rate),
#'            "prod", "money"
#'   )
#'
#' ge <- sdm2(
#'   A = list(dst.firm, dst.laborer, dst.money.owner),
#'   B = diag(c(1, 0, 0)),
#'   S0Exg = matrix(c(
#'     NA, NA, NA,
#'     NA, 100, NA,
#'     NA, NA, 100
#'   ), 3, 3, TRUE),
#'   names.commodity = c("prod", "lab", "money"),
#'   names.agent = c("firm", "laborer", "money.owner"),
#'   numeraire = "prod"
#' )
#'
#' ge$p
#' ge$z
#' ge$D
#' ge$S
#' ge$DV
#' ge$SV
#'
#' ## Take money as numeraire, that is, let the asset price of money equal to 1,
#' ## and let the interest per unit of money equal to the exogenous interest rate.
#' ge2 <- sdm2(
#'   A = list(dst.firm, dst.laborer, dst.money.owner),
#'   B = diag(c(1, 0, 0)),
#'   S0Exg = matrix(c(
#'     NA, NA, NA,
#'     NA, 100, NA,
#'     NA, NA, 100
#'   ), 3, 3, TRUE),
#'   names.commodity = c("prod", "lab", "money"),
#'   names.agent = c("firm", "laborer", "money.owner"),
#'   numeraire = c(money = interest.rate)
#' )
#'
#' ge2$p
#' ge2$z
#' ge2$D
#' ge2$S
#' ge2$DV
#' ge2$SV
#'
#' #### another model (Li, 2019, example 7.2)
#' interest.rate <- 0.25
#' dst <- node_new("demand",
#'                 type = "FIN", rate = c(1, interest.rate),
#'                 "cc1", "money"
#' )
#' node_set(dst, "cc1",
#'          type = "CD", alpha = 1, beta = c(0.5, 0.5),
#'          "prod", "lab"
#' )
#'
#' ge <- sdm2(
#'   A = list(dst, dst, dst),
#'   B = diag(c(1, 0, 0)),
#'   S0Exg = matrix(c(
#'     NA, NA, NA,
#'     NA, 100, NA,
#'     NA, NA, 100
#'   ), 3, 3, TRUE),
#'   names.commodity = c("prod", "lab", "money"),
#'   names.agent = c("firm", "laborer", "money.lender"),
#'   numeraire = c(money = interest.rate)
#' )
#'
#' ge$p
#' ge$z
#' ge$D
#' ge$S
#' ge$DV
#' ge$SV
#' }

gemMoney_3_3 <- function(...) sdm2(...)
