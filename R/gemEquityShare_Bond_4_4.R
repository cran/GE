#' @export
#' @title A General Equilibrium Model with Equity Shares and Bond
#' @aliases gemEquityShare_Bond_4_4
#' @description A general equilibrium model with equity shares and bond.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' dst.firm <- node_new("output",
#'                      type = "FIN",
#'                      rate = c(1, dividend.rate = 0.15, bond.yield.rate = 0.1),
#'                      "cc1", "equity.share", "bond"
#' )
#' node_set(dst.firm, "cc1",
#'          type = "CD",
#'          alpha = 2, beta = c(0.5, 0.5),
#'          "prod", "lab"
#' )
#'
#' dst.laborer <- dst.shareholder <- dst.bondholder <-
#'   node_new("util",
#'            type = "Leontief",  a = 1,
#'            "prod"
#'   )
#'
#' ge <- sdm2(
#'   A = list(dst.firm, dst.laborer, dst.shareholder, dst.bondholder),
#'   B = diag(c(1, 0, 0, 0)),
#'   S0Exg = {
#'     S0Exg <- matrix(NA, 4, 4)
#'     S0Exg[2, 2] <- S0Exg[3, 3] <-
#'       S0Exg[4, 4] <- 100
#'     S0Exg
#'   },
#'   names.commodity = c("prod", "lab", "equity.share", "bond"),
#'   names.agent = c("firm", "laborer", "shareholder", "bondholder"),
#'   numeraire = "prod"
#' )
#'
#' ge$p
#' ge$DV
#' ge$SV
#' }

gemEquityShare_Bond_4_4 <- function(...) sdm2(...)
