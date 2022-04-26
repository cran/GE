#' @export
#' @title A General Equilibrium Model with Equity Shares
#' @aliases gemEquityShare_3_3
#' @description A general equilibrium model with equity shares and dividend.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' dst.firm <- node_new("output",
#'                      type = "FIN",
#'                      rate = c(1, dividend.rate = 0.25),
#'                      "cc1", "equity.share"
#' )
#' node_set(dst.firm, "cc1",
#'          type = "CD",
#'          alpha = 2, beta = c(0.5, 0.5),
#'          "prod", "lab"
#' )
#'
#' dst.laborer <- node_new("util",
#'                         type = "Leontief",  a = 1,
#'                         "prod"
#' )
#'
#' dst.shareholder <- Clone(dst.laborer)
#'
#' ge <- sdm2(
#'   A = list(dst.firm, dst.laborer, dst.shareholder),
#'   B = diag(c(1, 0, 0)),
#'   S0Exg = {
#'     S0Exg <- matrix(NA, 3, 3)
#'     S0Exg[2, 2] <- S0Exg[3, 3] <- 100
#'     S0Exg
#'   },
#'   names.commodity = c("prod", "lab", "equity.share"),
#'   names.agent = c("firm", "laborer", "shareholder"),
#'   numeraire = "prod"
#' )
#'
#' ge$p #The third component is the dividend per unit of share.
#' ge$DV
#' ge$SV
#' }

gemEquityShare_3_3 <- function(...) sdm2(...)
