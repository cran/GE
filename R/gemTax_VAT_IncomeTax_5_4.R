#' @export
#' @title A General Equilibrium Model with Value-added Tax and Income Tax
#' @aliases gemTax_VAT_IncomeTax_5_4
#' @description A general equilibrium model with value-added tax and income tax.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' vat.rate <- 0.2 # value-added tax rate
#' income.tax.rate <- 0.2
#'
#' dst.manu <- node_new("output",
#'                      type = "FIN",
#'                      rate = c(5 / 6, vat.rate),
#'                      "cc1", "vat"
#' )
#' node_set(dst.manu, "cc1",
#'          type = "SCES",
#'          es = 0.5, alpha = 1,
#'          beta = c(0.25, 0.75),
#'          "lab", "cap"
#' )
#'
#' dst.serv <- node_new("output",
#'                      type = "FIN",
#'                      rate = c(5 / 6, vat.rate),
#'                      "cc1", "vat"
#' )
#' node_set(dst.serv, "cc1",
#'          type = "SCES",
#'          es = 0.5, alpha = 1,
#'          beta = c(0.75, 0.25),
#'          "lab", "cap"
#' )
#'
#' dst.household <- node_new("util",
#'                           type = "SCES",
#'                           es = 0.5, alpha = 1,
#'                           beta = c(0.2, 0.8),
#'                           "manu", "serv"
#' )
#'
#' dst.government <- node_new("util",
#'                           type = "SCES",
#'                           es = 0.5, alpha = 1,
#'                           beta = c(0.5, 0.5),
#'                           "manu", "serv"
#' )
#'
#' dstl <- list(dst.manu, dst.serv, dst.household, dst.government)
#'
#' ge <- sdm2(
#'   A = dstl,
#'   B = diag(c(1, 1, 0, 0), 5, 4),
#'   S0Exg = matrix(c(
#'     NA, NA, NA, NA,
#'     NA, NA, NA, NA,
#'     NA, NA, 360 * (1 - income.tax.rate), 360 * income.tax.rate,
#'     NA, NA, 240 * (1 - income.tax.rate), 240 * income.tax.rate,
#'     NA, NA, NA, 120
#'   ), 5, 4, TRUE),
#'   names.commodity = c("manu", "serv", "lab", "cap", "vat"),
#'   names.agent = c("manu", "serv", "household", "government"),
#'   numeraire = "lab"
#' )
#'
#' ge$p
#' ge$z
#' addmargins(ge$DV)
#' addmargins(ge$SV)
#'
#' ## VAT rate reduction
#' dst.manu$rate <- dst.serv$rate <- c(5 / 6, vat.rate = 0.1)
#'
#' ge.new <- sdm2(
#'   A = dstl,
#'   B = diag(c(1, 1, 0, 0), 5, 4),
#'   S0Exg = matrix(c(
#'     NA, NA, NA, NA,
#'     NA, NA, NA, NA,
#'     NA, NA, 360 * (1 - income.tax.rate), 360 * income.tax.rate,
#'     NA, NA, 240 * (1 - income.tax.rate), 240 * income.tax.rate,
#'     NA, NA, NA, 120
#'   ), 5, 4, TRUE),
#'   names.commodity = c("manu", "serv", "lab", "cap", "vat"),
#'   names.agent = c("manu", "serv", "household", "government"),
#'   numeraire = "lab"
#' )
#'
#' ge.new$p
#' ge.new$z
#' addmargins(ge.new$DV)
#' addmargins(ge.new$SV)
#' }

gemTax_VAT_IncomeTax_5_4 <- function(...) sdm2(...)
