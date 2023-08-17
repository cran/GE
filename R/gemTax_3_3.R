#' @export
#' @title Some General Equilibrium Models with Tax
#' @aliases gemTax_3_3
#' @description Some general equilibrium models with tax.
#' @param ... arguments to be passed to the function sdm2.
#' @references LI Wu (2019, ISBN: 9787521804225) General Equilibrium and Structural Dynamics: Perspectives of New Structural Economics. Beijing: Economic Science Press. (In Chinese)
#' @examples
#' \donttest{
#' #### the turnover tax.
#' dst.firm <- node_new("prod",
#'                      type = "FIN",
#'                      rate = c(1, tax.rate = 0.25),
#'                      "cc1", "tax"
#' )
#' node_set(dst.firm, "cc1",
#'          type = "CD",
#'          alpha = 2, beta = c(0.5, 0.5),
#'          "prod", "lab"
#' )
#'
#' dst.laborer <- dst.government <-
#'   node_new("util",
#'     type = "Leontief",
#'     a = 1,
#'     "prod"
#'   )
#'
#' ge.TT <- sdm2(
#'   A = list(dst.firm, dst.laborer, dst.government),
#'   B = diag(c(1, 0, 0)),
#'   S0Exg = matrix(c(
#'     NA, NA, NA,
#'     NA, 100, NA,
#'     NA, NA, 100
#'   ), 3, 3, TRUE),
#'   names.commodity = c("prod", "lab", "tax"),
#'   names.agent = c("firm", "laborer", "government"),
#'   numeraire = "prod"
#' )
#'
#' ge.TT$p
#' ge.TT$z
#' ge.TT$D
#' ge.TT$S
#'
#' #### the product tax.
#' dst.taxed.prod <- node_new("taxed.prod",
#'                            type = "FIN",
#'                            rate = c(1, tax.rate = 0.25),
#'                            "prod", "tax"
#' )
#'
#' dst.firm <- node_new("prod",
#'                      type = "CD",
#'                      alpha = 2, beta = c(0.5, 0.5),
#'                      dst.taxed.prod, "lab"
#' )
#'
#' dst.laborer <- dst.government <-
#'   node_new("util",
#'     type = "Leontief",
#'     a = 1,
#'     dst.taxed.prod
#'   )
#'
#' ge.PT <- sdm2(
#'   A = list(dst.firm, dst.laborer, dst.government),
#'   B = diag(c(1, 0, 0)),
#'   S0Exg = matrix(c(
#'     NA, NA, NA,
#'     NA, 100, NA,
#'     NA, NA, 100
#'   ), 3, 3, TRUE),
#'   names.commodity = c("prod", "lab", "tax"),
#'   names.agent = c("firm", "laborer", "government"),
#'   numeraire = "prod"
#' )
#'
#' ge.PT$p
#' ge.PT$z
#' ge.PT$D
#' ge.PT$S
#'
#' #### the consumption tax.
#' dst.firm <- node_new("output",
#'                      type = "CD", alpha = 2,
#'                      beta = c(0.5, 0.5),
#'                      "prod", "lab"
#' )
#'
#' dst.laborer <- node_new("util",
#'                         type = "FIN",
#'                         rate = c(1, consumption.tax.rate = 1/3),
#'                         "prod", "tax"
#' )
#'
#' dst.government <- node_new("utility",
#'                            type = "Leontief",
#'                            a = 1,
#'                            "prod"
#' )
#'
#' ge.CT <- sdm2(
#'   A = list(dst.firm, dst.laborer, dst.government),
#'   B = diag(c(1, 0, 0)),
#'   S0Exg = matrix(c(
#'     NA, NA, NA,
#'     NA, 100, NA,
#'     NA, NA, 100
#'   ), 3, 3, TRUE),
#'   names.commodity = c("prod", "lab", "tax"),
#'   names.agent = c("firm", "laborer", "government"),
#'   numeraire = "prod"
#' )
#'
#' ge.CT$p
#' ge.CT$z
#' ge.CT$D
#' ge.CT$S
#'
#' #### the value added tax.
#' dst.firm <- node_new("output",
#'   type = "CD", alpha = 2,
#'   beta = c(0.5, 0.5),
#'   "prod", "taxed.lab"
#' )
#' node_set(dst.firm, "taxed.lab",
#'   type = "FIN",
#'   rate = c(1, vat.rate = 1/3),
#'   "lab", "tax"
#' )
#'
#' dst.laborer <- dst.government <-
#'   node_new("util",
#'     type = "Leontief",
#'     a = 1,
#'     "prod"
#'   )
#'
#' ge.VAT <- sdm2(
#'   A = list(dst.firm, dst.laborer, dst.government),
#'   B = diag(c(1, 0, 0)),
#'   S0Exg = matrix(c(
#'     NA, NA, NA,
#'     NA, 100, NA,
#'     NA, NA, 100
#'   ), 3, 3, TRUE),
#'   names.commodity = c("prod", "lab", "tax"),
#'   names.agent = c("firm", "laborer", "government"),
#'   numeraire = "prod"
#' )
#'
#' ge.VAT$p
#' ge.VAT$z
#' ge.VAT$D
#' ge.VAT$S
#'
#' #### the income tax.
#' income.tax.rate <- 1 / 4
#'
#' dst.firm <- node_new("output",
#'                      type = "CD",
#'                      alpha = 2, beta = c(0.5, 0.5),
#'                      "prod", "lab"
#' )
#'
#' dst.laborer <- dst.government <-
#'   node_new("util",
#'     type = "Leontief",
#'     a = 1,
#'     "prod"
#'   )
#'
#' ge.IT <- sdm2(
#'   A = list(dst.firm, dst.laborer, dst.government),
#'   B <- diag(c(1, 0), 2, 3),
#'   S0Exg = matrix(c(
#'     NA, NA, NA,
#'     NA, 100 * (1 - income.tax.rate), 100 * income.tax.rate
#'   ), 2, 3, TRUE),
#'   names.commodity = c("prod", "lab"),
#'   names.agent = c("firm", "laborer", "government"),
#'   numeraire = "prod"
#' )
#'
#' ge.IT$p
#' ge.IT$z
#' ge.IT$D
#' ge.IT$S
#'
#' #### the turnover tax (Li, 2019, example 4.11).
#' dst.firm <- node_new("output",
#'                      type = "FIN",
#'                      rate = c(1, turnover.tax.rate = 1),
#'                      "cc1", "tax"
#' )
#' node_set(dst.firm, "cc1",
#'          type = "CD",
#'          alpha = 1, beta = c(0.5, 0.5),
#'          "prod", "lab"
#' )
#'
#' dst.laborer1 <- node_new("util",
#'                           type = "CD",
#'                           alpha = 1, beta = c(0.5, 0.5),
#'                           "prod", "lab"
#' )
#'
#' dst.laborer2 <- node_new("utility",
#'                           type = "Leontief",
#'                           a = 1,
#'                           "prod"
#' )
#'
#' ge.TT2 <- sdm2(
#'   A = list(dst.firm, dst.laborer1, dst.laborer2),
#'   B = diag(c(1, 0, 0)),
#'   S0Exg = matrix(c(
#'     NA, NA, NA,
#'     NA, 100, NA,
#'     NA, NA, 100
#'   ), 3, 3, TRUE),
#'   names.commodity = c("prod", "lab", "tax"),
#'   names.agent = c("firm", "laborer1", "laborer2"),
#'   numeraire = "lab"
#' )
#'
#' ge.TT2$p
#' ge.TT2$z
#' }

gemTax_3_3 <- function(...) sdm2(...)
