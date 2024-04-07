#' @export
#' @title A General Equilibrium Model with Tax
#' @aliases gemTax_5_5
#' @description A general equilibrium model with tax.
#' The model contains 5 types of commodities (i.e. prod1, prod2, labor, capital and tax payment receipts)
#' and 5 agents (i.e. firm1, firm2, laborer, capital owner and government).
#'
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' tax.rate.cap1 <- 0.25
#' tax.rate.lab1 <- 0.25
#'
#' tax.rate.cap2 <- 0.25
#' tax.rate.lab2 <- 0.25
#'
#' es.prod <- 0.5
#' es.cap.lab <- 0.5
#'
#' beta.firm1 <- c(0.2, 0.8)
#' beta.firm2 <- c(0.8, 0.2)
#'
#' beta.laborer <- c(0.5, 0.5)
#' beta.capitalOwner <- c(0.5, 0.5)
#' beta.government <- c(0.8, 0.2)
#'
#' dst.firm1 <- node_new("prod",
#'   type = "SCES",
#'   alpha = 1, beta = beta.firm1, es = es.cap.lab,
#'   "cc1", "cc2"
#' )
#' node_set(dst.firm1, "cc1",
#'   type = "FIN",
#'   rate = c(1, tax.rate = tax.rate.lab1),
#'   "lab", "tax"
#' )
#' node_set(dst.firm1, "cc2",
#'   type = "FIN",
#'   rate = c(1, tax.rate = tax.rate.cap1),
#'   "cap", "tax"
#' )
#'
#' node_plot(dst.firm1, TRUE)
#'
#' dst.firm2 <- node_new("prod",
#'   type = "SCES",
#'   alpha = 1, beta = beta.firm2, es = es.cap.lab,
#'   "cc1", "cc2"
#' )
#' node_set(dst.firm2, "cc1",
#'   type = "FIN",
#'   rate = c(1, tax.rate = tax.rate.lab2),
#'   "lab", "tax"
#' )
#' node_set(dst.firm2, "cc2",
#'   type = "FIN",
#'   rate = c(1, tax.rate = tax.rate.cap2),
#'   "cap", "tax"
#' )
#'
#' dst.laborer <- node_new("util",
#'   type = "SCES",
#'   alpha = 1, beta = beta.laborer, es = es.prod,
#'   "prod1", "prod2"
#' )
#'
#' dst.capitalOwner <- node_new("util",
#'   type = "SCES",
#'   alpha = 1, beta = beta.capitalOwner, es = es.prod,
#'   "prod1", "prod2"
#' )
#'
#' dst.government <- node_new("util",
#'   type = "SCES",
#'   alpha = 1, beta = beta.government, es = es.prod,
#'   "prod1", "prod2"
#' )
#'
#' ge <- sdm2(
#'   A = list(dst.firm1, dst.firm2, dst.laborer, dst.capitalOwner, dst.government),
#'   B = diag(c(1, 1, 0, 0, 0)),
#'   S0Exg = matrix(c(
#'     NA, NA, NA, NA, NA,
#'     NA, NA, NA, NA, NA,
#'     NA, NA, 100, NA, NA,
#'     NA, NA, NA, 100, NA,
#'     NA, NA, NA, NA, 100
#'   ), 5, 5, TRUE),
#'   names.commodity = c("prod1", "prod2", "lab", "cap", "tax"),
#'   names.agent = c("firm1", "firm2", "laborer", "capitalOwner", "government"),
#'   numeraire = "lab"
#' )
#'
#' ge$p
#' ge$z
#' ge$D
#' ge$S
#' addmargins(ge$DV)
#' addmargins(ge$SV)
#' }

gemTax_5_5 <- function(...) sdm2(...)
