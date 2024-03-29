#' @export
#' @title A 4-by-4 Open Economy with Bond
#' @aliases gemOpenEconomy_4_4
#' @description Some examples of a 4-by-4 open economy with bond.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' #### an open economy with foreign bond (bond.ROW)
#' dst.firm <- node_new(
#'   "output",
#'   type = "SCES", alpha = 1, beta = c(0.5, 0.5), es = 1,
#'   "prod.CHN", "lab.CHN"
#' )
#'
#' dst.consumer <- node_new(
#'   "util",
#'   type = "FIN", beta = c(0.8, 0.2),
#'   "cc1", "bond.ROW"
#' )
#' node_set(dst.consumer, "cc1",
#'          type = "SCES", alpha = 1, beta = c(0.5, 0.5), es = 1,
#'          "prod.CHN", "prod.ROW"
#' )
#'
#' dst.FT <- node_new(
#'   "prod.ROW",
#'   type = "SCES", alpha = 1, beta = c(2/3, 1/3), es = 1,
#'   "prod.CHN", "lab.CHN"
#' )
#'
#' dst.ROW <- node_new(
#'   "util",
#'   type = "SCES", alpha = 1, beta = c(2/3, 1/3), es = 1,
#'   "prod.CHN", "lab.CHN"
#' )
#'
#' ge.open <- sdm2(
#'   A = list(dst.firm, dst.consumer, dst.FT, dst.ROW),
#'   B = matrix(c(
#'     1, 0, 0, 0,
#'     0, 0, 0, 0,
#'     0, 0, 1, 0,
#'     0, 0, 0, 0
#'   ), 4, 4, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA, NA, NA,
#'     NA, 300, NA, NA,
#'     NA, NA, NA, NA,
#'     NA, NA, NA, 60
#'   ), 4, 4, TRUE),
#'   names.commodity = c("prod.CHN", "lab.CHN", "prod.ROW", "bond.ROW"),
#'   names.agent = c("firm", "consumer", "FT", "ROW"),
#'   numeraire = "lab.CHN"
#' )
#'
#' ge.open$p
#' addmargins(ge.open$D, 2)
#' addmargins(ge.open$S, 2)
#'
#' ## a corresponding two-country model
#' dst.firm.CHN <- node_new(
#'   "output",
#'   type = "SCES", alpha = 1, beta = c(0.5, 0.5), es = 1,
#'   "prod.CHN", "lab.CHN"
#' )
#'
#' dst.consumer.CHN <- node_new(
#'   "util",
#'   type = "FIN", beta = c(0.8, 0.2),
#'   "cc1", "bond.ROW"
#' )
#' node_set(dst.consumer.CHN, "cc1",
#'          type = "SCES", alpha = 1, beta = c(0.5, 0.5), es = 1,
#'          "prod.CHN", "prod.ROW"
#' )
#'
#' dst.firm.ROW <- node_new(
#'   "prod.ROW",
#'   type = "SCES", alpha = 1, beta = c(0.25, 0.25, 0.5), es = 1,
#'   "prod.CHN", "prod.ROW", "lab.ROW"
#' )
#'
#' dst.consumer.ROW <- node_new(
#'   "util",
#'   type = "SCES", alpha = 1, beta = c(0.25, 0.25, 0.25, 0.25), es = 1,
#'   "prod.CHN", "prod.ROW", "lab.CHN", "lab.ROW"
#' )
#'
#' ge.TC <- sdm2(
#'   A = list(dst.firm.CHN, dst.consumer.CHN, dst.firm.ROW, dst.consumer.ROW),
#'   B = matrix(c(
#'     1, 0, 0, 0,
#'     0, 0, 0, 0,
#'     0, 0, 1, 0,
#'     0, 0, 0, 0,
#'     0, 0, 0, 0
#'   ), 5, 4, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA, NA, NA,
#'     NA, 300, NA, NA,
#'     NA, NA, NA, NA,
#'     NA, NA, NA, 180,
#'     NA, NA, NA, 60
#'   ), 5, 4, TRUE),
#'   names.commodity = c("prod.CHN", "lab.CHN", "prod.ROW", "lab.ROW", "bond.ROW"),
#'   names.agent = c("firm.CHN", "consumer.CHN", "firm.ROW", "consumer.ROW"),
#'   numeraire = "lab.CHN"
#' )
#'
#' ge.TC$p
#' addmargins(ge.TC$D, 2)
#' addmargins(ge.TC$S, 2)
#'
#' #### an open economy with domestic bond (bond.CHN)
#' dst.firm <- node_new(
#'   "output",
#'   type = "CD", alpha = 5, beta = c(0.5, 0.5),
#'   "prod.CHN", "lab.CHN"
#' )
#'
#' dst.consumer <- node_new(
#'   "util",
#'   type = "CD", alpha = 1, beta = c(0.5, 0.5),
#'   "prod.CHN", "prod.ROW"
#' )
#'
#' dst.FT <- node_new(
#'   "prod.ROW",
#'   type = "Leontief", a = 2,
#'   "prod.CHN"
#' )
#'
#' dst.ROW <- node_new(
#'   "prod.ROW",
#'   type = "Leontief", a = 1,
#'   "bond.CHN"
#' )
#'
#' ge <- sdm2(
#'   A = list(dst.firm, dst.consumer, dst.FT, dst.ROW),
#'   B = matrix(c(
#'     1, 0, 0, 0,
#'     0, 0, 0, 0,
#'     0, 0, 1, 1,
#'     0, 0, 0, 0
#'   ), 4, 4, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA, NA, NA,
#'     NA, 1, NA, NA,
#'     NA, NA, NA, NA,
#'     NA, 0.2, NA, NA
#'   ), 4, 4, TRUE),
#'   names.commodity = c("prod.CHN", "lab.CHN", "prod.ROW", "bond.CHN"),
#'   names.agent = c("firm", "consumer", "FT", "ROW"),
#'   numeraire = "lab.CHN"
#' )
#'
#' ge$p
#' addmargins(ge$D, 2)
#' addmargins(ge$S, 2)
#' }

gemOpenEconomy_4_4 <- function(...) sdm2(...)
