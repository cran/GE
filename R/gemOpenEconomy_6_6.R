#' @export
#' @title A 6-by-6 Open Economy with Bond
#' @aliases gemOpenEconomy_6_6
#' @description Some examples of a 6-by-6 open economy with bond.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' #### an open economy with foreign bond (bond.ROW)
#' dst.firm1 <- node_new(
#'   "prod1.CHN",
#'   type = "SCES", es=1, alpha = 1, beta = c(0.4, 0.4, 0.2),
#'   "prod2.CHN", "prod2.ROW", "lab"
#' )
#'
#' dst.firm2 <- node_new(
#'   "prod2.CHN",
#'   type = "SCES", es=1, alpha = 1, beta = c(0.4, 0.4, 0.2),
#'   "prod2.CHN", "prod2.ROW", "lab"
#' )
#'
#' dst.consumer <- node_new(
#'   "util",
#'   type = "FIN", beta = c(0.8, 0.2),
#'   "cc1", "bond.ROW"
#' )
#' node_set(dst.consumer, "cc1",
#'          type = "SCES", es=1, alpha = 1, beta = c(0.5, 0.5),
#'          "prod1.CHN", "prod1.ROW"
#' )
#'
#' dst.FT1 <- node_new(
#'   "prod1.ROW",
#'   type = "SCES", es=1, alpha = 1, beta = c(0.5, 0.5),
#'   "prod1.CHN", "prod2.CHN"
#' )
#'
#' dst.FT2 <- node_new(
#'   "prod2.ROW",
#'   type = "SCES", es=1, alpha = 1, beta = c(0.5, 0.5),
#'   "prod1.CHN", "prod2.CHN"
#' )
#'
#' dst.Bond <- node_new(
#'   "util",
#'   type = "SCES", es=1, alpha = 1, beta = c(0.5, 0.5),
#'   "prod1.CHN", "prod2.CHN"
#' )
#'
#' ge <- sdm2(
#'   A = list(dst.firm1, dst.firm2, dst.consumer, dst.FT1, dst.FT2, dst.Bond),
#'   B = matrix(c(
#'     1, 0, 0, 0, 0, 0,
#'     0, 1, 0, 0, 0, 0,
#'     0, 0, 0, 0, 0, 0,
#'     0, 0, 0, 1, 0, 0,
#'     0, 0, 0, 0, 1, 0,
#'     0, 0, 0, 0, 0, 0
#'   ), 6, 6, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA, NA, NA, NA, NA,
#'     NA, NA, NA, NA, NA, NA,
#'     NA, NA, 100, NA, NA, NA,
#'     NA, NA, NA, NA, NA, NA,
#'     NA, NA, NA, NA, NA, NA,
#'     NA, NA, NA, NA, NA, 20
#'   ), 6, 6, TRUE),
#'   names.commodity = c(
#'     "prod1.CHN", "prod2.CHN", "lab",
#'     "prod1.ROW", "prod2.ROW", "bond.ROW"
#'   ),
#'   names.agent = c("firm1", "firm2", "consumer", "FT1", "FT2", "Bond"),
#'   numeraire = "lab"
#' )
#'
#' ge$D
#' ge$p
#' ge$z
#'
#' ## Suppose the domestic consumer owns some foreign product by borrowing.
#' ge <- sdm2(
#'   A = list(dst.firm1, dst.firm2, dst.consumer, dst.FT1, dst.FT2, dst.Bond),
#'   B = matrix(c(
#'     1, 0, 0, 0, 0, 0,
#'     0, 1, 0, 0, 0, 0,
#'     0, 0, 0, 0, 0, 0,
#'     0, 0, 0, 1, 0, 0,
#'     0, 0, 0, 0, 1, 0,
#'     0, 0, 0, 0, 0, 0
#'   ), 6, 6, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA, NA, NA, NA, NA,
#'     NA, NA, NA, NA, NA, NA,
#'     NA, NA, 100, NA, NA, NA,
#'     NA, NA, 10, NA, NA, NA,
#'     NA, NA, NA, NA, NA, NA,
#'     NA, NA, NA, NA, NA, 20
#'   ), 6, 6, TRUE),
#'   names.commodity = c(
#'     "prod1.CHN", "prod2.CHN", "lab",
#'     "prod1.ROW", "prod2.ROW", "bond.ROW"
#'   ),
#'   names.agent = c("firm1", "firm2", "consumer", "FT1", "FT2", "Bond"),
#'   numeraire = "lab"
#' )
#'
#' ge$D
#' ge$p
#' ge$z
#' }

gemOpenEconomy_6_6 <- function(...) sdm2(...)
