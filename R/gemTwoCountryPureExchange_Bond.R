#' @export
#' @title Some Examples of Two-Country Pure Exchange Economy with Bond
#' @aliases gemTwoCountryPureExchange_Bond
#' @description Some general equilibrium examples of two-country pure exchange economy with bond.
#' @param ... arguments to be passed to the function sdm2.
#' @return  A general equilibrium.
#' @examples
#' \donttest{
#' es.DFProd <- 0.8 # substitution elasticity between domestic and foreign products
#' technology.level.CHN <- 1
#' # the amount of foreign investment corresponding to each unit of consumption
#' foreign.investment.rate <- 0.1
#'
#' dst.consumption <- node_new("consumption",
#'   type = "SCES", alpha = 1, beta = c(0.5, 0.5), es = es.DFProd,
#'   "prod.CHN", "prod.USA"
#' )
#' node_set(dst.consumption, "prod.CHN",
#'   type = "Leontief", a = 1 / technology.level.CHN,
#'   "lab.CHN"
#' )
#' node_set(dst.consumption, "prod.USA",
#'   type = "Leontief", a = 1,
#'   "lab.USA"
#' )
#' dst.CHN <- node_new("CHN",
#'   type = "FIN", rate = c(1, foreign.investment.rate),
#'   dst.consumption, "bond.USA"
#' )
#' node_plot(dst.CHN)
#'
#' dst.USA <- Clone(dst.consumption)
#'
#' dstl <- list(dst.CHN, dst.USA)
#'
#' ge <- sdm2(dstl,
#'   names.commodity = c("lab.CHN", "lab.USA", "bond.USA"),
#'   names.agent = c("CHN", "USA"),
#'   B = matrix(0, 3, 2, TRUE),
#'   S0Exg = matrix(c(
#'     100, 0,
#'     0, 100,
#'     0, 100
#'   ), 3, 2, TRUE),
#'   numeraire = "lab.CHN"
#' )
#' ge$p[2]
#'
#' #### Add currencies to the example above.
#' es.DFProd <- 0.8
#' technology.level.CHN <- 1
#' foreign.investment.rate <- 0.1
#' interest.rate <- 1e-4
#'
#' prod_money.CHN <- node_new("prod_money.CHN",
#'   type = "FIN", rate = c(1, interest.rate),
#'   "prod.CHN", "money.CHN"
#' )
#' node_set(prod_money.CHN, "prod.CHN",
#'   type = "Leontief", a = 1 / technology.level.CHN,
#'   "lab.CHN"
#' )
#'
#' prod_money.USA <- node_new("prod_money.USA",
#'   type = "FIN", rate = c(1, interest.rate),
#'   "prod.USA", "money.USA"
#' )
#' node_set(prod_money.USA, "prod.USA",
#'   type = "Leontief", a = 1,
#'   "lab.USA"
#' )
#'
#' dst.CHN <- node_new("CHN",
#'   type = "FIN",
#'   rate = c(
#'     1, foreign.investment.rate,
#'     foreign.investment.rate * interest.rate
#'   ),
#'   "consumption", "bond.USA", "money.USA"
#' )
#' node_set(dst.CHN, "consumption",
#'   type = "SCES", alpha = 1, beta = c(0.5, 0.5), es = es.DFProd,
#'   prod_money.CHN, prod_money.USA
#' )
#'
#' node_plot(dst.CHN)
#'
#' dst.USA <- Clone(node_set(dst.CHN, "consumption"))
#' node_plot(dst.USA)
#'
#' dstl <- list(dst.CHN, dst.USA)
#'
#' ge <- sdm2(dstl,
#'   names.commodity = c(
#'     "lab.CHN", "money.CHN",
#'     "lab.USA", "money.USA",
#'     "bond.USA"
#'   ),
#'   names.agent = c("CHN", "USA"),
#'   B = matrix(0, 5, 2, TRUE),
#'   S0Exg = matrix(c(
#'     100, 0,
#'     100, 0,
#'     0, 100,
#'     0, 100,
#'     0, 100
#'   ), 5, 2, TRUE),
#'   numeraire = c("money.CHN" = interest.rate)
#' )
#'
#' ge$p["money.USA"] / ge$p["money.CHN"] # the exchange rate
#' }

gemTwoCountryPureExchange_Bond <- function(...) sdm2(...)
