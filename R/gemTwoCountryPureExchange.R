#' @export
#' @title Some Examples of Two-Country Pure Exchange Economy
#' @aliases gemTwoCountryPureExchange
#' @description Some general equilibrium examples of two-country pure exchange economy.
#' @param ... arguments to be passed to the function sdm2.
#' @return  A general equilibrium.
#' @examples
#' \donttest{
#' es.DFProd <- 0.8 # substitution elasticity between domestic and foreign products
#' technology.level.CHN <- 0.9
#'
#' dst.CHN <- node_new("util",
#'   type = "SCES", alpha = 1, beta = c(0.5, 0.5), es = es.DFProd,
#'   "prod.CHN", "prod.USA"
#' )
#' node_set(dst.CHN, "prod.CHN",
#'   type = "Leontief", a = 1 / technology.level.CHN,
#'   "lab.CHN"
#' )
#' node_set(dst.CHN, "prod.USA",
#'   type = "Leontief", a = 1,
#'   "lab.USA"
#' )
#' node_plot(dst.CHN)
#'
#' dst.USA <- Clone(dst.CHN)
#'
#' dstl <- list(dst.CHN, dst.USA)
#'
#' ge <- sdm2(dstl,
#'   names.commodity = c("lab.CHN", "lab.USA"),
#'   names.agent = c("CHN", "USA"),
#'   B = matrix(0, 2, 2, TRUE),
#'   S0Exg = matrix(c(
#'     100, 0,
#'     0, 100
#'   ), 2, 2, TRUE),
#'   numeraire = "lab.CHN"
#' )
#' ge$p[2]
#' # the same as above
#' technology.level.CHN^(1 / es.DFProd - 1)
#'
#' ## supply change
#' geSC <- sdm2(dstl,
#'   names.commodity = c("lab.CHN", "lab.USA"),
#'   names.agent = c("CHN", "USA"),
#'   B = matrix(0, 2, 2, TRUE),
#'   S0Exg = matrix(c(
#'     200, 0,
#'     0, 100
#'   ), 2, 2, TRUE),
#'   numeraire = "lab.CHN"
#' )
#' geSC$p[2]
#'
#' ## preference change
#' dst.CHN$beta <- c(0.6, 0.4)
#' gePC <- sdm2(dstl,
#'   names.commodity = c("lab.CHN", "lab.USA"),
#'   names.agent = c("CHN", "USA"),
#'   B = matrix(0, 2, 2, TRUE),
#'   S0Exg = matrix(c(
#'     100, 0,
#'     0, 100
#'   ), 2, 2, TRUE),
#'   numeraire = "lab.CHN"
#' )
#'
#' gePC$p[2]
#'
#' #### Add currencies to the example above.
#' interest.rate <- 1e-4
#' es.DFProd <- 0.8
#' technology.level.CHN <- 0.9
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
#' dst.CHN <- node_new("util",
#'   type = "SCES", alpha = 1, beta = c(0.5, 0.5), es = es.DFProd,
#'   prod_money.CHN, prod_money.USA
#' )
#'
#' dst.USA <- Clone(dst.CHN)
#'
#' dstl <- list(dst.CHN, dst.USA)
#'
#' ge <- sdm2(dstl,
#'   names.commodity = c(
#'     "lab.CHN", "money.CHN",
#'     "lab.USA", "money.USA"
#'   ),
#'   names.agent = c("CHN", "USA"),
#'   B = matrix(0, 4, 2, TRUE),
#'   S0Exg = matrix(c(
#'     100, 0,
#'     100, 0,
#'     0, 100,
#'     0, 100
#'   ), 4, 2, TRUE),
#'   numeraire = c("money.CHN" = interest.rate)
#' )
#'
#' ge$p["money.USA"] / ge$p["money.CHN"] # the exchange rate
#'
#' #### supply change
#' geSC <- sdm2(dstl,
#'   names.commodity = c(
#'     "lab.CHN", "money.CHN",
#'     "lab.USA", "money.USA"
#'   ),
#'   names.agent = c("CHN", "USA"),
#'   B = matrix(0, 4, 2, TRUE),
#'   S0Exg = matrix(c(
#'     200, 0,
#'     100, 0,
#'     0, 100,
#'     0, 100
#'   ), 4, 2, TRUE),
#'   numeraire = c("money.CHN" = interest.rate)
#' )
#' geSC$p["money.USA"] / geSC$p["money.CHN"]
#'
#' ## preference change
#' dst.CHN$beta <- c(0.6, 0.4)
#' gePC <- sdm2(dstl,
#'   names.commodity = c(
#'     "lab.CHN", "money.CHN",
#'     "lab.USA", "money.USA"
#'   ),
#'   names.agent = c("CHN", "USA"),
#'   B = matrix(0, 4, 2, TRUE),
#'   S0Exg = matrix(c(
#'     100, 0,
#'     100, 0,
#'     0, 100,
#'     0, 100
#'   ), 4, 2, TRUE),
#'   numeraire = c("money.CHN" = interest.rate)
#' )
#'
#' gePC$p["money.USA"] / gePC$p["money.CHN"]
#'
#' #### the exchange rate under a high substitution elasticity
#' #### between domestic and foreign products.
#' interest.rate <- 1e-4
#' es.DFProd <- 3
#' technology.level.CHN <- 0.9
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
#' dst.CHN <- node_new("util",
#'   type = "SCES", alpha = 1, beta = c(0.5, 0.5), es = es.DFProd,
#'   prod_money.CHN, prod_money.USA
#' )
#'
#' dst.USA <- Clone(dst.CHN)
#'
#' dstl <- list(dst.CHN, dst.USA)
#'
#' ge <- sdm2(dstl,
#'   names.commodity = c(
#'     "lab.CHN", "money.CHN",
#'     "lab.USA", "money.USA"
#'   ),
#'   names.agent = c("CHN", "USA"),
#'   B = matrix(0, 4, 2, TRUE),
#'   S0Exg = matrix(c(
#'     100, 0,
#'     100, 0,
#'     0, 100,
#'     0, 100
#'   ), 4, 2, TRUE),
#'   numeraire = c("money.CHN" = interest.rate)
#' )
#'
#' ge$p["money.USA"] / ge$p["money.CHN"] # the exchange rate
#'
#' ## supply change and high substitution elasticity
#' geSC <- sdm2(dstl,
#'   names.commodity = c(
#'     "lab.CHN", "money.CHN",
#'     "lab.USA", "money.USA"
#'   ),
#'   names.agent = c("CHN", "USA"),
#'   B = matrix(0, 4, 2, TRUE),
#'   S0Exg = matrix(c(
#'     200, 0,
#'     100, 0,
#'     0, 100,
#'     0, 100
#'   ), 4, 2, TRUE),
#'   numeraire = c("money.CHN" = interest.rate)
#' )
#' geSC$p["money.USA"] / geSC$p["money.CHN"]
#'
#' }

gemTwoCountryPureExchange <- function(...) sdm2(...)
