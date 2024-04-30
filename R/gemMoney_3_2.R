#' @import CGE data.tree DiagrammeR
#' @export
#' @title A General Equilibrium Model with Money
#' @aliases gemMoney_3_2
#' @description A general equilibrium model with money as a medium of exchange and a means of payment.
#' @details A general equilibrium model with 3 commodities (i.e. product, labor,
#' and money) and 2 agents (i.e. a firm and a household).
#' To produce, the firm needs product, labor and money.
#' The household only consumes the product. But money is also needed to buy the product.
#' The household supplies labor and money. \cr
#'
#' In the calculation results, the price of the currency is the interest per unit of currency (i.e., the rental price).
#' It should be noted that the unit of currency can be arbitrarily selected.
#' For example, a unit of currency may be two dollars or ten dollars.
#' The rental price divided by the interest rate is the asset price of 1 unit of the currency.
#'
#' @param dstl the demand structure tree list.
#' @param supply.labor the supply of labor.
#' @param supply.money the supply of money.
#' @param names.commodity names of commodities.
#' @param names.agent names of agents.
#' @param ... arguments to be to be passed to the function sdm2.
#' @return A general equilibrium (see \code{\link{sdm2}})
#'
#' @examples
#' #### Leontief-type firm
#' interest.rate <- 0.25
#' vm <- 1 # the velocity of money
#'
#' dst.Leontief.firm <- node_new("output",
#'   type = "FIN", rate = c(1, interest.rate / vm),
#'   "cc1", "money"
#' )
#' node_set(dst.Leontief.firm, "cc1",
#'   type = "Leontief", a = c(0.6, 0.2),
#'   "product", "labor"
#' )
#'
#' dst.household <- node_new("utility",
#'   type = "FIN", rate = c(1, interest.rate / vm),
#'   "product", "money"
#' )
#'
#' dstl.Leontief <- list(dst.Leontief.firm, dst.household)
#'
#' ge.Leontief <- gemMoney_3_2(dstl.Leontief)
#' ge.Leontief$p
#'
#' ## SCES-type firm
#' dst.SCES.firm <- Clone(dst.Leontief.firm)
#' node_set(dst.SCES.firm, "cc1",
#'   type = "SCES", alpha = 1, beta = c(0.6, 0.2),
#'   es = 0 # es is the elasticity of substitution.
#' )
#'
#' node_plot(dst.SCES.firm, TRUE)
#'
#' dstl.SCES <- list(dst.SCES.firm, dst.household)
#'
#' ge.SCES <- gemMoney_3_2(dstl.SCES)
#' ge.SCES$p
#' p.money <- ge.SCES$p
#' p.money["money"] <- p.money["money"] / interest.rate
#' p.money <- p.money / p.money["money"] # prices in terms of the asset price of the currency
#' p.money
#'
#' ## The price of money is the interest rate.
#' ## The other prices are in terms of the asset price of the currency.
#' gemMoney_3_2(dstl.SCES,
#'              numeraire = c("money" = interest.rate)
#' )
#'

gemMoney_3_2 <- function(dstl,
                         supply.labor = 100,
                         supply.money = 300,
                         names.commodity = c("product", "labor", "money"),
                         names.agent = c("firm", "household"),
                         ...) {
  ge <- sdm2(
    A = dstl,
    B = matrix(c(
      1, 0,
      0, 0,
      0, 0
    ), 3, 2, TRUE,
    dimnames = list(names.commodity, names.agent)
    ),
    S0Exg = {
      tmp <- matrix(NA, 3, 2, dimnames = list(names.commodity, names.agent))
      tmp[2, 2] <- supply.labor
      tmp[3, 2] <- supply.money
      tmp
    },
    names.commodity = names.commodity,
    names.agent = names.agent,
    ...
  )

  return(ge)
}
