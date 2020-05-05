#' @import CGE data.tree DiagrammeR
#' @export
#' @title A General Equilibrium Model with Money.
#' @aliases gemMoney_3_2
#' @description This is a general equilibrium model with money.
#' @details A general equilibrium model with 3 commodities (i.e. product, labor,
#' and money) and 2 agents (i.e. a firm and a household).
#' To produce, the firm needs product, labor and money.
#' The household only consumes the product. But money is also needed to buy the product.
#' The household supplies labor and money. \cr
#'
#' In the calculation results, the price of the currency is the interest per unit of currency (i.e. the rent price, interest price).
#' It should be noted that the unit of currency can be arbitrarily selected.
#' For example, a unit of currency may be two dollars or ten dollars.
#' The interest price divided by the interest rate is the asset price of 1 unit of the currency.
#'
#' @param dstl the demand structure tree list.
#' @param supply.labor the supply of labor.
#' @param supply.money the supply of money.
#' @param names.commodity names of commodities.
#' @param names.agent names of agents.
#' @return A general equilibrium, which is a list with the following elements:
#' \itemize{
#' \item p - the price vector with labor as numeraire.
#' \item D - the demand matrix.
#' \item DV - the demand value matrix.
#' \item SV - the supply value matrix.
#' \item dstl - the demand structure tree list.
#' \item ... - some elements returned by the CGE::sdm function
#' }
#'
#' @examples
#' interest.rate <- 1 / 3
#'
#' ds.firm <- Node$new("firm", type = "FIN", rate = c(0.75, interest.rate))
#' ds.firm$AddChild("cc1",
#'   type = "CES",
#'   alpha = 1,
#'   beta = c(0.8, 0.2),
#'   es = 0 # es is the elasticity of substitution.
#' )$
#'   AddChild("product")$AddSibling("labor")$
#'   parent$
#'   AddSibling("money")
#' dst_plot(ds.firm)
#'
#' ds.household <- Node$new("household", type = "FIN", rate = c(1, interest.rate))
#' ds.household$AddChild("product")$
#'   AddSibling("money")
#'
#' dstl <- list(ds.firm, ds.household)
#'
#' ge <- gemMoney_3_2(dstl)
#' dst_plot(ge$dstl[[1]])
#' dst_plot(ge$dstl[[2]])
#' ge$p
#' p.money <- ge$p
#' p.money["money"] <- p.money["money"] / interest.rate
#' p.money <- p.money / p.money["money"] #prices in terms of the asset price of the currency
#'
#' #### Leontief-type firm
#' dst.firm.Leontief <- Node$new("firm", type = "FIN", rate = c(0.75, interest.rate))
#' dst.firm.Leontief$AddChild("cc1", type = "Leontief", a = c(0.8, 0.2))$
#'   AddChild("product")$AddSibling("labor")$
#'   parent$
#'   AddSibling("money")
#'
#' dstl.Leontief <- list(dst.firm.Leontief, ds.household)
#'
#' ge.Leontief <- gemMoney_3_2(dstl.Leontief)
#' dst_plot(ge.Leontief$dstl[[1]])
#' ge.Leontief$p

gemMoney_3_2 <- function(dstl,
                             supply.labor = 100,
                             supply.money = 1000,
                             names.commodity = c("product", "labor", "money"),
                             names.agent = c("firm", "household")) {
  ge <- sdm(
    A = function(state) {
      p <- c(state$p)
      names(p) <- names.commodity
      sapply(dstl, demand_coefficient, p)
    },
    B = matrix(c(
      1, 0,
      0, 1,
      0, 1
    ), 3, 2, TRUE),
    S0Exg = {
      tmp <- matrix(NA, 3, 2)
      tmp[2, 2] <- supply.labor
      tmp[3, 2] <- supply.money
      tmp
    }
  )

  ge$p <- ge$p/ge$p[2] # labor as numeraire

  ge <- ge_tidy(ge, names.commodity, names.agent)

  ge$dstl <- dstl
  return(ge)
}
