#' @import CGE data.tree DiagrammeR
#' @export
#' @title A CES-type General Equilibrium Model with Money.
#' @aliases gemMoney_CES_3_2
#' @description This is a CES-type general equilibrium model with money.
#' @details A CES-type general equilibrium model with 3 commodities (i.e. product, labor,
#' and money) and 2 agents (i.e. a firm and a household).
#' To produce, the firm needs product, labor and money.
#' The household only consumes the product. But money is also needed to buy products.
#' The household supplies labor and money. sigma is the coefficient of elasticity of substitution between the
#' product and labor used in the firm's production. 1/(1-sigma) is the elasticity of substitution.
#' @param dstl the demand structure tree list.
#' @param supply.labor the supply of labor.
#' @param supply.money the supply of money.
#' @param names.commodity names of commodities.
#' @param names.agent names of agents.
#' @return a general equilibrium.
#' @examples
#' interest.rate <- 1 / 3
#'
#' ds.firm <- Node$new("firm", type = "FIN", rate = c(0.75, interest.rate))
#' ds.firm$AddChild("cc1", type = "CES", sigma = -Inf, alpha = 1, beta = c(0.8, 0.2))$
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
#' ge <- gemMoney_CES_3_2(dstl)
#' dst_plot(ge$dstl[[1]])
#' dst_plot(ge$dstl[[2]])
#' ge$p
#' p.money <- ge$p
#' p.money["money"] <- p.money["money"] / interest.rate
#' p.money <- p.money / p.money["money"]

#' #### Leontief-type firm
#' dst.firm.Leontief <- Node$new("firm", type = "FIN", rate = c(0.75, interest.rate))
#' dst.firm.Leontief$AddChild("cc1", type = "Leontief", a = c(0.8, 0.2))$
#'   AddChild("product")$AddSibling("labor")$
#'   parent$
#'   AddSibling("money")
#'
#' dstl.Leontief <- list(dst.firm.Leontief, ds.household)
#'
#' ge.Leontief <- gemMoney_CES_3_2(dstl.Leontief)
#' dst_plot(ge.Leontief$dstl[[1]])
#' ge.Leontief$p
gemMoney_CES_3_2 <- function(dstl, supply.labor = 100, supply.money = 1000,
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

  ge <- ge_tidy(ge, names.commodity, names.agent)

  ge$dstl <- dstl
  return(ge)
}
