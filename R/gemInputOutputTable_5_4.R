#' @export
#' @title A General Equilibrium Model based on A 5x4 Input-Output Table (see Zhang Xin, 2017, Table 8.6.1).
#' @aliases gemInputOutputTable_5_4
#' @description This is a general equilibrium model based on a 5x4 input-output table (see Zhang Xin, 2017, Table 8.6.1).
#' @details Given a 5x4 input-output table (e.g., see Zhang Xin, 2017, Table 8.6.1), this model calculates
#' the corresponding generalequilibrium.
#' This input-output table contains 3 production sectors and one household.
#' The household consumes products and supplies labor and capital.
#' @param dstl the demand structure tree list.
#' @param supply.labor the supply of labor.
#' @param supply.capital the supply of capital.
#' @param names.commodity names of commodities.
#' @param names.agent names of agents.
#' @return A general equilibrium.
#' @references Zhang Xin. (2017, ISBN: 9787543227637). Principles of Computable General Equilibrium Modeling and Programming (Second Edition). Shanghai: Gezhi Press. (In Chinese)
#' @examples
#' rhoq.agri <- 0.2
#' rhoq.manu <- 0.3
#' rhoq.serv <- 0.1
#'
#' rhoVA.agri <- 0.25
#' rhoVA.manu <- 0.5
#' rhoVA.serv <- 0.8
#'
#' d.agri <- c(260, 345, 400, 200, 160)
#' d.manu <- c(320, 390, 365, 250, 400)
#' d.serv <- c(150, 390, 320, 400, 210)
#' d.hh <- c(635, 600, 385, 0, 0)
#' #d.hh <- c(635, 600, 100, 0, 0)
#'
#' D <- cbind(d.agri, d.manu, d.serv, d.hh)
#'
#' ds.agri <- Node$new("sector.agri",
#'   type = "CES", sigma = 1 - 1 / rhoq.agri,
#'   alpha = 1,
#'   beta = prop.table(c(
#'     sum(d.agri[1:3]),
#'     sum(d.agri[4:5])
#'   ))
#' )
#' ds.agri$AddChild("cc1.agri",
#'   type = "Leontief",
#'   a = prop.table(d.agri[1:3])
#' )$
#'   AddChild("agri")$AddSibling("manu")$AddSibling("serv")$
#'   parent$
#'   AddSibling("cc1.2.agri",
#'   type = "CES", sigma = 1 - 1 / rhoVA.agri,
#'   alpha = 1,
#'   beta = prop.table(d.agri[4:5])
#' )$
#'   AddChild("lab")$AddSibling("cap")
#'
#' ##
#' ds.manu <- Node$new("sector.manu",
#'   type = "CES", sigma = 1 - 1 / rhoq.manu,
#'   alpha = 1,
#'   beta = prop.table(c(
#'     sum(d.manu[1:3]),
#'     sum(d.manu[4:5])
#'   ))
#' )
#' ds.manu$AddChild("cc1.manu",
#'   type = "Leontief",
#'   a = prop.table(d.manu[1:3])
#' )$
#'   AddChild("agri")$AddSibling("manu")$AddSibling("serv")$
#'   parent$
#'   AddSibling("cc1.2.manu",
#'   type = "CES", sigma = 1 - 1 / rhoVA.manu,
#'   alpha = 1,
#'   beta = prop.table(d.manu[4:5])
#' )$
#'   AddChild("lab")$AddSibling("cap")
#'
#' ##
#' ds.serv <- Node$new("sector.serv",
#'   type = "CES", sigma = 1 - 1 / rhoq.serv,
#'   alpha = 1,
#'   beta = prop.table(c(
#'     sum(d.serv[1:3]),
#'     sum(d.serv[4:5])
#'   ))
#' )
#' ds.serv$AddChild("cc1.serv",
#'   type = "Leontief",
#'   a = prop.table(d.serv[1:3])
#' )$
#'   AddChild("agri")$AddSibling("manu")$AddSibling("serv")$
#'   parent$
#'   AddSibling("cc1.2.serv",
#'   type = "CES", sigma = 1 - 1 / rhoVA.serv,
#'   alpha = 1,
#'   beta = prop.table(d.serv[4:5])
#' )$
#'   AddChild("lab")$AddSibling("cap")
#'
#' ##
#' ds.hh <- Node$new("sector.hh",
#'   type = "CES", sigma = -1,
#'   alpha = 1,
#'   beta = prop.table(d.hh[1:3])
#' )
#' ds.hh$AddChild("agri")$AddSibling("manu")$AddSibling("serv")
#'
#' dstl <- list(
#'   ds.agri,
#'   ds.manu,
#'   ds.serv,
#'   ds.hh
#' )
#'
#' ge <- gemInputOutputTable_5_4(dstl)


gemInputOutputTable_5_4 <- function(dstl,
                             supply.labor = 850, #* 1.08,
                             supply.capital = 770,
                             names.commodity = c("agri", "manu", "serv", "lab", "cap"),
                             names.agent = c("agri", "manu", "serv", "hh")) {
  account.zero <- rep(0, length(names.commodity))
  names(account.zero) <- names.commodity

  ge <- sdm(
    A = function(state) {
      p <- c(state$p)
      names(p) <- names.commodity

      result <- sapply(dstl, demand_coefficient, p)
      return(result)
    },
    B = {
      tmp <- matrix(c(
        1, 0, 0, 0,
        0, 1, 0, 0,
        0, 0, 1, 0,
        0, 0, 0, 1,
        0, 0, 0, 1
      ), 5, 4, T)
    },
    S0Exg = {
      tmp <- matrix(NA, 5, 4)
      tmp[4, 4] <- supply.labor
      tmp[5, 4] <- supply.capital
      tmp
    },
    priceAdjustmentVelocity = 0.05,
    tolCond = 1e-8
  )

  ge$p <- ge$p/ge$p[4]
  ge <- ge_tidy(ge, names.commodity, names.agent)
}
