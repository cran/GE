#' @export
#' @title A General Equilibrium Model based on a 5x4 Input-Output Table (see Zhang Xin, 2017, Table 8.6.1)
#' @aliases gemInputOutputTable_5_4
#' @description This is a general equilibrium model based on a 5x4 input-output table (see Zhang Xin, 2017, Table 8.6.1).
#' @details Given a 5x4 input-output table (e.g., see Zhang Xin, 2017, Table 8.6.1), this model calculates
#' the corresponding general equilibrium.
#' This input-output table contains 3 production sectors and one household.
#' The household consumes products and supplies labor and capital.
#' @param dstl a demand structure tree list.
#' @param supply.labor the supply of labor.
#' @param supply.capital the supply of capital.
#' @param names.commodity names of commodities.
#' @param names.agent names of agents.
#' @return A general equilibrium which is a list with the following elements:
#' \itemize{
#' \item D - the demand matrix, also called the input table. Wherein the benchmark prices are used.
#' \item DV - the demand value matrix, also called the value input table. Wherein the current price is used.
#' \item SV - the supply value matrix, also called the value output table. Wherein the current price is used.
#' \item ... - some elements returned by the CGE::sdm function
#' }
#' @references Zhang Xin (2017, ISBN: 9787543227637) Principles of Computable General Equilibrium Modeling and Programming (Second Edition). Shanghai: Gezhi Press. (In Chinese)
#' @examples
#' \donttest{
#' es.agri <- 0.2 # the elasticity of substitution
#' es.manu <- 0.3
#' es.serv <- 0.1
#'
#' es.VA.agri <- 0.25
#' es.VA.manu <- 0.5
#' es.VA.serv <- 0.8
#'
#' d.agri <- c(260, 345, 400, 200, 160)
#' d.manu <- c(320, 390, 365, 250, 400)
#' d.serv <- c(150, 390, 320, 400, 210)
#' d.hh <- c(635, 600, 385, 0, 0)
#' # d.hh <- c(635, 600, 100, 0, 0)
#'
#' D <- cbind(d.agri, d.manu, d.serv, d.hh)
#'
#' dst.agri <- node_new("sector.agri",
#'                      type = "SCES", es = es.agri,
#'                      alpha = 1,
#'                      beta = prop.table(
#'                        c(sum(d.agri[1:3]), sum(d.agri[4:5]))
#'                      ),
#'                      "cc1.agri", "cc2.agri"
#' )
#' node_set(dst.agri, "cc1.agri",
#'          type = "Leontief",
#'          a = prop.table(d.agri[1:3]),
#'          "agri", "manu", "serv"
#' )
#' node_set(dst.agri, "cc2.agri",
#'          type = "SCES", es = es.VA.agri,
#'          alpha = 1,
#'          beta = prop.table(d.agri[4:5]),
#'          "lab", "cap"
#' )
#'
#'
#' dst.manu <- node_new("sector.manu",
#'                      type = "SCES", es = es.manu,
#'                      alpha = 1,
#'                      beta = prop.table(
#'                        c(sum(d.manu[1:3]), sum(d.manu[4:5]))
#'                      ),
#'                      "cc1.manu", "cc2.manu"
#' )
#' node_set(dst.manu, "cc1.manu",
#'          type = "Leontief",
#'          a = prop.table(d.manu[1:3]),
#'          "agri", "manu", "serv"
#' )
#' node_set(dst.manu, "cc2.manu",
#'          type = "SCES", es = es.VA.manu,
#'          alpha = 1,
#'          beta = prop.table(d.manu[4:5]),
#'          "lab", "cap"
#' )
#'
#' dst.serv <- node_new("sector.serv",
#'                      type = "SCES", es = es.serv,
#'                      alpha = 1,
#'                      beta = prop.table(
#'                        c(sum(d.serv[1:3]), sum(d.serv[4:5]))
#'                      ),
#'                      "cc1.serv", "cc2.serv"
#' )
#' node_set(dst.serv, "cc1.serv",
#'          type = "Leontief",
#'          a = prop.table(d.serv[1:3]),
#'          "agri", "manu", "serv"
#' )
#' node_set(dst.serv, "cc2.serv",
#'          type = "SCES", es = es.VA.serv,
#'          alpha = 1,
#'          beta = prop.table(d.serv[4:5]),
#'          "lab", "cap"
#' )
#'
#' ##
#' dst.hh <- node_new("sector.hh",
#'                    type = "SCES", es = 0.5,
#'                    alpha = 1,
#'                    beta = prop.table(d.hh[1:3]),
#'                    "agri", "manu", "serv"
#' )
#'
#' dstl <- list(dst.agri, dst.manu, dst.serv, dst.hh)
#'
#' ge <- gemInputOutputTable_5_4(dstl)
#'
#' #### labor supply increase
#' geLSI <- gemInputOutputTable_5_4(dstl, supply.labor = 850 * 1.08)
#' geLSI$p
#' geLSI$z / ge$z
#' }

gemInputOutputTable_5_4 <- function(dstl,
                                    supply.labor = 850,
                                    supply.capital = 770,
                                    names.commodity = c("agri", "manu", "serv", "lab", "cap"),
                                    names.agent = c("agri", "manu", "serv", "hh")) {

  ge <- sdm(
    A = function(state) {
      p <- c(state$p)
      names(p) <- names.commodity

      result <- sapply(dstl, demand_coefficient, p)
      return(result)
    },
    B = matrix(c(
        1, 0, 0, 0,
        0, 1, 0, 0,
        0, 0, 1, 0,
        0, 0, 0, 1,
        0, 0, 0, 1
      ), 5, 4, TRUE),
    S0Exg = {
      tmp <- matrix(NA, 5, 4)
      tmp[4, 4] <- supply.labor
      tmp[5, 4] <- supply.capital
      tmp
    },
    priceAdjustmentVelocity = 0.05,
    tolCond = 1e-8
  )

  ge$p <- ge$p / ge$p[4]
  ge_tidy(ge, names.commodity, names.agent)
}
