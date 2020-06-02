#' @export
#' @title An Easy General Equilibrium Model based on a 5x4 Input-Output Table (see Zhang Xin, 2017)
#' @aliases gemInputOutputTable_easy_5_4
#' @description This is a general equilibrium model based on a 5x4 input-output table (see Zhang Xin, 2017, Table 8.6.1).
#' @details Given a 5x4 input-output table (e.g., see Zhang Xin, 2017, Table 8.6.1), this model calculates
#' the corresponding general equilibrium.
#' This input-output table contains 3 production sectors and one household.
#' The household consumes products and supplies labor and capital.
#' @param IT the input and consumption part of the input-output table.
#' @param supply.labor the supply of labor.
#' @param supply.capital the supply of capital.
#' @param es.agri,es.manu,es.serv the elasticity of substitution between the intermediate input
#' and the value-added input of the agriculture sector, manufacturing sector and service sector.
#' @param es.VA.agri,es.VA.manu,es.VA.serv the elasticity of substitution between labor input and capital input
#' of the agriculture sector, manufacturing sector and service sector.
#' @return A general equilibrium, which is a list with the following elements:
#' \itemize{
#' \item p - the price vector with labor as numeraire.
#' \item D - the demand matrix, also called the input table. Wherein the benchmark prices are used.
#' \item DV - the demand value matrix, also called the value input table. Wherein the current price is used.
#' \item SV - the supply value matrix, also called the value output table. Wherein the current price is used.
#' \item ... - some elements returned by the CGE::sdm function
#' }
#'
#' @references Zhang Xin (2017, ISBN: 9787543227637) Principles of Computable General Equilibrium Modeling and Programming (Second Edition). Shanghai: Gezhi Press. (In Chinese)
#' @examples
#' sector.agri <- c(260, 345, 400, 200, 160)
#' sector.manu <- c(320, 390, 365, 250, 400)
#' sector.serv <- c(150, 390, 320, 400, 210)
#' sector.hh <- c(635, 600, 100, 0, 0)
#'
#' IT <- cbind(sector.agri, sector.manu, sector.serv, sector.hh)
#' rownames(IT) <-  c("agri", "manu", "serv", "lab", "cap")
#'
#' ge <- gemInputOutputTable_easy_5_4(IT)
#'
#' ####
#' ge <- gemInputOutputTable_easy_5_4(supply.capital = 1870)
#' prop.table(ge$z[1:3])

gemInputOutputTable_easy_5_4 <- function(IT = cbind(
                                           sector.agri = c(agri = 260, manu = 345, serv = 400, lab = 200, cap = 160),
                                           sector.manu = c(agri = 320, manu = 390, serv = 365, lab = 250, cap = 400),
                                           sector.serv = c(agri = 150, manu = 390, serv = 320, lab = 400, cap = 210),
                                           sector.hh = c(agri = 635, manu = 600, serv = 385, lab = 0, cap = 0)
                                         ),
                                         supply.labor = 850, #* 1.08,
                                         supply.capital = 770,
                                         es.agri = 0.2,
                                         es.manu = 0.3,
                                         es.serv = 0.1,

                                         es.VA.agri = 0.25,
                                         es.VA.manu = 0.5,
                                         es.VA.serv = 0.8) {
  names.commodity <- rownames(IT)
  names.agent <- colnames(IT)

  d.agri <- IT[, 1]
  d.manu <- IT[, 2]
  d.serv <- IT[, 3]
  d.hh <- IT[, 4]

  dst.agri <- Node$new("sector.agri",
    type = "SCES", sigma = 1 - 1 / es.agri,
    alpha = 1,
    beta = prop.table(c(
      sum(d.agri[1:3]),
      sum(d.agri[4:5])
    ))
  )
  dst.agri$AddChild("cc1.agri",
    type = "Leontief",
    a = prop.table(d.agri[1:3])
  )$
    AddChild("agri")$AddSibling("manu")$AddSibling("serv")$
    parent$
    AddSibling("cc2.agri",
    type = "SCES", sigma = 1 - 1 / es.VA.agri,
    alpha = 1,
    beta = prop.table(d.agri[4:5])
  )$
    AddChild("lab")$AddSibling("cap")

  ##
  dst.manu <- Node$new("sector.manu",
    type = "SCES", sigma = 1 - 1 / es.manu,
    alpha = 1,
    beta = prop.table(c(
      sum(d.manu[1:3]),
      sum(d.manu[4:5])
    ))
  )
  dst.manu$AddChild("cc1.manu",
    type = "Leontief",
    a = prop.table(d.manu[1:3])
  )$
    AddChild("agri")$AddSibling("manu")$AddSibling("serv")$
    parent$
    AddSibling("cc2.manu",
    type = "SCES", sigma = 1 - 1 / es.VA.manu,
    alpha = 1,
    beta = prop.table(d.manu[4:5])
  )$
    AddChild("lab")$AddSibling("cap")

  ##
  dst.serv <- Node$new("sector.serv",
    type = "SCES", sigma = 1 - 1 / es.serv,
    alpha = 1,
    beta = prop.table(c(
      sum(d.serv[1:3]),
      sum(d.serv[4:5])
    ))
  )
  dst.serv$AddChild("cc1.serv",
    type = "Leontief",
    a = prop.table(d.serv[1:3])
  )$
    AddChild("agri")$AddSibling("manu")$AddSibling("serv")$
    parent$
    AddSibling("cc2.serv",
    type = "SCES", sigma = 1 - 1 / es.VA.serv,
    alpha = 1,
    beta = prop.table(d.serv[4:5])
  )$
    AddChild("lab")$AddSibling("cap")

  ##
  dst.hh <- Node$new("sector.hh",
    type = "SCES", sigma = -1,
    alpha = 1,
    beta = prop.table(d.hh[1:3])
  )
  dst.hh$AddChild("agri")$AddSibling("manu")$AddSibling("serv")

  dstl <- list(
    dst.agri,
    dst.manu,
    dst.serv,
    dst.hh
  )

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

  ge$p <- ge$p / ge$p[4]
  ge_tidy(ge, names.commodity, names.agent)
}
