#' @export
#' @title A General Equilibrium Model based on a 7x4 (Standard) Input-Output Table
#' @aliases gemInputOutputTable_7_4
#' @description This is a general equilibrium model based on a 7x4 standard input-output table.
#' There is no negative number in this standard input-output table,
#' and both the input and output parts are 7x4 matrices.
#' The standard input-output table consists of input and output parts with the same dimensions.
#' @details Given a 7x4 input-output table, this model calculates
#' the corresponding general equilibrium.
#' This input-output table contains 3 production sectors and 1 household.
#' The household consumes products and supplies labor, capital, stock and tax receipt.
#' Generally speaking, the value of the elasticity of substitution in this model should be between 0 and 1.
#' @param IT the input part of the input-output table in the base period (unit: trillion yuan).
#' @param OT the output part of the input-output table in the base period (unit: trillion yuan).
#' @param es.agri,es.manu,es.serv the elasticity of substitution between the intermediate input
#' and the value-added input of the agriculture sector, manufacturing sector and service sector.
#' @param es.hh the elasticity of substitution among products consumed by the household sector.
#' @param es.VA.agri,es.VA.manu,es.VA.serv the elasticity of substitution between labor input and capital input
#' of the agriculture sector, manufacturing sector and service sector.
#' @param ... arguments to be transferred to the function sdm of the package CGE.
#' @return A general equilibrium, which is a list with the following elements:
#' \itemize{
#' \item p - the price vector with labor as numeraire.
#' \item D - the demand matrix, also called the input table. Wherein the benchmark prices are used.
#' \item DV - the demand value matrix, also called the value input table. Wherein the current price is used.
#' \item SV - the supply value matrix, also called the value output table. Wherein the current price is used.
#' \item value.added - the value-added of the three production sectors.
#' \item dstl - the demand structure tree list of sectors.
#' \item ... - some elements returned by the sdm2 function.
#' }
#'
#' @examples
#' \donttest{
#' IT2017 <- matrix(c(
#'   1.47, 6.47, 0.57, 2.51,
#'   2.18, 76.32, 12.83, 44.20,
#'   0.82, 19.47, 23.33, 35.61,
#'   6.53, 13.92, 21.88, 0,
#'   0.23, 4.05, 6.76, 0,
#'   0, 6.43, 3.40, 0,
#'   0.13, 8.87, 10.46, 0
#' ), 7, 4, TRUE)
#'
#' OT2017 <- matrix(c(
#'   11.02, 0, 0, 0,
#'   0, 135.53, 0, 0,
#'   0, 0, 79.23, 0,
#'   0, 0, 0, 42.33,
#'   0, 0, 0, 11.04,
#'   0.34, 0, 0, 9.49,
#'   0, 0, 0, 19.46
#' ), 7, 4, TRUE)
#'
#' rownames(IT2017) <- rownames(OT2017) <-
#'   c("agri", "manu", "serv", "lab", "cap", "tax", "dividend")
#' colnames(IT2017) <- colnames(OT2017) <-
#'   c("sector.agri", "sector.manu", "sector.serv", "sector.hh")
#'
#' ge <- gemInputOutputTable_7_4(
#'   IT = IT2017,
#'   OT = OT2017
#' )
#'
#' #### labor supply reduction
#' OTLSR <- OT2017
#' OTLSR["lab", "sector.hh"] <- OTLSR["lab", "sector.hh"] * 0.9
#' geLSR <- gemInputOutputTable_7_4(
#'   IT = IT2017,
#'   OT = OTLSR
#' )
#'
#' geLSR$z / ge$z
#' geLSR$p / ge$p
#'
#' #### capital accumulation
#' OTCA <- OT2017
#' OTCA["cap", "sector.hh"] <- OTCA["cap", "sector.hh"] * 1.1
#' geCA <- gemInputOutputTable_7_4(
#'   IT = IT2017,
#'   OT = OTCA
#' )
#'
#' geCA$z / ge$z
#' geCA$p / ge$p
#'
#' #### technology progress
#' IT.TP <- IT2017
#' IT.TP ["lab", "sector.manu"] <-
#'   IT.TP ["lab", "sector.manu"] * 0.9
#'
#' geTP <- gemInputOutputTable_7_4(
#'   IT = IT.TP,
#'   OT = OT2017
#' )
#'
#' geTP$z / ge$z
#' geTP$p / ge$p
#'
#' ##
#' IT.TP2 <- IT.TP
#' IT.TP2 ["cap", "sector.manu"] <-
#'   IT.TP2["cap", "sector.manu"] * 1.02
#' geTP2 <- gemInputOutputTable_7_4(
#'   IT = IT.TP2,
#'   OT = OT2017
#' )
#'
#' geTP2$z / ge$z
#' geTP2$p / ge$p
#'
#' ##
#' IT.TP3 <- IT2017
#' IT.TP3 ["lab", "sector.manu"] <-
#'   IT.TP3 ["lab", "sector.manu"] * 0.9
#' IT.TP3 ["lab", "sector.agri"] <-
#'   IT.TP3 ["lab", "sector.agri"] * 0.8
#'
#' geTP3 <- gemInputOutputTable_7_4(
#'   IT = IT.TP3,
#'   OT = OT2017
#' )
#'
#' geTP3$value.added / ge$value.added
#' prop.table(geTP3$value.added) - prop.table(ge$value.added)
#'
#' #### demand structure change
#' IT.DSC <- IT2017
#' IT.DSC["serv", "sector.hh"] <- IT.DSC ["serv", "sector.hh"] * 1.2
#'
#' geDSC <- gemInputOutputTable_7_4(
#'   IT = IT.DSC,
#'   OT = OT2017
#' )
#'
#' geDSC$z[1:3] / ge$z[1:3]
#' geDSC$p / ge$p
#'
#' #### tax change
#' OT.TC <- OT2017
#' OT.TC["tax", "sector.agri"] <- OT.TC["tax", "sector.agri"] * 2
#'
#' geTC <- gemInputOutputTable_7_4(
#'   IT = IT2017,
#'   OT = OT.TC
#' )
#'
#' geTC$z / ge$z
#' geTC$p / ge$p
#'
#' ##
#' IT.TC2 <- IT2017
#' IT.TC2["tax", "sector.manu"] <- IT.TC2["tax", "sector.manu"] * 0.8
#'
#' geTC2 <- gemInputOutputTable_7_4(
#'   IT = IT.TC2,
#'   OT = OT2017
#' )
#'
#' geTC2$z / ge$z
#' geTC2$p / ge$p
#' }
#'
gemInputOutputTable_7_4 <- function(IT,
                                    OT,

                                    es.agri = 0,
                                    es.manu = 0,
                                    es.serv = 0,
                                    es.hh = 0,

                                    es.VA.agri = 0.25,
                                    es.VA.manu = 0.5,
                                    es.VA.serv = 0.8,
                                    ...) {

  # the dst of the agri sector
  dst.agri <- node_new(
    "output",
    "cc1", "cc2",
    type = "SCES",
    es = es.agri,
    alpha = OT[1, 1] / sum(IT[, 1]), # alpha = 0.970
    beta = prop.table(c(
      sum(IT[1:3, 1]),
      sum(IT[4:7, 1])
    ))
  )

  node_set(dst.agri, "cc1",
    "agri", "manu", "serv",
    type = "Leontief",
    a = prop.table(IT[1:3, 1])
  )

  node_set(dst.agri, "cc2",
    "cc2.1", "tax", "dividend",
    type = "FIN",
    rate = c(
      sum(IT[4:5, 1]) / sum(IT[4:7, 1]),
      tax.rate.agri = IT["tax", "sector.agri"] /
        sum(IT[c("lab", "cap"), "sector.agri"]),
      dividend.rate.agri = IT["dividend", "sector.agri"] /
        sum(IT[c("lab", "cap"), "sector.agri"])
    )
  )

  node_set(dst.agri, "cc2.1",
    "lab", "cap",
    type = "SCES",
    es = es.VA.agri,
    alpha = 1,
    beta = prop.table(IT[4:5, 1])
  )

  # the dst of the manu sector
  dst.manu <- Clone(dst.agri)

  node_set(dst.manu, "output",
    type = "SCES",
    es = es.manu, # 即es = 0
    alpha = OT[2, 2] / sum(IT[, 2]), # alpha = 1
    beta = prop.table(c(
      sum(IT[1:3, 2]), # intermediate input
      sum(IT[4:7, 2]) # value added
    )) # beta = c(0.755, 0.245)
  )

  node_set(dst.manu, "cc1",
    type = "Leontief",
    a = prop.table(IT[1:3, 2]) # a = c(0.063,0.746, 0.191)
  )

  node_set(dst.manu, "cc2",
    type = "FIN",
    rate = c(
      # amount of labor and fixed asset depreciation included in unit value added
      sum(IT[4:5, 2]) / sum(IT[4:7, 2]),
      tax.rate.manu = IT["tax", "sector.manu"] /
        sum(IT[c("lab", "cap"), "sector.manu"]),
      dividend.rate.manu = IT["dividend", "sector.manu"] /
        sum(IT[c("lab", "cap"), "sector.manu"])
    ) # rate = c(0.540, 0.358, 0.494)
  )

  node_set(dst.manu, "cc2.1",
    type = "SCES",
    es = es.VA.manu, # es = 0.5
    alpha = 1,
    beta = prop.table(IT[4:5, 2]) # beta = c(0.775, 0.225)
  )

  # the dst of the serv sector
  dst.serv <- Clone(dst.agri)

  node_set(dst.serv, "output",
    type = "SCES",
    es = es.serv,
    alpha = OT[3, 3] / sum(IT[, 3]),
    beta = prop.table(c(
      sum(IT[1:3, 3]),
      sum(IT[4:7, 3])
    ))
  )

  node_set(dst.serv, "cc1",
    type = "Leontief",
    a = prop.table(IT[1:3, 3])
  )

  node_set(dst.serv, "cc2",
    type = "FIN",
    rate = c(
      sum(IT[4:5, 3]) / sum(IT[4:7, 3]),
      tax.rate.serv = IT["tax", "sector.serv"] /
        sum(IT[c("lab", "cap"), "sector.serv"]),
      dividend.rate.serv = IT["dividend", "sector.serv"] /
        sum(IT[c("lab", "cap"), "sector.serv"])
    )
  )

  node_set(dst.serv, "cc2.1",
    type = "SCES",
    es = es.VA.serv,
    alpha = 1,
    beta = prop.table(IT[4:5, 3])
  )

  # the dst of the hh sector
  dst.hh <- node_new("util",
    type = "SCES", es = es.hh,
    alpha = 1,
    beta = prop.table(IT[1:3, 4]),
    "agri", "manu", "serv"
  )

  ## computating equilibrium
  dstl <- list(dst.agri, dst.manu, dst.serv, dst.hh)

  ge <- sdm2(
    A = dstl,
    B = matrix(c(
      1, 0, 0, 0,
      0, 1, 0, 0,
      0, 0, 1, 0,
      0, 0, 0, 0,
      0, 0, 0, 0,
      0, 0, 0, 0,
      0, 0, 0, 0
    ), 7, 4, TRUE),
    S0Exg = {
      tmp <- OT
      tmp[tmp == 0] <- NA
      tmp[1:3, 1:3] <- NA
      tmp
    },
    names.commodity = rownames(IT),
    names.agent = colnames(IT),
    numeraire = "lab",
    tolCond = 1e-8
  )

  ge$value.added <- colSums(ge$DV[4:7, 1:3]) - ge$SV["tax", 1:3]
  ge$dstl <- dstl
  return(ge)
}
