#' @export
#' @title A General Equilibrium Model based on a 7x4 Standard Input-Output Table
#' @aliases gemStandardInputOutputTable_7_4
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
#' @return A general equilibrium.
#' @seealso \cite{\link{gemInputOutputTable_7_4}}
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
#' ge <- gemStandardInputOutputTable_7_4(
#'   IT = IT2017,
#'   OT = OT2017
#' )
#'
#'
#'
#' #### labor supply reduction
#' OTLSR <- OT2017
#' OTLSR["lab", "sector.hh"] <- OTLSR["lab", "sector.hh"] * 0.9
#' geLSR <- gemStandardInputOutputTable_7_4(
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
#' geCA <- gemStandardInputOutputTable_7_4(
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
#' geTP <- gemStandardInputOutputTable_7_4(
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
#' geTP2 <- gemStandardInputOutputTable_7_4(
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
#' geTP3 <- gemStandardInputOutputTable_7_4(
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
#' geDSC <- gemStandardInputOutputTable_7_4(
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
#' geTC <- gemStandardInputOutputTable_7_4(
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
#' geTC2 <- gemStandardInputOutputTable_7_4(
#'   IT = IT.TC2,
#'   OT = OT2017
#' )
#'
#' geTC2$z / ge$z
#' geTC2$p / ge$p
#' }

gemStandardInputOutputTable_7_4 <- function(IT,
                                           OT,

                                           es.agri = 0,
                                           es.manu = 0,
                                           es.serv = 0,
                                           es.hh = 0,

                                           es.VA.agri = 0.25,
                                           es.VA.manu = 0.5,
                                           es.VA.serv = 0.8,
                                           ...) {
  product.output <- diag(OT[1:3, 1:3])

  names.commodity <- rownames(IT)
  names.agent <- colnames(IT)

  d.agri <- IT[, 1]
  d.manu <- IT[, 2]
  d.serv <- IT[, 3]
  d.hh <- IT[, 4]


  tax.rate.agri <- IT["tax", "sector.agri"] / sum(IT[c("lab", "cap"), "sector.agri"])
  tax.rate.manu <- IT["tax", "sector.manu"] / sum(IT[c("lab", "cap"), "sector.manu"])
  tax.rate.serv <- IT["tax", "sector.serv"] / sum(IT[c("lab", "cap"), "sector.serv"])

  dividend.rate.agri <- IT["dividend", "sector.agri"] / sum(IT[c("lab", "cap"), "sector.agri"])
  dividend.rate.manu <- IT["dividend", "sector.manu"] / sum(IT[c("lab", "cap"), "sector.manu"])
  dividend.rate.serv <- IT["dividend", "sector.serv"] / sum(IT[c("lab", "cap"), "sector.serv"])

  dst.industry <- Node$new("industry")
  dst.industry$AddChild("cc1")$
    AddChild("agri")$AddSibling("manu")$AddSibling("serv")$
    parent$
    AddSibling("cc2")$
    AddChild("cc2.1")$
    AddChild("lab")$AddSibling("cap")$
    parent$
    AddSibling("tax")$AddSibling("dividend")


  # dst.agri ----------------------------------------------------------------
  dst.agri <- Clone(dst.industry)

  tmp <- FindNode(dst.agri, "industry")
  tmp$type <- "CES"
  tmp$sigma <- 1 - 1 / es.agri
  tmp$alpha <- product.output[1] / sum(d.agri)
  tmp$beta <- prop.table(c(
    sum(d.agri[1:3]),
    sum(d.agri[4:7])
  ))


  tmp <- FindNode(dst.agri, "cc1")
  tmp$type <- "Leontief"
  tmp$a <- prop.table(d.agri[1:3])

  tmp <- FindNode(dst.agri, "cc2")
  tmp$type <- "FIN"
  tmp$rate <- c(sum(d.agri[4:5]) / sum(d.agri[4:7]), tax.rate.agri, dividend.rate.agri)


  tmp <- FindNode(dst.agri, "cc2.1")
  tmp$type <- "CES"
  tmp$sigma <- 1 - 1 / es.VA.agri
  tmp$alpha <- 1
  tmp$beta <- prop.table(d.agri[4:5])

  # dst.manu ----------------------------------------------------------------
  dst.manu <- Clone(dst.industry)

  tmp <- FindNode(dst.manu, "industry")
  tmp$type <- "CES"
  tmp$sigma <- 1 - 1 / es.manu
  tmp$alpha <- product.output[2] / sum(d.manu)
  tmp$beta <- prop.table(c(
    sum(d.manu[1:3]),
    sum(d.manu[4:7])
  ))


  tmp <- FindNode(dst.manu, "cc1")
  tmp$type <- "Leontief"
  tmp$a <- prop.table(d.manu[1:3])

  tmp <- FindNode(dst.manu, "cc2")
  tmp$type <- "FIN"
  tmp$rate <- c(sum(d.manu[4:5]) / sum(d.manu[4:7]), tax.rate.manu, dividend.rate.manu)


  tmp <- FindNode(dst.manu, "cc2.1")
  tmp$type <- "CES"
  tmp$sigma <- 1 - 1 / es.VA.manu
  tmp$alpha <- 1
  tmp$beta <- prop.table(d.manu[4:5])

  # dst.serv ----------------------------------------------------------------
  dst.serv <- Clone(dst.industry)

  tmp <- FindNode(dst.serv, "industry")
  tmp$type <- "CES"
  tmp$sigma <- 1 - 1 / es.serv
  tmp$alpha <- product.output[3] / sum(d.serv)
  tmp$beta <- prop.table(c(
    sum(d.serv[1:3]),
    sum(d.serv[4:7])
  ))


  tmp <- FindNode(dst.serv, "cc1")
  tmp$type <- "Leontief"
  tmp$a <- prop.table(d.serv[1:3])

  tmp <- FindNode(dst.serv, "cc2")
  tmp$type <- "FIN"
  tmp$rate <- c(sum(d.serv[4:5]) / sum(d.serv[4:7]), tax.rate.serv, dividend.rate.serv)


  tmp <- FindNode(dst.serv, "cc2.1")
  tmp$type <- "CES"
  tmp$sigma <- 1 - 1 / es.VA.serv
  tmp$alpha <- 1
  tmp$beta <- prop.table(d.serv[4:5])


  # dst.hh ------------------------------------------------------------------
  dst.hh <- Node$new("hh",
    type = "CES", sigma = 1 - 1 / es.hh,
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
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 7, 4, TRUE)
    },
    S0Exg = {
      tmp <- matrix(NA, 7, 4, TRUE)
      colnames(tmp) <- names.agent
      rownames(tmp) <- names.commodity
      tmp["lab", "sector.hh"] <- sum(OT["lab", ])
      tmp["cap", "sector.hh"] <- sum(OT["cap", ])

      tmp["tax", "sector.agri"] <- OT["tax", "sector.agri"]
      tmp["tax", "sector.hh"] <- sum(IT["tax", ]) - OT["tax", "sector.agri"]

      tmp["dividend", "sector.hh"] <- sum(OT["dividend", ])
      tmp
    },
    tolCond = 1e-8,
    ...
  )

  ge$p <- ge$p / ge$p[4]

  ge <- ge_tidy(ge, names.commodity, names.agent)


  ge$value.added <- colSums(ge$DV[4:7, 1:3]) - ge$SV["tax", 1:3]
  ge$dstl <- dstl
  return(ge)
}

