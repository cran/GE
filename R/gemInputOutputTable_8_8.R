#' @export
#' @title A General Equilibrium Model based on an 8x8 Input-Output Table
#' @aliases gemInputOutputTable_8_8
#' @description This is a general equilibrium model based on a 8x8 input-output table.
#' @details Given an 8x8 input-output table, this model calculates
#' the corresponding general equilibrium.
#' This input-output table contains 3 production sectors, 1 household, 1 foreign trade sector importing agriculture goods,
#' 1 foreign trade sector importing manufacturing goods, 1 foreign trade sector importing service, 1 foreign trade sector importing bond.
#' There are 8 kinds of commodities (or subjects) in the table, i.e. agriculture product,
#' manufacturing product, service, labor, capital goods, tax, dividend and bond of ROW (i.e. the rest of the world).
#' The household consumes products and supplies labor, capital, stock and tax receipt.
#' Generally speaking, the value of the elasticity of substitution in this model should be between 0 and 1.
#' @param IT the input part of the input-output table in the base period (unit: trillion yuan).
#' @param OT the output part of the input-output table in the base period (unit: trillion yuan).
#' @param es.agri,es.manu,es.serv the elasticity of substitution between the intermediate input
#' and the value-added input of the agriculture sector, manufacturing sector and service sector.
#' @param es.hh the elasticity of substitution among products consumed by the household sector.
#' @param es.FT the elasticity of substitution among exported products.
#' @param es.VA.agri,es.VA.manu,es.VA.serv the elasticity of substitution between labor input and capital input
#' of the agriculture sector, manufacturing sector and service sector.
#' @param es.prodcctDI the elasticity of substitution between domestic product and imported product.
#' @param ... arguments to be transferred to the function sdm of the package CGE.
#' @return A general equilibrium, which is a list with the following elements:
#' \itemize{
#' \item p - the price vector with labor as numeraire.
#' \item D - the demand matrix, also called the input table. Wherein the benchmark prices are used.
#' \item DV - the demand value matrix, also called the value input table. Wherein the current price is used.
#' \item SV - the supply value matrix, also called the value output table. Wherein the current price is used.
#' \item value.added - the value-added of the three production sectors.
#' \item dstl - the demand structure tree list of sectors.
#' \item ... - some elements returned by the CGE::sdm function
#' }
#'
#' @examples
#' \donttest{
#' IT17 <- matrix(c(
#'   1.47, 6.47, 0.57, 2.99, 0.12 * 0.60 / (0.60 + 12.10 + 2.23 + 1.45),
#'   0.12 * 12.10 / (0.60 + 12.10 + 2.23 + 1.45),
#'   0.12 * 2.23 / (0.60 + 12.10 + 2.23 + 1.45),
#'   0.12 * 1.45 / (0.60 + 12.10 + 2.23 + 1.45),
#'
#'   2.18, 76.32, 12.83, 43, 13.30 * 0.60 / (0.60 + 12.10 + 2.23 + 1.45),
#'   13.30 * 12.10 / (0.60 + 12.10 + 2.23 + 1.45),
#'   13.30 * 2.23 / (0.60 + 12.10 + 2.23 + 1.45),
#'   13.30 * 1.45 / (0.60 + 12.10 + 2.23 + 1.45),
#'
#'
#'   0.82, 19.47, 23.33, 34.88, 2.96 * 0.60 / (0.60 + 12.10 + 2.23 + 1.45),
#'   2.96 * 12.10 / (0.60 + 12.10 + 2.23 + 1.45),
#'   2.96 * 2.23 / (0.60 + 12.10 + 2.23 + 1.45),
#'   2.96 * 1.45 / (0.60 + 12.10 + 2.23 + 1.45),
#'
#'   6.53, 13.92, 21.88, 0, 0, 0, 0, 0,
#'   0.23, 4.05, 6.76, 0, 0, 0, 0, 0,
#'   0, 6.43, 3.40, 0, 0, 0, 0, 0,
#'   0.13, 8.87, 10.46, 0, 0, 0, 0, 0,
#'   0, 0, 0, 1.45, 0, 0, 0, 0
#' ), 8, 8, TRUE)
#'
#' OT17 <- matrix(c(
#'   11.02, 0, 0, 0, 0.60, 0, 0, 0,
#'   0, 135.53, 0, 0, 0, 12.10, 0, 0,
#'   0, 0, 79.23, 0, 0, 0, 2.23, 0,
#'   0, 0, 0, 42.33, 0, 0, 0, 0,
#'   0, 0, 0, 11.04, 0, 0, 0, 0,
#'   0.34, 0, 0, 9.49, 0, 0, 0, 0,
#'   0, 0, 0, 19.46, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 0, 0, 1.45
#' ), 8, 8, TRUE)
#'
#' rownames(IT17) <- rownames(OT17) <-
#'   c("agri", "manu", "serv", "lab", "cap", "tax", "dividend", "bond.ROW")
#' colnames(IT17) <- colnames(OT17) <- c(
#'   "sector.agri", "sector.manu", "sector.serv", "sector.hh",
#'   "sector.FT.agri", "sector.FT.manu", "sector.FT.serv", "sector.FT.bond.ROW"
#' )
#'
#'
#' ge <- gemInputOutputTable_8_8(
#'   IT = IT17,
#'   OT = OT17
#' )
#'
#' #### technology progress
#' IT.TP <- IT17
#' IT.TP ["lab", "sector.manu"] <-
#'   IT.TP ["lab", "sector.manu"] * 0.9
#'
#' geTP <- gemInputOutputTable_8_8(
#'   IT = IT.TP,
#'   OT = OT17
#' )
#'
#' geTP$z / ge$z
#' geTP$p / ge$p
#' geTP$value.added
#' prop.table(geTP$value.added) - prop.table(ge$value.added)
#'
#' #### capital accumulation
#' OT.CA <- OT17
#' OT.CA["cap", "sector.hh"] <- OT.CA["cap", "sector.hh"] * 1.1
#' geCA <- gemInputOutputTable_8_8(
#'   IT = IT17,
#'   OT = OT.CA
#' )
#'
#' geCA$z / ge$z
#' geCA$p / ge$p
#' geCA$p
#' geCA$value.added
#' prop.table(geCA$value.added) - prop.table(ge$value.added)
#'
#' #### tax change
#' OT.TC <- OT17
#' OT.TC["tax", "sector.agri"] <- OT.TC["tax", "sector.agri"] * 2
#'
#' geTC <- gemInputOutputTable_8_8(
#'   IT = IT17,
#'   OT = OT.TC
#' )
#'
#' geTC$z / ge$z
#' geTC$p / ge$p
#'
#' ##
#' IT.TC2 <- IT17
#' IT.TC2["tax", "sector.manu"] <- IT.TC2["tax", "sector.manu"] * 0.8
#'
#' geTC2 <- gemInputOutputTable_8_8(
#'   IT = IT.TC2,
#'   OT = OT17
#' )
#'
#' geTC2$z / ge$z
#' geTC2$p / ge$p
#' }

gemInputOutputTable_8_8 <- function(IT,
                                    OT,

                                    es.agri = 0,
                                    es.manu = 0,
                                    es.serv = 0,
                                    es.hh = 0,
                                    es.FT = 0,

                                    es.VA.agri = 0.25,
                                    es.VA.manu = 0.5,
                                    es.VA.serv = 0.8,

                                    es.prodcctDI = 0.5,
                                    ...) {
  names.commodity <- c(
    "agriD", "manuD", "servD", "agriI", "manuI", "servI",
    "lab", "cap", "tax", "dividend", "bond.ROW"
  )
  names.agent <- c(
    "sector.agri", "sector.manu", "sector.serv", "sector.hh",
    "sector.FT.agri", "sector.FT.manu", "sector.FT.serv", "sector.FT.bond.ROW"
  )

  product.used <- rowSums(IT[1:3, 1:4])
  productI <- rowSums(OT[1:3, c("sector.FT.agri", "sector.FT.manu", "sector.FT.serv")])
  prop.productI <- productI / product.used

  d.agri <- IT[, 1]
  d.manu <- IT[, 2]
  d.serv <- IT[, 3]
  d.hh <- IT[, 4]

  tmp.tax <- IT["tax", ]
  supply.tax <- sum(tmp.tax[tmp.tax > 0])
  supply.stock <- sum(IT["dividend", ])

  if (IT["tax", "sector.agri"] >= 0) {
    tax.rate.agri <- IT["tax", "sector.agri"] / sum(IT[c("lab", "cap"), "sector.agri"])
    supply.tax.hh <- supply.tax
    supply.tax.agri <- NA
  } else {
    tax.rate.agri <- 0
    supply.tax.agri <- -IT["tax", "sector.agri"]
    supply.tax.hh <- supply.tax - supply.tax.agri
  }

  tax.rate.manu <- IT["tax", "sector.manu"] / sum(IT[c("lab", "cap"), "sector.manu"])
  tax.rate.serv <- IT["tax", "sector.serv"] / sum(IT[c("lab", "cap"), "sector.serv"])

  dividend.rate.agri <- IT["dividend", "sector.agri"] / sum(IT[c("lab", "cap"), "sector.agri"])
  dividend.rate.manu <- IT["dividend", "sector.manu"] / sum(IT[c("lab", "cap"), "sector.manu"])
  dividend.rate.serv <- IT["dividend", "sector.serv"] / sum(IT[c("lab", "cap"), "sector.serv"])


  # dst industry ------------------------------------------------------------
  dst.industry <- Node$new("industry")
  dst.industry$AddChild("cc1")$
    AddChild("cc.agri",
    type = "SCES",
    alpha = 1,
    beta = c(1 - prop.productI["agri"], prop.productI["agri"]),
    es = es.prodcctDI
  )$
    AddSibling("cc.manu",
    type = "SCES",
    alpha = 1,
    beta = c(1 - prop.productI["manu"], prop.productI["manu"]),
    es = es.prodcctDI
  )$
    AddSibling("cc.serv",
    type = "SCES",
    alpha = 1,
    beta = c(1 - prop.productI["serv"], prop.productI["serv"]),
    es = es.prodcctDI
  )
  FindNode(dst.industry, "industry")$AddChild("cc2")$
    AddChild("cc2.1")$
    AddChild("lab")$AddSibling("cap")
  FindNode(dst.industry, "cc2")$AddChild("tax")$AddSibling("dividend")

  FindNode(dst.industry, "cc.agri")$AddChild("agriD")$AddSibling("agriI")
  FindNode(dst.industry, "cc.manu")$AddChild("manuD")$AddSibling("manuI")
  FindNode(dst.industry, "cc.serv")$AddChild("servD")$AddSibling("servI")

  # dst.agri ----------------------------------------------------------------
  dst.agri <- Clone(dst.industry)

  tmp <- FindNode(dst.agri, "industry")
  tmp$type <- "SCES"
  tmp$sigma <- 1 - 1 / es.agri
  tmp$alpha <- OT["agri", "sector.agri"] / sum(d.agri)
  tmp$beta <- prop.table(c(
    sum(d.agri[1:3]),
    sum(d.agri[4:7])
  ))


  tmp <- FindNode(dst.agri, "cc1")
  tmp$type <- "Leontief"
  tmp$a <- prop.table(d.agri[1:3])

  tmp <- FindNode(dst.agri, "cc2")
  tmp$type <- "FIN"
  tmp$beta <- prop.table(c(sum(d.agri[4:5]), d.agri[6], d.agri[7]))


  tmp <- FindNode(dst.agri, "cc2.1")
  tmp$type <- "SCES"
  tmp$sigma <- 1 - 1 / es.VA.agri
  tmp$alpha <- 1
  tmp$beta <- prop.table(d.agri[4:5])

  # dst.manu ----------------------------------------------------------------
  dst.manu <- Clone(dst.industry)

  tmp <- FindNode(dst.manu, "industry")
  tmp$type <- "SCES"
  tmp$sigma <- 1 - 1 / es.manu
  tmp$alpha <- OT["manu", "sector.manu"] / sum(d.manu)
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
  tmp$type <- "SCES"
  tmp$sigma <- 1 - 1 / es.VA.manu
  tmp$alpha <- 1
  tmp$beta <- prop.table(d.manu[4:5])

  # dst.serv ----------------------------------------------------------------
  dst.serv <- Clone(dst.industry)

  tmp <- FindNode(dst.serv, "industry")
  tmp$type <- "SCES"
  tmp$sigma <- 1 - 1 / es.serv
  tmp$alpha <- OT["serv", "sector.serv"] / sum(d.serv)
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
  tmp$type <- "SCES"
  tmp$sigma <- 1 - 1 / es.VA.serv
  tmp$alpha <- 1
  tmp$beta <- prop.table(d.serv[4:5])


  # dst.hh ------------------------------------------------------------------
  dst.hh <- Node$new("hh",
    type = "FIN",
    beta = prop.table(c(
      sum(IT[, "sector.hh"]) - IT["bond.ROW", "sector.hh"],
      IT["bond.ROW", "sector.hh"]
    ))
  )

  dst.hh$AddChild("cc1",
    type = "SCES",
    es = es.hh,
    alpha = 1,
    beta = prop.table(d.hh[1:3])
  )$AddChild("cc.agri",
    type = "SCES",
    alpha = 1,
    beta = c(1 - prop.productI["agri"], prop.productI["agri"]),
    es = es.prodcctDI
  )$
    AddSibling("cc.manu",
    type = "SCES",
    alpha = 1,
    beta = c(1 - prop.productI["manu"], prop.productI["manu"]),
    es = es.prodcctDI
  )$
    AddSibling("cc.serv",
    type = "SCES",
    alpha = 1,
    beta = c(1 - prop.productI["serv"], prop.productI["serv"]),
    es = es.prodcctDI
  )

  dst.hh$AddChild("bond.ROW")

  FindNode(dst.hh, "cc.agri")$AddChild("agriD")$AddSibling("agriI")
  FindNode(dst.hh, "cc.manu")$AddChild("manuD")$AddSibling("manuI")
  FindNode(dst.hh, "cc.serv")$AddChild("servD")$AddSibling("servI")


  # dst.FT ------------------------------------------------------------------
  dst.FT.agri <- Node$new("FT.agri",
    type = "SCES",
    es = es.FT,
    alpha = sum(OT[, "sector.FT.agri"]) / sum(IT[, "sector.FT.agri"]),
    beta = prop.table(IT[1:3, "sector.FT.agri"])
  )
  dst.FT.agri$AddChild("agriD")$AddSibling("manuD")$AddSibling("servD")


  dst.FT.manu <- Node$new("FT.manu",
    type = "SCES",
    es = es.FT,
    alpha = sum(OT[, "sector.FT.manu"]) / sum(IT[, "sector.FT.manu"]),
    beta = prop.table(IT[1:3, "sector.FT.manu"])
  )
  dst.FT.manu$AddChild("agriD")$AddSibling("manuD")$AddSibling("servD")


  dst.FT.serv <- Node$new("FT.serv",
    type = "SCES",
    es = es.FT,
    alpha = sum(OT[, "sector.FT.serv"]) / sum(IT[, "sector.FT.serv"]),
    beta = prop.table(IT[1:3, "sector.FT.serv"])
  )
  dst.FT.serv$AddChild("agriD")$AddSibling("manuD")$AddSibling("servD")


  dst.FT.bond.ROW <- Node$new("FT.bond.ROW",
    type = "SCES",
    es = es.FT,
    alpha = sum(OT[, "sector.FT.bond.ROW"]) / sum(IT[, "sector.FT.bond.ROW"]),
    beta = prop.table(IT[1:3, "sector.FT.bond.ROW"])
  )
  dst.FT.bond.ROW$AddChild("agriD")$AddSibling("manuD")$AddSibling("servD")


  dstl <- list(
    dst.agri,
    dst.manu,
    dst.serv,
    dst.hh,
    dst.FT.agri,
    dst.FT.manu,
    dst.FT.serv,
    dst.FT.bond.ROW
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
        1, 0, 0, 0, 0, 0, 0, 0,
        0, 1, 0, 0, 0, 0, 0, 0,
        0, 0, 1, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 1, 0, 0, 0,
        0, 0, 0, 0, 0, 1, 0, 0,
        0, 0, 0, 0, 0, 0, 1, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 1
      ), 11, 8, TRUE)
    },
    S0Exg = {
      tmp <- matrix(NA, 11, 8, TRUE)
      colnames(tmp) <- names.agent
      rownames(tmp) <- names.commodity
      tmp["lab", "sector.hh"] <- sum(OT["lab", ])
      tmp["cap", "sector.hh"] <- sum(OT["cap", ])
      tmp["tax", ] <- OT["tax", ]
      tmp["dividend", "sector.hh"] <- sum(OT["dividend", ])
      tmp
    },
    tolCond = 1e-8,
    ...
  )

  names(ge$p) <- names.commodity
  ge$p <- ge$p / ge$p["lab"]

  ge <- ge_tidy(ge, names.commodity, names.agent)

  ge$value.added <- colSums(ge$DV[
    c("lab", "cap", "tax", "dividend"),
    c("sector.agri", "sector.manu", "sector.serv")
  ]) - ge$SV["tax", c("sector.agri", "sector.manu", "sector.serv")]

  ge$dstl <- dstl
  return(ge)
}
