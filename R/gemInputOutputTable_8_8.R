#' @export
#' @title A General Equilibrium Model based on an 8×8 Input-Output Table
#' @aliases gemInputOutputTable_8_8
#' @description This is a general equilibrium model based on a 8×8 input-output table.
#' @details Given an 8×8 input-output table, this model calculates
#' the corresponding general equilibrium.
#' This input-output table contains 3 production sectors, 1 consumption and (temporarily unproductive) investment sector (CI sector), 1 foreign trade sector importing agriculture goods,
#' 1 foreign trade sector importing manufacturing goods, 1 foreign trade sector importing service, 1 foreign trade sector importing bond.
#' There are 8 kinds of commodities (or subjects) in the table, i.e. agriculture product,
#' manufacturing product, service, labor, capital goods, tax, dividend and bond of ROW (i.e. the rest of the world).
#' The CI sector uses products and supplies labor, capital, stock and tax receipt.
#' Generally speaking, the value of the elasticity of substitution in this model should be between 0 and 1.
#' @param IT the input part of the input-output table in the base period (unit: trillion yuan).
#' @param OT the output part of the input-output table in the base period (unit: trillion yuan).
#' @param es.agri,es.manu,es.serv the elasticity of substitution between the intermediate input
#' and the value-added input of the agriculture sector, manufacturing sector and service sector.
#' @param es.CI the elasticity of substitution among products used by the CI sector.
#' @param es.FT the elasticity of substitution among exported products.
#' @param es.VA.agri,es.VA.manu,es.VA.serv the elasticity of substitution between labor input and capital input
#' of the agriculture sector, manufacturing sector and service sector.
#' @param es.prodDM the elasticity of substitution between domestic product and imported product.
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
#'   "sector.agri", "sector.manu", "sector.serv", "sector.CI",
#'   "sector.FT.agri", "sector.FT.manu", "sector.FT.serv", "sector.FT.bond.ROW"
#' )
#'
#' # the benchmark equilibrium.
#' ge <- gemInputOutputTable_8_8(
#'   IT = IT17,
#'   OT = OT17
#' )
#'
#' #### technology progress.
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
#' #### capital accumulation.
#' OT.CA <- OT17
#' OT.CA["cap", "sector.CI"] <- OT.CA["cap", "sector.CI"] * 1.1
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
#' #### tax change.
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
                                    es.CI = 0,
                                    es.FT = 0,
                                    es.VA.agri = 0.25,
                                    es.VA.manu = 0.5,
                                    es.VA.serv = 0.8,
                                    es.prodDM = 0.5,
                                    ...) {
  names.commodity <- c(
    "agriD", "manuD", "servD", "agriI", "manuI", "servI",
    "lab", "cap", "tax", "dividend", "bond.ROW"
  )
  names.agent <- c(
    "sector.agri", "sector.manu", "sector.serv", "sector.CI",
    "sector.FT.agri", "sector.FT.manu", "sector.FT.serv", "sector.FT.bond.ROW"
  )

  product.used <- rowSums(IT[1:3, 1:4])
  productI <- rowSums(OT[1:3, c("sector.FT.agri", "sector.FT.manu", "sector.FT.serv")])
  prop.productI <- productI / product.used

  d.agri <- IT[, 1]
  d.manu <- IT[, 2]
  d.serv <- IT[, 3]
  d.CI <- IT[, 4]

  tmp.tax <- IT["tax", ]
  supply.tax <- sum(tmp.tax[tmp.tax > 0])
  supply.stock <- sum(IT["dividend", ])

  if (IT["tax", "sector.agri"] >= 0) {
    tax.rate.agri <- IT["tax", "sector.agri"] / sum(IT[c("lab", "cap"), "sector.agri"])
    supply.tax.CI <- supply.tax
    supply.tax.agri <- NA
  } else {
    tax.rate.agri <- 0
    supply.tax.agri <- -IT["tax", "sector.agri"]
    supply.tax.CI <- supply.tax - supply.tax.agri
  }

  tax.rate.manu <- IT["tax", "sector.manu"] / sum(IT[c("lab", "cap"), "sector.manu"])
  tax.rate.serv <- IT["tax", "sector.serv"] / sum(IT[c("lab", "cap"), "sector.serv"])

  dividend.rate.agri <- IT["dividend", "sector.agri"] / sum(IT[c("lab", "cap"), "sector.agri"])
  dividend.rate.manu <- IT["dividend", "sector.manu"] / sum(IT[c("lab", "cap"), "sector.manu"])
  dividend.rate.serv <- IT["dividend", "sector.serv"] / sum(IT[c("lab", "cap"), "sector.serv"])

  # dst industry ------------------------------------------------------------
  dst.industry <- node_new(
    "output",
    "cc1", "cc2"
  )
  node_set(
    dst.industry, "cc1",
    "cc.agri", "cc.manu", "cc.serv"
  )

  node_set(dst.industry, "cc.agri",
    type = "SCES",
    alpha = 1,
    beta = c(1 - prop.productI["agri"], prop.productI["agri"]),
    es = es.prodDM,
    "agriD", "agriI"
  )

  node_set(dst.industry, "cc.manu",
    type = "SCES",
    alpha = 1,
    beta = c(1 - prop.productI["manu"], prop.productI["manu"]),
    es = es.prodDM,
    "manuD", "manuI"
  )

  node_set(dst.industry, "cc.serv",
    type = "SCES",
    alpha = 1,
    beta = c(1 - prop.productI["serv"], prop.productI["serv"]),
    es = es.prodDM,
    "servD", "servI"
  )

  node_set(
    dst.industry, "cc2",
    "cc2.1", "tax", "dividend"
  )

  node_set(
    dst.industry, "cc2.1",
    "lab", "cap"
  )

  # dst.agri ----------------------------------------------------------------
  dst.agri <- Clone(dst.industry)

  dst.agri$type <- "SCES"
  dst.agri$sigma <- 1 - 1 / es.agri
  dst.agri$alpha <- OT["agri", "sector.agri"] / sum(d.agri)
  dst.agri$beta <- prop.table(c(
    sum(d.agri[1:3]),
    sum(d.agri[4:7])
  ))

  node_set(dst.agri, "cc1",
    type = "Leontief",
    a = prop.table(d.agri[1:3])
  )

  node_set(dst.agri, "cc2",
    type = "FIN",
    beta = prop.table(c(sum(d.agri[4:5]), d.agri[6], d.agri[7]))
  )


  node_set(dst.agri, "cc2.1",
    type = "SCES",
    sigma = 1 - 1 / es.VA.agri,
    alpha = 1,
    beta = prop.table(d.agri[4:5])
  )

  # dst.manu ----------------------------------------------------------------
  dst.manu <- Clone(dst.industry)

  dst.manu$type <- "SCES"
  dst.manu$sigma <- 1 - 1 / es.manu
  dst.manu$alpha <- OT["manu", "sector.manu"] / sum(d.manu)
  dst.manu$beta <- prop.table(c(
    sum(d.manu[1:3]),
    sum(d.manu[4:7])
  ))

  node_set(dst.manu, "cc1",
    type = "Leontief",
    a = prop.table(d.manu[1:3])
  )

  node_set(dst.manu, "cc2",
    type = "FIN",
    rate = c(sum(d.manu[4:5]) / sum(d.manu[4:7]), tax.rate.manu, dividend.rate.manu)
  )

  node_set(dst.manu, "cc2.1",
    type = "SCES",
    sigma = 1 - 1 / es.VA.manu,
    alpha = 1,
    beta = prop.table(d.manu[4:5])
  )

  # dst.serv ----------------------------------------------------------------
  dst.serv <- Clone(dst.industry)

  dst.serv$type <- "SCES"
  dst.serv$sigma <- 1 - 1 / es.serv
  dst.serv$alpha <- OT["serv", "sector.serv"] / sum(d.serv)
  dst.serv$beta <- prop.table(c(
    sum(d.serv[1:3]),
    sum(d.serv[4:7])
  ))

  node_set(dst.serv, "cc1",
    type = "Leontief",
    a = prop.table(d.serv[1:3])
  )

  node_set(dst.serv, "cc2",
    type = "FIN",
    rate = c(sum(d.serv[4:5]) / sum(d.serv[4:7]), tax.rate.serv, dividend.rate.serv)
  )

  node_set(dst.serv, "cc2.1",
    type = "SCES",
    sigma = 1 - 1 / es.VA.serv,
    alpha = 1,
    beta = prop.table(d.serv[4:5])
  )

  # dst.CI ------------------------------------------------------------------
  dst.CI <- node_new("util",
    type = "FIN",
    beta = prop.table(c(
      sum(IT[, "sector.CI"]) - IT["bond.ROW", "sector.CI"],
      IT["bond.ROW", "sector.CI"]
    )),
    "cc1", "bond.ROW"
  )

  node_set(dst.CI, "cc1",
    type = "SCES",
    es = es.CI,
    alpha = 1,
    beta = prop.table(d.CI[1:3]),
    "cc.agri", "cc.manu", "cc.serv"
  )

  node_set(dst.CI, "cc.agri",
    type = "SCES",
    alpha = 1,
    beta = c(1 - prop.productI["agri"], prop.productI["agri"]),
    es = es.prodDM,
    "agriD", "agriI"
  )

  node_set(dst.CI, "cc.manu",
    type = "SCES",
    alpha = 1,
    beta = c(1 - prop.productI["manu"], prop.productI["manu"]),
    es = es.prodDM,
    "manuD", "manuI"
  )

  node_set(dst.CI, "cc.serv",
    type = "SCES",
    alpha = 1,
    beta = c(1 - prop.productI["serv"], prop.productI["serv"]),
    es = es.prodDM,
    "servD", "servI"
  )


  # dst.FT ------------------------------------------------------------------
  dst.FT.agri <- node_new("output",
    type = "SCES",
    es = es.FT,
    alpha = sum(OT[, "sector.FT.agri"]) / sum(IT[, "sector.FT.agri"]),
    beta = prop.table(IT[1:3, "sector.FT.agri"]),
    "agriD", "manuD", "servD"
  )

  dst.FT.manu <- Clone(dst.FT.agri)
  dst.FT.serv <- Clone(dst.FT.agri)
  dst.bond <- Clone(dst.FT.agri)

  dstl <- list(
    dst.agri,
    dst.manu,
    dst.serv,
    dst.CI,
    dst.FT.agri,
    dst.FT.manu,
    dst.FT.serv,
    dst.bond
  )
  ge <- sdm2(
    A = dstl,
    B = matrix(c(
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
    ), 11, 8, TRUE),
    S0Exg = {
      tmp <- matrix(NA, 11, 8, TRUE)
      colnames(tmp) <- names.agent
      rownames(tmp) <- names.commodity
      tmp["lab", "sector.CI"] <- sum(OT["lab", ])
      tmp["cap", "sector.CI"] <- sum(OT["cap", ])
      tmp["tax", ] <- OT["tax", ]
      tmp["dividend", "sector.CI"] <- sum(OT["dividend", ])
      # tmp["bond.ROW", "sector.FT.bond.ROW"] <- OT17["bond.ROW", "sector.FT.bond.ROW"]
      tmp
    },
    names.commodity = names.commodity,
    names.agent = names.agent,
    numeraire = "lab",
    tolCond = 1e-8
  )


  ge$value.added <- colSums(ge$DV[
    c("lab", "cap", "tax", "dividend"),
    c("sector.agri", "sector.manu", "sector.serv")
  ]) - ge$SV["tax", c("sector.agri", "sector.manu", "sector.serv")]

  ge$dstl <- dstl
  return(ge)
}
