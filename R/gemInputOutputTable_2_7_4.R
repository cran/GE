#' @export
#' @title A Two-Country General Equilibrium Model
#' @aliases gemInputOutputTable_2_7_4
#' @description A two-country general equilibrium model.
#' This general equilibrium model is based on a two-country (i.e. CHN and ROW) input-output table consisting of an input part and an output part.
#' Each country contains 4 sectors and 7 commodities (or subjects).
#' The 4 sectors are production, consumption, investment and foreign trade.
#' The 7 commodities (or subjects) are product, labor, capital goods, bond, tax, dividend, imported product.
#' Hence the input-output table has 14 rows and 8 columns.
#' @param IT the input part of the input-output table.
#' @param OT the output part of the input-output table.
#' @param es.DIProduct.production.CHN the elasticity of substitution between
#' domestic product and imported product used by the production sector of CHN.
#' @param es.DIProduct.production.ROW the elasticity of substitution between
#' domestic product and imported product used by the production sector of ROW.
#' @param es.laborCapital.production.CHN the elasticity of substitution between
#' labor and capital goods used by the production sector of CHN.
#' @param es.laborCapital.production.ROW the elasticity of substitution between
#' labor and capital goods used by the production sector of ROW.
#' @param es.consumption.CHN the elasticity of substitution between
#' domestic product and imported product used by the consumption sector of CHN.
#' @param es.consumption.ROW the elasticity of substitution between
#' domestic product and imported product used by the consumption sector of ROW.
#' @param es.investment.CHN the elasticity of substitution between
#' domestic product and imported product used by the investment sector of CHN.
#' @param es.investment.ROW the elasticity of substitution between
#' domestic product and imported product used by the investment sector of ROW.
#' @param return.dstl If TRUE, the demand structure tree will be returned.
#' @param ... arguments to be transferred to the function \code{\link{sdm2}}.
#' @return A general equilibrium, which usually is a list with the following elements:
#' \itemize{
#' \item p - the price vector with CHN labor as numeraire.
#' \item dstl - the demand structure tree list of sectors if return.dstl == TRUE.
#' \item ... - some elements returned by the function \code{\link{sdm2}}.
#' }
#' @examples
#' \donttest{
#' IT <- matrix(c(
#'   30, 12, 9, 0, 0, 0, 0, 13,
#'   15, 0, 0, 0, 0, 0, 0, 0,
#'   2, 0, 0, 0, 0, 0, 0, 0,
#'   0, 9, 0, 0, 0, 2, 0, 0,
#'   3, 0, 0, 1, 0, 0, 0, 0,
#'   6, 0, 0, 0, 0, 0, 0, 0,
#'   8, 3, 3, 0, 0, 0, 0, 0,
#'   0, 0, 0, 13, 150, 316, 258, 0,
#'   0, 0, 0, 0, 288, 0, 0, 0,
#'   0, 0, 0, 0, 92, 0, 0, 0,
#'   0, 2, 0, 0, 0, 269, 0, 0,
#'   0, 0, 0, 0, 35, 0, 0, 1,
#'   0, 0, 0, 0, 172, 0, 0, 0,
#'   0, 0, 0, 0, 1, 5, 13, 0
#' ), 14, 8, TRUE)
#'
#' OT <- matrix(c(
#'   64, 0, 0, 0, 0, 0, 0, 0,
#'   0, 15, 0, 0, 0, 0, 0, 0,
#'   0, 2, 0, 0, 0, 0, 0, 0,
#'   0, 0, 11, 0, 0, 0, 0, 0,
#'   0, 3, 0, 0, 0, 0, 0, 0,
#'   0, 6, 0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 13, 0, 0, 0, 0,
#'   0, 0, 0, 0, 738, 0, 0, 0,
#'   0, 0, 0, 0, 0, 288, 0, 0,
#'   0, 0, 0, 0, 0, 92, 0, 0,
#'   0, 0, 0, 0, 0, 0, 271, 0,
#'   0, 0, 0, 0, 0, 36, 0, 0,
#'   0, 0, 0, 0, 0, 172, 0, 0,
#'   0, 0, 0, 0, 0, 0, 0, 14
#' ), 14, 8, TRUE)
#'
#' dimnames(IT) <- dimnames(OT) <- list(
#'   c(
#'     "product.CHN", "labor.CHN", "capital.CHN", "bond.CHN",
#'     "tax.CHN", "dividend.CHN", "imported.product.CHN",
#'     "product.ROW", "labor.ROW", "capital.ROW", "bond.ROW",
#'     "tax.ROW", "dividend.ROW", "imported.product.ROW"
#'   ),
#'   c(
#'     "production.CHN", "consumption.CHN", "investment.CHN", "foreign.trade.CHN",
#'     "production.ROW", "consumption.ROW", "investment.ROW", "foreign.trade.ROW"
#'   )
#' )
#'
#' ge <- gemInputOutputTable_2_7_4(IT, OT, return.dstl = TRUE)
#' ge$p
#' ge$z
#' node_plot(ge$dstl[[1]])
#' ge$dstl[[1]]$a
#'
#' #### technology progress in CHN
#' OT.TP <- OT
#' OT.TP["product.CHN", "production.CHN"] <- OT["product.CHN", "production.CHN"] * 1.2
#'
#' ge.TP <- gemInputOutputTable_2_7_4(IT, OT.TP, return.dstl = TRUE)
#' ge.TP$p
#' ge.TP$z
#' ge.TP$dstl[[1]]$a
#'
#' #### capital accumulation in CHN
#' OT.CA <- OT
#' OT.CA["capital.CHN", "consumption.CHN"] <- OT["capital.CHN", "consumption.CHN"] * 2
#'
#' ge.CA <- gemInputOutputTable_2_7_4(IT, OT.CA)
#' ge.CA$p
#' ge.CA$z
#'
#' #### labor supply change in CHN
#' OT.LSC <- OT
#' OT.LSC["labor.CHN", "consumption.CHN"] <- OT["labor.CHN", "consumption.CHN"] * 0.5
#'
#' ge.LSC <- gemInputOutputTable_2_7_4(IT, OT.LSC)
#' ge.LSC$p
#' ge.LSC$z
#'
#' #### tariff rate change in CHN
#' IT.TRC <- IT
#' IT.TRC["tax.CHN","foreign.trade.CHN"] <- IT.TRC["tax.CHN","foreign.trade.CHN"] * 1.2
#' ge.TRC <- gemInputOutputTable_2_7_4(IT.TRC, OT)
#' ge.TRC$p
#' ge.TRC$z
#' }
#'
gemInputOutputTable_2_7_4 <- function(IT, OT,
                                      es.DIProduct.production.CHN = 3,
                                      es.DIProduct.production.ROW = 3,

                                      es.laborCapital.production.CHN = 0.75,
                                      es.laborCapital.production.ROW = 0.75,

                                      es.consumption.CHN = 3,
                                      es.consumption.ROW = 3,

                                      es.investment.CHN = 3,
                                      es.investment.ROW = 3,

                                      return.dstl = FALSE,

                                      ...) {
  dimnames(IT) <- dimnames(OT) <- list(
    c(
      "product.CHN", "labor.CHN", "capital.CHN", "bond.CHN",
      "tax.CHN", "dividend.CHN", "imported.product.CHN",
      "product.ROW", "labor.ROW", "capital.ROW", "bond.ROW",
      "tax.ROW", "dividend.ROW", "imported.product.ROW"
    ),
    c(
      "production.CHN", "consumption.CHN", "investment.CHN", "foreign.trade.CHN",
      "production.ROW", "consumption.ROW", "investment.ROW", "foreign.trade.ROW"
    )
  )

  # exogenous supply --------------------------------------------------------
  SExg <- IT * NA
  SExg[c("labor.CHN", "dividend.CHN", "tax.CHN", "capital.CHN"), "consumption.CHN"] <-
    c(
      sum(OT["labor.CHN", ]),
      sum(OT["dividend.CHN", ]),
      sum(OT["tax.CHN", ]),
      sum(OT["capital.CHN", ])
    )
  SExg[c("labor.ROW", "dividend.ROW", "tax.ROW", "capital.ROW"), "consumption.ROW"] <-
    c(
      sum(OT["labor.ROW", ]),
      sum(OT["dividend.ROW", ]),
      sum(OT["tax.ROW", ]),
      sum(OT["capital.ROW", ])
    )

  SExg["bond.CHN", "investment.CHN"] <- sum(OT["bond.CHN", ])
  SExg["bond.ROW", "investment.ROW"] <- sum(OT["bond.ROW", ])


  # demand structure tree ---------------------------------------------------
  # production.CHN ----------------------------------------------------------
  dst.production.CHN <- node_new("production.CHN",
    type = "Leontief",
    a =
      c(
        sum(IT[c("product.CHN", "imported.product.CHN"), "production.CHN"]),
        sum(IT[c("labor.CHN", "capital.CHN", "tax.CHN", "dividend.CHN"), "production.CHN"])
      ) / OT["product.CHN", "production.CHN"],
    "cc1", "cc2"
  )

  node_set(dst.production.CHN,
    "cc1",
    type = "SCES",
    es = es.DIProduct.production.CHN,
    alpha = 1,
    beta = prop.table(IT[c("product.CHN", "imported.product.CHN"), "production.CHN"]),
    "product.CHN", "imported.product.CHN"
  )

  node_set(dst.production.CHN,
    "cc2",
    type = "FIN",
    beta = prop.table(c(
      sum(IT[c("labor.CHN", "capital.CHN"), "production.CHN"]),
      tax.CHN = IT["tax.CHN", "production.CHN"],
      dividend.CHN = IT["dividend.CHN", "production.CHN"]
    )),
    "cc2.1", "tax.CHN", "dividend.CHN"
  )


  node_set(dst.production.CHN, "cc2.1",
    type = "SCES",
    es = es.laborCapital.production.CHN,
    alpha = 1, # alpha.laborCapital.production.CHN,
    beta = prop.table(IT[c("labor.CHN", "capital.CHN"), "production.CHN"]),
    "labor.CHN", "capital.CHN"
  )

  # production.ROW ----------------------------------------------------------
  dst.production.ROW <- node_new("production.ROW",
    type = "Leontief",
    a = c(
      sum(IT[c("product.ROW", "imported.product.ROW"), "production.ROW"]),
      sum(IT[c("labor.ROW", "capital.ROW", "tax.ROW", "dividend.ROW"), "production.ROW"])
    ) / OT["product.ROW", "production.ROW"],
    "cc1", "cc2"
  )

  node_set(dst.production.ROW,
    "cc1",
    type = "SCES",
    es = es.DIProduct.production.ROW,
    alpha = 1,
    beta = prop.table(IT[c("product.ROW", "imported.product.ROW"), "production.ROW"]),
    "product.ROW", "imported.product.ROW"
  )

  node_set(dst.production.ROW,
    "cc2",
    type = "FIN",
    beta = prop.table(c(
      sum(IT[c("labor.ROW", "capital.ROW"), "production.ROW"]),
      tax.ROW = IT["tax.ROW", "production.ROW"],
      dividend.ROW = IT["dividend.ROW", "production.ROW"]
    )),
    "cc2.1", "tax.ROW", "dividend.ROW"
  )

  node_set(dst.production.ROW, "cc2.1",
    type = "SCES",
    es = es.laborCapital.production.ROW,
    alpha = 1,
    beta = prop.table(IT[c("labor.ROW", "capital.ROW"), "production.ROW"]),
    "labor.ROW", "capital.ROW"
  )

  # consumption.CHN ---------------------------------------------------------
  dst.consumption.CHN <- node_new("consumption.CHN",
    type = "FIN",
    beta = prop.table(c(
      sum(IT[c("product.CHN", "imported.product.CHN"), "consumption.CHN"]),
      IT["bond.CHN", "consumption.CHN"],
      IT["bond.ROW", "consumption.CHN"]
    )),
    "cc1", "bond.CHN", "bond.ROW"
  )

  node_set(dst.consumption.CHN, "cc1",
    type = "SCES",
    es = es.consumption.CHN,
    alpha = 1,
    beta = prop.table(IT[c("product.CHN", "imported.product.CHN"), "consumption.CHN"]),
    "product.CHN", "imported.product.CHN"
  )

  # consumption.ROW ---------------------------------------------------------
  dst.consumption.ROW <- node_new("consumption.ROW",
    type = "FIN",
    beta = prop.table(c(
      sum(IT[c("product.ROW", "imported.product.ROW"), "consumption.ROW"]),
      IT["bond.ROW", "consumption.ROW"],
      IT["bond.CHN", "consumption.ROW"]
    )),
    "cc1", "bond.ROW", "bond.CHN"
  )

  node_set(dst.consumption.ROW, "cc1",
    type = "SCES",
    es = es.consumption.ROW,
    alpha = 1,
    beta = prop.table(IT[c("product.ROW", "imported.product.ROW"), "consumption.ROW"]),
    "product.ROW", "imported.product.ROW"
  )

  # investment.CHN ----------------------------------------------------------
  dst.investment.CHN <- node_new("investment.CHN",
    type = "SCES",
    es = es.investment.CHN,
    alpha = 1,
    beta = prop.table(IT[c("product.CHN", "imported.product.CHN"), "investment.CHN"]),
    "product.CHN", "imported.product.CHN"
  )

  # investment.ROW ----------------------------------------------------------
  dst.investment.ROW <- node_new("investment.ROW",
    type = "SCES",
    es = es.investment.ROW,
    alpha = 1,
    beta = prop.table(IT[c("product.ROW", "imported.product.ROW"), "investment.ROW"]),
    "product.ROW", "imported.product.ROW"
  )

  # foreign.trade.CHN -------------------------------------------------------
  dst.foreign.trade.CHN <- node_new("foreign.trade.CHN",
    type = "FIN",
    beta = prop.table(c(
      IT["product.ROW", "foreign.trade.CHN"],
      IT["tax.CHN", "foreign.trade.CHN"]
    )),
    "product.ROW", "tax.CHN"
  )

  # foreign.trade.ROW -------------------------------------------------------
  dst.foreign.trade.ROW <- node_new("foreign.trade.ROW",
    type = "FIN",
    beta = prop.table(c(
      IT["product.CHN", "foreign.trade.ROW"],
      IT["tax.ROW", "foreign.trade.ROW"]
    )),
    "product.CHN", "tax.ROW"
  )

  dstl <- list(
    dst.production.CHN,
    dst.consumption.CHN,
    dst.investment.CHN,
    dst.foreign.trade.CHN,
    dst.production.ROW,
    dst.consumption.ROW,
    dst.investment.ROW,
    dst.foreign.trade.ROW
  )

  # run sdm2 -----------------------------------------------------------------
  ge <- sdm2(
    A = dstl,
    B = {
      B <- IT * 0
      B["product.CHN", "production.CHN"] <-
        B["imported.product.CHN", "foreign.trade.CHN"] <-
        B["product.ROW", "production.ROW"] <-
        B["imported.product.ROW", "foreign.trade.ROW"] <- 1
      B
    },
    S0Exg = SExg,
    names.commodity = rownames(IT),
    names.agent = colnames(IT),
    numeraire = "labor.CHN",
    priceAdjustmentVelocity = 0.05,
    ...
  )
  if (return.dstl) ge$dstl <- dstl

  return(ge)
}
