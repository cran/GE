#' @export
#' @title A Two-Country General Equilibrium Model
#' @aliases gemInputOutputTable_2_7_2
#' @description A two-country general equilibrium model.
#' This general equilibrium model is based on a two-country (i.e. CHN and ROW) input-output table consisting of an input part and an output part.
#' Each country contains 2 sectors and 7 commodities (or subjects).
#' The 2 sectors are firm and household.
#' The 7 commodities (or subjects) are product, labor, capital goods, bond, tax, dividend, tariff.
#' Hence the input-output table has 14 rows and 4 columns.
#' @param IT the input part of the input-output table.
#' @param OT the output part of the input-output table.
#' @param es.DIProduct.firm.CHN the elasticity of substitution between
#' domestic product and imported product used by the production sector of CHN.
#' @param es.DIProduct.firm.ROW the elasticity of substitution between
#' domestic product and imported product used by the production sector of ROW.
#' @param es.laborCapital.firm.CHN the elasticity of substitution between
#' labor and capital goods used by the production sector of CHN.
#' @param es.laborCapital.firm.ROW the elasticity of substitution between
#' labor and capital goods used by the production sector of ROW.
#' @param es.household.CHN the elasticity of substitution between
#' domestic product and imported product used by the consumption sector of CHN.
#' @param es.household.ROW the elasticity of substitution between
#' domestic product and imported product used by the consumption sector of ROW.
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
#'   142, 84, 13, 4.1,
#'   47, 0, 0, 0,
#'   13, 0, 0, 0,
#'   0, 0, 0, 3.4,
#'   9.3, 0, 0, 0,
#'   22, 0, 0, 0,
#'   0.15, 0.091, 0, 0,
#'   10, 6, 381, 451,
#'   0, 0, 252, 0,
#'   0, 0, 81, 0,
#'   0, 4.9, 0, 0,
#'   0, 0, 26, 0,
#'   0, 0, 92, 0,
#'   0, 0, 1.9, 0.59
#' ), 14, 4, TRUE)
#'
#' OT <- matrix(c(
#'   244, 0, 0, 0,
#'   0, 47, 0, 0,
#'   0, 13, 0, 0,
#'   0, 3.4, 0, 0,
#'   0, 9.3, 0, 0,
#'   0, 22, 0, 0,
#'   0, 0.24, 0, 0,
#'   0, 0, 849, 0,
#'   0, 0, 0, 252,
#'   0, 0, 0, 81,
#'   0, 0, 0, 4.9,
#'   0, 0, 0, 26,
#'   0, 0, 0, 92,
#'   0, 0, 0, 2.5
#' ), 14, 4, TRUE)
#'
#' dimnames(IT) <- dimnames(OT) <- list(
#'   c(
#'     "product.CHN", "labor.CHN", "capital.CHN", "bond.CHN",
#'     "tax.CHN", "dividend.CHN", "tariff.CHN",
#'     "product.ROW", "labor.ROW", "capital.ROW", "bond.ROW",
#'     "tax.ROW", "dividend.ROW", "tariff.ROW"
#'   ),
#'   c(
#'     "firm.CHN", "household.CHN",
#'     "firm.ROW", "household.ROW"
#'   )
#' )
#'
#' ge <- gemInputOutputTable_2_7_2(IT, OT, return.dstl = TRUE)
#' ge$p
#' ge$z
#' node_plot(ge$dstl[[1]])
#' ge$dstl[[1]]$a
#'
#' ## tariff rate change in CHN
#' dstl <- lapply(ge$dstl, Clone)
#' tmp <- node_set(dstl[[1]], "cc1.1")
#' tmp$beta[2] <- tmp$beta[2] * 10
#'
#' ge.TRC <- sdm2(
#'   A = dstl, B = ge$B, S0Exg = ge$S0Exg,
#'   names.commodity = rownames(ge$B),
#'   names.agent = colnames(ge$B),
#'   numeraire = "labor.CHN"
#' )
#'
#' ge.TRC$p
#' ge.TRC$z
#' #### technology progress in CHN
#' OT.TP <- OT
#' OT.TP["product.CHN", "firm.CHN"] <- OT["product.CHN", "firm.CHN"] * 1.2
#'
#' ge.TP <- gemInputOutputTable_2_7_2(IT, OT.TP, return.dstl = TRUE)
#' ge.TP$p
#' ge.TP$z
#' ge.TP$dstl[[1]]$a
#'
#' #### capital accumulation in CHN
#' OT.CA <- OT
#' OT.CA["capital.CHN", "household.CHN"] <- OT["capital.CHN", "household.CHN"] * 2
#'
#' ge.CA <- gemInputOutputTable_2_7_2(IT, OT.CA)
#' ge.CA$p
#' ge.CA$z
#'
#' #### labor supply change in CHN
#' OT.LSC <- OT
#' OT.LSC["labor.CHN", "household.CHN"] <- OT["labor.CHN", "household.CHN"] * 0.5
#'
#' ge.LSC <- gemInputOutputTable_2_7_2(IT, OT.LSC)
#' ge.LSC$p
#' ge.LSC$z
#' }
#'
gemInputOutputTable_2_7_2 <- function(IT, OT,
                                      es.DIProduct.firm.CHN = 3,
                                      es.DIProduct.firm.ROW = 3,

                                      es.laborCapital.firm.CHN = 0.75,
                                      es.laborCapital.firm.ROW = 0.75,

                                      es.household.CHN = 3,
                                      es.household.ROW = 3,

                                      return.dstl = FALSE,

                                      ...) {
  dimnames(IT) <- dimnames(OT) <- list(
    c(
      "product.CHN", "labor.CHN", "capital.CHN", "bond.CHN",
      "tax.CHN", "dividend.CHN", "tariff.CHN",
      "product.ROW", "labor.ROW", "capital.ROW", "bond.ROW",
      "tax.ROW", "dividend.ROW", "tariff.ROW"
    ),
    c(
      "firm.CHN", "household.CHN",
      "firm.ROW", "household.ROW"
    )
  )

  # exogenous supply --------------------------------------------------------
  SExg <- IT * NA
  SExg[c("labor.CHN", "capital.CHN", "bond.CHN", "tax.CHN", "dividend.CHN", "tariff.CHN"), "household.CHN"] <-
    c(
      sum(OT["labor.CHN", ]),
      sum(OT["capital.CHN", ]),
      sum(OT["bond.CHN", ]),
      sum(OT["tax.CHN", ]),
      sum(OT["dividend.CHN", ]),
      sum(OT["tariff.CHN", ])
    )

  SExg[c("labor.ROW", "capital.ROW", "bond.ROW", "tax.ROW", "dividend.ROW", "tariff.ROW"), "household.ROW"] <-
    c(
      sum(OT["labor.ROW", ]),
      sum(OT["capital.ROW", ]),
      sum(OT["bond.ROW", ]),
      sum(OT["tax.ROW", ]),
      sum(OT["dividend.ROW", ]),
      sum(OT["tariff.ROW", ])
    )

  # demand structure tree ---------------------------------------------------
  # firm.CHN ----------------------------------------------------------
  dst.firm.CHN <- node_new("firm.CHN",
                           type = "Leontief",
                           a =
                             c(
                               sum(IT[c("product.CHN", "product.ROW", "tariff.CHN"), "firm.CHN"]),
                               sum(IT[c("labor.CHN", "capital.CHN", "tax.CHN", "dividend.CHN"), "firm.CHN"])
                             ) / OT["product.CHN", "firm.CHN"],
                           "cc1", "cc2"
  )

  node_set(dst.firm.CHN,
           "cc1",
           type = "SCES",
           es = es.DIProduct.firm.CHN,
           alpha = 1,
           beta = prop.table(c(
             IT["product.CHN", "firm.CHN"],
             sum(IT[c("product.ROW", "tariff.CHN"), "firm.CHN"])
           )),
           "product.CHN", "cc1.1" # cc.product.ROW_tariff.CHN
  )

  node_set(dst.firm.CHN,
           "cc1.1",
           type = "FIN",
           beta = prop.table(IT[c("product.ROW", "tariff.CHN"), "firm.CHN"]),
           "product.ROW", "tariff.CHN"
  )

  node_set(dst.firm.CHN,
           "cc2",
           type = "FIN",
           beta = prop.table(c(
             sum(IT[c("labor.CHN", "capital.CHN"), "firm.CHN"]),
             tax.CHN = IT["tax.CHN", "firm.CHN"],
             dividend.CHN = IT["dividend.CHN", "firm.CHN"]
           )),
           "cc2.1", "tax.CHN", "dividend.CHN"
  )


  node_set(dst.firm.CHN, "cc2.1",
           type = "SCES",
           es = es.laborCapital.firm.CHN,
           alpha = 1, # alpha.laborCapital.firm.CHN,
           beta = prop.table(IT[c("labor.CHN", "capital.CHN"), "firm.CHN"]),
           "labor.CHN", "capital.CHN"
  )

  # firm.ROW ----------------------------------------------------------
  dst.firm.ROW <- node_new("firm.ROW",
                           type = "Leontief",
                           a =
                             c(
                               sum(IT[c("product.ROW", "product.CHN", "tariff.ROW"), "firm.ROW"]),
                               sum(IT[c("labor.ROW", "capital.ROW", "tax.ROW", "dividend.ROW"), "firm.ROW"])
                             ) / OT["product.ROW", "firm.ROW"],
                           "cc1", "cc2"
  )

  node_set(dst.firm.ROW,
           "cc1",
           type = "SCES",
           es = es.DIProduct.firm.ROW,
           alpha = 1,
           beta = prop.table(c(
             IT["product.ROW", "firm.ROW"],
             sum(IT[c("product.CHN", "tariff.ROW"), "firm.ROW"])
           )),
           "product.ROW", "cc1.1" # cc.product.ROW_tariff.ROW
  )

  node_set(dst.firm.ROW,
           "cc1.1",
           type = "FIN",
           beta = prop.table(IT[c("product.CHN", "tariff.ROW"), "firm.ROW"]),
           "product.CHN", "tariff.ROW"
  )

  node_set(dst.firm.ROW,
           "cc2",
           type = "FIN",
           beta = prop.table(c(
             sum(IT[c("labor.ROW", "capital.ROW"), "firm.ROW"]),
             tax.ROW = IT["tax.ROW", "firm.ROW"],
             dividend.ROW = IT["dividend.ROW", "firm.ROW"]
           )),
           "cc2.1", "tax.ROW", "dividend.ROW"
  )


  node_set(dst.firm.ROW, "cc2.1",
           type = "SCES",
           es = es.laborCapital.firm.ROW,
           alpha = 1, # alpha.laborCapital.firm.ROW,
           beta = prop.table(IT[c("labor.ROW", "capital.ROW"), "firm.ROW"]),
           "labor.ROW", "capital.ROW"
  )

  # household.CHN ---------------------------------------------------------
  dst.household.CHN <- node_new("household.CHN",
                                type = "FIN",
                                beta = prop.table(c(
                                  sum(IT[c("product.CHN", "product.ROW", "tariff.CHN"), "household.CHN"]),
                                  IT["bond.ROW", "household.CHN"]
                                )),
                                "cc1", "bond.ROW"
  )

  node_set(dst.household.CHN, "cc1",
           type = "SCES",
           es = es.household.CHN,
           alpha = 1,
           beta = prop.table(c(
             IT["product.CHN", "household.CHN"],
             sum(IT[c("product.ROW", "tariff.CHN"), "household.CHN"])
           )),
           "product.CHN", "cc1.1"
  )

  node_set(dst.household.CHN,
           "cc1.1",
           type = "FIN",
           beta = prop.table(IT[c("product.ROW", "tariff.CHN"), "household.CHN"]),
           "product.ROW", "tariff.CHN"
  )

  # household.ROW ---------------------------------------------------------
  dst.household.ROW <- node_new("household.ROW",
                                type = "FIN",
                                beta = prop.table(c(
                                  sum(IT[c("product.ROW", "product.CHN", "tariff.ROW"), "household.ROW"]),
                                  IT["bond.CHN", "household.ROW"]
                                )),
                                "cc1", "bond.CHN"
  )

  node_set(dst.household.ROW, "cc1",
           type = "SCES",
           es = es.household.ROW,
           alpha = 1,
           beta = prop.table(c(
             IT["product.ROW", "household.ROW"],
             sum(IT[c("product.CHN", "tariff.ROW"), "household.ROW"])
           )),
           "product.ROW", "cc1.1"
  )

  node_set(dst.household.ROW,
           "cc1.1",
           type = "FIN",
           beta = prop.table(IT[c("product.CHN", "tariff.ROW"), "household.ROW"]),
           "product.CHN", "tariff.ROW"
  )


  dstl <- list(
    dst.firm.CHN,
    dst.household.CHN,
    dst.firm.ROW,
    dst.household.ROW
  )

  # run sdm2 -----------------------------------------------------------------
  ge <- sdm2(
    A = dstl,
    B = {
      B <- IT * 0
      B["product.CHN", "firm.CHN"] <-
        B["product.ROW", "firm.ROW"] <- 1
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
