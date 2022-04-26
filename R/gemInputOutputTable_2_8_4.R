#' @export
#' @title A Two-Country General Equilibrium Model with Money
#' @aliases gemInputOutputTable_2_8_4
#' @description A two-country general equilibrium model with money.
#' This general equilibrium model is based on a two-country (i.e. CHN and ROW) input-output table.
#' Each country contains four sectors and eight commodities (or subjects).
#' The four sectors are production, consumption, investment and foreign trade.
#' The eight commodities (or subjects) are product, labor, capital goods, bond, tax, dividend, imported product and money interest.
#' Hence the input-output table has 16 rows and 8 columns.
#' @param IT the input part of the input-output table (unit: trillion yuan).
#' @param product.output.CHN the product output of the production sector of CHN.
#' @param product.output.ROW the product output of the production sector of ROW.
#' @param labor.supply.CHN the labor supply of CHN.
#' @param labor.supply.ROW the labor supply of ROW.
#' @param capital.supply.CHN the capital supply of CHN.
#' @param capital.supply.ROW the capital supply of ROW.
#' @param money.interest.supply.CHN the money interest supply of CHN,
#' that is, the exogenous money supply multiplied by the exogenous interest rate.
#' @param money.interest.supply.ROW the money interest supply of ROW.
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
#' @param interest.rate.CHN the interest rate of CHN.
#' @param interest.rate.ROW the interest rate of ROW.
#' @param return.dstl If TRUE, the demand structure tree will be returned.
#' @param ... arguments to be transferred to the function \code{\link{sdm2}}.
#' @return A general equilibrium, which usually is a list with the following elements:
#' \itemize{
#' \item p - the price vector with CHN labor as numeraire, wherein
#' the price of a currency is the interest per unit of currency.
#' \item D - the demand matrix, also called the input table. Wherein the benchmark prices are used.
#' \item DV - the demand value matrix, also called the value input table. Wherein the current price is used.
#' \item SV - the supply value matrix, also called the value output table. Wherein the current price is used.
#' \item eri.CHN - the exchange rate index of CHN currency.
#' \item eri.ROW - the exchange rate index of ROW currency.
#' \item p.money - the price vector with CHN money as numeraire
#' if both interest.rate.CHN and interest.rate.CHN are not NA.
#' \item dstl - the demand structure tree list of sectors if return.dstl == TRUE.
#' \item ... - some elements returned by the function \code{\link{sdm2}}.
#' }
#' @details If interest.rate.CHN is NA or interest.rate.CHN is NA, they are assumed to be equal.
#' And in this case, the exchange rate is determined by the ratio of the interest of unit currency of the two countries.
#' In this model, the ratio of a sector's monetary interest expenditure to its transaction value may not be equal to the interest rate
#' because the ratio is not only affected by the interest rate,
#' but also by the sector's currency circulation velocity and other factors.
#' @examples
#' \donttest{
#' ITExample <- matrix(0, 16, 8, dimnames = list(
#'   c(
#'     "product.CHN", "labor.CHN", "capital.CHN", "bond.CHN",
#'     "tax.CHN", "dividend.CHN", "imported.product.CHN", "money.interest.CHN",
#'     "product.ROW", "labor.ROW", "capital.ROW", "bond.ROW",
#'     "tax.ROW", "dividend.ROW", "imported.product.ROW", "money.interest.ROW"
#'   ),
#'   c(
#'     "production.CHN", "consumption.CHN", "investment.CHN", "foreign.trade.CHN",
#'     "production.ROW", "consumption.ROW", "investment.ROW", "foreign.trade.ROW"
#'   )
#' ))
#'
#' production.CHN <- c(
#'   product.CHN = 140, labor.CHN = 40, capital.CHN = 10,
#'   tax.CHN = 10, dividend.CHN = 20, imported.product.CHN = 5, money.interest.CHN = 5
#' )
#' production.ROW <- c(
#'   product.ROW = 840, labor.ROW = 240, capital.ROW = 60,
#'   tax.ROW = 60, dividend.ROW = 120, imported.product.ROW = 6, money.interest.ROW = 30
#' )
#'
#' consumption.CHN <- c(
#'   product.CHN = 40, bond.CHN = 30, imported.product.CHN = 5, money.interest.CHN = 2
#' )
#'
#' consumption.ROW <- c(
#'   product.ROW = 240, bond.ROW = 180, imported.product.ROW = 6, money.interest.ROW = 12
#' )
#'
#' investment.CHN <- c(
#'   product.CHN = 30,
#'   imported.product.CHN = 4, money.interest.CHN = 1,
#'   bond.ROW = 1,
#'   money.interest.ROW = 0.02
#' )
#'
#' investment.ROW <- c(
#'   bond.CHN = 1,
#'   money.interest.CHN = 0.02,
#'   product.ROW = 180,
#'   imported.product.ROW = 4, money.interest.ROW = 6
#' )
#'
#'
#' foreign.trade.CHN <- c(
#'   product.ROW = 13,
#'   tax.CHN = 0.65,
#'   money.interest.ROW = 0.26
#' )
#'
#' foreign.trade.ROW <- c(
#'   product.CHN = 15,
#'   tax.ROW = 0.75,
#'   money.interest.CHN = 0.3
#' )
#'
#' ITExample <- matrix_add_by_name(
#'   ITExample, production.CHN, consumption.CHN, investment.CHN, foreign.trade.CHN,
#'   production.ROW, consumption.ROW, investment.ROW, foreign.trade.ROW
#' )
#'
#' ge <- gemInputOutputTable_2_8_4(
#'   IT = ITExample,
#'   return.dstl = TRUE
#' )
#' ge$eri.CHN
#' ge$p
#' node_plot(ge$dstl[[4]])
#'
#' ge2 <- gemInputOutputTable_2_8_4(
#'   IT = ge$DV,
#'   money.interest.supply.CHN = sum(ge$DV["money.interest.CHN", ]),
#'   money.interest.supply.ROW = sum(ge$DV["money.interest.ROW", ]),
#'   return.dstl = TRUE
#' )
#' ge2$eri.CHN
#' ge2$p
#'
#'
#' #### technology progress in CHN
#' ITTmp <- ITExample
#' ITTmp["labor.CHN", "production.CHN"] <- ITTmp["labor.CHN", "production.CHN"] * 0.8
#' geTmp <- gemInputOutputTable_2_8_4(
#'   IT = ITTmp,
#'   product.output.CHN = sum(ITExample[, "production.CHN"]),
#'   return.dstl = TRUE
#' )
#' geTmp$eri.CHN
#'
#'
#' #### increased demand for imported product in CHN
#' ITTmp <- ITExample
#' ITTmp["imported.product.CHN", "production.CHN"] <-
#'   ITTmp["imported.product.CHN", "production.CHN"] * 1.2
#' geTmp <- gemInputOutputTable_2_8_4(
#'   IT = ITTmp,
#'   return.dstl = TRUE
#' )
#' geTmp$eri.CHN
#'
#'
#' #### capital accumulation in CHN
#' geTmp <- gemInputOutputTable_2_8_4(
#'   IT = ITExample,
#'   capital.supply.CHN = sum(ITExample["capital.CHN", ]) * 1.2,
#'   return.dstl = TRUE
#' )
#' geTmp$eri.CHN
#'
#' ##
#' geTmp <- gemInputOutputTable_2_8_4(
#'   IT = ITExample,
#'   capital.supply.CHN = sum(ITExample["capital.CHN", ]) * 1.2,
#'   es.DIProduct.production.CHN = 0.3,
#'   return.dstl = TRUE
#' )
#' geTmp$eri.CHN
#' }
#'
gemInputOutputTable_2_8_4 <- function(IT,
                                      product.output.CHN = sum(IT[, "production.CHN"]),
                                      product.output.ROW = sum(IT[, "production.ROW"]),

                                      labor.supply.CHN = sum(IT["labor.CHN", ]),
                                      labor.supply.ROW = sum(IT["labor.ROW", ]),

                                      capital.supply.CHN = sum(IT["capital.CHN", ]),
                                      capital.supply.ROW = sum(IT["capital.ROW", ]),

                                      money.interest.supply.CHN = 5,
                                      money.interest.supply.ROW = 30,

                                      es.DIProduct.production.CHN = 0.5,
                                      es.DIProduct.production.ROW = 0.5,

                                      es.laborCapital.production.CHN = 0.75,
                                      es.laborCapital.production.ROW = 0.75,

                                      es.consumption.CHN = 0.5,
                                      es.consumption.ROW = 0.5,

                                      es.investment.CHN = 0.9,
                                      es.investment.ROW = 0.9,

                                      interest.rate.CHN = NA,
                                      interest.rate.ROW = NA,
                                      return.dstl = FALSE,
                                      ...) {



  # exogenous supply --------------------------------------------------------
  SExg <- IT * NA
  SExg[c("labor.CHN", "dividend.CHN", "tax.CHN", "money.interest.CHN", "capital.CHN"), "consumption.CHN"] <-
    c(
      labor.supply.CHN,
      sum(IT["dividend.CHN", ]),
      sum(IT["tax.CHN", ]),
      money.interest.supply.CHN,
      capital.supply.CHN
    )
  SExg[c("labor.ROW", "dividend.ROW", "tax.ROW", "money.interest.ROW", "capital.ROW"), "consumption.ROW"] <-
    c(
      labor.supply.ROW,
      sum(IT["dividend.ROW", ]),
      sum(IT["tax.ROW", ]),
      money.interest.supply.ROW,
      capital.supply.ROW
    )

  SExg["bond.CHN", "investment.CHN"] <- sum(IT["bond.CHN", ])
  SExg["bond.ROW", "investment.ROW"] <- sum(IT["bond.ROW", ])


  # demand structure tree ---------------------------------------------------
  # production.CHN ----------------------------------------------------------
  dst.production.CHN <- node_new(
    "production.CHN",
    type = "FIN",
    beta = prop.table(c((sum(IT[, "production.CHN"]) - IT["money.interest.CHN", "production.CHN"]), IT["money.interest.CHN", "production.CHN"])),
    "cc1.production.CHN",    "money.interest.CHN"
  )

  node_set(
    dst.production.CHN,
    "cc1.production.CHN",
    type = "Leontief",
    a = prop.table(c(sum(IT[
      c("product.CHN", "imported.product.CHN"),
      "production.CHN"
    ]), sum(IT[c(
      "labor.CHN", "capital.CHN",
      "tax.CHN", "dividend.CHN"
    ), "production.CHN"]))),
    "cc1.1.production.CHN",
    "cc1.2.production.CHN"
  )

  node_set(
    dst.production.CHN,
    "cc1.1.production.CHN",
    type = "SCES",
    es = es.DIProduct.production.CHN,
    alpha = 1,
    beta = prop.table(IT[
      c("product.CHN", "imported.product.CHN"),
      "production.CHN"
    ]),
    "product.CHN",
    "imported.product.CHN"
  )

  node_set(
    dst.production.CHN,
    "cc1.2.production.CHN",
    type = "FIN",
    beta = prop.table(c(
      sum(IT[c(
        "labor.CHN",
        "capital.CHN"
      ), "production.CHN"]),
      tax.CHN = IT[
        "tax.CHN",
        "production.CHN"
      ],
      dividend.CHN = IT[
        "dividend.CHN",
        "production.CHN"
      ]
    )),
    "cc1.2.1.production.CHN",
    "tax.CHN",
    "dividend.CHN"
  )

  node_set(
    dst.production.CHN,
    "cc1.2.1.production.CHN",
    type = "SCES",
    es = es.laborCapital.production.CHN,
    alpha = 1,
    beta = prop.table(IT[
      c("labor.CHN", "capital.CHN"),
      "production.CHN"
    ]),
    "labor.CHN",
    "capital.CHN"
  )

  # production.ROW ----------------------------------------------------------
  dst.production.ROW <- node_new("production.ROW",
    type = "FIN",
    beta = prop.table(c((sum(IT[, "production.ROW"]) - IT[
      "money.interest.ROW",
      "production.ROW"
    ]), IT["money.interest.ROW", "production.ROW"])),
    "cc1.production.ROW", "money.interest.ROW"
  )

  node_set(dst.production.ROW, "cc1.production.ROW",
    type = "Leontief",
    a = prop.table(c(sum(IT[
      c("product.ROW", "imported.product.ROW"),
      "production.ROW"
    ]), sum(IT[c(
      "labor.ROW", "capital.ROW",
      "tax.ROW", "dividend.ROW"
    ), "production.ROW"]))),
    "cc1.1.production.ROW", "cc1.2.production.ROW"
  )

  node_set(
    dst.production.ROW,
    "cc1.1.production.ROW",
    type = "SCES",
    es = es.DIProduct.production.ROW,
    alpha = 1,
    beta = prop.table(IT[
      c("product.ROW", "imported.product.ROW"),
      "production.ROW"
    ]),
    "product.ROW", "imported.product.ROW"
  )

  node_set(
    dst.production.ROW,
    "cc1.2.production.ROW",
    type = "FIN",
    beta = prop.table(c(
      sum(IT[c(
        "labor.ROW",
        "capital.ROW"
      ), "production.ROW"]),
      tax.ROW = IT[
        "tax.ROW",
        "production.ROW"
      ], dividend.ROW = IT[
        "dividend.ROW",
        "production.ROW"
      ]
    )),
    "cc1.2.1.production.ROW",
    "tax.ROW",
    "dividend.ROW"
  )

  node_set(
    dst.production.ROW,
    "cc1.2.1.production.ROW",
    type = "SCES",
    es = es.laborCapital.production.ROW,
    alpha = 1,
    beta = prop.table(IT[
      c("labor.ROW", "capital.ROW"),
      "production.ROW"
    ]),
    "labor.ROW",
    "capital.ROW"
  )

  # consumption.CHN ---------------------------------------------------------
  dst.consumption.CHN <- node_new("consumption.CHN",
    type = "FIN",
    beta = prop.table(c(sum(IT[, "consumption.CHN"]) - IT[
      "money.interest.CHN",
      "consumption.CHN"
    ], IT["money.interest.CHN", "consumption.CHN"])),
    "cc1.consumption.CHN", "money.interest.CHN"
  )

  node_set(dst.consumption.CHN, "cc1.consumption.CHN",
    type = "FIN",
    beta = prop.table(c(sum(IT[
      c("product.CHN", "imported.product.CHN"),
      "consumption.CHN"
    ]), IT["bond.CHN", "consumption.CHN"])),
    "cc1.1.consumption.CHN", "bond.CHN"
  )

  node_set(
    dst.consumption.CHN,
    "cc1.1.consumption.CHN",
    type = "SCES",
    es = es.consumption.CHN,
    alpha = 1,
    beta = prop.table(IT[c(
      "product.CHN",
      "imported.product.CHN"
    ), "consumption.CHN"]),
    "product.CHN", "imported.product.CHN"
  )

  # consumption.ROW ---------------------------------------------------------
  dst.consumption.ROW <- node_new("consumption.ROW",
    type = "FIN",
    beta = prop.table(c(sum(IT[, "consumption.ROW"]) - IT[
      "money.interest.ROW",
      "consumption.ROW"
    ], IT["money.interest.ROW", "consumption.ROW"])),
    "cc1.consumption.ROW", "money.interest.ROW"
  )

  node_set(dst.consumption.ROW, "cc1.consumption.ROW",
    type = "FIN",
    beta = prop.table(c(sum(IT[
      c("product.ROW", "imported.product.ROW"),
      "consumption.ROW"
    ]), IT["bond.ROW", "consumption.ROW"])),
    "cc1.1.consumption.ROW", "bond.ROW"
  )

  node_set(
    dst.consumption.ROW,
    "cc1.1.consumption.ROW",
    type = "SCES",
    es = es.consumption.ROW,
    alpha = 1,
    beta = prop.table(IT[c(
      "product.ROW",
      "imported.product.ROW"
    ), "consumption.ROW"]),
    "product.ROW", "imported.product.ROW"
  )

  # investment.CHN ----------------------------------------------------------
  dst.investment.CHN <- node_new("investment.CHN",
    type = "FIN",
    beta = prop.table(c(sum(IT[c(
      "product.CHN", "imported.product.CHN",
      "money.interest.CHN"
    ), "investment.CHN"]), sum(IT[c(
      "bond.ROW",
      "money.interest.ROW"
    ), "investment.CHN"]))),
    "cc1.investment.CHN", "cc2"
  )

  node_set(dst.investment.CHN, "cc1.investment.CHN",
    type = "FIN",
    beta = prop.table(c(sum(IT[
      c("product.CHN", "imported.product.CHN"),
      "investment.CHN"
    ]), IT["money.interest.CHN", "investment.CHN"])),
    "cc1.1.investment.CHN", "money.interest.CHN"
  )

  node_set(
    dst.investment.CHN,
    "cc1.1.investment.CHN",
    type = "SCES",
    es = es.investment.CHN,
    alpha = 1,
    beta = prop.table(IT[c(
      "product.CHN",
      "imported.product.CHN"
    ), "investment.CHN"]),
    "product.CHN", "imported.product.CHN"
  )

  node_set(dst.investment.CHN,
    "cc2",
    type = "FIN",
    beta = prop.table(c(IT[
      "bond.ROW",
      "investment.CHN"
    ], IT["money.interest.ROW", "investment.CHN"])),
    "bond.ROW", "money.interest.ROW"
  )

  # investment.ROW ----------------------------------------------------------
  dst.investment.ROW <- node_new("investment.ROW",
    type = "FIN",
    beta = prop.table(c(sum(IT[c(
      "product.ROW", "imported.product.ROW",
      "money.interest.ROW"
    ), "investment.ROW"]), sum(IT[c(
      "bond.CHN",
      "money.interest.CHN"
    ), "investment.ROW"]))),
    "cc1.investment.ROW", "cc2"
  )

  node_set(dst.investment.ROW,
    "cc1.investment.ROW",
    type = "FIN",
    beta = prop.table(c(sum(IT[
      c("product.ROW", "imported.product.ROW"),
      "investment.ROW"
    ]), IT["money.interest.ROW", "investment.ROW"])),
    "cc1.1.investment.ROW", "money.interest.ROW"
  )

  node_set(
    dst.investment.ROW,
    "cc1.1.investment.ROW",
    type = "SCES",
    es = es.investment.ROW,
    alpha = 1,
    beta = prop.table(IT[c(
      "product.ROW",
      "imported.product.ROW"
    ), "investment.ROW"]),
    "product.ROW", "imported.product.ROW"
  )

  node_set(dst.investment.ROW,
    "cc2",
    type = "FIN",
    beta = prop.table(c(IT[
      "bond.CHN",
      "investment.ROW"
    ], IT["money.interest.CHN", "investment.ROW"])),
    "bond.CHN", "money.interest.CHN"
  )

  # foreign.trade.CHN -------------------------------------------------------
  dst.foreign.trade.CHN <-
    node_new(
      "foreign.trade.CHN",
      type = "FIN",
      beta = prop.table(c(sum(IT[
        c("product.ROW", "tax.CHN"),
        "foreign.trade.CHN"
      ]), IT[
        "money.interest.ROW",
        "foreign.trade.CHN"
      ])),
      "cc1.foreign.trade.CHN",
      "money.interest.ROW"
    )

  node_set(
    dst.foreign.trade.CHN,
    "cc1.foreign.trade.CHN",
    type = "FIN",
    beta = prop.table(c(IT[
      "product.ROW",
      "foreign.trade.CHN"
    ], IT["tax.CHN", "foreign.trade.CHN"])),
    "product.ROW",
    "tax.CHN"
  )


  # foreign.trade.ROW -------------------------------------------------------
  dst.foreign.trade.ROW <-
    node_new("foreign.trade.ROW",
      type = "FIN",
      beta = prop.table(c(sum(IT[
        c("product.CHN", "tax.ROW"),
        "foreign.trade.ROW"
      ]), IT[
        "money.interest.CHN",
        "foreign.trade.ROW"
      ])),
      "cc1.foreign.trade.ROW", "money.interest.CHN"
    )

  node_set(
    dst.foreign.trade.ROW,
    "cc1.foreign.trade.ROW",
    type = "FIN",
    beta = prop.table(c(IT[
      "product.CHN",
      "foreign.trade.ROW"
    ], IT["tax.ROW", "foreign.trade.ROW"])),
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

  ge$eri.ROW <- unname(ge$p["money.interest.ROW"] / ge$p["money.interest.CHN"]) # exchange rate index
  ge$eri.CHN <- unname(ge$p["money.interest.CHN"] / ge$p["money.interest.ROW"])
  if (!is.na(interest.rate.CHN) & !is.na(interest.rate.ROW)) {
    money.value.CHN <- ge$p["money.interest.CHN"] / interest.rate.CHN
    money.value.ROW <- ge$p["money.interest.ROW"] / interest.rate.ROW
    ge$p.money <- ge$p
    ge$p.money["money.interest.CHN"] <- money.value.CHN
    ge$p.money["money.interest.ROW"] <- money.value.ROW
    ge$p.money <- ge$p.money / ge$p.money["money.interest.CHN"]

    ge$eri.ROW <- unname(ge$p.money["money.interest.ROW"] / ge$p.money["money.interest.CHN"])
    ge$eri.CHN <- unname(ge$p.money["money.interest.CHN"] / ge$p.money["money.interest.ROW"])
  }

  if (return.dstl) ge$dstl <- dstl

  return(ge)
}
