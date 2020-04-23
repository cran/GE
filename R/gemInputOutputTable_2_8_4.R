#' @export
#' @title A Two-Country General Equilibrium Model with Money
#' @aliases gemInputOutputTable_2_8_4
#' @description A two-country general equilibrium model with money.
#' This general equilibrium model is based on a two-country (i.e. CHN and ROW) input-output table.
#' Each country contains four sectors and eight commodities (or subjects).
#' The four sectors are production, consumption, investment and foreign trade.
#' The eight commodities (or subjects) are product, labor, capital goods, bond, tax, dividends, imported product and money.
#' Hence the input-output table has 16 rows and 8 columns.
#' @param IT the input-output table.
#' @param product.output.CHN the product output of the production sector of CHN.
#' @param product.output.ROW the product output of the production sector of ROW.
#' @param labor.supply.CHN the labor supply of CHN.
#' @param labor.supply.ROW the labor supply of ROW.
#' @param capital.supply.CHN the capital supply of CHN.
#' @param capital.supply.ROW the capital supply of ROW.
#' @param money.supply.CHN the money supply of CHN.
#' @param money.supply.ROW the money supply of ROW.
#' @param es.productDI.production.CHN the elasticity of substitution between
#' domestic product and imported product used by the production sector of CHN.
#' @param es.productDI.production.ROW the elasticity of substitution between
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
#' @param ... arguments to be transferred to the function sdm of the package CGE.
#' @return A general equilibrium, which usually is a list with the following elements:
#' \itemize{
#' \item p - the price vector with CHN labor as numeraire.
#' \item sector.names - names of sectors.
#' \item subject.names - names of commodities or subjects.
#' \item dstl - the demand structure tree list of sectors if return.dstl == TRUE.
#' \item D - the demand matrix.
#' \item DV - the demand value matrix.
#' \item SV - the supply value matrix.
#' \item eri.CHN - the exchange rate index of CHN currency.
#' \item eri.ROW - the exchange rate index of ROW currency.
#' \item p.money - the price vector with CHN money as numeraire
#' if both interest.rate.CHN and interest.rate.CHN are not NA.
#' \item ... - some elements returned by the CGE::sdm function
#' }
#' @details If interest.rate.CHN is NA or interest.rate.CHN is NA, they are assumed to be equal.
#' And in this case, the exchange rate is determined by the ratio of the interest of unit currency of the two countries.
#' @examples
#' \donttest{
#' ITExample <- matrix(0, 16, 8, dimnames = list(
#'   c(
#'     "product.CHN", "labor.CHN", "capital.CHN", "bond.CHN",
#'     "tax.CHN", "dividend.CHN", "imported.product.CHN", "money.CHN",
#'     "product.ROW", "labor.ROW", "capital.ROW", "bond.ROW", "tax.ROW",
#'     "dividend.ROW", "imported.product.ROW", "money.ROW"
#'   ),
#'   c(
#'     "production.CHN", "consumption.CHN", "investment.CHN", "foreign.trade.CHN",
#'     "production.ROW", "consumption.ROW", "investment.ROW", "foreign.trade.ROW"
#'   )
#' ))
#'
#' production.CHN <- c(
#'   product.CHN = 140, labor.CHN = 40, capital.CHN = 10,
#'   tax.CHN = 10, dividend.CHN = 20, imported.product.CHN = 5, money.CHN = 5
#' )
#' production.ROW <- c(
#'   product.ROW = 840, labor.ROW = 240, capital.ROW = 60,
#'   tax.ROW = 60, dividend.ROW = 120, imported.product.ROW = 6, money.ROW = 30
#' )
#'
#' consumption.CHN <- c(
#'   product.CHN = 40, bond.CHN = 30, imported.product.CHN = 5, money.CHN = 2
#' )
#'
#' consumption.ROW <- c(
#'   product.ROW = 240, bond.ROW = 180,
#'   imported.product.ROW = 6, money.ROW = 12
#' )
#'
#' investment.CHN <- c(
#'   product.CHN = 30,
#'   imported.product.CHN = 4, money.CHN = 1,
#'   bond.ROW = 1,
#'   money.ROW = 0.02
#' )
#'
#' investment.ROW <- c(
#'   bond.CHN = 1,
#'   money.CHN = 0.02,
#'   product.ROW = 180,
#'   imported.product.ROW = 4, money.ROW = 6
#' )
#'
#'
#' foreign.trade.CHN <- c(
#'   product.ROW = 13,
#'   tax.CHN = 0.65,
#'   money.ROW = 0.26
#' )
#'
#' foreign.trade.ROW <- c(
#'   product.CHN = 15,
#'   tax.ROW = 0.75,
#'   money.CHN = 0.3
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
#' dst_plot(ge$dstl[[4]])
#'
#' ge2 <- gemInputOutputTable_2_8_4(
#'   IT = ge$DV,
#'   money.supply.CHN = sum(ge$DV["money.CHN", ]),
#'   money.supply.ROW = sum(ge$DV["money.ROW", ]),
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
#'   es.productDI.production.CHN = 0.3,
#'   return.dstl = TRUE
#' )
#' geTmp$eri.CHN
#' }

gemInputOutputTable_2_8_4 <- function(IT,
                                      product.output.CHN = sum(IT[, "production.CHN"]),
                                      product.output.ROW = sum(IT[, "production.ROW"]),

                                      labor.supply.CHN = sum(IT["labor.CHN", ]),
                                      labor.supply.ROW = sum(IT["labor.ROW", ]),

                                      capital.supply.CHN = sum(IT["capital.CHN", ]),
                                      capital.supply.ROW = sum(IT["capital.ROW", ]),

                                      money.supply.CHN = 100,
                                      money.supply.ROW = 600,


                                      es.productDI.production.CHN = 0.5, # elasticity of substitution
                                      es.productDI.production.ROW = 0.5,

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
  SExg[c("labor.CHN", "dividend.CHN", "tax.CHN", "money.CHN", "capital.CHN"), "consumption.CHN"] <-
    c(
      labor.supply.CHN,
      sum(IT["dividend.CHN", ]),
      sum(IT["tax.CHN", ]),
      money.supply.CHN,
      capital.supply.CHN
    )
  SExg[c("labor.ROW", "dividend.ROW", "tax.ROW", "money.ROW", "capital.ROW"), "consumption.ROW"] <-
    c(
      labor.supply.ROW,
      sum(IT["dividend.ROW", ]),
      sum(IT["tax.ROW", ]),
      money.supply.ROW,
      capital.supply.ROW
    )

  SExg["bond.CHN", "investment.CHN"] <- sum(IT["bond.CHN", ])
  SExg["bond.ROW", "investment.ROW"] <- sum(IT["bond.ROW", ])


  # demand structure tree ---------------------------------------------------
  # production.CHN ----------------------------------------------------------
  dst.production.CHN <- Node$new("production.CHN",
    type = "money",
    beta = prop.table(c(
      (sum(IT[, "production.CHN"]) - IT["money.CHN", "production.CHN"]),
      IT["money.CHN", "production.CHN"]
    ))
  )

  dst.production.CHN$AddChild("cc1.production.CHN",
    type = "Leontief",
    a = prop.table(
      c(
        sum(IT[c("product.CHN", "imported.product.CHN"), "production.CHN"]),
        sum(IT[c("labor.CHN", "capital.CHN", "tax.CHN", "dividend.CHN"), "production.CHN"])
      )
    )
  )$
    AddChild("cc1.1.production.CHN",
    type = "CES",
    es = es.productDI.production.CHN,
    alpha = 1, # alpha.production.CHN,
    beta = prop.table(IT[c("product.CHN", "imported.product.CHN"), "production.CHN"])
  )$AddSibling("cc1.2.production.CHN",
    type = "FIN",
    beta = prop.table(c(
      sum(IT[c("labor.CHN", "capital.CHN"), "production.CHN"]),
      tax.CHN = IT["tax.CHN", "production.CHN"],
      dividend.CHN = IT["dividend.CHN", "production.CHN"]
    ))
  )$
    parent$
    AddSibling("money.CHN")

  FindNode(dst.production.CHN, "cc1.1.production.CHN")$
    AddChild("product.CHN")$AddSibling("imported.product.CHN")

  FindNode(dst.production.CHN, "cc1.2.production.CHN")$
    AddChild("cc1.2.1.production.CHN",
    type = "CES",
    es = es.laborCapital.production.CHN,
    alpha = 1, # alpha.laborCapital.production.CHN,
    beta = prop.table(IT[c("labor.CHN", "capital.CHN"), "production.CHN"]),
  )$
    AddChild("labor.CHN")$AddSibling("capital.CHN")$
    parent$
    AddSibling("tax.CHN")$AddSibling("dividend.CHN")



  # production.ROW ----------------------------------------------------------
  dst.production.ROW <- Node$new("production.ROW",
    type = "money",
    beta = prop.table(c(
      (sum(IT[, "production.ROW"]) - IT["money.ROW", "production.ROW"]),
      IT["money.ROW", "production.ROW"]
    ))
  )

  dst.production.ROW$AddChild("cc1.production.ROW",
    type = "Leontief",
    a = prop.table(
      c(
        sum(IT[c("product.ROW", "imported.product.ROW"), "production.ROW"]),
        sum(IT[c("labor.ROW", "capital.ROW", "tax.ROW", "dividend.ROW"), "production.ROW"])
      )
    )
  )$
    AddChild("cc1.1.production.ROW",
    type = "CES",
    es = es.productDI.production.ROW,
    alpha = 1, # alpha.production.ROW,
    beta = prop.table(IT[c("product.ROW", "imported.product.ROW"), "production.ROW"])
  )$AddSibling("cc1.2.production.ROW",
    type = "FIN",
    beta = prop.table(c(
      sum(IT[c("labor.ROW", "capital.ROW"), "production.ROW"]),
      tax.ROW = IT["tax.ROW", "production.ROW"],
      dividend.ROW = IT["dividend.ROW", "production.ROW"]
    ))
  )$
    parent$
    AddSibling("money.ROW")

  FindNode(dst.production.ROW, "cc1.1.production.ROW")$
    AddChild("product.ROW")$AddSibling("imported.product.ROW")

  FindNode(dst.production.ROW, "cc1.2.production.ROW")$
    AddChild("cc1.2.1.production.ROW",
    type = "CES",
    es = es.laborCapital.production.ROW,
    alpha = 1, # alpha.laborCapital.production.ROW,
    beta = prop.table(IT[c("labor.ROW", "capital.ROW"), "production.ROW"]),
  )$
    AddChild("labor.ROW")$AddSibling("capital.ROW")$
    parent$
    AddSibling("tax.ROW")$AddSibling("dividend.ROW")


  # consumption.CHN ---------------------------------------------------------
  dst.consumption.CHN <- Node$new("consumption.CHN",
    type = "money",
    beta = prop.table(c(
      sum(IT[, "consumption.CHN"]) - IT["money.CHN", "consumption.CHN"],
      IT["money.CHN", "consumption.CHN"]
    ))
  )
  dst.consumption.CHN$AddChild("cc1.consumption.CHN",
    type = "bond",
    beta = prop.table(c(
      sum(IT[c("product.CHN", "imported.product.CHN"), "consumption.CHN"]),
      IT["bond.CHN", "consumption.CHN"]
    ))
  )$
    AddChild("cc1.1.consumption.CHN",
    type = "CES",
    es = es.consumption.CHN,
    alpha = 1,
    beta = prop.table(IT[c("product.CHN", "imported.product.CHN"), "consumption.CHN"])
  )$AddChild("product.CHN")$AddSibling("imported.product.CHN")$
    parent$
    AddSibling("bond.CHN")$
    parent$
    AddSibling("money.CHN")


  # consumption.ROW ---------------------------------------------------------
  dst.consumption.ROW <- Node$new("consumption.ROW",
    type = "money",
    beta = prop.table(c(
      sum(IT[, "consumption.ROW"]) - IT["money.ROW", "consumption.ROW"],
      IT["money.ROW", "consumption.ROW"]
    ))
  )
  dst.consumption.ROW$AddChild("cc1.consumption.ROW",
    type = "bond",
    beta = prop.table(c(
      sum(IT[c("product.ROW", "imported.product.ROW"), "consumption.ROW"]),
      IT["bond.ROW", "consumption.ROW"]
    ))
  )$
    AddChild("cc1.1.consumption.ROW",
    type = "CES",
    es = es.consumption.ROW,
    alpha = 1,
    beta = prop.table(IT[c("product.ROW", "imported.product.ROW"), "consumption.ROW"])
  )$AddChild("product.ROW")$AddSibling("imported.product.ROW")$
    parent$
    AddSibling("bond.ROW")$
    parent$
    AddSibling("money.ROW")


  # investment.CHN ----------------------------------------------------------
  dst.investment.CHN <- Node$new("investment.CHN",
    type = "FIN",
    beta = prop.table(c(
      sum(IT[c("product.CHN", "imported.product.CHN", "money.CHN"), "investment.CHN"]),
      sum(IT[c("bond.ROW", "money.ROW"), "investment.CHN"])
    ))
  )

  dst.investment.CHN$
    AddChild("cc1.investment.CHN",
    type = "money",
    beta = prop.table(c(
      sum(IT[c("product.CHN", "imported.product.CHN"), "investment.CHN"]),
      IT["money.CHN", "investment.CHN"]
    ))
  )$
    AddChild("cc1.1.investment.CHN",
    type = "CES",
    es = es.investment.CHN,
    alpha = 1,
    beta = prop.table(IT[c("product.CHN", "imported.product.CHN"), "investment.CHN"])
  )$
    AddChild("product.CHN")$AddSibling("imported.product.CHN")$
    parent$
    AddSibling("money.CHN")

  dst.investment.CHN$AddChild("cc2",
    type = "money",
    beta = prop.table(c(
      IT["bond.ROW", "investment.CHN"],
      IT["money.ROW", "investment.CHN"]
    ))
  )$
    AddChild("bond.ROW")$AddSibling("money.ROW")


  # investment.ROW ----------------------------------------------------------
  dst.investment.ROW <- Node$new("investment.ROW",
    type = "FIN",
    beta = prop.table(c(
      sum(IT[c("product.ROW", "imported.product.ROW", "money.ROW"), "investment.ROW"]),
      sum(IT[c("bond.CHN", "money.CHN"), "investment.ROW"])
    ))
  )

  dst.investment.ROW$
    AddChild("cc1.investment.ROW",
    type = "money",
    beta = prop.table(c(
      sum(IT[c("product.ROW", "imported.product.ROW"), "investment.ROW"]),
      IT["money.ROW", "investment.ROW"]
    ))
  )$
    AddChild("cc1.1.investment.ROW",
    type = "CES",
    es = es.investment.ROW,
    alpha = 1,
    beta = prop.table(IT[c("product.ROW", "imported.product.ROW"), "investment.ROW"])
  )$
    AddChild("product.ROW")$AddSibling("imported.product.ROW")$
    parent$
    AddSibling("money.ROW")

  dst.investment.ROW$AddChild("cc2",
    type = "money",
    beta = prop.table(c(
      IT["bond.CHN", "investment.ROW"],
      IT["money.CHN", "investment.ROW"]
    ))
  )$
    AddChild("bond.CHN")$AddSibling("money.CHN")


  # foreign.trade.CHN -------------------------------------------------------
  dst.foreign.trade.CHN <- Node$new("foreign.trade.CHN",
    type = "money",
    beta = prop.table(c(
      sum(IT[c("product.ROW", "tax.CHN"), "foreign.trade.CHN"]),
      IT["money.ROW", "foreign.trade.CHN"]
    ))
  )
  dst.foreign.trade.CHN$AddChild("cc1.foreign.trade.CHN",
    type = "tax",
    beta = prop.table(c(
      IT["product.ROW", "foreign.trade.CHN"],
      IT["tax.CHN", "foreign.trade.CHN"]
    ))
  )$
    AddChild("product.ROW")$AddSibling("tax.CHN")$
    parent$
    AddSibling("money.ROW")


  # foreign.trade.ROW -------------------------------------------------------
  dst.foreign.trade.ROW <- Node$new("foreign.trade.ROW",
    type = "money",
    beta = prop.table(c(
      sum(IT[c("product.CHN", "tax.ROW"), "foreign.trade.ROW"]),
      IT["money.CHN", "foreign.trade.ROW"]
    ))
  )
  dst.foreign.trade.ROW$AddChild("cc1.foreign.trade.ROW",
    type = "tax",
    beta = prop.table(c(
      IT["product.CHN", "foreign.trade.ROW"],
      IT["tax.ROW", "foreign.trade.ROW"]
    ))
  )$
    AddChild("product.CHN")$AddSibling("tax.ROW")$
    parent$
    AddSibling("money.CHN")

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

  # run sdm -----------------------------------------------------------------
  ge <- sdm(
    A = function(state) {
      p <- c(state$p)
      names(p) <- rownames(IT)

      result <- sapply(dstl, demand_coefficient, p)
      return(result)
    },
    B = {
      B <- IT * 0
      B["product.CHN", "production.CHN"] <-
        B["imported.product.CHN", "foreign.trade.CHN"] <-
        B["product.ROW", "production.ROW"] <-
        B["imported.product.ROW", "foreign.trade.ROW"] <- 1
      B
    },
    S0Exg = SExg,
    priceAdjustmentVelocity = 0.05,
    ...
  )


  ge$sector.names <- colnames(IT)
  ge$subject.names <- rownames(IT)
  names(ge$p) <- ge$subject.names
  names(ge$z) <- ge$sector.names


  if (return.dstl) ge$dstl <- dstl

  ge$e <- NULL
  ge$p <- ge$p / ge$p["labor.CHN"]
  ge <- ge_tidy(ge, ge$subject.names, ge$sector.names)
  ge$eri.ROW <- ge$p["money.ROW"] / ge$p["money.CHN"] # exchange rate index
  ge$eri.CHN <- ge$p["money.CHN"] / ge$p["money.ROW"]
  if (!is.na(interest.rate.CHN) & !is.na(interest.rate.ROW)) {
    money.value.CHN <- ge$p["money.CHN"] / interest.rate.CHN
    money.value.ROW <- ge$p["money.ROW"] / interest.rate.ROW
    ge$p.money <- ge$p
    ge$p.money["money.CHN"] <- money.value.CHN
    ge$p.money["money.ROW"] <- money.value.ROW
    ge$p.money <- ge$p.money / ge$p.money["money.CHN"]
  }

  return(ge)
}
