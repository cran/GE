#' @export
#' @title A General Equilibrium Model based on a 7x4 Input-Output Table
#' @aliases gemInputOutputTable_7_4
#' @description This is a general equilibrium model based on a 7x4 input-output table.
#' @details Given a 7x4 input-output table, this model calculates
#' the corresponding general equilibrium.
#' This input-output table contains 3 production sectors and 1 household.
#' The household consumes products and supplies labor, capital, stock and tax receipt.
#' Generally speaking, the value of the elasticity of substitution in this model should be between 0 and 1.
#' @param IT the input and consumption part of the input-output table in the base period.
#' @param product.output the outputs of products in the base period.
#' @param supply.labor the supply of labor.
#' @param supply.capital the supply of capital.
#' @param rhoq.agri,rhoq.manu,rhoq.serv the elasticity of substitution between the intermediate input
#' and the value-added input of the agriculture sector, manufacturing sector and service sector.
#' @param rhoq.hh the elasticity of substitution between products consummed by the household sector.
#' @param rhoVA.agri,rhoVA.manu,rhoVA.serv the elasticity of substitution between labor input and capital input
#' of the agriculture sector, manufacturing sector and service sector.
#' @param ... arguments to be transferred to the function sdm of the package CGE.
#' @return A general equilibrium.
#' @examples
#' \donttest{
#' IT17 <- matrix(c(
#'   1.47, 6.47, 0.57, 2.51,
#'   2.18, 76.32, 12.83, 44.20,
#'   0.82, 19.47, 23.33, 35.61,
#'   6.53, 13.92, 21.88, 0,
#'   0.23, 4.05, 6.76, 0,
#'   -0.34, 6.43, 3.40, 0,
#'   0.13, 8.87, 10.46, 0
#' ), 7, 4, TRUE)
#'
#' product.output <- c(11.02, 135.53, 79.23)
#'
#' rownames(IT17) <- c("agri", "manu", "serv", "lab", "cap", "tax", "dividend")
#' colnames(IT17) <- c("sector.agri", "sector.manu", "sector.serv", "sector.hh")
#'
#' ge <- gemInputOutputTable_7_4(
#'   IT = IT17,
#'   product.output = product.output,
#'   supply.labor = 42.33,
#'   supply.capital = 11.04
#' )
#'
#' #### labor supply reduction
#' geLSR <- gemInputOutputTable_7_4(
#'   IT = IT17,
#'   product.output = product.output,
#'   supply.labor = 42.33 * 0.9,
#'   supply.capital = 11.04
#' )
#'
#' geLSR$z / ge$z
#' geLSR$p / ge$p
#'
#' #### capital accumulation
#' geCA <- gemInputOutputTable_7_4(
#'   IT = IT17,
#'   product.output = product.output,
#'   supply.labor = 42.33,
#'   supply.capital = 11.04 * 1.1
#' )
#'
#' geCA$z / ge$z
#' geCA$p / ge$p
#'
#' #### technology progress
#' IT.TP <- IT17
#' IT.TP ["lab", "sector.manu"] <-
#'   IT.TP ["lab", "sector.manu"] * 0.9
#'
#' geTP <- gemInputOutputTable_7_4(
#'   IT = IT.TP,
#'   product.output = product.output,
#'   supply.labor = 42.33,
#'   supply.capital = 11.04
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
#'   product.output = product.output,
#'   supply.labor = 42.33,
#'   supply.capital = 11.04
#' )
#'
#' geTP2$z / ge$z
#' geTP2$p / ge$p
#'
#' ##
#' IT.TP3 <- IT17
#' IT.TP3 ["lab", "sector.manu"] <-
#'   IT.TP3 ["lab", "sector.manu"] * 0.9
#' IT.TP3 ["lab", "sector.agri"] <-
#'   IT.TP3 ["lab", "sector.agri"] * 0.8
#'
#' geTP3 <- gemInputOutputTable_7_4(
#'   IT = IT.TP3,
#'   product.output = product.output,
#'   supply.labor = 42.33,
#'   supply.capital = 11.04
#' )
#'
#' colSums(geTP3$ITV[4:7, 1:3]) / colSums(ge$ITV[4:7, 1:3]) # change in added value
#' prop.table(colSums(geTP3$ITV[4:7, 1:3])) -
#'   prop.table(colSums(ge$ITV[4:7, 1:3]))
#'
#' #### demand structure change
#' IT.DSC <- IT17
#' IT.DSC["serv", "sector.hh"] <- IT.DSC ["serv", "sector.hh"] * 1.2
#'
#' geDSC <- gemInputOutputTable_7_4(
#'   IT = IT.DSC,
#'   product.output = product.output,
#'   supply.labor = 42.33,
#'   supply.capital = 11.04
#' )
#'
#' geDSC$z[1:3] / ge$z[1:3]
#' geDSC$p / ge$p
#'
#' #### tax change
#' IT.TC <- IT17
#' IT.TC["tax", "sector.agri"] <- IT.TC["tax", "sector.agri"] * 2
#'
#' geTC <- gemInputOutputTable_7_4(
#'   IT = IT.TC,
#'   product.output = product.output,
#'   supply.labor = 42.33,
#'   supply.capital = 11.04
#' )
#'
#' geTC$z / ge$z
#' geTC$p / ge$p
#'
#' ##
#' IT.TC2 <- IT17
#' IT.TC2["tax", "sector.manu"] <- IT.TC2["tax", "sector.manu"] * 0.8
#'
#' geTC2 <- gemInputOutputTable_7_4(
#'   IT = IT.TC2,
#'   product.output = product.output,
#'   supply.labor = 42.33,
#'   supply.capital = 11.04
#' )
#'
#' geTC2$z / ge$z
#' geTC2$p / ge$p
#' }
#'
gemInputOutputTable_7_4 <- function(IT,
                                    product.output,
                                    supply.labor,
                                    supply.capital,

                                    rhoq.agri = 0,
                                    rhoq.manu = 0,
                                    rhoq.serv = 0,
                                    rhoq.hh = 0,

                                    rhoVA.agri = 0.25,
                                    rhoVA.manu = 0.5,
                                    rhoVA.serv = 0.8,
                                    ...) {
  names.commodity <- rownames(IT)
  names.agent <- colnames(IT)

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
  tmp$sigma <- 1 - 1 / rhoq.agri
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
  tmp$sigma <- 1 - 1 / rhoVA.agri
  tmp$alpha <- 1
  tmp$beta <- prop.table(d.agri[4:5])

  # dst.manu ----------------------------------------------------------------
  dst.manu <- Clone(dst.industry)

  tmp <- FindNode(dst.manu, "industry")
  tmp$type <- "CES"
  tmp$sigma <- 1 - 1 / rhoq.manu
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
  tmp$sigma <- 1 - 1 / rhoVA.manu
  tmp$alpha <- 1
  tmp$beta <- prop.table(d.manu[4:5])

  # dst.serv ----------------------------------------------------------------
  dst.serv <- Clone(dst.industry)

  tmp <- FindNode(dst.serv, "industry")
  tmp$type <- "CES"
  tmp$sigma <- 1 - 1 / rhoq.serv
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
  tmp$sigma <- 1 - 1 / rhoVA.serv
  tmp$alpha <- 1
  tmp$beta <- prop.table(d.serv[4:5])


  # dst.hh ------------------------------------------------------------------
  dst.hh <- Node$new("hh",
    type = "CES", sigma = 1 - 1 / rhoq.hh,
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
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0
      ), 7, 4, TRUE)
    },
    S0Exg = {
      tmp <- matrix(NA, 7, 4)
      tmp[4, 4] <- supply.labor
      tmp[5, 4] <- supply.capital
      tmp[6, 4] <- supply.tax.hh
      tmp[6, 1] <- supply.tax.agri
      tmp[7, 4] <- supply.stock
      tmp
    },
    tolCond = 1e-8,
    ...
  )

  ge$p <- ge$p / ge$p[4]

  ge <- ge_tidy(ge, names.commodity, names.agent)


  ge$IT <- ge$D
  ge$ITV <- ge$DV
  if (IT["tax", "sector.agri"] < 0) {
    ge$IT["tax", "sector.agri"] <- -ge$S["tax", "sector.agri"] # tax of sector agri
    ge$ITV["tax", "sector.agri"] <- -ge$SV["tax", "sector.agri"] # tax of sector agri
  }

  ge$value.added <- colSums(ge$DV[4:7, 1:3])
  ge$dstl <- dstl
  return(ge)
}
