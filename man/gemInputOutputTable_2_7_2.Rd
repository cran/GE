% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gemInputOutputTable_2_7_2.R
\name{gemInputOutputTable_2_7_2}
\alias{gemInputOutputTable_2_7_2}
\title{A Two-Country General Equilibrium Model}
\usage{
gemInputOutputTable_2_7_2(
  IT,
  OT,
  es.DIProduct.firm.CHN = 3,
  es.DIProduct.firm.ROW = 3,
  es.laborCapital.firm.CHN = 0.75,
  es.laborCapital.firm.ROW = 0.75,
  es.household.CHN = 3,
  es.household.ROW = 3,
  return.dstl = FALSE,
  ...
)
}
\arguments{
\item{IT}{the input part of the input-output table.}

\item{OT}{the output part of the input-output table.}

\item{es.DIProduct.firm.CHN}{the elasticity of substitution between
domestic product and imported product used by the production sector of CHN.}

\item{es.DIProduct.firm.ROW}{the elasticity of substitution between
domestic product and imported product used by the production sector of ROW.}

\item{es.laborCapital.firm.CHN}{the elasticity of substitution between
labor and capital goods used by the production sector of CHN.}

\item{es.laborCapital.firm.ROW}{the elasticity of substitution between
labor and capital goods used by the production sector of ROW.}

\item{es.household.CHN}{the elasticity of substitution between
domestic product and imported product used by the consumption sector of CHN.}

\item{es.household.ROW}{the elasticity of substitution between
domestic product and imported product used by the consumption sector of ROW.}

\item{return.dstl}{If TRUE, the demand structure tree will be returned.}

\item{...}{arguments to be transferred to the function \code{\link{sdm2}}.}
}
\value{
A general equilibrium, which usually is a list with the following elements:
\itemize{
\item p - the price vector with CHN labor as numeraire.
\item dstl - the demand structure tree list of sectors if return.dstl == TRUE.
\item ... - some elements returned by the function \code{\link{sdm2}}.
}
}
\description{
A two-country general equilibrium model.
This general equilibrium model is based on a two-country (i.e. CHN and ROW) input-output table consisting of an input part and an output part.
Each country contains 2 sectors and 7 commodities (or subjects).
The 2 sectors are firm and household.
The 7 commodities (or subjects) are product, labor, capital goods, bond, tax, dividend, tariff.
Hence the input-output table has 14 rows and 4 columns.
}
\examples{
\donttest{
IT <- matrix(c(
  142, 84, 13, 4.1,
  47, 0, 0, 0,
  13, 0, 0, 0,
  0, 0, 0, 3.4,
  9.3, 0, 0, 0,
  22, 0, 0, 0,
  0.15, 0.091, 0, 0,
  10, 6, 381, 451,
  0, 0, 252, 0,
  0, 0, 81, 0,
  0, 4.9, 0, 0,
  0, 0, 26, 0,
  0, 0, 92, 0,
  0, 0, 1.9, 0.59
), 14, 4, TRUE)

OT <- matrix(c(
  244, 0, 0, 0,
  0, 47, 0, 0,
  0, 13, 0, 0,
  0, 3.4, 0, 0,
  0, 9.3, 0, 0,
  0, 22, 0, 0,
  0, 0.24, 0, 0,
  0, 0, 849, 0,
  0, 0, 0, 252,
  0, 0, 0, 81,
  0, 0, 0, 4.9,
  0, 0, 0, 26,
  0, 0, 0, 92,
  0, 0, 0, 2.5
), 14, 4, TRUE)

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

ge <- gemInputOutputTable_2_7_2(IT, OT, return.dstl = TRUE)
ge$p
ge$z
node_plot(ge$dstl[[1]])
ge$dstl[[1]]$a

## tariff rate change in CHN
dstl <- lapply(ge$dstl, Clone)
tmp <- node_set(dstl[[1]], "cc1.1")
tmp$beta[2] <- tmp$beta[2] * 10

ge.TRC <- sdm2(
  A = dstl, B = ge$B, S0Exg = ge$S0Exg,
  names.commodity = rownames(ge$B),
  names.agent = colnames(ge$B),
  numeraire = "labor.CHN"
)

ge.TRC$p
ge.TRC$z
#### technology progress in CHN
OT.TP <- OT
OT.TP["product.CHN", "firm.CHN"] <- OT["product.CHN", "firm.CHN"] * 1.2

ge.TP <- gemInputOutputTable_2_7_2(IT, OT.TP, return.dstl = TRUE)
ge.TP$p
ge.TP$z
ge.TP$dstl[[1]]$a

#### capital accumulation in CHN
OT.CA <- OT
OT.CA["capital.CHN", "household.CHN"] <- OT["capital.CHN", "household.CHN"] * 2

ge.CA <- gemInputOutputTable_2_7_2(IT, OT.CA)
ge.CA$p
ge.CA$z

#### labor supply change in CHN
OT.LSC <- OT
OT.LSC["labor.CHN", "household.CHN"] <- OT["labor.CHN", "household.CHN"] * 0.5

ge.LSC <- gemInputOutputTable_2_7_2(IT, OT.LSC)
ge.LSC$p
ge.LSC$z
}

}
