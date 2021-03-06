% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gemInputOutputTable_7_4.R
\name{gemInputOutputTable_7_4}
\alias{gemInputOutputTable_7_4}
\title{A General Equilibrium Model based on a 7x4 Input-Output Table}
\usage{
gemInputOutputTable_7_4(
  IT,
  product.output,
  supply.labor,
  supply.capital,
  es.agri = 0,
  es.manu = 0,
  es.serv = 0,
  es.hh = 0,
  es.VA.agri = 0.25,
  es.VA.manu = 0.5,
  es.VA.serv = 0.8,
  ...
)
}
\arguments{
\item{IT}{the input part of the input-output table in the base period (unit: trillion yuan).}

\item{product.output}{the outputs of products in the base period.}

\item{supply.labor}{the supply of labor.}

\item{supply.capital}{the supply of capital.}

\item{es.agri, es.manu, es.serv}{the elasticity of substitution between the intermediate input
and the value-added input of the agriculture sector, manufacturing sector and service sector.}

\item{es.hh}{the elasticity of substitution among products consumed by the household sector.}

\item{es.VA.agri, es.VA.manu, es.VA.serv}{the elasticity of substitution between labor input and capital input
of the agriculture sector, manufacturing sector and service sector.}

\item{...}{arguments to be transferred to the function sdm of the package CGE.}
}
\value{
A general equilibrium, which is a list with the following elements:
\itemize{
\item p - the price vector with labor as numeraire.
\item D - the demand matrix, also called the input table. Wherein the benchmark prices are used.
\item DV - the demand value matrix, also called the value input table. Wherein the current price is used.
\item SV - the supply value matrix, also called the value output table. Wherein the current price is used.
\item IT - the nonstandard input table, wherein the agricultural tax can be negative.
\item ITV - the nonstandard value input table, wherein the agricultural tax can be negative.
\item value.added - the value-added of the three production sectors.
\item dstl - the demand structure tree list of sectors.
\item ... - some elements returned by the CGE::sdm function
}
}
\description{
This is a general equilibrium model based on a 7x4 input-output table.
}
\details{
Given a 7x4 input-output table, this model calculates
the corresponding general equilibrium.
This input-output table contains 3 production sectors and 1 household.
The household consumes products and supplies labor, capital, stock and tax receipt.
Generally speaking, the value of the elasticity of substitution in this model should be between 0 and 1.
}
\examples{
\donttest{
IT17 <- matrix(c(
  1.47, 6.47, 0.57, 2.51,
  2.18, 76.32, 12.83, 44.20,
  0.82, 19.47, 23.33, 35.61,
  6.53, 13.92, 21.88, 0,
  0.23, 4.05, 6.76, 0,
  -0.34, 6.43, 3.40, 0,
  0.13, 8.87, 10.46, 0
), 7, 4, TRUE)

product.output <- c(11.02, 135.53, 79.23)

rownames(IT17) <- c("agri", "manu", "serv", "lab", "cap", "tax", "dividend")
colnames(IT17) <- c("sector.agri", "sector.manu", "sector.serv", "sector.hh")

ge <- gemInputOutputTable_7_4(
  IT = IT17,
  product.output = product.output,
  supply.labor = 42.33,
  supply.capital = 11.04
)

#### labor supply reduction
geLSR <- gemInputOutputTable_7_4(
  IT = IT17,
  product.output = product.output,
  supply.labor = 42.33 * 0.9,
  supply.capital = 11.04
)

geLSR$z / ge$z
geLSR$p / ge$p

#### capital accumulation
geCA <- gemInputOutputTable_7_4(
  IT = IT17,
  product.output = product.output,
  supply.labor = 42.33,
  supply.capital = 11.04 * 1.1
)

geCA$z / ge$z
geCA$p / ge$p

#### technology progress
IT.TP <- IT17
IT.TP ["lab", "sector.manu"] <-
  IT.TP ["lab", "sector.manu"] * 0.9

geTP <- gemInputOutputTable_7_4(
  IT = IT.TP,
  product.output = product.output,
  supply.labor = 42.33,
  supply.capital = 11.04
)

geTP$z / ge$z
geTP$p / ge$p

##
IT.TP2 <- IT.TP
IT.TP2 ["cap", "sector.manu"] <-
  IT.TP2["cap", "sector.manu"] * 1.02
geTP2 <- gemInputOutputTable_7_4(
  IT = IT.TP2,
  product.output = product.output,
  supply.labor = 42.33,
  supply.capital = 11.04
)

geTP2$z / ge$z
geTP2$p / ge$p

##
IT.TP3 <- IT17
IT.TP3 ["lab", "sector.manu"] <-
  IT.TP3 ["lab", "sector.manu"] * 0.9
IT.TP3 ["lab", "sector.agri"] <-
  IT.TP3 ["lab", "sector.agri"] * 0.8

geTP3 <- gemInputOutputTable_7_4(
  IT = IT.TP3,
  product.output = product.output,
  supply.labor = 42.33,
  supply.capital = 11.04
)

geTP3$value.added / ge$value.added
prop.table(geTP3$value.added) - prop.table(ge$value.added)

#### demand structure change
IT.DSC <- IT17
IT.DSC["serv", "sector.hh"] <- IT.DSC ["serv", "sector.hh"] * 1.2

geDSC <- gemInputOutputTable_7_4(
  IT = IT.DSC,
  product.output = product.output,
  supply.labor = 42.33,
  supply.capital = 11.04
)

geDSC$z[1:3] / ge$z[1:3]
geDSC$p / ge$p

#### tax change
IT.TC <- IT17
IT.TC["tax", "sector.agri"] <- IT.TC["tax", "sector.agri"] * 2

geTC <- gemInputOutputTable_7_4(
  IT = IT.TC,
  product.output = product.output,
  supply.labor = 42.33,
  supply.capital = 11.04
)

geTC$z / ge$z
geTC$p / ge$p

##
IT.TC2 <- IT17
IT.TC2["tax", "sector.manu"] <- IT.TC2["tax", "sector.manu"] * 0.8

geTC2 <- gemInputOutputTable_7_4(
  IT = IT.TC2,
  product.output = product.output,
  supply.labor = 42.33,
  supply.capital = 11.04
)

geTC2$z / ge$z
geTC2$p / ge$p
}

}
