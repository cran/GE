% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gemstEndogenousLaborSupply_2_2.R
\name{gemstEndogenousLaborSupply_2_2}
\alias{gemstEndogenousLaborSupply_2_2}
\title{A General Equilibrium Model with Endogenous Labor Supply}
\usage{
gemstEndogenousLaborSupply_2_2(...)
}
\arguments{
\item{...}{arguments to be passed to the function sdm2.}
}
\description{
This is an example of the spot market clearing path (alias instantaneous equilibrium path) with endogenous labor supply.
Assume that as the level of utility increases, laborer will purchase or receive more education and training,
resulting in an increase in human capital, which can be regarded as an increase in labor supply.
That is to say, the utility level as an endogenous variable will affect the supply of labor.
Therefore, labor supply becomes an endogenous variable.
}
\examples{
\donttest{
f <- function(z0 = c(20, 1)) {
  ge <- sdm2(
    A = function(state) {
      a.firm <- CD_A(alpha = 5, Beta = c(0.5, 0.5), p = state$p)
      a.consumer <- c(1, 0)
      cbind(a.firm, a.consumer)
    },
    B = matrix(c(
      1, 0,
      0, 0
    ), 2, 2, TRUE),
    S0Exg = matrix(c(
      NA, NA,
      NA, 1
    ), 2, 2, TRUE),
    names.commodity = c("prod", "lab"),
    names.agent = c("firm", "consumer"),
    numeraire = "lab",
    z0 = z0,
    ts = TRUE,
    policy = list(
      function(state) {
        state$S[2, 2] <- structural_function(state$last.z[2], c(6.5, 7), 1, 2)
        state
      },
      policyMarketClearingPrice
    ),
    numberOfPeriods = 20,
    maxIteration = 1
  )
  matplot(ge$ts.z, type = "o", pch = 20)
  print(ge$z)
  print(ge$S)
}

f()

f(c(10,1))
}
}
