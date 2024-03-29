% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gemCanonicalDynamicMacroeconomic_4_3.R
\name{gemCanonicalDynamicMacroeconomic_4_3}
\alias{gemCanonicalDynamicMacroeconomic_4_3}
\title{A Canonical Dynamic Macroeconomic General Equilibrium Model (see Torres, 2016)}
\usage{
gemCanonicalDynamicMacroeconomic_4_3(
  discount.factor = 0.97,
  depreciation.rate = 0.06,
  beta.prod.firm = 0.35,
  beta.prod.consumer = 0.4,
  ...
)
}
\arguments{
\item{discount.factor}{the intertemporal discount factor.}

\item{depreciation.rate}{the physical depreciation rate of capital stock.}

\item{beta.prod.firm}{the share parameter of the product in the Cobb-Douglas production function of the production firm.}

\item{beta.prod.consumer}{the share parameter of the product in the Cobb-Douglas period utility function of the consumer.}

\item{...}{arguments to be to be passed to the function sdm2.}
}
\value{
A general equilibrium (see \code{\link{sdm2}})
}
\description{
A canonical dynamic macroeconomic general equilibrium model (see Torres, 2016, Table 2.1 and 2.2).
}
\details{
A general equilibrium model with 4 commodities (i.e. product, labor, capital and equity shares)
and 3 agents (i.e. a production firm, a consumer and a capital-leasing firm).
}
\examples{
\donttest{
#### a spot market clearing path that converges to the steady-state equilibrium
ge <- gemCanonicalDynamicMacroeconomic_4_3(
  numberOfPeriods = 100,
  policy = policyMarketClearingPrice
)

matplot(ge$ts.z, type = "o", pch = 20)
matplot(ge$ts.p, type = "o", pch = 20)

## population growth: a spot market clearing path
## that converges to a balanced growth path
ge <- gemCanonicalDynamicMacroeconomic_4_3(
  numberOfPeriods = 100,
  GRExg = 0.01,
  policy = policyMarketClearingPrice
)

matplot((ge$ts.p), type = "l")
matplot((ge$ts.z), type = "l")
matplot(growth_rate(ge$ts.z), type = "l")

#### a disequilibrium path and the steady-state equilibrium
ge <- gemCanonicalDynamicMacroeconomic_4_3(
  numberOfPeriods = 5000,
  priceAdjustmentVelocity = 0.03,
)

ge$p
ge$z
matplot(ge$ts.z, type = "l")
node_plot(ge$dstl[[3]], param = TRUE)

## a small disturbance to the product supply
ge <- gemCanonicalDynamicMacroeconomic_4_3(
  numberOfPeriods = 4000,
  priceAdjustmentVelocity = 0.03,
  policy = function(time, state) {
    if (time == 1500) {
      state$S[1, 1] <- state$S[1, 1] * 0.999
    }
    state
  }
)

#### business cycles
de <- gemCanonicalDynamicMacroeconomic_4_3(
  numberOfPeriods = 1000,
  priceAdjustmentVelocity = 0.15
)

## A tax rate policy is implemented from the 600th period to stabilize the economy.
ge <- gemCanonicalDynamicMacroeconomic_4_3(
  numberOfPeriods = 1500,
  priceAdjustmentVelocity = 0.15,
  policy = Example9.10.policy.tax
)

matplot(ge$ts.z, type = "l")
plot(ge$policy.data, type = "l") # tax rates

#### a spot market-clearing path with a productivity shock
nPeriod <- 100 # the number of periods of the spot market clearing path
set.seed(1)
alpha.shock <- rep(1, nPeriod)
alpha.shock[11] <- exp(0.01)
for (t in 12:nPeriod) {
  alpha.shock[t] <- exp(0.95 * log(alpha.shock[t - 1]))
}
plot(alpha.shock)

ge <- gemCanonicalDynamicMacroeconomic_4_3(
  numberOfPeriods = nPeriod,
  p0 = c(1, 1.34312, 0.09093, 0.08865),
  z0 = c(74.47, 61.20, 286.65),
  policy = list(
    function(time, A) {
      A[[1]]$alpha <- alpha.shock[time]
    },
    policyMarketClearingPrice
  )
)

matplot(ge$ts.z[, 1], type = "o", pch = 20)
}

}
\references{
Torres, Jose L. (2016, ISBN: 9781622730452) Introduction to Dynamic Macroeconomic General Equilibrium Models (Second Edition). Vernon Press.
}
