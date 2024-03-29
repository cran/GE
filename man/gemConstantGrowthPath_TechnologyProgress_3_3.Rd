% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gemConstantGrowthPath_TechnologyProgress_3_3.R
\name{gemConstantGrowthPath_TechnologyProgress_3_3}
\alias{gemConstantGrowthPath_TechnologyProgress_3_3}
\title{Constant Growth Paths with Technology Progress}
\usage{
gemConstantGrowthPath_TechnologyProgress_3_3(...)
}
\arguments{
\item{...}{arguments to be passed to the function sdm2.}
}
\description{
This is an example of a spot market clearing path (alias instantaneous equilibrium path) converging to a constant growth path.
In a constant growth path, the supply of each commodity grows at a constant rate.
The balanced growth path is a special case of the constant growth path.
}
\examples{
\donttest{
dst.firm1 <- node_new(
  "output",
  type = "CD", alpha = 1, beta = c(0.35, 0.65),
  "prod1", "lab"
)

dst.firm2 <- node_new(
  "output",
  type = "CD", alpha = 1, beta = c(0.4, 0.6),
  "prod1", "lab"
)

dst.consumer <- node_new(
  "util",
  type = "Leontief", a = 1,
  "prod2"
)

ge <- sdm2(
  A = list(dst.firm1, dst.firm2, dst.consumer),
  B = matrix(c(
    1, 0, 0,
    0, 1, 0,
    0, 0, 0
  ), 3, 3, TRUE),
  S0Exg = matrix(c(
    NA, NA, NA,
    NA, NA, NA,
    NA, NA, 1
  ), 3, 3, TRUE),
  names.commodity = c("prod1", "prod2", "lab"),
  names.agent = c("firm1", "firm2", "consumer"),
  numeraire = "lab",
  z0 = c(0.2, 0.2, 1),
  ts = TRUE,
  policy = list(
    function(time, A, state) {
      A[[1]]$alpha <- exp(time * 0.01)
      A[[2]]$alpha <- exp(time * 0.01)
      state$S[3, 3] <- exp(time * 0.01)
      state
    },
    policyMarketClearingPrice
  ),
  numberOfPeriods = 20,
  maxIteration = 1
)

matplot(ge$ts.z, type = "l")
matplot(log(ge$ts.z[, 1:2]), type = "l")
matplot(growth_rate(ge$ts.z[, 1:2], log = TRUE), type = "o", pch = 20)
matplot(growth_rate(ge$ts.p[, 1:2], log = TRUE), type = "o", pch = 20)
}
}
