% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AMSD.R
\name{AMSD}
\alias{AMSD}
\alias{AMV}
\title{Additive-Mean-Variance Utility Function and Additive-Mean-Standard-Deviation Utility Function}
\usage{
AMSD(x, gamma = 1, wt = NULL, theta = 1)

AMV(x, gamma = 1, wt = NULL)
}
\arguments{
\item{x}{a numeric vector.}

\item{gamma}{the risk aversion coefficient.}

\item{wt}{a numeric vector of weights (or probability).
If wt is NULL, all elements of x are given the same weight.}

\item{theta}{a non-negative scalar with a default value of 1.}
}
\description{
Compute the utility function mean(x) - 0.5 * gamma * sd.p(x)^theta or
weighted.mean(x, wt) - 0.5 * gamma * sd.p(x, wt)^theta.
}
\section{Functions}{
\itemize{
\item \code{AMSD}: Computes the utility function mean(x) - 0.5 * gamma * sd.p(x)^theta or
weighted.mean(x, wt) - 0.5 * gamma * sd.p(x, wt)^theta. When theta == 2, it is the additive mean-variance utility function (i.e.
the function AMV).
When theta == 1 (the default value), it is the additive mean and standard deviation utility function.

\item \code{AMV}: Compute the additive mean-variance utility function mean(x) - 0.5 * gamma * var.p(x) or
weighted.mean(x, wt) - 0.5 * gamma * var.p(x, wt).
}}

\examples{
\donttest{
AMSD(1:2, gamma = 0.1)
AMSD(1:2, gamma = 1, theta = 2)

marginal_utility(
  c(1, 1.001),
  c(0, 1), AMSD
)
marginal_utility(
  c(1.001, 1),
  c(0, 1), AMSD
)
####
## This example computes the equilibrium prices of securities under the additive
## mean-variance utility function.In the calculation, the demand structure of each
## agent is adjusted according to the marginal utility and the prices of securities.
secy1 <- c(1, 0)
secy2 <- c(0, 1)
prob <- c(0.5, 0.5)
USP <- cbind(secy1, secy2) # unit security payoff matrix

fun <- function(gamma.agt1 = 1, gamma.agt2 = 1,
                S0Exg = matrix(c(
                  0.5, 0.5,
                  1, 1
                ), 2, 2, TRUE)) {
  sdm2(
    A = function(state) {
      Payoff <- USP \%*\% (state$last.A \%*\% dg(state$last.z))

      VMU <- marginal_utility(Payoff, USP, list(
        function(x) AMV(x, gamma = gamma.agt1, prob),
        function(x) AMV(x, gamma = gamma.agt2, prob)
      ), state$p)

      A <- state$last.A * pmax(VMU, 0)
      prop.table(A, 2)
    },
    B = matrix(0, 2, 2),
    S0Exg = S0Exg,
    names.commodity = c("secy1", "secy2"),
    names.agent = c("agt1", "agt2")
  )
}

fun()$p
Payoff <- USP \%*\% c(0.5, 1)
marginal_utility(
  Payoff, USP,
  function(x) AMV(x, gamma = 1, prob)
)

fun(S0Exg = matrix(c(
  1, 0,
  0, 2
), 2, 2, TRUE))$p

fun(gamma.agt1 = 0.5, gamma.agt2 = 1)$p

####
## This example computes the equilibrium prices of securities under
## the additive mean and standard deviation utility function.
secy1 <- c(1, 0)
secy2 <- c(0, 1)
prob <- c(0.5, 0.5)
USP <- cbind(secy1, secy2) # security structure matrix

ge <- sdm2(
  A = function(state) {
    Payoff <- USP \%*\% (state$last.A \%*\% dg(state$last.z))

    VMU <- marginal_utility(
      Payoff, USP,
      function(x) AMSD(x, gamma = 0.1, prob), state$p
    )
    Ratio <- sweep(VMU, 2, colMeans(VMU), "/")

    A <- state$last.A * ratio_adjust(Ratio, coef = 0.3, method = "linear")
    prop.table(A, 2)
  },
  B = matrix(0, 2, 2),
  S0Exg = matrix(c(
    1, 0,
    0, 4
  ), 2, 2, TRUE),
  names.commodity = c("secy1", "secy2"),
  names.agent = c("agt1", "agt2"),
  ts = TRUE,
  maxIteration = 1,
  numberOfPeriods = 2000,
  policy = policyMeanValue
)

matplot(tail(ge$ts.q, 100), type = "l")

ge$D
ge$p
Payoff <- USP \%*\% ge$D
marginal_utility(
  Payoff, USP,
  function(x) AMSD(x, gamma = 0.1, prob)
)

marginal_utility(
  c(1,4), USP,
  function(x) AMSD(x, gamma = 0.1, prob)
)
}

}
\references{
Nakamura, Yutaka (2015). Mean-Variance Utility. Journal of Economic Theory, 160: 536-556.
}