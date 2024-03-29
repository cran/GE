% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/QL_demand.R
\name{QL_demand}
\alias{QL_demand}
\title{Quasilinear Demand Functions}
\usage{
QL_demand(
  w,
  p,
  alpha,
  beta,
  type = c("power", "log", "quadratic1", "quadratic2", "min", "CRRA")
)
}
\arguments{
\item{w}{a scalar indicating the income.}

\item{p}{a 2-vector indicating the prices.}

\item{alpha}{a scalar.}

\item{beta}{a scalar.}

\item{type}{a character string specifying the type of the function. The default type is "power".
Other possible values are "log", "quadratic1", "quadratic2" and "min".}
}
\value{
A 2-by-1 matrix indicating demands.
}
\description{
Some quasilinear demand functions. The corresponding utility functions are as follows: \cr
  power: x1 + alpha * x2^beta, wherein alpha>0, 0<beta<1. \cr
  log: x1 + alpha * log(x2), wherein alpha>0. \cr
  quadratic1: x1 + alpha * x2 - 0.5 * beta * x2^2, wherein alpha>0, beta>0. \cr
  quadratic2: x1 + beta * (alpha * x2 - 0.5 * x2^2), wherein alpha>0, beta>0. \cr
  min: x1 + alpha * min(x2, beta), wherein alpha>0, beta>0. \cr
  CRRA: x1 + alpha * (x2^(1 - beta) - 1) / (1 - beta), wherein alpha>0, beta>0. If beta==1, the function becomes
  x1 + alpha * log(x2).
}
\examples{
\donttest{
QL_demand(w = 0.5, p = c(1, 1), alpha = 1, type = "log")
QL_demand(w = 2, p = c(1, 1), alpha = 1, type = "log")

QL_demand(w = 1, p = c(1, 5), alpha = 2, beta = 0.5)
}

}
