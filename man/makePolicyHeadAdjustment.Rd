% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makePolicyHeadAdjustment.R
\name{makePolicyHeadAdjustment}
\alias{makePolicyHeadAdjustment}
\title{Make a Policy of Head Adjustment for a Timeline Model}
\usage{
makePolicyHeadAdjustment(ind, gr = 0)
}
\arguments{
\item{ind}{a 4-column matrix or a numeric 4-vector that will be converted into a 4-column matrix.
In each row of the matrix, the first element corresponds to the index number of a type of product supplied in the first period,
the second element corresponds to the index number of its supplier,
the third element corresponds to the index number of the type of product supplied in the second period,
and the fourth element corresponds corresponds to the index number of its supplier.

Head adjustments are usually made simultaneously with tail adjustments to compute the steady-state equilibrium path.
There is usually no need to make head adjustments alone.}

\item{gr}{the growth rate.}
}
\value{
A policy, which is often used as an argument of the function sdm2.
}
\description{
Make a policy of head adjustment for a timeline model.
Head adjustment refers to the adjustment of the initial product supply to a steady-state value.
}
\seealso{
{
\code{\link{makePolicyHeadTailAdjustment}};
\code{\link{makePolicyTailAdjustment}}
}
}
