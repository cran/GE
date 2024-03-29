% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/output.R
\name{output}
\alias{output}
\title{Compute the Utility of a Consumer or the Output of a Firm by the Demand Structural Tree}
\usage{
output(node, input)
}
\arguments{
\item{node}{a demand structural tree.}

\item{input}{an input vector with names of commodities.}
}
\value{
A scalar.
}
\description{
Given a demand structural tree and an input vector,
this function computes the utility of a consumer or the output of a firm.
If the demand structural tree has a FUNC-type node,
the node should has an attribute named fun that is a function computing the output.
}
\examples{
dst <- node_new("output",
                type = "SCES", es = 0, alpha = 1, beta = c(0.5, 0.5),
                "cc1", "cc2"
)
node_set(dst, "cc1",
         type = "Leontief", a = c(0.6, 0.4),
         "wheat", "iron"
)
node_set(dst, "cc2",
         type = "SCES", sigma = -1, alpha = 1, beta = c(0.5, 0.5),
         "labor", "capital"
)

node_plot(dst, TRUE)

p <- c(wheat = 1, iron = 3, labor = 2, capital = 4)
x <-demand_coefficient(dst, p)
output(dst, x)

output(dst, c(wheat = 3, iron = 3, labor = 3, capital = 3))
SCES(
  es = 0, alpha = 1, beta = c(0.5, 0.5),
  x = c(
    min(3 / 0.6, 3 / 0.4),
    SCES(es = 0.5, alpha = 1, beta = c(0.5, 0.5), x = c(3, 3))
  )
)
}
