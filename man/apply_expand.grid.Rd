% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apply_expand.grid.R
\name{apply_expand.grid}
\alias{apply_expand.grid}
\title{Applying a Function to All Combinations of the Supplied Vectors}
\usage{
apply_expand.grid(FUN, ...)
}
\arguments{
\item{FUN}{the function to be applied. The argument is a numeric vector.}

\item{...}{numeric vectors.}
}
\value{
A data frame.
}
\description{
A wrapper of the functions apply and expand.grid.
Returns a data frame of values obtained by applying a function
to all combinations of the supplied vectors.
Firstly, the function expand.grid will be used for the supplied vectors in ... and
we will get a data frame containing one row for each combination of the supplied vectors.
Then the function will be applied to each row of the data frame.
The values of the data frame will also be included in the returned data frame.
}
\examples{
\donttest{
apply_expand.grid(prod, a = 1:9, b = 1:9)

####
f <- function(x) c(r1 = sum(x), r2 = unname(x["b"] - x["a"]))
apply_expand.grid(f, a = c(1, 2), b = c(3, 4))

####
f <- function(x) list(list(sum(x)), prod(x))
apply_expand.grid(f, a = c(1, 2), b = c(3, 4))

####
f <- function(x){
  result <- SCES_A(alpha = 1, Beta = c(0.5, 0.5), p = c(x["p1"], 1), es = x["es"])
  names(result) <- c("dc1","dc2")
  result
}

apply_expand.grid(f, p1 = seq(0.1, 10, 0.1), es = c(0.3, 0.5, 1))

####
f <- function(x) {
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
    supply.labor = x[1],
    supply.capital = x[2]
  )

  ge$p
}

apply_expand.grid(f, supply.labor = seq(10, 20, 10), supply.capital = seq(8, 16, 4))
}
}
