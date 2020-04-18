#' @export

#' @title Compute the Growth Rate
#' @aliases growth_rate
#' @description Compute the growth rates for a vector or each column of a matrix.
#'
#' @param x a vector or a matrix.
#' @param log If log == TRUE, the logarithmic growth rate will be computed.
#' @param first.na If first.na==FALSE, the result doesn't contain the first NA.
#' @return a vector or a matrix consisting of growth rates.
#' @examples
#' \donttest{
#' x <- matrix(1:8, 4, 2)
#' growth_rate(x)
#' }
#'
growth_rate <- function(x, log = FALSE, first.na = FALSE) {
  vector.growth.rate <- function(v) {
    result <- v[-1] / v[-length(v)] - 1
    if (first.na) {
      return(c(NA, result))
    } else {
      return(result)
    }
  }

  if (!is.vector(x)) x <- as.matrix(x)

  if (log) {
    result <- diff(log(x))
    if (first.na) {
      if (is.matrix(x)) {
        result <- rbind(rep(NA, ncol(x)), result)
      } else {
        result <- c(NA, result)
      }
    }
    return(result)
  }

  if (is.matrix(x)) {
    return(apply(x, 2, vector.growth.rate))
  } else {
    return(vector.growth.rate(x))
  }
}
