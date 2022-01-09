#' @export
#' @title Population Variance and Population Standard Deviation
#' @aliases var.p
#' @aliases sd.p
#' @description The function var.p computes a population variance. The function sd.p computes a population standard deviation.
#' @describeIn var.p Population variance.
#' @param x a numeric vector.
#' @param wt a numeric vector of weights.
#' By default all elements of x are given the same weight.
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds.
#' @examples
#' var.p(1:6)
#'
#' var.p(x = 1:5, wt = 6:10)
#' var.p(x = rep(1:5, 6:10))
#'
#' sd.p(x = 1:5, wt = 6:10)
#' sd.p(x = rep(1:5, 6:10))
#'

var.p <- function(x, wt = rep(1, length(x)), na.rm = FALSE) {
  if (na.rm) {
    wt <- wt[i <- !is.na(x)]
    x <- x[i]
  }
  weighted.mean((x - weighted.mean(x, wt))^2, wt)
}

#' @export
#' @describeIn var.p
#' Population standard deviation.
sd.p <- function(x, wt = rep(1, length(x)), na.rm = FALSE) sqrt(var.p(x, wt, na.rm))
