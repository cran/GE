#' @export
#' @title Population Variance and Population Standard Deviation
#' @aliases var.p
#' @aliases sd.p
#' @description The function var.p computes a population variance. The function sd.p computes a population standard deviation.
#' @describeIn var.p Population variance.
#' @param x a numeric vector.
#' @param w a numeric vector of weights.
#' By default all elements of x are given the same weight.
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds.
#' @examples
#' var.p(1:6)
#'
#' var.p(x = 1:5, w = 6:10)
#' var.p(x = rep(1:5, 6:10))
#'
#' sd.p(x = 1:5, w = 6:10)
#' sd.p(x = rep(1:5, 6:10))
#'

var.p <- function(x, w = rep(1, length(x)), na.rm = FALSE) {
  if (na.rm) {
    w <- w[i <- !is.na(x)]
    x <- x[i]
  }
  weighted.mean((x - weighted.mean(x, w))^2, w)
}

#' @export
#' @describeIn var.p
#' Population standard deviation.
sd.p <- function(x, w = rep(1, length(x)), na.rm = FALSE) sqrt(var.p(x, w, na.rm))
