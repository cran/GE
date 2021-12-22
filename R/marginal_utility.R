#' @export
#' @title Marginal Utility
#' @aliases marginal_utility
#' @description If the argument price is null, this function computes the (delta) marginal utility.
#' By default, delta is set to 1e-10.
#' Otherwise this function computes the (delta) value marginal utility.
#' For a utility function U(x), two vector x, y and a scalar price, the value marginal utility is (U(x + delta * y) - U(x)) / delta, and
#' the value marginal utility is (U(x + delta * y / price) - U(x)) / delta.
#' @param x a numeric k-by-m matrix or a numeric vector (i.e. a k-by-1 matrix).
#' @param y a numeric k-by-n matrix or a numeric vector (i.e. a k-by-1 matrix).
#' @param func a utility function or a list consisting of m utility functions.
#' @param price NULL or a numeric n-vector consisting of prices of each column of y.
#' @param delta a scalar.
#' @return A n-by-m marginal utility matrix. Its (i,j)-th element corresponds to the i-th column of y and the j-th column of x.
#' @examples
#' marginal_utility(1:2, cbind(1:2, 1:1), AMV)
#' marginal_utility(1:2, cbind(1:2, 1:1), AMV, delta = 100)
#' marginal_utility(cbind(1:2, 3:4), cbind(1:2, 1:1), AMV)
#' marginal_utility(
#'   cbind(1:2, 3:4), cbind(1:2, 1:1),
#'   list(AMV, function(x) AMV(x, gamma = 0.5))
#' )
#'
#' ####
#' marginal_utility(
#'   1:2, cbind(1:2, 1:1),
#'   function(x, gamma = 1, p = rep(1, length(x))) CRRA(x, gamma, p)$CE
#' )
#' marginal_utility(1:2, cbind(1:2, 1:1), function(x) sqrt(prod(x)))
#'
#' gamma <- 0.8
#' w <- c(0.25, 0.75)
#' marginal_utility(
#'   1:2, cbind(1:2, 1:1),
#'   function(x) CRRA(x, gamma = gamma, p = w)$CE
#' )
#' ## the same as above. CRRA and CES utility funcitons are essentially the same.
#' es <- 1 / gamma
#' beta <- w^es
#' marginal_utility(
#'   1:2, cbind(1:2, 1:1),
#'   function(x) CES(x = x, sigma = 1 - 1 / es, alpha = 1, beta = w)
#' )
#'
#' prop.table(marginal_utility(
#'   1:2, cbind(1:2, 1:1),
#'   function(x) CRRA(x, gamma = gamma, p = w)$CE
#' ))
#' prop.table(marginal_utility(
#'   1:2, cbind(1:2, 1:1),
#'   function(x) CRRA(x, gamma = gamma, p = w)$u
#' ))

marginal_utility <- function(x, y, func, price = NULL, delta = 1e-10) {
  x <- as.matrix(x)
  y <- as.matrix(y)

  if (!is.null(price)) {
    y <- y %*% dg(1 / price)
  }

  m <- ncol(x)
  n <- ncol(y)
  result <- matrix(NA, n, m)

  if (is.function(func)) {
    for (kc in 1:m) {
      for (kr in 1:n) {
        result[kr, kc] <- (func(x[, kc] + delta * y[, kr]) - func(x[, kc])) / delta
      }
    }
  } else { # function list
    for (kc in 1:m) {
      for (kr in 1:n) {
        result[kr, kc] <- (func[[kc]](x[, kc] + delta * y[, kr]) - func[[kc]](x[, kc])) / delta
      }
    }
  }

  result
}
