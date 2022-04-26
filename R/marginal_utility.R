#' @export
#' @title Marginal Utility
#' @aliases marginal_utility
#' @description If the argument price is null, this function computes the (delta) marginal utility.
#' By default, delta is set to 1e-10.
#' Otherwise this function computes the (delta) value marginal utility.
#' For a utility function U(x), two vector x, y and a scalar price, the marginal utility is (U(x + delta * y) - U(x)) / delta, and
#' the value marginal utility is (U(x + delta * y / price) - U(x)) / delta.
#' For a marginal utility function M(x), three vector x, y, wt and a scalar price, the marginal utility is sum(M(x) * y * wt), and
#' the value marginal utility is sum(M(x) * y * wt / price).
#' @param x a numeric k-by-m matrix or a numeric vector (i.e. a k-by-1 matrix).
#' @param y a numeric k-by-n matrix or a numeric vector (i.e. a k-by-1 matrix).
#' @param uf a utility function or a list consisting of m utility functions.
#' @param price NULL or a numeric n-vector consisting of prices of each column of y.
#' @param delta a scalar.
#' @param muf a marginal utility function or a list consisting of m marginal utility functions.
#' @param wt a weight vector.
#' @return A n-by-m marginal utility matrix. Its (i,j)-th element corresponds to the i-th column of y and the j-th column of x.
#' @references Sharpe, William F. (2008, ISBN: 9780691138503) Investors and Markets: Portfolio Choices, Asset Prices, and Investment Advice. Princeton University Press.
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
#' wt <- 1:2
#' uf <- function(x) (x - x^2 / 400) %*% wt
#' muf <- function(x) 1 - 1 / 200 * x
#' marginal_utility(1:2, cbind(1:2, 1:1), uf)
#' marginal_utility(1:2, cbind(1:2, 1:1), muf = muf, wt = 1:2)
#'
#' ####
#' marginal_utility(
#'   1:2, cbind(1:2, 1:1),
#'   function(x, gamma = 1, p = rep(1, length(x))) CRRA(x, gamma, p)$CE
#' )
#' marginal_utility(1:2, cbind(1:2, 1:1), function(x) sqrt(prod(x)))
#'
#' gamma <- 0.8
#' wt <- c(0.25, 0.75)
#' marginal_utility(
#'   1:2, cbind(1:2, 1:1),
#'   function(x) CRRA(x, gamma = gamma, p = wt)$CE
#' )
#' ## the same as above. CRRA and CES utility funcitons are essentially the same.
#' es <- 1 / gamma
#' beta <- wt^es
#' marginal_utility(
#'   1:2, cbind(1:2, 1:1),
#'   function(x) CES(x = x, sigma = 1 - 1 / es, alpha = 1, beta = wt)
#' )
#'
#' prop.table(marginal_utility(
#'   1:2, cbind(1:2, 1:1),
#'   function(x) CRRA(x, gamma = gamma, p = wt)$CE
#' ))
#' prop.table(marginal_utility(
#'   1:2, cbind(1:2, 1:1),
#'   function(x) CRRA(x, gamma = gamma, p = wt)$u
#' ))
marginal_utility <- function(x, y, uf, price = NULL, delta = 1e-10,
                             muf = NULL, wt = rep(1, nrow(x))) {
  x <- as.matrix(x)
  y <- as.matrix(y)

  if (!is.null(price)) {
    y <- y %*% dg(1 / price)
  }

  m <- ncol(x)
  n <- ncol(y)
  result <- matrix(NA, n, m)


  if (!is.null(muf)) {
    if (is.function(muf)) {
      for (kc in 1:m) {
        for (kr in 1:n) {
          result[kr, kc] <- sum(muf(x[, kc]) * y[, kr] * wt)
        }
      }
    } else { # function list
      for (kc in 1:m) {
        for (kr in 1:n) {
          result[kr, kc] <- sum(muf[[kc]](x[, kc]) * y[, kr] * wt)
        }
      }
    }

    return(result)
  }


  if (is.function(uf)) {
    for (kc in 1:m) {
      for (kr in 1:n) {
        result[kr, kc] <- (uf(x[, kc] + delta * y[, kr]) - uf(x[, kc])) / delta
      }
    }
  } else { # function list
    for (kc in 1:m) {
      for (kr in 1:n) {
        result[kr, kc] <- (uf[[kc]](x[, kc] + delta * y[, kr]) - uf[[kc]](x[, kc])) / delta
      }
    }
  }

  result
}
