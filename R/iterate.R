#' @import stats
#' @export

#' @title Iteration Function
#' @aliases iterate
#' @description Iteration function
#'
#' @param x the initial state vector.
#' If x has a name attribute, the names will be used to label the output matrix.
#' @param f a user-supplied function that computes the values of the next time.
#' @param times the iteration times.
#' @param tol the tolerance for stopping calculation. If the canberra distance of the last two
#' state vectors is less than tol the calculation will stop.
#' @param ... optional arguments passed to the f function.
#' @return A matrix consisting of state vectors.
#' @examples
#' \donttest{
#' x <- c(1, 2)
#' f <- function(x, a) prop.table(c(sum(x), a * prod(x)^(1 / 2)))
#' iterate(x, f, 100, a = 3)
#' iterate(x, f, 100, tol = 1e-5, a = 3)
#'
#' #### Heron's method for finding square roots
#' x <- 1
#' f <- function(x, n) (x + n / x) / 2
#' iterate(x, f, 10, n = 5)
#'
#' #### Find a root of the equation x^3-x-1==0.
#' x <- 1.5
#' f <- function(x) (x + 1)^(1 / 3)
#' iterate(x, f, 10)
#'
#' ####
#' x <- c(1, 2, 3)
#' f <- function(x) {
#'   n <- length(x)
#'   sigma <- seq(-1, 1, length.out = n)
#'   result <- rep(NA, n)
#'   for (k in 1:n) result[k] <- CES(sigma[k], 1, rep(1 / n, n), x, rep(1 / n, n))
#'   prop.table(result)
#' }
#' iterate(x, f, 100)
#' }

iterate <- function(x, f, times = 100, tol = NA, ...) {
  n.periods <- times + 1
  result <- matrix(NA, n.periods, length(x))
  result[1, ] <- x

  if (is.na(tol)) {
    if (...length() == 0) {
      for (k in 2:n.periods) result[k, ] <- f(result[k - 1, ])
    } else {
      for (k in 2:n.periods) result[k, ] <- f(result[k - 1, ], ...)
    }
  } else {
    if (...length() == 0) {
      for (k in 2:n.periods) {
        result[k, ] <- f(result[k - 1, ])
        if (dist(result[(k - 1):k, ], method = "canberra") < tol) {
          result <- result[1:k, ]
          break
        }
      }
    } else {
      for (k in 2:n.periods) {
        result[k, ] <- f(result[k - 1, ], ...)
        if (dist(result[(k - 1):k, ], method = "canberra") < tol) {
          result <- result[1:k, ]
          break
        }
      }
    }
  }


  if (!is.null(names(x))) colnames(result) <- names(x)

  result
}
