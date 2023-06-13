#' @export
#' @title Quasilinear Demand Functions
#' @aliases QL_demand
#' @description Some quasilinear demand functions. The corresponding utility functions are as follows: \cr
#'   power: x1 + alpha * x2^beta, wherein alpha>0, 0<beta<1 \cr
#'   log: x1 + alpha * log(x2), wherein alpha>0 \cr
#'   quadratic1: x1 + alpha * x2 - 0.5 * beta * x2^2, wherein alpha>0, beta>0 \cr
#'   quadratic2: x1 + beta * (alpha * x2 - 0.5 * x2^2), wherein alpha>0, beta>0 \cr
#'   min: x1 + alpha * min(x2, beta)
#' @param w a scalar indicating the income.
#' @param p a 2-vector indicating the prices.
#' @param alpha a scalar.
#' @param beta a scalar.
#' @param type a character string specifying the type of the function. The default type is "power".
#' Other possible values are "log", "quadratic1", "quadratic2" and "min".
#' @return A 2-by-1 matrix indicating demands.
#' @examples
#' \donttest{
#' QL_demand(w = 0.5, p = c(1, 1), alpha = 1, type = "log")
#' QL_demand(w = 2, p = c(1, 1), alpha = 1, type = "log")
#'
#' QL_demand(w = 1, p = c(1, 5), alpha = 2, beta = 0.5)
#' }

QL_demand <- function(w, p, alpha, beta,
                      type = c("power", "log", "quadratic1", "quadratic2", "min")) {
  d <- rbind(0, 0)
  w <- w / p[1]
  p2 <- p[2] / p[1]
  switch(type[1],
    "power" = {
      d[2] <- (p2 / alpha / beta)^(1 / (beta - 1))
    },
    "log" = {
      d[2] <- alpha / p2
    },
    "quadratic1" = {
      d[2] <- max((alpha - p2) / beta, 0)
    },
    "quadratic2" = {
      d[2] <- max(alpha - p2 / beta, 0)
    },
    "min" = {
      if (p2 <= alpha) {
        d[2] <- beta
      } else {
        d[2] <- 0
      }
    },
    stop("Wrong type.")
  )

  if (d[2] * p2 > w) {
    d[2] <- w / p2
    d[1] <- 0
  } else {
    d[1] <- w - d[2] * p2
  }

  return(d)
}
