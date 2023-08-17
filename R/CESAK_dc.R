#' @export
#' @title Demand coefficients of the CESAK Production Function
#' @aliases CESAK_dc
#' @description Computing the demand coefficients of the CESAK production function
#' alpha * (betaK * x1^((es - 1) / es) + (1 - betaK) * x2^((es - 1) / es))^(es / (es - 1)) + alphaK * x1.
#' When es==1, the CESAK production function becomes the CDAK production function alpha * x1^betaK * x2^(1 - betaK) + alphaK * x1.
#' @param alpha a positive scalar.
#' @param betaK a scalar between 0 and 1.
#' @param es a nonnegative scalar specifying the elasticity of substitution.
#' @param alphaK a nonnegative scalar.
#' @param p a 2-vector indicating the prices.
#' @return A 2-vector indicating the demand coefficients.
#' @examples
#' \donttest{
#' CESAK_dc(alpha = 1, betaK = 0.35, alphaK = 1 - 0.06, p = c(10, 1))
#' CESAK_dc(alpha = 1, betaK = 0.35, alphaK = 1 - 0.06, p = c(1, 10))
#' CESAK_dc(alpha = 1, betaK = 0.35, alphaK = 2, p = c(1, 1))
#'
#' CESAK_dc(alpha = 1, betaK = 0.35, alphaK = 1 - 0.06, p = c(1, 1), es = 0.5)
#' CESAK_dc(alpha = 1, betaK = 0.35, alphaK = 0.1, p = c(1, 10), es = 0.5)
#' CESAK_dc(alpha = 1, betaK = 0.35, alphaK = 1.5, p = c(1, 10), es = 0.5)
#' }
#'
CESAK_dc <- function(alpha, betaK, alphaK, p, es = 1) {
  sigma <- 1 - 1 / es

  if (alphaK == 0) {
    return(c(CES_A(sigma, alpha, Beta = c(betaK, 1 - betaK), p)))
  }

  f.output <- function(x) {
    CES(sigma = sigma, alpha = alpha, beta = c(betaK, 1 - betaK), x = x) + alphaK * x[1]
  }

  ratio.input.2to1 <- function(pr) {
    f <- function(ratio.2to1) {
      if (sigma == 0) {
        result <- pr - (betaK * ratio.2to1 + alphaK * ratio.2to1^betaK / alpha) / (1 - betaK)
      } else {
        result <- pr * (1 / betaK - 1) - ratio.2to1^(1 - sigma) -
          alphaK * (alpha * betaK)^-1 *
            (betaK * ratio.2to1^-sigma + 1 - betaK)^(1 - 1 / sigma)
      }

      return(result)
    }

    uniroot(f, interval = c(1e-300, (pr * (1 / betaK - 1) + 1)^(1 / (1 - sigma))), tol = 1e-6)$root
  }

  pr <- p[1] / p[2]
  if (sigma == 0 || pr > alphaK * alpha^(-1) * (1 - betaK)^(-1 / sigma)) {
    r <- ratio.input.2to1(pr)
    return(c(1, r) / f.output(c(1, r)))
  } else {
    return(c(1 / alphaK, 0))
  }
}
