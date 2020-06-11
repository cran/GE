#' @export
#' @title Standard CES Demand Coefficient Matrix
#' @aliases SCES_A
#' @description This function computes the standard CES demand
#' coefficient matrix (i.e. Theta==Beta), which is a wrapper of CES_A of CGE package.
#' @param sigma a numeric m-vector or m-by-1 matrix. 1/(1-sigma) is the elasticity of substitution.
#' @param alpha a nonnegative numeric m-vector or m-by-1 matrix.
#' @param Beta a nonnegative numeric n-by-m matrix, where the sum of each column is equal to 1.
#' If a vector is provided, then it will be converted into a single-column matrix.
#' @param p a nonnegative numeric n-vector or n-by-1 matrix.
#' @param es a numeric m-vector or m-by-1 matrix of elasticity of substitution. If es is not NA, the value of sigma will be ignored.
#' @return A demand coefficient n-by-m matrix.
#' @examples
#' SCES_A(-1, 1, c(0.9, 0.1), c(1, 1))
#' SCES_A(alpha = 1, Beta = c(0.9, 0.1), p = c(1, 1), es = 0.5)
#' SCES_A(0, 1, c(0.9, 0.1), c(1, 1))
#' beta <- c(0.9, 0.1)
#' CD_A(prod(beta^-beta), c(0.9, 0.1), c(1, 1))
#'
#' ####
#' SCES_A(0, 1, c(0.9, 0.1, 0), c(1, 1, 1))
#'
#' ####
#' input <- matrix(c(
#'   200, 300, 100,
#'   150, 320, 530,
#'   250, 380, 0
#' ), 3, 3, TRUE)
#' Beta <- prop.table(input, 2)
#' SCES_A(sigma = rep(0, 3), alpha = c(1, 1, 1), Beta = Beta, p = c(1, 1, 1))
#' SCES_A(sigma = rep(-Inf, 3), alpha = c(1, 1, 1), Beta = Beta, p = c(1, 1, 1))


SCES_A <- function(sigma=1-1/es, alpha, Beta, p, es=NA) {
  if (all(!is.na(es))) sigma <- 1-1/es
  if (any(sigma >= 1)) stop("Li: sigma should be less than 1")
  if (is.vector(Beta)) Beta <- cbind(Beta)
  if (all(Beta != 0)) {
    result <- CES_A(sigma, alpha, Beta, p, Beta)
  } else
  if (all(sigma != 0) && all(sigma != -Inf)) {
    result <- CES_A(
      sigma, alpha,
      sweep(Beta, 2, 1 - sigma, "^"),
      p
    )
  } else {
    result <- c()
    for (k in seq_along(sigma)) {
      switch(as.character(sigma[k]),
        "0" = {
          tmp.result <- CD_A(alpha = prod(Beta[, k]^-Beta[, k]), Beta = Beta[, k], p = p)
        },
        "-Inf" = {
          tmp.result <- Beta[, k] / alpha
        },
        {
          tmp.result <- CES_A(
            sigma[k], alpha[k],
            Beta[, k]^(1 - sigma),
            p
          )
        }
      )
      result <- cbind(result, tmp.result)
    }
  }

  return(result)
}
