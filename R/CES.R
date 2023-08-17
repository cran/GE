#' @export
#' @title CES Function
#' @aliases CES
#' @description CES function, e.g. alpha * (beta1 * (x1 / theta1)^sigma + beta2 * (x2 / theta2)^sigma)^(1 / sigma).
#' @param sigma a scalar not greater than one.
#' @param alpha a nonnegative scalar.
#' @param beta a nonnegative n-vector.
#' @param x a nonnegative n-vector consisting of the inputs.
#' @param theta the all-ones n-vector (default) or a positive n-vector.
#' @param es the elasticity of substitution. If es is not NA, the value of sigma (i.e. 1 - 1 / es) will be ignored.
#' @return A scalar indicating the output or utility level.
#' @examples
#' \donttest{
#' CES(0.5, 1, c(0.4, 0.6), c(1, 1), c(0.4, 0.6))
#' CES(0.5, 1, c(0.4, 0.6), c(1, 2))
#' }
#'
CES <- function(sigma = 1 - 1 / es, alpha, beta, x, theta = rep(1, length(beta)), es = NA) {
  if (all(beta == 0)) {
    return(0)
  }

  tmp.logical <- beta > 0
  beta <- beta[tmp.logical]
  x <- x[tmp.logical]
  theta <- theta[tmp.logical]

  if (!is.na(es)) sigma <- 1 - 1 / es

  if ((sigma < 0) && any(is.infinite(beta / x))) {
    return(0)
  }

  if (sigma == 0) {
    return(alpha * prod((x / theta)^beta))
  }

  if (sigma == -Inf) {
    return(alpha * min(x / theta))
  }

  return(alpha * sum(beta * (x / theta)^sigma)^(1 / sigma))
}
