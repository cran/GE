#' @export

#' @title CES Function
#' @aliases CES
#' @description CES function, e.g. alpha * (beta1 * (x1 / theta1)^sigma + beta2 * (x2 / theta2)^sigma)^(1 / sigma).
#'
#' @param sigma the sigma coefficient.
#' @param alpha the alpha coefficient.
#' @param beta a vector consisting of the beta coefficients.
#' @param x a vector consisting of the inputs.
#' @param theta a vector consisting of the theta coefficients.
#' @param es the elasticity of substitution. If es is not NA, the value of sigma will be ignored.
#' @return The output or utility level.
#' @examples
#' \donttest{
#' CES(1, 1, c(0.4, 0.6), c(1, 1), c(0.4, 0.6))
#' }
#'
CES <- function(sigma = 1 - 1 / es, alpha, beta, x, theta = NULL, es = NA) {
  if (!is.na(es)) sigma <- 1 - 1 / es

  if (length(sigma) != length(alpha)) {
    message("Li: length(sigma)!=length(alpha)")
  }

  if (sigma == 0) {
    if (is.null(theta)) {
      return(alpha * prod(x^beta))
    } else {
      return(alpha * prod((x / theta)^beta))
    }
  }

  if (sigma == -Inf) {
    if (is.null(theta)) {
      return(alpha * min(x))
    } else {
      return(alpha * min(x / theta))
    }
  }

  if (is.null(theta)) {
    if (any(x^sigma == Inf)) stop("Li: Inf. Failed")
    return(alpha * sum(beta * x^sigma)^(1 / sigma))
  } else {
    if (any(((x / theta)^sigma) == Inf)) stop("Li: Inf. Failed")
    return(alpha * sum(beta * (x / theta)^sigma)^(1 / sigma))
  }
}
