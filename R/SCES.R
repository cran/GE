#' @export
#' @title Standard CES Function
#' @aliases SCES
#' @description Standard CES function, e.g. alpha * (beta1 * (x1 / beta1)^sigma + beta2 * (x2 / beta2)^sigma)^(1 / sigma)
#' wherein beta1 + beta2 == 1.
#'
#' @param sigma the sigma coefficient.
#' @param alpha the alpha coefficient.
#' @param beta a vector consisting of the beta coefficients.
#' @param x a vector consisting of the inputs.
#' @param es the elasticity of substitution. If es is not NA, the value of sigma will be ignored.
#' @return The output or utility level.
#' @examples
#' \donttest{
#' beta <- c(0.6, 0.4)
#' SCES(alpha = 1, beta = beta, x = beta, es = 0.5)
#' }
#'
SCES <- function(sigma = 1 - 1 / es, alpha, beta, x, es = NA) {
  if (!is.na(es)) sigma <- 1 - 1 / es
  CES(sigma, alpha, beta, x, theta = beta)
}
