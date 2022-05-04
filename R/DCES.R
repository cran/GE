#' @export
#' @title Displaced CES Utility Function and Displaced CES Demand Function
#' @aliases DCES
#' @aliases DCES_demand
#' @description Compute the displaced CES utility function and the displaced CES demand function (Fullerton, 1989).
#' @describeIn DCES
#' Compute the displaced CES utility function (Fullerton, 1989),
#' e.g. (beta_1 ^ (1 / es) * (x_1 - xi_1) ^ (1 - 1 / es) +
#' beta_2 ^ (1 / es) * (x_2 - xi_2) ^ (1 - 1 / es)) ^ (es / (es - 1)
#' wherein beta1 + beta2 == 1.
#'
#' When es==1, the DCES utility function becomes the Stone-Geary utility function.
#' @param es the elasticity of substitution.
#' @param beta a n-vector consisting of the marginal expenditure share coefficients.
#' @param xi a n-vector. Each element of xi parameterizes whether
#' the particular good is a necessity for the household (Acemoglu, 2009, page 152).
#' For example, xi[i] > 0 may mean that the household needs to consume at least a certain amount of good i to survive.
#' @param x a n-vector consisting of the inputs.
#' @param w a scalar indicating the income.
#' @param p a n-vector indicating the prices.
#' @param u a scalar indicating the utility level.
#' @references Acemoglu, D. (2009, ISBN: 9780691132921) Introduction to Modern Economic Growth. Princeton University Press.
#' @references Fullerton, D. (1989) Notes on Displaced CES Functional Forms. Available at: https://works.bepress.com/don_fullerton/39/
#' @examples
#' \donttest{
#' es <- 0.99
#' beta <- prop.table(1:5)
#' xi <- 0
#' w <- 500
#' p <- 2:6
#'
#' x <- DCES_demand (
#'   es = es,
#'   beta = beta,
#'   xi = xi,
#'   w = w,
#'   p = p
#' )
#'
#' u <- DCES(es = es,
#'           beta = beta,
#'           xi = xi,
#'           x = x)
#'
#' SCES(es = es,
#'      alpha = 1,
#'      beta = beta,
#'      x = x)
#'
#' DCES_compensated_demand(
#'   es = es,
#'   beta = beta,
#'   xi = xi,
#'   u = u,
#'   p = p
#' )
#'
#' DCES_compensated_demand(
#'   es = es,
#'   beta = beta,
#'   xi = seq(10, 50, 10),
#'   u = u,
#'   p = p
#' )
#' }

DCES <- function(es, beta, xi, x) {
  if (!isTRUE(all.equal(sum(beta),1))) warning("The sum of beta should be 1.")
  if (es == 1)
    return(prod(((x - xi) / beta) ^ beta))
  else
    return(sum(beta ^ (1 / es) * (x - xi) ^ (1 - 1 / es)) ^ (es / (es - 1)))
}

#' @export
#' @describeIn DCES
#' Compute the displaced CES demand (Fullerton, 1989).
DCES_demand <- function(es, beta, xi, w, p) {
  if (!isTRUE(all.equal(sum(beta),1))) warning("The sum of beta should be 1.")
  (xi + beta * (w - sum(p * xi)) / p ^ es / sum(beta * p ^ (1 - es)))
}

#' @export
#' @describeIn DCES
#' Compute the displaced CES compensated demand (Fullerton, 1989).
DCES_compensated_demand <- function(es, beta, xi, u, p) {
  if (!isTRUE(all.equal(sum(beta),1))) warning("The sum of beta should be 1.")
  xi+u*SCES_A(sigma=1-1/es, alpha = 1, Beta = beta, p=p)
}

