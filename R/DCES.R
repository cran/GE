#' @export
#' @title Displaced CES Utility Function and Displaced CES Demand Function
#' @aliases DCES
#' @aliases DCES_demand
#' @description The displaced CES utility function and the displaced CES demand function (Fullerton, 1989).
#' @describeIn DCES
#' Compute the displaced CES utility function (Fullerton, 1989),
#' e.g. (beta1 ^ (1 / es) * (x1 - xi1) ^ (1 - 1 / es) +
#' beta2 ^ (1 / es) * (x2 - xi2) ^ (1 - 1 / es)) ^ (es / (es - 1)
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
#'
#' #### A 2-by-2 general equilibrium model
#' #### with a DCES utility function.
#' ge <- sdm2(
#'   A = function(state) {
#'     a.consumer <- DCES_demand(
#'       es = 2, beta = c(0.2, 0.8), xi = c(1000, 500),
#'       w = state$w[1], p = state$p
#'     )
#'     a.firm <- c(1.1, 0)
#'     cbind(a.consumer, a.firm)
#'   },
#'   B = diag(c(0, 1)),
#'   S0Exg = matrix(c(
#'     3500, NA,
#'     NA, NA
#'   ), 2, 2, TRUE),
#'   names.commodity = c("corn", "iron"),
#'   names.agent = c("consumer", "firm"),
#'   numeraire = "corn"
#' )
#'
#' ge$p
#' ge$z
#' ge$A
#' ge$D
#'
#' #### A 3-by-3 general equilibrium model
#' #### with a DCES utility function.
#' lab <- 1 # the amount of labor supplied by each laborer
#' n.laborer <- 100 # the number of laborers
#' ge <- sdm2(
#'   A = function(state) {
#'     a.firm.corn <- CD_A(alpha = 1, Beta = c(0, 0.5, 0.5), state$p)
#'     a.firm.iron <- CD_A(alpha = 5, Beta = c(0, 0.5, 0.5), state$p)
#'     a.laborer <- DCES_demand(
#'       es = 0, beta = c(0, 1, 0), xi = c(0.1, 0, 0),
#'       w = state$w[3] / n.laborer, p = state$p
#'     )
#'
#'     cbind(a.firm.corn, a.firm.iron, a.laborer)
#'   },
#'   B = matrix(c(
#'     1, 0, 0,
#'     0, 1, 0,
#'     0, 0, 0
#'   ), 3, 3, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA, NA,
#'     NA, NA, NA,
#'     NA, NA, lab * n.laborer
#'   ), 3, 3, TRUE),
#'   names.commodity = c("corn", "iron", "lab"),
#'   names.agent = c("firm.corn", "firm.iron", "laborer"),
#'   numeraire = "lab",
#'   priceAdjustmentVelocity = 0.1
#' )
#'
#' ge$z
#' ge$A
#' ge$D
#' }

DCES <- function(es, beta, xi, x) {
  if (!isTRUE(all.equal(sum(beta), 1))) warning("The sum of beta should be 1.")

  if (any((x - xi) < 0)) {
    warning("Li: Some element of x is too small.")
    return(NaN)
  }

  if (es == 1) {
    return(prod(((x - xi) / beta)^beta))
  } else {
    return(sum(beta^(1 / es) * (x - xi)^(1 - 1 / es))^(es / (es - 1)))
  }
}

#' @export
#' @describeIn DCES
#' The displaced CES demand function (Fullerton, 1989).
DCES_demand <- function(es, beta, xi, w, p) {
  if (!isTRUE(all.equal(sum(beta), 1))) warning("The sum of beta should be 1.")

  ID <- w - sum(p * xi)
  if (ID < 0) {
    warning("Li: w is too small.")
    return(beta * 0)
  }

  xi + beta * ID / p^es / sum(beta * p^(1 - es))
}

#' @export
#' @describeIn DCES
#' The displaced CES compensated demand function (Fullerton, 1989).
DCES_compensated_demand <- function(es, beta, xi, u, p) {
  if (!isTRUE(all.equal(sum(beta), 1))) warning("The sum of beta should be 1.")
  xi + u * SCES_A(sigma = 1 - 1 / es, alpha = 1, Beta = beta, p = p)
}

#' @export
#' @describeIn DCES
#' The displaced CES indirect utility function (Fullerton, 1989).
DCES_indirect <- function(es, beta, xi, w, p) {
  if (!isTRUE(all.equal(sum(beta), 1))) warning("The sum of beta should be 1.")

  ID <- w - sum(p * xi)
  if (ID < 0) {
    warning("Li: w is too small.")
    return(NaN)
  }

  if (es == 1) {
    p_bar <- prod(p^beta)
  } else {
    p_bar <- sum(beta * p^(1 - es))^(1 / (1 - es))
  }

  ID / p_bar
}
