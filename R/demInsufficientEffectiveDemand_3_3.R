#' @export
#' @title A Disequilibrium Model Illustrating Insufficient Effective Demand (Supply-demand Structural Mismatch)
#' @aliases demInsufficientEffectiveDemand_3_3
#' @description  A disequilibrium model illustrating supply-demand structural mismatch and insufficient effective demand.
#' Assume that from the 5th period to the 30th period, the producer expects the sales rate of products to decline,
#' so he reduces investment in production and increases the demand for value storage means (such as foreign assets, gold, etc.);
#' the laborer expects the unemployment rate to rise, so he reduces consumption and increases the demand for value storage means.
#'
#' Here the supplier of value storage means is referred to as ROW (the rest of the world).
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' dst.firm <- node_new("firm",
#'   type = "SCES", es = 1,
#'   alpha = 1, beta = c(1, 0),
#'   "production", "store of value"
#' )
#' node_set(dst.firm, "production",
#'   type = "SCES", alpha = 1,
#'   beta = c(0.5, 0.5), es = 0.5,
#'   "prod", "lab"
#' )
#'
#' dst.laborer <- node_new("util",
#'   type = "SCES", es = 1,
#'   alpha = 1, beta = c(1, 0),
#'   "prod", "store of value"
#' )
#'
#' dst.ROW <- node_new("util",
#'   type = "Leontief", a = 1,
#'   "prod"
#' )
#'
#' policy.demand <- function(time, A, state) {
#'   if ((time >= 5 && time <= 30) || (time >= 100)) {
#'     A[[1]]$beta <- c(0.9, 0.1)
#'     A[[2]]$beta <- c(0.9, 0.1)
#'   }
#'
#'   if ((time > 30) && (time < 100)) {
#'     A[[1]]$beta <- c(1, 0)
#'     A[[2]]$beta <- c(1, 0)
#'   }
#'
#'   state
#' }
#'
#' ge <- sdm2(
#'   A = list(dst.firm, dst.laborer, dst.ROW),
#'   B = matrix(c(
#'     1, 0, 0,
#'     0, 0, 0,
#'     0, 0, 0
#'   ), 3, 3, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA, NA,
#'     NA, 100, NA,
#'     NA, NA, 10
#'   ), 3, 3, TRUE),
#'   names.commodity = c("prod", "lab", "store of value"),
#'   names.agent = c("firm", "laborer", "ROW"),
#'   ts = TRUE,
#'   policy = policy.demand,
#'   numberOfPeriods = 50,
#'   maxIteration = 1,
#'   numeraire = "store of value",
#'   z0 = c(200, 0, 0),
#'   depreciationCoef = 1,
#'   priceAdjustmentVelocity = 0.05
#' )
#'
#' matplot(ge$ts.z, type = "o", pch = 20)
#' matplot(ge$ts.p, type = "o", pch = 20)
#' matplot(ge$ts.q, type = "o", pch = 20)
#' ge$ts.p
#'
#' #### the relationship between the proportion of demand for value
#' ## storage means and the sales rate under a fixed price vector.
#' p <- c(1, 1, 1)
#' result <- c()
#' for (xi in seq(0, 1, 0.01)) {
#'   A <- matrix(c(
#'     (1 - xi) * 0.5, 1 - xi, 1 - xi,
#'     (1 - xi) * 0.5, 0, 0,
#'     xi, xi, xi
#'   ), 3, 3, TRUE)
#'
#'   # Assume that only the producer has pessimistic expectations.
#'   # A <- matrix(c(
#'   #   (1 - xi) * 0.5, 1, 1,
#'   #   (1 - xi) * 0.5, 0, 0,
#'   #   xi, 0, 0
#'   # ), 3, 3, TRUE)
#'   #
#'   # Assume that only the laborer has pessimistic expectations.
#'   # A <- matrix(c(
#'   #   0.5, 1 - xi, 1 - xi,
#'   #   0.5, 0, 0,
#'   #   0, xi, xi
#'   # ), 3, 3, TRUE)
#'
#'
#'   S <- matrix(c(
#'     200, 0, 0,
#'     0, 100, 0,
#'     0, 0, 10
#'   ), 3, 3, TRUE)
#'
#'   tmp <- F_Z(A, p, S)
#'   result <- rbind(result, c(xi, tmp$q[1:2]))
#' }
#'
#' matplot(result[, 1], result[, 2:3],
#'   type = "o", pch = 20,
#'   xlab = "proportion of demand for value storage means", ylab = "sales rates of prod and lab"
#' )
#' }

demInsufficientEffectiveDemand_3_3 <- function(...) sdm2(...)
