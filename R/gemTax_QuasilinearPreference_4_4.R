#' @export
#' @title A General Equilibrium Model with Tax and Quasilinear Utility Functions.
#' @aliases gemTax_QuasilinearPreference_4_4
#' @description This model is essentially a pure exchange economy.
#' The model contains 4 types of commodities (i.e. corn, iron, taxed iron and tax payment receipts) and 4 agents (i.e. consumer 1, consumer 2, a firm and the government).
#' Consumer 1 has corn and the utility function is x1 + beta1 * (alpha1 * x3 - 0.5 * x3^2) wherein x1 is corn and x3 is taxed iron.
#' Consumer 2 has iron and the utility function is x1 + beta2 * (alpha2 * x2 - 0.5 * x2^2) wherein x1 is corn and x2 is iron.
#' Consumer 1 (i.e. the iron demander) wants to buy iron from consumer 2 (i.e. the iron supplier) and the government will tax the transaction.
#' The firm (i.e. a tax agency) inputs iron and tax payment receipts (similar to tax stamp) to output taxed iron,
#' and due to government taxation requirements consumer 1 have to buy taxed iron from the firm and consumer 2 have to sell iron through the firm.
#' Government has tax payment receipts and the utility function is x1.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' tax.rate <- 1
#'
#' beta1 <- 0.05
#' alpha1 <- 3 / beta1 + 60
#'
#' iron.endowment <- 100
#' beta2 <- 0.05
#' alpha2 <- iron.endowment - 60 + (3 / beta2)
#'
#' ge <- sdm2(
#'   A = function(state) {
#'     a1 <- QL_demand(
#'       w = state$w[1],
#'       p = c(state$p[1], state$p[3]),
#'       alpha = alpha1, beta = beta1,
#'       type = "quadratic2"
#'     )
#'     a1 <- c(a1[1], 0, a1[2], 0)
#'
#'     a2 <- QL_demand(
#'       w = state$w[2],
#'       p = state$p[1:2],
#'       alpha = alpha2, beta = beta2,
#'       type = "quadratic2"
#'     )
#'     a2 <- c(a2, 0, 0)
#'
#'     a.firm <- c(0, 1, 0, tax.rate * state$p[2] / state$p[4])
#'
#'     a.gov <- c(1, 0, 0, 0)
#'
#'     cbind(a1, a2, a.firm, a.gov)
#'   },
#'   B = matrix(c(
#'     0, 0, 0, 0,
#'     0, 0, 0, 0,
#'     0, 0, 1, 0,
#'     0, 0, 0, 0
#'   ), 4, 4, TRUE),
#'   S0Exg = matrix(c(
#'     1000, NA, NA, NA,
#'     NA, iron.endowment, NA, NA,
#'     NA, NA, NA, NA,
#'     NA, NA, NA, 1
#'   ), 4, 4, TRUE),
#'   names.commodity = c("corn", "iron", "taxed.iron", "tax"),
#'   names.agent = c("consumer1", "consumer2", "firm", "gov"),
#'   numeraire = "corn",
#'   priceAdjustmentVelocity = 0.05
#' )
#'
#' ge$p
#' ge$D
#' ge$S
#' addmargins(ge$DV)
#' addmargins(ge$SV)
#'
#' ge.x <- ge$D[3, 1]
#' ge.pl <- ge$p[2]
#' ge.ph <- ge$p[3]
#' plot(function(x) (alpha1 - x) * beta1, 0, alpha1,
#'   xlim = c(0, 100), ylim = c(0, 6), xlab = "iron", ylab = "price"
#' )
#' curve((alpha2 - iron.endowment + x) * beta2, 0,
#'   alpha1,
#'   add = TRUE
#' )
#' grid()
#' points(ge.x, ge.ph, col = "red", pch = 20) # pch=8
#' points(ge.x, ge.pl, col = "red", pch = 20)
#'
#' polygon(c(0, ge.x, ge.x, 0), c(ge.ph, ge.ph, ge.pl, ge.pl))
#' segments(0, 3, x1 = 60, y1 = 3, col = "red")
#' text(c(0, ge.x, ge.x, 0) + 3, c(
#'   ge.ph + 0.3, ge.ph + 0.3,
#'   ge.pl - 0.3, ge.pl - 0.3
#' ), c("A", "B", "C", "D"))
#' text(c(3, ge.x + 3, 60), 3.3, c("E", "F", "G"))
#'
#' u.consumer1 <- function(x) x[1] + beta1 * (alpha1 * x[2] - 0.5 * x[2]^2)
#' u.consumer2 <- function(x) x[1] + beta2 * (alpha2 * x[2] - 0.5 * x[2]^2)
#'
#' u.consumer1(ge$D[c(1, 3), 1]) + u.consumer2(ge$D[c(1:2), 2]) + ge$z[4]
#' # The value above is 1430 when the tax rate is 0.
#' }

gemTax_QuasilinearPreference_4_4 <- function(...) sdm2(...)
