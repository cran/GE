#' @export
#' @title Convert between (Primitive) Period Interest Rates and (Primitive) Intraperiod Interest Rates
#' @aliases convert_ir
#' @description This function converts between (primitive) period interest rates and (primitive) intraperiod interest rates.
#' Here, a period is considered as the shortest term for monetary lending.
#'
#' The primitive interest rate is defined as the interest rate when the money stock is adjusted to be constant under the assumption of monetary neutrality.
#'
#' In the structural dynamic model, the period interest rate refers to the interest rate at which principal and interest are repaid
#' at the beginning of the next period after borrowing money in the current period,
#' while the intraperiod interest rate refers to the interest rate at which the principal and interest are repaid
#' during the current period after borrowing money in the current period.
#'
#' When the velocity of money is equal to one, these two types of interest rates are the same.
#' When the velocity of money, namely vm, is greater than one, the intraperiod interest will be repaid in vm installments within the period,
#' and there is usually a difference between the two types of interest rates.
#' @param ir a vector consisting of period interest rates or intraperiod interest rates.
#' @param vm a scalar, or a vector consisting of velocities of money.
#' The velocity of money in each period is usually a positive integer.
#' @param to type of conversion. Can be abbreviated.
#' @return A vector consisting of intraperiod interest rates or period interest rates.
#' @examples
#' \donttest{
#' ir <- seq(0, 1, 0.1)
#' plot(ir, convert_ir(ir, 2, "period"), "b")
#' plot(ir, convert_ir(ir, 2, "intraperiod"), "b")
#' }

convert_ir <- function(ir, vm, to = c("period", "intraperiod")) {
  to <- match.arg(to)
  switch(to,
    period = ir / (1 + ir / vm - ir),
    intraperiod = 1 / (1 / ir + 1 - 1 / vm)
  )
}

