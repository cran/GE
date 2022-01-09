#' @export
#' @title Make a Mean Value Policy Function
#' @aliases makePolicyMeanValue
#' @description  This function returns a mean value policy function with a given span to accelerate convergence when calculating general equilibrium.
#' We can observe the number of periods included in the economic cycle of the time series, and then set the number of periods as the parameter (i.e. span) of this function.
#' See \code{\link{policyMeanValue}}
#' @param span a positive integer. When the time index is an integer multiple of span,
#' the mean value policy sets the current prices and supplies to the averages of the previous span-1 periods.
#' @return A mean value policy function.
#' @seealso \code{\link{policyMeanValue}}
#' \code{\link{gemDualLinearProgramming}}.
#' @examples
#' \donttest{
#' ## See the function gemDualLinearProgramming.
#' A <- matrix(c(
#'   0, 0, 0, 1,
#'   8, 6, 1, 0,
#'   4, 2, 1.5, 0,
#'   2, 1.5, 0.5, 0
#' ), 4, 4, TRUE)
#' B <- matrix(c(
#'   60, 30, 20, 0,
#'   0, 0, 0, 0,
#'   0, 0, 0, 0,
#'   0, 0, 0, 0
#' ), 4, 4, TRUE)
#' S0Exg <- {
#'   S0Exg <- matrix(NA, 4, 4)
#'   S0Exg[2:4, 4] <- c(48, 20, 8)
#'   S0Exg
#' }
#'
#' ge <- sdm2(
#'   A = A, B = B, S0Exg = S0Exg,
#'   maxIteration = 1,
#'   numberOfPeriods = 1000,
#'   ts = TRUE
#' )
#' matplot(ge$ts.q, type = "l")
#'
#' ge2 <- sdm2(
#'   A = A, B = B, S0Exg = S0Exg,
#'   maxIteration = 1,
#'   numberOfPeriods = 1000,
#'   ts = TRUE,
#'   policy = makePolicyMeanValue(150)
#' )
#' matplot(ge2$ts.q, type = "l")
#' }

makePolicyMeanValue <- function(span = 200) {
  function(time, state, state.history) {
    if (time >= span && (time %% span == 0)) {
      state$p <- colMeans(state.history$p[(time - span + 1):(time - 1), ])
      state$S <- apply(state.history$S[, , (time - span + 1):(time - 1), drop = FALSE], c(1, 2), mean)
    }

    state
  }
}

