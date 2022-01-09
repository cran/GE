#' @export
#' @title Mean Value Policy Function
#' @aliases policyMeanValue
#' @description  When the time index is an integer multiple of 200, this policy sets the current prices and supplies to the averages of the previous 199 periods.
#' This policy function is mainly used as an argument of the function sdm2 in order to accelerate convergence when calculating general equilibrium.
#' @param time the current time.
#' @param state the current state.
#' @param state.history the state history, which is a list consisting of the time series of p, S, q, and z.
#' @return A list consisting of p, S and B which specify the prices, supplies and supply coefficient matrix after adjustment.
#' @seealso \code{\link{makePolicyMeanValue}} \code{\link{sdm2}}  \code{\link{gemDualLinearProgramming}}.

policyMeanValue <- function(time, state, state.history) {
  span <- 200
  if (time >= span && (time %% span == 0)) {
    state$p <- colMeans(state.history$p[(time - span + 1):(time - 1), ])
    state$S <- apply(state.history$S[, , (time - span + 1):(time - 1), drop = FALSE], c(1, 2), mean)
  }

  state
}
