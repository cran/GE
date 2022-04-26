#' @export
#' @title Make a Countercyclical Product Tax Policy Function
#' @aliases makeCountercyclicalProductTax
#' @description  This function returns a countercyclical product tax policy function to accelerate convergence when calculating general equilibrium.
#' In some cases this tax policy with variable tax rates can stabilize the economy (see Li, 2019, section 9.4.5.4) .
#' When a firm's output is higher than the average output in previous periods, a tax is imposed on the firm to reduce the output of the product.
#' Tax revenue will be used for implicit public spending.
#' The way of taxation is to directly deduct a part of the supply of the firm.
#' @param agent a vector specifying the indices or names of firms to be taxed.
#' @param time.win the time window vector, i.e. a 2-vector specifying the start time and end time of policy implementation.
#' @param span a positive integer which indicates the number of periods when calculating the average output.
#' @return A countercyclical product tax policy function.
#' @seealso CGE::Example9.10.policy.tax
#' @examples
#' \donttest{
#' ge <- gemCanonicalDynamicMacroeconomic_4_3(
#'   numberOfPeriods = 1000
#' )
#'
#' ge <- gemCanonicalDynamicMacroeconomic_4_3(
#'   numberOfPeriods = 1000,
#'   policy = makeCountercyclicalProductTax(time.win = c(500, Inf))
#' )
#' }

makeCountercyclicalProductTax <- function(agent = 1, time.win = c(100, Inf), span = 50) {
  f <- function(time, state, state.history) {
    if (is.character(agent)) {
      agent.index <- match(agent, state$names.agent)
    } else {
      agent.index <- agent
    }

    if (time >= time.win[1] && time <= time.win[2]) {
      for (k in agent.index) {
        upsilon <- state.history$z[time - 1, k] / mean(state.history$z[(time - span):(time - 1), k])

        tau <- 0
        if (upsilon > 1) {
          tau <- min((upsilon - 1) / 2, 0.2)
          state$S[, k] <- state$S[, k] * (1 - tau)
        }
      }
    }
    state
  }


  return(f)
}
