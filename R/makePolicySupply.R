#' @export
#' @title Make a Policy of Supply
#' @aliases makePolicySupply
#' @description Given a supply matrix and a time window vector, this function returns a policy function that sets the supply during this time window.
#' By default, the time window of this function is c(1, 1), which means that this function will set the initial supply.
#' @param S a supply matrix.
#' @param time.win the time window vector, i.e. a 2-vector specifying the start time and end time of policy implementation.
#' @return A policy function, which is often used as an argument of the function sdm2.
#' @seealso \code{\link{sdm2}}
#' @examples
#' \donttest{
#' InitialEndowments <- {
#'   tmp <- matrix(0, 3, 2)
#'   tmp[1, 1] <- 0.01
#'   tmp[2, 2] <- tmp[3, 2] <- 1
#'   tmp
#' }
#'
#' ge <- gemCanonicalDynamicMacroeconomic_3_2(
#'   policy.supply = makePolicySupply(InitialEndowments),
#'   policy.price = policyMarketClearingPrice,
#'   ts = TRUE,
#'   maxIteration = 1,
#'   numberOfPeriods = 50
#' )
#'
#' par(mfrow = c(1, 2))
#' matplot(ge$ts.z, type = "b", pch = 20)
#' matplot(ge$ts.p, type = "b", pch = 20)
#'
#' }

makePolicySupply <- function(S, time.win=c(1,1)){
  policy.supply <- function(time, state) {
    if (time >= time.win[1] && time <= time.win[2] ) {
      state$S <- S
    }
    state
  }
}



