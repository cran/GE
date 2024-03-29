#' @export
#' @title Some Examples of Instantaneous Equilibrium Paths with Sticky Decisions
#' @aliases gemInstantaneousEquilibriumPath_StickyDecisions
#' @description Some examples of instantaneous equilibrium paths with sticky decisions of a firm, that is,
#' the firm sluggishly adjusts its technology in response to price changes.
#'
#' Under the assumption of (complete) rationality, economic agents will make decisions that are most beneficial to them based on the information they have.
#' If the information does not change, then the decision will not change.
#' However, under the assumption of bounded rationality, the decisions made by economic agents may not be optimal.
#' They may follow some simple rules-of-thumb, and might adjust their previous decisions sluggishly according to the changes in information,
#' even though they have the capability to adjust flexibly,
#' so that the new decisions are better than the old ones under the new information.
#' Hence the current decision is not necessarily the optimal decision.
#' Even if the information does not change, it is still possible for agents to make further improvements to this decision in the next period.
#' It can also be said that in this case, the decision maker's decision is sticky, that is, it only makes limited improvements to the previous decision based on new information,
#' rather than directly adjusting to the optimal decision.
#' @param ... arguments to be passed to the function sdm2.
#' @seealso {
#' \code{\link{policyMarketClearingPrice}}
#' }
#' @examples
#' \donttest{
#' f <- function(stickiness.firm = 0) {
#'   dst.firm <- node_new("output",
#'     type = "Leontief", a = c(1 - stickiness.firm, stickiness.firm),
#'     "cc1", "cc2"
#'   )
#'   node_set(dst.firm, "cc1",
#'     type = "CD", alpha = 5,
#'     beta = c(0.5, 0.5),
#'     "prod", "lab"
#'   )
#'   node_set(dst.firm, "cc2",
#'     type = "CD", alpha = 5,
#'     beta = c(0.5, 0.5),
#'     "prod", "lab"
#'   )
#'
#'   dst.consumer <- node_new("utility",
#'     type = "CD", alpha = 1,
#'     beta = c(0.5, 0.5),
#'     "prod", "lab"
#'   )
#'
#'   ge <- sdm2(
#'     A = list(dst.firm, dst.consumer),
#'     B = diag(c(1, 0)),
#'     S0Exg = {
#'       S0Exg <- matrix(NA, 2, 2)
#'       S0Exg[2, 2] <- 100
#'       S0Exg
#'     },
#'     names.commodity = c("prod", "lab"),
#'     names.agent = c("firm", "consumer"),
#'     numeraire = "lab",
#'     maxIteration = 1,
#'     numberOfPeriods = 20,
#'     policy = list(
#'       function(time, A, state) {
#'         if (time > 1) {
#'           node_set(A[[1]], "cc2",
#'             type = "Leontief", a = state$last.A[, 1]
#'           )
#'         }
#'       },
#'       policyMarketClearingPrice
#'     ),
#'     ts = TRUE
#'   )
#'
#'   print(ge$p)
#'   print(ge$z)
#'   par(mfrow = c(1, 2))
#'   matplot(ge$ts.p, type = "l")
#'   matplot(ge$ts.z, type = "l")
#' }
#'
#' f()
#' f(stickiness.firm = 0.8)
#'
#' }

gemInstantaneousEquilibriumPath_StickyDecisions <- function(...) sdm2(...)
