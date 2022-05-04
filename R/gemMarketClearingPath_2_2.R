#' @export
#' @title Some Examples of Market Clearing Paths
#' @aliases gemMarketClearingPath_2_2
#' @description Some examples of market clearing paths containing a firm and a laborer (consumer).
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' dst.firm <- node_new(
#'   "prod",
#'   type = "CD", alpha = 5, beta = c(0.5, 0.5),
#'   "prod", "lab"
#' )
#'
#' dst.consumer <- node_new(
#'   "util",
#'   type = "Leontief", a = 1,
#'   "prod"
#' )
#'
#' dstl <- list(dst.firm, dst.consumer)
#'
#' f <- function(policy = NULL) {
#'   sdm2(
#'     A = dstl,
#'     B = matrix(c(
#'       1, 0,
#'       0, 0
#'     ), 2, 2, TRUE),
#'     S0Exg = matrix(c(
#'       NA, NA,
#'       NA, 1
#'     ), 2, 2, TRUE),
#'     names.commodity = c("prod", "lab"),
#'     names.agent = c("firm", "consumer"),
#'     numeraire = "lab",
#'     z0 = c(1, 1),
#'     ts = TRUE,
#'     policy = policy,
#'     numberOfPeriods = 40,
#'     maxIteration = 1
#'   )
#' }
#'
#' ge <- f(policy = policyMarketClearingPrice)
#' matplot(ge$ts.S[1, 1, ], type = "o", pch = 20)
#' matplot(ge$ts.z, type = "o", pch = 20)
#'
#' ## labor supply change
#' ge.LSC <- f(policy = list(
#'   function(time, state) {
#'     if (time >= 21) state$S[2, 2] <- state$S[2, 2] * 2
#'     state
#'   },
#'   policyMarketClearingPrice
#' ))
#'
#' matplot(ge.LSC$ts.z, type = "o", pch = 20)
#'
#' ## technology progress
#' ge.TP <- f(policy = list(
#'   makePolicyTechnologyChange(
#'     adjumentment.ratio = 2,
#'     agent = "firm",
#'     time.win = c(21, 21)
#'   ),
#'   policyMarketClearingPrice
#' ))
#'
#' matplot(ge.TP$ts.z, type = "o", pch = 20)
#'
#' ## the same as above
#' ge.TP2 <- f(policy = list(
#'   function(time, A) {
#'     if (time >= 21) {
#'       A[[1]]$alpha <- 10
#'     } else {
#'       A[[1]]$alpha <- 5
#'     }
#'   },
#'   policyMarketClearingPrice
#' ))
#'
#' matplot(ge.TP2$ts.z, type = "o", pch = 20)
#' }

gemMarketClearingPath_2_2 <- function(...) sdm2(...)