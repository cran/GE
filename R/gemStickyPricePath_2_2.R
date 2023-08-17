#' @export
#' @title An Example Illustrating the Sticky-Price Path
#' @aliases gemStickyPricePath_2_2
#' @description This is an example that illustrates the sticky-price path.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' stickiness <- 0.7
#'
#' dst.firm <- node_new(
#'   "output",
#'   type = "CD", alpha = 2, beta = c(0.5, 0.5),
#'   "prod", "lab"
#' )
#'
#' dst.consumer <- node_new(
#'   "util",
#'   type = "Leontief", a = 1,
#'   "prod"
#' )
#'
#' ge <- sdm2(
#'   A = list(dst.firm, dst.consumer),
#'   B = matrix(c(
#'     1, 0,
#'     0, 1
#'   ), 2, 2, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA,
#'     NA, 100
#'   ), 2, 2, TRUE),
#'   names.commodity = c("prod", "lab"),
#'   names.agent = c("firm", "consumer"),
#'   numeraire = "lab",
#'   z0 = c(100, 100),
#'   p0 = c(1, 1),
#'   ts = TRUE,
#'   policy = list(
#'     makePolicyTechnologyChange(
#'       adjumentment.ratio = 2,
#'       agent = "firm",
#'       time.win = c(30, 30)
#'     ),
#'     makePolicyStickyPrice(stickiness = stickiness)
#'   ),
#'   priceAdjustmentVelocity = 0,
#'   numberOfPeriods = 60,
#'   maxIteration = 1
#' )
#'
#' matplot(ge$ts.z, type = "b", pch = 20)
#' }

gemStickyPricePath_2_2 <- function(...) sdm2(...)
