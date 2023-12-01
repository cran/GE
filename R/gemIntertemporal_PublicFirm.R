#' @export
#' @title Some Examples of Intertemporal (Timeline) Models with Production and Public Firms
#' @aliases gemIntertemporal_PublicFirm
#' @description Some examples of intertemporal (timeline) models with production and public firms.
#' A public producer is akin to a consumer with an infinite lifespan. The public producer owns the products it manufactures.
#' In each period, it exchanges the products it has produced for the inputs required for production.
#' In intertemporal models, a public producer can be treated as multiple public firms that each only produces for a single period.
#' Each public firm hands over its products to the public firm of the next period, which in turn uses these products for trading.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' np <- 15 # the number of economic periods, firms.
#' gr <- 0  # the growth rate of the labor supply
#' eis <- 0.5 # the elasticity of intertemporal substitution
#' rho.beta <- 0.9 # the subjective discount factor
#' y1 <- 100 # the initial product supply
#'
#' n <- 2 * np # the number of commodity kinds
#' m <- np + 1 # the number of agent kinds
#'
#' names.commodity <- c(paste0("prod", 1:np), paste0("lab", 1:np))
#' names.agent <- c(paste0("firm", 1:np), "consumer")
#'
#' # the exogenous supply matrix.
#' S0Exg <- matrix(NA, n, m, dimnames = list(names.commodity, names.agent))
#' # the supply of labor.
#' S0Exg[paste0("lab", 1:np), "consumer"] <- 100 * (1 + gr)^(0:(np - 1))
#' S0Exg["prod1", "firm1"] <- y1
#'
#' # the output coefficient matrix.
#' B <- matrix(0, n, m, dimnames = list(names.commodity, names.agent))
#' for (k in 1:(np - 1)) {
#'   B[paste0("prod", k + 1), paste0("firm", k)] <- 1
#' }
#'
#' dstl.firm <- list()
#' for (k in 1:np) {
#'   dstl.firm[[k]] <- node_new(
#'     "prod",
#'     type = "CD", alpha = 2, beta = c(0.5, 0.5),
#'     paste0("lab", k), paste0("prod", k)
#'   )
#' }
#'
#' dst.consumer <- node_new(
#'   "util",
#'   type = "CES", es = eis,
#'   alpha = 1, beta = prop.table(rho.beta^(1:np)),
#'   paste0("prod", 1:np)
#' )
#'
#' policy.PublicFirm <- function(state) {
#'   for (k in 1:(np - 1)) {
#'     state$S[k + 1, k + 1] <- state$S[k + 1, k]
#'     state$S[k + 1, k] <- 0
#'   }
#'   state
#' }
#'
#' ge <- sdm2(
#'   A = c(dstl.firm, dst.consumer),
#'   B = B,
#'   S0Exg = S0Exg,
#'   names.commodity = names.commodity,
#'   names.agent = names.agent,
#'   numeraire = "prod1",
#'   policy=policy.PublicFirm
#' )
#'
#' ge$p
#' ge$z[1:15]
#'
#' #### the sequential form of the above model.
#' dst.firm <- node_new(
#'   "prod",
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
#' dstl <- list(dst.firm, dst.consumer)
#'
#' ge.seq <- sdm2(
#'   A = dstl,
#'   B = matrix(c(
#'     1, 0,
#'     0, 0
#'   ), 2, 2, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA,
#'     NA, 100
#'   ), 2, 2, TRUE),
#'   names.commodity = c("prod", "lab"),
#'   names.agent = c("firm", "consumer"),
#'   numeraire = "lab",
#'   z0 = c(100, 1),
#'   ts = TRUE,
#'   policy = policyMarketClearingPrice,
#'   numberOfPeriods = 20,
#'   maxIteration = 1
#' )
#'
#' ge.seq$ts.z[, 1]
#' }

gemIntertemporal_PublicFirm <- function(...) sdm2(...)
