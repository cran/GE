#' @export
#' @title Some Examples of a 2-by-2 Intertemporal Equilibrium Model
#' @aliases gemIntertemporal_2_2
#' @description Some examples of an intertemporal equilibrium model with two types of commodities and
#' two types of agents.
#'
#' In these examples, there is an np-period-lived consumer maximizing intertemporal utility,
#' and there is a type of firm which produces from period 1 to np-1.
#' There are two types of commodities, i.e. product and labor.
#' Assume the consumer has some product in the first period.
#' That is, the product supply in the first period is an exogenous variable.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' #### an example with a Cobb-Douglas intertemporal utility function
#' np <- 5 # the number of economic periods
#' y1 <- 150 # the initial product supply
#'
#' n <- 2 * np - 1 # the number of commodity kinds
#' m <- np # the number of agent kinds
#'
#' names.commodity <- c(paste0("prod", 1:np), paste0("lab", 1:(np - 1)))
#' names.agent <- c(paste0("firm", 1:(np - 1)), "consumer")
#'
#' # the exogenous supply matrix.
#' S0Exg <- matrix(NA, n, m, dimnames = list(names.commodity, names.agent))
#' S0Exg[paste0("lab", 1:(np - 1)), "consumer"] <- 100
#' S0Exg["prod1", "consumer"] <- y1
#'
#' # the output coefficient matrix.
#' B <- matrix(0, n, m, dimnames = list(names.commodity, names.agent))
#' for (k in 1:(np - 1)) {
#'   B[paste0("prod", k + 1), paste0("firm", k)] <- 1
#' }
#'
#' dstl.firm <- list()
#' for (k in 1:(np - 1)) {
#'   dstl.firm[[k]] <- node_new(
#'     "prod",
#'     type = "CD",
#'     alpha = 2, beta = c(0.5, 0.5),
#'     paste0("prod", k), paste0("lab", k)
#'   )
#' }
#'
#' dst.consumer.CD <- node_new(
#'   "util",
#'   type = "CD",
#'   alpha = 1, beta = prop.table(rep(1, np)),
#'   paste0("prod", 1:np)
#' )
#'
#' f <- function(dstl) {
#'   sdm2(
#'     A = dstl,
#'     B = B,
#'     S0Exg = S0Exg,
#'     names.commodity = names.commodity,
#'     names.agent = names.agent,
#'     numeraire = "prod1",
#'     ts = TRUE
#'   )
#' }
#'
#' ge <- f(c(dstl.firm, dst.consumer.CD))
#'
#' ge$p
#' ge$z
#' ge$D
#' ge$S
#' ge$DV
#' ge$SV
#'
#' ## an example with a Leontief intertemporal utility function
#' dst.consumer.Leontief <- node_new(
#'   "util",
#'   type = "Leontief",
#'   a = rep(1, np),
#'   paste0("prod", 1:np)
#' )
#'
#' ge2 <- f(c(dstl.firm, dst.consumer.Leontief))
#'
#' ge2$p
#' ge2$z
#' ge2$D
#' ge2$S
#' ge2$DV
#' ge2$SV
#'
#' ## Assume that the consumer has a CES (i.e. CRRA) intertemporal utility function.
#' # eis is the elasticity of intertemporal substitution.
#' # Gamma.beta is the subjective discount factor.
#' f2 <- function(eis = 1, Gamma.beta = 1, head.tail.adjustment = "none") {
#'   dst.consumer <- node_new(
#'     "util",
#'     type = "CES", es = eis,
#'     alpha = 1, beta = prop.table(Gamma.beta^(1:np)),
#'     paste0("prod", 1:np)
#'   )
#'
#'   ge <- sdm2(
#'     A = c(dstl.firm, dst.consumer),
#'     B = B,
#'     S0Exg = S0Exg,
#'     names.commodity = names.commodity,
#'     names.agent = names.agent,
#'     numeraire = "prod1",
#'     ts = TRUE,
#'     policy = makePolicyHeadTailAdjustment(head.tail.adjustment, np = np)
#'   )
#'
#'   list(
#'     p = ge$p, z = ge$z,
#'     D = addmargins(ge$D, 2), S = addmargins(ge$S, 2),
#'     DV = addmargins(ge$DV), SV = addmargins(ge$SV)
#'   )
#' }
#'
#' f2(Gamma.beta = 0.9)
#' f2(Gamma.beta = 0.9, head.tail.adjustment = "both") # the steady state in the worldsheet
#' f2(Gamma.beta = 1.25, head.tail.adjustment = "both") # the steady state in the worldsheet
#' f2(eis = 2, Gamma.beta = 0.9)
#' }


gemIntertemporal_2_2 <- function(...) sdm2(...)
