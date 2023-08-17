#' @export
#' @title An Example of a 1-by-2 Intertemporal Equilibrium Model
#' @aliases gemIntertemporal_1_2
#' @description An example of an intertemporal equilibrium model with one type of commodity (i.e., product) and
#' two types of agents (i.e., a firm with an AK production function and a consumer).
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' eis <- 0.5 # the elasticity of intertemporal substitution of the consumer
#' rho.beta <- 0.97 # the subjective discount factor of the consumer
#' alphaK <- 1.1 # the parameter of the AK production function
#'
#' np <- 5 # the number of planning periods
#'
#' n <- np # the number of commodity kinds
#' m <- np # the number of agent kinds
#'
#' names.commodity <- paste0("prod", 1:np)
#' names.agent <- c(paste0("firm", 1:(np - 1)), "consumer")
#'
#' # the exogenous supply matrix.
#' S0Exg <- matrix(NA, n, m, dimnames = list(names.commodity, names.agent))
#' S0Exg[paste0("prod", 1:np), "consumer"] <- 100
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
#'     type = "Leontief",
#'     a = 1 / alphaK,
#'     paste0("prod", k)
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
#' ge <- sdm2(
#'   A = c(dstl.firm, dst.consumer),
#'   B = B,
#'   S0Exg = S0Exg,
#'   names.commodity = names.commodity,
#'   names.agent = names.agent,
#'   numeraire = "prod1"
#' )
#'
#' ge$p
#' ge$z
#' ge$D
#' ge$S
#' growth_rate(ge$D[, m])
#' (rho.beta * alphaK)^eis - 1
#' }

gemIntertemporal_1_2 <- function(...) sdm2(...)
