#' @export
#' @title Some Examples of Spot Market Clearing Paths
#' @aliases gemMarketClearingPath_2_2
#' @description Some examples of zero-dividend spot market clearing paths (alias instantaneous equilibrium paths) containing a firm and a laborer (consumer).
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' ## the benchmark equilibrium
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
#'
#' #### A timeline model, the equilibrium of which is the same as the benchmark equilibrium.
#' # In this model, in terms of form, firms are treated as consumer-type agents rather than
#' # producer-type agents. Firms hold products. The utility level of each firm determines
#' # the quantity of the product that the firm owns in the subsequent economic period.
#' np <- 5 # the number of economic periods
#' y1 <- 1 # the initial product supply
#' eis <- 1 # elasticity of intertemporal substitution
#' Gamma.beta <- 1 # the subjective discount factor
#'
#' n <- 2 * np # the number of commodity kinds
#' m <- np + 1 # the number of agent kinds
#'
#' names.commodity <- c(paste0("prod", 1:np), paste0("lab", 1:np))
#' names.agent <- c(paste0("firm", 1:np), "consumer")
#'
#' # the exogenous supply matrix.
#' S0Exg <- matrix(0, n, m, dimnames = list(names.commodity, names.agent))
#' S0Exg[paste0("lab", 1:np), "consumer"] <- 1
#' for (k in 1:np) {
#'   S0Exg[paste0("prod", k), paste0("firm", k)] <- y1
#' }
#'
#' dstl.firm <- list()
#' for (k in 1:np) {
#'   dstl.firm[[k]] <- node_new(
#'     "prod",
#'     type = "CD",
#'     alpha = 5, beta = c(0.5, 0.5),
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
#' dst.consumer <- node_new(
#'   "util",
#'   type = "CES", es = eis,
#'   alpha = 1, beta = prop.table(Gamma.beta^(1:np)),
#'   paste0("prod", 1:np)
#' )
#'
#' ge.timeline <- sdm2(
#'   A = c(dstl.firm, dst.consumer),
#'   B = matrix(0, n, m),
#'   S0Exg = S0Exg,
#'   names.commodity = names.commodity,
#'   names.agent = names.agent,
#'   numeraire = "prod1",
#'   ts = TRUE,
#'   policy = function(time, state) {
#'     names(state$last.z) <- state$names.agent
#'     dimnames(state$S) <- list(names.commodity, names.agent)
#'
#'     for (k in 2:np) {
#'       state$S[paste0("prod", k), paste0("firm", k)] <- state$last.z[paste0("firm", k - 1)]
#'     }
#'
#'     return(state)
#'   }
#' )
#'
#' head(ge.timeline$p, np) / tail(ge.timeline$p, np)
#' ge$ts.p[1:5, 1] # the same as above
#'
#' ge.timeline$z[1:np]
#' ge$ts.z[1:np, 1] # the same as above
#'
#' ge.timeline$D
#' ge.timeline$S
#' }

gemMarketClearingPath_2_2 <- function(...) sdm2(...)
