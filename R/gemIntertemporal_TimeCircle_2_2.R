#' @export
#' @title Some Examples of a 2-by-2 Time Circle Equilibrium Model
#' @aliases gemIntertemporal_TimeCircle_2_2
#' @description Some examples of a 2-by-2 (intertemporal) time circle equilibrium model.
#' In a time circle model, the economy borrows some resources from the outside in the beginning,
#' and will repay it after the economy ends.
#'
#' In these examples, there is an np-period-lived consumer maximizing intertemporal utility,
#' and there is a type of firm which produces from period 1 to np.
#' There are two commodities, i.e. product and labor.
#' Suppose the firm can borrow some product from outside in the first period and return them in the (np+1)-th period.
#' And the supply of product in the first period can be regarded as the output of the firm in the (np+1)-th period.
#' Hence the product supply in the first period is an endogenous variable.
#' Suppose that the amount returned is zeta times the amount borrowed.
#' @param ... arguments to be passed to the function sdm2.
#' @seealso {
#' \code{\link{gemOLG_TimeCircle}}
#' }
#' @examples
#' \donttest{
#' #### an example with a Cobb-Douglas intertemporal utility function
#' np <- 5 # the number of economic periods, firms.
#' gr <- 0 # the growth rate of the labor supply
#' zeta <- 1.25 # the ratio of repayments to loans
#' # zeta <- (1 + gr)^np
#' Gamma.beta <- 1 # the subjective discount factor
#'
#' n <- 2 * np # the number of commodity kinds
#' m <- np + 1 # the number of agent kinds
#'
#' names.commodity <- c(paste0("prod", 1:np), paste0("lab", 1:np))
#' names.agent <- c(paste0("firm", 1:np), "consumer")
#'
#' # the exogenous supply matrix.
#' S0Exg <- matrix(NA, n, m, dimnames = list(names.commodity, names.agent))
#' S0Exg[paste0("lab", 1:np), "consumer"] <- 100 * (1 + gr)^(0:(np - 1)) # the supply of labor
#'
#' # the output coefficient matrix.
#' B <- matrix(0, n, m, dimnames = list(names.commodity, names.agent))
#' for (k in 1:(np - 1)) {
#'   B[paste0("prod", k + 1), paste0("firm", k)] <- 1
#' }
#' B["prod1", paste0("firm", np)] <- 1 / zeta
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
#'   type = "CD", alpha = 1, beta = prop.table(Gamma.beta^(1:np)),
#'   paste0("prod", 1:np)
#' )
#'
#' ge <- sdm2(
#'   A = c(dstl.firm, dst.consumer),
#'   B = B,
#'   S0Exg = S0Exg,
#'   names.commodity = names.commodity,
#'   names.agent = names.agent,
#'   numeraire = "prod1",
#'   ts = TRUE
#' )
#'
#' ge$p
#' ge$z
#' ge$D
#' ge$S
#' ge$DV
#' ge$SV
#'
#' ## an example with a Leontief intertemporal utility function
#' dst.consumer <- node_new(
#'   "util",
#'   type = "Leontief", a = rep(1, np),
#'   paste0("prod", 1:np)
#' )
#'
#' ge2 <- sdm2(
#'   A = c(dstl.firm, dst.consumer),
#'   B = B,
#'   S0Exg = S0Exg,
#'   names.commodity = names.commodity,
#'   names.agent = names.agent,
#'   numeraire = "lab1",
#'   ts = TRUE
#' )
#'
#' ge2$p
#' ge2$z
#' ge2$D
#' ge2$S
#' ge2$DV
#' ge2$SV
#'
#' ## Use a mean-value policy function to accelerate convergence.
#' ge3 <- sdm2(
#'   A = c(dstl.firm, dst.consumer),
#'   B = B,
#'   S0Exg = S0Exg,
#'   names.commodity = c(paste0("prod", 1:np), paste0("lab", 1:np)),
#'   names.agent = c(paste0("firm", 1:np), "consumer"),
#'   numeraire = "lab1",
#'   ts = TRUE,
#'   policy = makePolicyMeanValue(30)
#' )
#'
#' #### an example with a linear intertemporal utility function (e.g. beta1 * x1 + beta2 * x2)
#' ## The demand structure of the consumer will be adjusted sluggishly to accelerate convergence.
#' np <- 5 # the number of economic periods, firms.
#' rho <- 0.9 # the subjective discount factor
#'
#' beta.consumer <- rep(rho^(0:(np - 1)))
#' zeta <- (1 / rho)^np
#'
#' n <- 2 * np # the number of commodity kinds
#' m <- np + 1 # the number of agent kinds
#'
#' names.commodity <- c(paste0("prod", 1:np), paste0("lab", 1:np))
#' names.agent <- c(paste0("firm", 1:np), "consumer")
#'
#' # the exogenous supply matrix.
#' S0Exg <- matrix(NA, n, m, dimnames = list(names.commodity, names.agent))
#' S0Exg[paste0("lab", 1:np), "consumer"] <- 100 # the supply of labor
#'
#' # the output coefficient matrix.
#' B <- matrix(0, n, m, dimnames = list(names.commodity, names.agent))
#' for (k in 1:(np - 1)) {
#'   B[paste0("prod", k + 1), paste0("firm", k)] <- 1
#' }
#' B["prod1", paste0("firm", np)] <- 1 / zeta
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
#'   type = "FUNC",
#'   last.a = rep(1, np),
#'   func = function(p) {
#'     value.marginal.utility <- beta.consumer / p
#'     ratio <- value.marginal.utility / mean(value.marginal.utility)
#'     a <- dst.consumer$last.a
#'     a <- prop.table(a * ratio_adjust(ratio, 0.15))
#'     dst.consumer$last.a <- a
#'     a
#'   },
#'   paste0("prod", 1:np)
#' )
#'
#' ge <- sdm2(
#'   A = c(dstl.firm, dst.consumer),
#'   B = B,
#'   S0Exg = S0Exg,
#'   names.commodity = names.commodity,
#'   names.agent = names.agent,
#'   numeraire = "lab1",
#'   ts = TRUE,
#'   priceAdjustmentVelocity = 0.1
#' )
#'
#' ge$p
#' ge$z
#' ge$D
#' ge$S
#' growth_rate(ge$p[1:np])
#' growth_rate(ge$p[(np + 1):(2 * np)])
#' }

gemIntertemporal_TimeCircle_2_2 <- function(...) sdm2(...)
