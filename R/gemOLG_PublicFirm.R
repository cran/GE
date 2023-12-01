#' @export
#' @title Some Examples of (Timeline) OLG Models with Production and Public Firms
#' @aliases gemOLG_PublicFirm
#' @description Some examples of (timeline) OLG models with production and public firms (see \cite{\link{gemIntertemporal_PublicFirm}}).
#' @param ... arguments to be passed to the function sdm2.
#' @seealso \cite{\link{gemIntertemporal_PublicFirm}}
#' @examples
#' \donttest{
#' ng <- 8 # the number of generations
#' alpha.firm <- 2 # the efficient parameter of firms
#' beta.prod.firm <- 0.5 # the product (i.e. capital) share parameter of firms
#' beta.consumer <- c(1 / 3, 1 / 3, 1 / 3) # the share parameter of consumers
#' gr.laborer <- 0 # the population growth rate
#' labor.first <- c(50, 50, 0) # the labor supply of the first generation
#' # the labor supply of the last generation.
#' labor.last <- 50 * (1 + gr.laborer)^((ng - 1):(ng + 1))
#' y1 <- 100 # the initial product supply
#'
#' policy.PublicFirm <- function(state) {
#'   for (k in 1:(ng + 1)) {
#'     state$S[k + 1, k + 1] <- state$S[k + 1, k]
#'     state$S[k + 1, k] <- 0
#'   }
#'   state
#' }
#'
#' f <- function(policy = policy.PublicFirm) {
#'   names.commodity <- c(paste0("prod", 1:(ng + 2)), paste0("lab", 1:(ng + 2)))
#'   names.agent <- c(paste0("firm", 1:(ng + 2)), paste0("consumer", 1:ng))
#'
#'   n <- length(names.commodity) # the number of commodity kinds
#'   m <- length(names.agent) # the number of agent kinds
#'
#'   # the exogenous supply matrix.
#'   S0Exg <- matrix(NA, n, m, dimnames = list(names.commodity, names.agent))
#'   for (k in 1:(ng - 1)) {
#'     S0Exg[paste0("lab", k:(k + 2)), paste0("consumer", k)] <-
#'       labor.first * (1 + gr.laborer)^(k - 1)
#'   }
#'   S0Exg[paste0("lab", ng:(ng + 2)), paste0("consumer", ng)] <- labor.last
#'   S0Exg["prod1", "firm1"] <- y1
#'
#'   B <- matrix(0, n, m, dimnames = list(names.commodity, names.agent))
#'   for (k in 1:(ng + 1)) {
#'     B[paste0("prod", k + 1), paste0("firm", k)] <- 1
#'   }
#'
#'   dstl.consumer <- list()
#'   for (k in 1:ng) {
#'     dstl.consumer[[k]] <- node_new(
#'       "util",
#'       type = "CD", alpha = 1,
#'       beta = beta.consumer,
#'       paste0("prod", k:(k + 2))
#'     )
#'   }
#'
#'   dstl.firm <- list()
#'   for (k in 1:(ng + 2)) {
#'     dstl.firm[[k]] <- node_new(
#'       "prod",
#'       type = "CD", alpha = alpha.firm,
#'       beta = c(beta.prod.firm, 1 - beta.prod.firm),
#'       paste0("prod", k), paste0("lab", k)
#'     )
#'   }
#'
#'   ge <- sdm2(
#'     A = c(dstl.firm, dstl.consumer),
#'     B = B,
#'     S0Exg = S0Exg,
#'     names.commodity = names.commodity,
#'     names.agent = names.agent,
#'     numeraire = "lab1",
#'     priceAdjustmentVelocity = 0.05,
#'     policy = policy
#'   )
#'
#'   cat("ge$p:\n")
#'   print(ge$p)
#'   cat("ge$z:\n")
#'   print(ge$z)
#'   invisible(ge)
#' }
#'
#' ge <- f()
#'
#' # the growth rates of prices
#' growth_rate(ge$p[paste0("prod", 1:ng)]) + 1
#' growth_rate(ge$p[paste0("lab", 1:ng)]) + 1
#'
#' ##
#' labor.first <- c(100 / 3, 100 / 3, 100 / 3) # the labor supply of the first generation
#' ge <- f()
#'
#' ##
#' tax.rate <- 0.1
#' policy.PublicFirm.Tax <- function(state) {
#'   for (k in 1:(ng + 1)) {
#'     state$S[k + 1, k + 1] <- state$S[k + 1, k] * (1 - tax.rate)
#'     state$S[k + 1, k + ng] <- state$S[k + 1, k] * tax.rate
#'     state$S[k + 1, k] <- 0
#'   }
#'   state
#' }
#'
#' ge <- f(policy.PublicFirm.Tax)
#'
#' ##
#' beta.consumer <- c(1 / 2, 1 / 2, 0) # the share parameter of consumers
#' labor.first <- c(90, 10, 0) # the labor supply of the first generation
#' ge <- f()
#' }

gemOLG_PublicFirm <- function(...) sdm2(...)
