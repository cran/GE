#' @export
#' @title The Identical Steady-state Equilibrium: Three Models with Money and Dividend
#' @aliases gemIntertemporal_Money_Dividend_Example7.5.1
#' @description  Three steady-state-identical models with money and dividend as follows:
#' (1) a sequential model (Li, 2019, example 7.5);
#' (2) a time-circle model;
#' (3) a timeline model with head-tail adjustment.
#'
#' Stocks, fiat currencies, bonds, and taxes, etc. can be collectively referred to as (financial) claims.
#' Sometimes we do not need to differentiate between these financial instruments when modeling.
#' Furthermore, sometimes we do not need to consider which period these financial instruments belong to.
#' @param ... arguments to be passed to the function sdm2.
#' @references LI Wu (2019, ISBN: 9787521804225) General Equilibrium and Structural Dynamics: Perspectives of New Structural Economics. Beijing: Economic Science Press. (In Chinese)
#' @examples
#' \donttest{
#' #### (1) a sequential model. See the first part of example 7.5 in Li (2019).
#' dividend.rate <- 0.25
#' ir <- 0.25 # the interest rate.
#'
#' dst.firm <- node_new(
#'   "output",
#'   type = "FIN", rate = c(1, dividend.rate),
#'   "cc1", "dividend"
#' )
#' node_set(dst.firm, "cc1",
#'          type = "FIN", rate = c(1, ir),
#'          "cc1.1", "money"
#' )
#' node_set(dst.firm, "cc1.1",
#'          type = "CD", alpha = 1, beta = c(0.5, 0.5),
#'          "prod", "lab"
#' )
#'
#' dst.consumer <- node_new(
#'   "util",
#'   type = "FIN", rate = c(1, ir),
#'   "cc1", "money"
#' )
#' node_set(dst.consumer, "cc1",
#'          type = "CD", alpha = 1, beta = c(0.5, 0.5),
#'          "prod", "lab"
#' )
#'
#' ge.seq <- sdm2(
#'   A = list(
#'     dst.firm, dst.consumer, dst.consumer, dst.consumer
#'   ),
#'   B = diag(c(1, 0, 0, 0)),
#'   S0Exg = {
#'     tmp <- matrix(NA, 4, 4)
#'     tmp[2, 2] <- tmp[3, 3] <- tmp[4, 4] <- 100
#'     tmp
#'   },
#'   names.commodity = c("prod", "lab", "money", "dividend"),
#'   names.agent = c("firm", "laborer", "money.owner", "shareholder"),
#'   numeraire = "prod",
#'   GRExg = 0.1,
#'   z0 = c(9.30909, 0, 0, 0),
#'   policy = policyMarketClearingPrice,
#'   maxIteration = 1,
#'   numberOfPeriods = 20,
#'   ts = TRUE
#' )
#'
#' matplot(ge.seq$ts.z, type = "o", pch = 20)
#' ge.seq$D
#' ge.seq$S
#' ge.seq$ts.z[,1]
#' growth_rate(ge.seq$ts.z[,1])
#'
#' #### (2) a time-circle model.
#' np <- 5 # the number of economic periods
#' gr <- 0.1 # the growth rate.
#' dividend.rate <- 0.25
#' ir <- 0.25
#' zeta <- (1 + gr)^np # the ratio of repayments to loans
#'
#' n <- 2 * np + 1 # the number of commodity kinds
#' m <- np + 1 # the number of agent kinds
#'
#' names.commodity <- c(paste0("prod", 1:np), paste0("lab", 1:np), "claim")
#' names.agent <- c(paste0("firm", 1:np), "consumer")
#'
#' # the exogenous supply matrix.
#' S0Exg <- matrix(NA, n, m, dimnames = list(names.commodity, names.agent))
#' S0Exg[paste0("lab", 1:np), "consumer"] <- 100 * (1 + gr)^(0:(np - 1)) # the labor supply.
#' S0Exg["claim", "consumer"] <- np * 100 # the financial claim supply.
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
#'     type = "FIN", rate = c(1, (1 + ir) * (1 + dividend.rate) - 1),
#'     "cc1", "claim"
#'   )
#'   node_set(dstl.firm[[k]], "cc1",
#'            type = "CD", alpha = 1, beta = c(0.5, 0.5),
#'            paste0("prod", k), paste0("lab", k)
#'   )
#' }
#'
#' dst.consumer <- node_new(
#'   "util",
#'   type = "FIN", rate = c(1, ir),
#'   "cc1", "claim"
#' )
#' node_set(dst.consumer, "cc1",
#'          # type = "CES", es = 1,
#'          type = "CD",
#'          alpha = 1, beta = rep(1 / np, np),
#'          paste0("cc1.", 1:np)
#' )
#' for (k in 1:np) {
#'   node_set(dst.consumer, paste0("cc1.", k),
#'            type = "CD", alpha = 1, beta = c(0.5, 0.5),
#'            paste0("prod", k), paste0("lab", k)
#'   )
#' }
#' node_plot(dst.consumer, TRUE)
#'
#' ge.tc <- sdm2(
#'   A = c(dstl.firm, dst.consumer),
#'   B = B,
#'   S0Exg = S0Exg,
#'   names.commodity = names.commodity,
#'   names.agent = names.agent,
#'   numeraire = "prod1"
#' )
#'
#' ge.tc$D
#' ge.tc$z
#'
#' #### (3) a timeline model with head-tail adjustment.
#' np <- 5 # the number of economic periods
#' gr <- 0.1
#' dividend.rate <- 0.25
#' ir <- 0.25
#'
#' n <- 2 * np + 1 # the number of commodity kinds
#' m <- np # the number of agent kinds
#'
#' names.commodity <- c(paste0("prod", 1:np), paste0("lab", 1:np), "claim")
#' names.agent <- c(paste0("firm", 1:(np - 1)), "consumer")
#'
#' # the exogenous supply matrix.
#' S0Exg <- matrix(NA, n, m, dimnames = list(names.commodity, names.agent))
#' S0Exg[paste0("lab", 1:np), "consumer"] <- 100 * (1 + gr)^(0:(np - 1))
#' S0Exg["claim", "consumer"] <- np * 100
#' S0Exg["prod1", "consumer"] <- 10 # the product supply in the first period, which will be adjusted.
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
#'     type = "FIN", rate = c(1, (1 + ir) * (1 + dividend.rate) - 1),
#'     "cc1", "claim"
#'   )
#'   node_set(dstl.firm[[k]], "cc1",
#'            type = "CD", alpha = 1, beta = c(0.5, 0.5),
#'            paste0("prod", k), paste0("lab", k)
#'   )
#' }
#'
#' dst.consumer <- node_new(
#'   "util",
#'   type = "FIN", rate = c(1, ir),
#'   "cc1", "claim"
#' )
#' node_set(dst.consumer, "cc1",
#'          type = "CD",
#'          alpha = 1, beta = rep(1 / np, np),
#'          paste0("cc1.", 1:np)
#' )
#' for (k in 1:np) {
#'   node_set(dst.consumer, paste0("cc1.", k),
#'            type = "CD", alpha = 1, beta = c(0.5, 0.5),
#'            paste0("prod", k), paste0("lab", k)
#'   )
#' }
#'
#' ge.tl <- sdm2(
#'   A = c(dstl.firm, dst.consumer),
#'   B = B,
#'   S0Exg = S0Exg,
#'   names.commodity = names.commodity,
#'   names.agent = names.agent,
#'   numeraire = "prod1",
#'   policy = makePolicyHeadTailAdjustment(gr = gr, np = np)
#' )
#'
#' node_plot(dst.consumer, TRUE)
#' ge.tl$D
#' ge.tl$z
#' }

gemIntertemporal_Money_Dividend_Example7.5.1 <- function(...) sdm2(...)
