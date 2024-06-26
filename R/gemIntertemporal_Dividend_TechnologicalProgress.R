#' @export
#' @title The Identical Steady-state Equilibrium: Four Models Illustrating Dividend and Technological Progress
#' @aliases gemIntertemporal_Dividend_TechnologicalProgress
#' @description  Four models with labor-saving technological progress are presented to illustrate dividend, which have the same steady-state equilibrium.
#'
#' These models are as follows:
#' (1) a real timeline model with head-tail adjustment;
#' (2) a financial timeline model with dividend and head-tail adjustment;
#' (3) a financial sequential model with dividend;
#' (4) a financial time-circle model with dividend.
#' @param ... arguments to be passed to the function sdm2.
#' @seealso \code{\link{gemIntertemporal_Dividend}}
#' @examples
#' \donttest{
#' #### (1) a real timeline model with head-tail adjustment.
#' eis <- 0.8 # the elasticity of intertemporal substitution
#' Gamma.beta <- 0.8 # the subjective discount factor
#' gr.tech <- 0.02 # the technological progress rate
#' gr.lab <- 0.03 # the growth rate of labor supply
#' gr <- (1 + gr.lab) * (1 + gr.tech) - 1 # the growth rate
#' np <- 4 # the number of economic periods
#'
#' n <- 2 * np - 1 # the number of commodity kinds
#' m <- np # the number of agent kinds
#'
#' names.commodity <- c(paste0("prod", 1:np), paste0("lab", 1:(np - 1)))
#' names.agent <- c(paste0("firm", 1:(np - 1)), "consumer")
#'
#' # the exogenous supply matrix.
#' S0Exg <- matrix(NA, n, m, dimnames = list(names.commodity, names.agent))
#' S0Exg[paste0("lab", 1:(np - 1)), "consumer"] <- 100 * (1 + gr.lab)^(0:(np - 2))
#' S0Exg["prod1", "consumer"] <- 140 # the product supply in the first period, which will be adjusted.
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
#'     paste0("prod", k), "cc1"
#'   )
#'   node_set(dstl.firm[[k]], "cc1",
#'            type = "Leontief", a = 1 / (1 + gr.tech)^(k - 1),
#'            paste0("lab", k)
#'   )
#' }
#'
#' node_plot(dstl.firm[[np - 1]], TRUE)
#'
#' dst.consumer <- node_new(
#'   "util",
#'   type = "CES", es = eis,
#'   alpha = 1, beta = prop.table(Gamma.beta^(1:np)),
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
#'   policy = makePolicyHeadTailAdjustment(gr = gr, np = np)
#' )
#'
#' sserr(eis, Gamma.beta, gr) # the steady-state equilibrium return rate
#' ge$p[1:(np - 1)] / ge$p[2:np] - 1 # the steady-state equilibrium return rate
#' ge$z
#' growth_rate(ge$z)
#'
#' ## (2) a financial timeline model with dividend and head-tail adjustment.
#' yield.rate <- sserr(eis, Gamma.beta, gr, prepaid = TRUE)
#'
#' n <- 2 * np # the number of commodity kinds
#' m <- np # the number of agent kinds
#'
#' names.commodity <- c(paste0("prod", 1:np), paste0("lab", 1:(np - 1)), "claim")
#' names.agent <- c(paste0("firm", 1:(np - 1)), "consumer")
#'
#' # the exogenous supply matrix.
#' S0Exg <- matrix(NA, n, m, dimnames = list(names.commodity, names.agent))
#' S0Exg[paste0("lab", 1:(np - 1)), "consumer"] <- 100 * (1 + gr.lab)^(0:(np - 2))
#' S0Exg["claim", "consumer"] <- 100
#' S0Exg["prod1", "consumer"] <- 140 # the product supply in the first period, which will be adjusted.
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
#'     type = "FIN", rate = c(1, yield.rate),
#'     "cc1", "claim"
#'   )
#'   node_set(dstl.firm[[k]], "cc1",
#'            type = "CD", alpha = 2, beta = c(0.5, 0.5),
#'            paste0("prod", k), "cc1.1"
#'   )
#'   node_set(dstl.firm[[k]], "cc1.1",
#'            type = "Leontief", a = 1 / (1 + gr.tech)^(k - 1),
#'            paste0("lab", k)
#'   )
#' }
#'
#' dst.consumer <- node_new(
#'   "util",
#'   type = "CES", es = 1,
#'   alpha = 1, beta = prop.table(rep(1, np)), # prop.table(Gamma.beta^(1:np)),
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
#'   policy = makePolicyHeadTailAdjustment(gr = gr, np = np)
#' )
#'
#' ge$z
#'
#' ## (3) a financial sequential model with dividend.
#' dst.firm <- node_new("output",
#'                      type = "FIN",
#'                      rate = c(1, dividend.rate = yield.rate),
#'                      "cc1", "equity.share"
#' )
#' node_set(dst.firm, "cc1",
#'          type = "CD",
#'          alpha = 2, beta = c(0.5, 0.5),
#'          "prod", "cc1.1"
#' )
#' node_set(dst.firm, "cc1.1",
#'          type = "Leontief", a = 1,
#'          "lab"
#' )
#'
#' node_plot(dst.firm, TRUE)
#'
#' dst.laborer <- node_new("util",
#'                         type = "Leontief", a = 1,
#'                         "prod"
#' )
#'
#' dst.shareholder <- Clone(dst.laborer)
#'
#' ge <- sdm2(
#'   A = list(dst.firm, dst.laborer, dst.shareholder),
#'   B = diag(c(1, 0, 0)),
#'   S0Exg = {
#'     S0Exg <- matrix(NA, 3, 3)
#'     S0Exg[2, 2] <- 100 / (1 + gr.lab)
#'     S0Exg[3, 3] <- 100
#'     S0Exg
#'   },
#'   names.commodity = c("prod", "lab", "equity.share"),
#'   names.agent = c("firm", "laborer", "shareholder"),
#'   numeraire = "equity.share",
#'   maxIteration = 1,
#'   numberOfPeriods = 20,
#'   z0 = c(143.1811, 0, 0),
#'   policy = list(policy.technology <- function(time, A, state) {
#'     node_set(A[[1]], "cc1.1",
#'              a = 1 / (1 + gr.tech)^(time - 1)
#'     )
#'     state$S[2, 2] <- 100 * (1 + gr.lab)^(time - 1)
#'
#'     state
#'   }, policyMarketClearingPrice),
#'   ts = TRUE
#' )
#'
#' ge$ts.z[, 1]
#' growth_rate(ge$ts.z[, 1])
#'
#' ## (4) a financial time-circle model with dividend.
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
#' S0Exg[paste0("lab", 1:np), "consumer"] <- 100 * (1 + gr.lab)^(0:(np - 1))
#' S0Exg["claim", "consumer"] <- 100
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
#'   dstl.firm[[k]] <- node_new("output",
#'                              type = "FIN", rate = c(1, yield.rate),
#'                              "cc1", "claim"
#'   )
#'   node_set(dstl.firm[[k]], "cc1",
#'            type = "CD", alpha = 2,
#'            beta = c(0.5, 0.5),
#'            paste0("prod", k), "cc1.1"
#'   )
#'   node_set(dstl.firm[[k]], "cc1.1",
#'            type = "Leontief", a = 1 / (1 + gr.tech)^(k - 1),
#'            paste0("lab", k)
#'   )
#' }
#'
#' dst.consumer <- node_new(
#'   "util",
#'   type = "CES", es = 1,
#'   alpha = 1, beta = prop.table(rep(1, np)),
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
#' ge$z
#' growth_rate(ge$z[1:np])
#' }

gemIntertemporal_Dividend_TechnologicalProgress <- function(...) sdm2(...)
