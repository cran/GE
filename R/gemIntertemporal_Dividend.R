#' @export
#' @title The Identical Steady-state Equilibrium: Four Models Illustrating Dividend
#' @aliases gemIntertemporal_Dividend
#' @description  Four models are presented to illustrate dividend, which have the same steady-state equilibrium.
#'
#' These models are as follows:
#' (1) a real timeline model with head-tail adjustment;
#' (2) a financial timeline model with dividend and head-tail adjustment;
#' (3) a financial sequential model with dividend;
#' (4) a financial time-circle model with dividend.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' #### (1) a real timeline model with head-tail adjustment.
#' eis <- 0.8 # the elasticity of intertemporal substitution
#' Gamma.beta <- 0.8 # the subjective discount factor
#' gr <- 0.03 # the growth rate
#' np <- 5 # the number of economic periods
#'
#' n <- 2 * np - 1 # the number of commodity kinds
#' m <- np # the number of agent kinds
#'
#' names.commodity <- c(paste0("prod", 1:np), paste0("lab", 1:(np - 1)))
#' names.agent <- c(paste0("firm", 1:(np - 1)), "consumer")
#'
#' # the exogenous supply matrix.
#' S0Exg <- matrix(NA, n, m, dimnames = list(names.commodity, names.agent))
#' S0Exg[paste0("lab", 1:(np - 1)), "consumer"] <- 100 * (1 + gr)^(0:(np - 2))
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
#'     paste0("prod", k), paste0("lab", k)
#'   )
#' }
#'
#' dst.consumer <- node_new(
#'   "util",
#'   type = "CES", es = eis,
#'   alpha = 1, beta = prop.table(Gamma.beta^(1:np)),
#'   paste0("prod", 1:np)
#' )
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
#' sserr(eis, Gamma.beta, gr) # the steady-state equilibrium return rate, 0.2970
#' ge.tl$p[1:(np - 1)] / ge.tl$p[2:np] - 1
#' ge.tl$z
#'
#' ## (2) a financial timeline model with dividend and head-tail adjustment.
#' yield.rate <- sserr(
#'   eis = eis, Gamma.beta = Gamma.beta,
#'   gr = gr, prepaid = TRUE
#' ) # the prepaid steady-state equilibrium return rate, 0.2593
#'
#' n <- 2 * np # the number of commodity kinds
#' m <- np # the number of agent kinds
#'
#' names.commodity <- c(paste0("prod", 1:np), paste0("lab", 1:(np - 1)), "claim")
#' names.agent <- c(paste0("firm", 1:(np - 1)), "consumer")
#'
#' # the exogenous supply matrix.
#' S0Exg <- matrix(NA, n, m, dimnames = list(names.commodity, names.agent))
#' S0Exg[paste0("lab", 1:(np - 1)), "consumer"] <- 100 * (1 + gr)^(0:(np - 2))
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
#'            paste0("prod", k), paste0("lab", k)
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
#' ge.ftl <- sdm2(
#'   A = c(dstl.firm, dst.consumer),
#'   B = B,
#'   S0Exg = S0Exg,
#'   names.commodity = names.commodity,
#'   names.agent = names.agent,
#'   numeraire = "prod1",
#'   policy = makePolicyHeadTailAdjustment(gr = gr, np = np)
#' )
#'
#' ge.ftl$z
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
#'          "prod", "lab"
#' )
#'
#' dst.laborer <- node_new("util",
#'                         type = "Leontief", a = 1,
#'                         "prod"
#' )
#'
#' dst.shareholder <- Clone(dst.laborer)
#'
#' ge.fs <- sdm2(
#'   A = list(dst.firm, dst.laborer, dst.shareholder),
#'   B = diag(c(1, 0, 0)),
#'   S0Exg = {
#'     S0Exg <- matrix(NA, 3, 3)
#'     S0Exg[2, 2] <- S0Exg[3, 3] <- 100
#'     S0Exg
#'   },
#'   names.commodity = c("prod", "lab", "equity.share"),
#'   names.agent = c("firm", "laborer", "shareholder"),
#'   numeraire = "prod",
#'   GRExg = gr
#' )
#'
#' ge.fs$z
#'
#' # a steady-state path.
#' ge2.fs <- sdm2(
#'   A = list(dst.firm, dst.laborer, dst.shareholder),
#'   B = diag(c(1, 0, 0)),
#'   S0Exg = {
#'     S0Exg <- matrix(NA, 3, 3)
#'     S0Exg[2, 2] <- S0Exg[3, 3] <- 100
#'     S0Exg
#'   },
#'   names.commodity = c("prod", "lab", "equity.share"),
#'   names.agent = c("firm", "laborer", "shareholder"),
#'   numeraire = "prod",
#'   GRExg = gr,
#'   maxIteration = 1,
#'   numberOfPeriods = 20,
#'   z0 = ge.fs$z,
#'   policy = policyMarketClearingPrice,
#'   ts = TRUE
#' )
#'
#' ge2.fs$ts.z[, 1]
#' growth_rate(ge2.fs$ts.z[, 1])
#'
#' ## (4) a financial time-circle model with dividend.
#' np <- 5
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
#' S0Exg[paste0("lab", 1:np), "consumer"] <- 100 * (1 + gr)^(0:(np - 1))
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
#'            paste0("lab", k), paste0("prod", k)
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
#' ge.ftc <- sdm2(
#'   A = c(dstl.firm, dst.consumer),
#'   B = B,
#'   S0Exg = S0Exg,
#'   names.commodity = names.commodity,
#'   names.agent = names.agent,
#'   numeraire = "prod1",
#'   ts = TRUE
#' )
#'
#' ge.ftc$z
#'
#' ##
#' ge.tc <- gemCanonicalDynamicMacroeconomic_TimeCircle_2_2(
#'   alpha.firm = rep(2, 5),
#'   es.prod.lab.firm = 1,
#'   beta.prod.firm = 0.5,
#'   depreciation.rate = 1,
#'   eis = 0.8,
#'   Gamma.beta = 0.8,
#'   beta.prod.consumer = 1,
#'   es.prod.lab.consumer = 1,
#'   gr = 0.03,
#'   wage.payment = "pre"
#' )
#'
#' ge.tc$z
#' }

gemIntertemporal_Dividend <- function(...) sdm2(...)
