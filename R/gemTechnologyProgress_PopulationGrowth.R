#' @export
#' @title Some General Equilibrium Models with Technology Progress and Population Growth
#' @aliases gemTechnologyProgress_PopulationGrowth
#' @description Some examples illustrating technology Progress and population growth.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' #### a financial sequential model
#' gr.e <- 0.03 # the population growth rate
#' tpr <- 0.02 # the rate of technological progress
#' gr <- (1 + gr.e) * (1 + tpr) - 1
#' eis <- 0.8 # the elasticity of intertemporal substitution
#' Gamma.beta <- 0.8 # the subjective discount factor
#' yield.rate <- (1 + gr)^(1 / eis - 1) / Gamma.beta - 1 # the dividend rate
#' y1 <- 143.18115 # the initial product supply
#'
#' dst.firm <- node_new("output",
#'   type = "FIN",
#'   rate = c(1, dividend.rate = yield.rate),
#'   "cc1", "equity.share"
#' )
#' node_set(dst.firm, "cc1",
#'   type = "CD",
#'   alpha = 2, beta = c(0.5, 0.5),
#'   "prod", "cc1.1"
#' )
#' node_set(dst.firm, "cc1.1",
#'   type = "Leontief", a = 1,
#'   "lab"
#' )
#'
#' dst.laborer <- node_new("util",
#'   type = "Leontief", a = 1,
#'   "prod"
#' )
#'
#' dst.shareholder <- Clone(dst.laborer)
#'
#' ge <- sdm2(
#'   A = list(dst.firm, dst.laborer, dst.shareholder),
#'   B = diag(c(1, 0, 0)),
#'   S0Exg = {
#'     S0Exg <- matrix(NA, 3, 3)
#'     S0Exg[2, 2] <- S0Exg[3, 3] <- 100 / (1 + gr.e)
#'     S0Exg
#'   },
#'   names.commodity = c("prod", "lab", "equity.share"),
#'   names.agent = c("firm", "laborer", "shareholder"),
#'   numeraire = "prod",
#'   maxIteration = 1,
#'   numberOfPeriods = 20,
#'   policy = list(function(time, A) {
#'     node_set(A[[1]], "cc1.1", a = 1 / (1 + tpr)^(time - 1))
#'   }, policyMarketClearingPrice),
#'   z0 = c(y1, 0, 0),
#'   GRExg = gr.e,
#'   ts = TRUE
#' )
#'
#' matplot(growth_rate(ge$ts.p), type = "l")
#' matplot(growth_rate(ge$ts.z), type = "l")
#' ge$ts.z
#'
#' ## a timeline model
#' np <- 5 # the number of economic periods.
#'
#' n <- 2 * np - 1 # the number of commodity kinds
#' m <- np # the number of agent kinds
#'
#' names.commodity <- c(paste0("prod", 1:np), paste0("lab", 1:(np - 1)))
#' names.agent <- c(paste0("firm", 1:(np - 1)), "consumer")
#'
#' # the exogenous supply matrix.
#' S0Exg <- matrix(NA, n, m, dimnames = list(names.commodity, names.agent))
#' S0Exg[paste0("lab", 1:(np - 1)), "consumer"] <- 100 * (1 + gr.e)^(0:(np - 2))
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
#'     paste0("prod", k), "cc1"
#'   )
#'   node_set(dstl.firm[[k]], "cc1",
#'            type = "Leontief", a = 1 / ((1 + tpr)^(k - 1)),
#'            paste0("lab", k)
#'   )
#' }
#'
#' dst.consumer <- node_new(
#'   "util",
#'   type = "CES",
#'   alpha = 1, beta = prop.table(Gamma.beta^(1:np)), es = eis,
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
#'   maxIteration = 1,
#'   numberOfPeriods = 40,
#'   ts = TRUE,
#'   policy = list(
#'     makePolicyTailAdjustment(ind = c(np - 1, np), gr = gr),
#'     policyMarketClearingPrice
#'   )
#' )
#'
#' ge$z
#' ge$D
#' ge$S
#' ge$p[1:3] / ge$p[2:4] - 1 # the steady-state equilibrium return rate
#' sserr(eis = eis, Gamma.beta = Gamma.beta, gr = gr) # the steady-state equilibrium return rate
#'
#' ## a financial time-circle model
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
#' S0Exg[paste0("lab", 1:np), "consumer"] <- 100 * (1 + gr.e)^(0:(np - 1))
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
#'            type = "Leontief", a = 1 / ((1 + tpr)^(k - 1)),
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
#' growth_rate(ge$z)
#' ge$D
#' ge$S
#' }

gemTechnologyProgress_PopulationGrowth <- function(...) sdm2(...)
