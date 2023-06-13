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
#' rho.beta <- 0.8 # the subjective discount factor
#' yield <- (1 + gr)^(1 / eis - 1) / rho.beta - 1 # dividend yield
#'
#' dst.firm <- node_new("output",
#'   type = "FIN",
#'   rate = c(1, dividend.rate = yield),
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
#'     S0Exg[2, 2] <- S0Exg[3, 3] <- 100
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
#'   z0 = c(147.47658, 0, 0), # 147.47658=1656.3629/(1.0506)^49
#'   GRExg = gr.e,
#'   ts = TRUE
#' )
#'
#' matplot(growth_rate(ge$ts.p), type = "l")
#' matplot(growth_rate(ge$ts.z), type = "l")
#' ge$ts.z
#' ge$ts.z / (1 + gr.e)
#'
#' ## a timeline model
#' np <- 5 # the number of internal periods.
#' y1 <- 143.18115
#' S <- matrix(NA, 2 * np - 1, np)
#' S[(np + 1):(2 * np - 1), np] <- 100 * (1 + gr.e)^(0:(np - 2))
#' S[1, np] <- y1
#'
#' B <- matrix(0, 2 * np - 1, np)
#' B[2:np, 1:(np - 1)] <- diag(np - 1)
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
#'     type = "Leontief", a = 1 / ((1 + tpr)^(k - 1)),
#'     paste0("lab", k)
#'   )
#' }
#'
#' dst.consumer <- node_new(
#'   "util",
#'   type = "CES",
#'   alpha = 1, beta = prop.table(rho.beta^(1:np)), es = eis,
#'   paste0("prod", 1:np)
#' )
#'
#' policy.tail.adjustment <- function(A, state) {
#'   ratio.output <- state$last.z[1] * (1 + gr)^(np - 2) / state$last.z[np - 1]
#'   tail.beta <- tail(A[[np]]$beta, 1)
#'   tail.beta <- tail.beta * ratio.output
#'   A[[np]]$beta <- prop.table(c(head(A[[np]]$beta, -1), tail.beta))
#' }
#'
#' ge <- sdm2(
#'   A = c(dstl.firm, dst.consumer),
#'   B = B,
#'   S0Exg = S,
#'   names.commodity = c(paste0("prod", 1:np), paste0("lab", 1:(np - 1))),
#'   names.agent = c(paste0("firm", 1:(np - 1)), "consumer"),
#'   numeraire = "prod1",
#'   maxIteration = 1,
#'   numberOfPeriods = 40,
#'   ts = TRUE,
#'   policy = list(policy.tail.adjustment, policyMarketClearingPrice)
#' )
#'
#' ge$z
#' ge$D
#' ge$S
#' ge$p[1:3] / ge$p[2:4] - 1 # the real interest rate
#' ((1 + gr.e) * (1 + tpr))^(1 / eis) / rho.beta - 1
#'
#' ## a financial time-circle model
#' zeta <- (1 + gr)^np # the ratio of repayments to loans
#' S <- matrix(NA, 2 * np + 1, np + 1)
#' S[(np + 1):(2 * np + 1), np + 1] <- c(100 * (1 + gr.e)^(0:(np - 1)), 100)
#'
#' B <- matrix(0, 2 * np + 1, np + 1)
#' B[1:np, 1:np] <- diag(np)[, c(2:np, 1)]
#' B[1, np] <- 1 / zeta
#'
#' dstl.firm <- list()
#' for (k in 1:np) {
#'   dstl.firm[[k]] <- node_new("output",
#'     type = "FIN", rate = c(1, yield),
#'     "cc1", "claim"
#'   )
#'   node_set(dstl.firm[[k]], "cc1",
#'     type = "CD", alpha = 2,
#'     beta = c(0.5, 0.5),
#'     paste0("prod", k), "cc1.1"
#'   )
#'   node_set(dstl.firm[[k]], "cc1.1",
#'     type = "Leontief", a = 1 / ((1 + tpr)^(k - 1)),
#'     paste0("lab", k)
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
#'   S0Exg = S,
#'   names.commodity = c(paste0("prod", 1:np), paste0("lab", 1:np), "claim"),
#'   names.agent = c(paste0("firm", 1:np), "consumer"),
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
