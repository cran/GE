#' @export
#' @title Some Examples of Basic (Timeline) OLG Models with Production
#' @aliases gemOLG_Basic
#' @description Some examples of basic (timeline) OLG models with production.
#' In these models there are two types of commodities (i.e., labor and product) and two types of economic agents (i.e., laborers and firms).
#' Laborers (i.e., consumers) live for two or three periods.
#' These models can be easily extended to include more types of consumers and firms, and to allow consumers to live for more periods.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' ## When beta.consumer[1]==0, beta.consumer[2:3]>0, labor.first[1]>0, labor.first[2:3]==0,
#' ## this model is actually the Diamond model. However, the division of periods is
#' ## slightly different from the Diamond model.
#' ng <- 15 # the number of generations
#' alpha.firm <- 2 # the efficient parameter of firms
#' beta.prod.firm <- 0.4 # the product (i.e. capital) share parameter of firms
#' beta.consumer <- c(0, 0.8, 0.2) # the share parameter of consumers
#' gr.laborer <- 0.03 # the population growth rate
#' labor.first <- c(100, 0, 0) # the labor supply of the first generation
#' labor.last <- 100 * (1 + gr.laborer)^((ng - 1):ng) # the labor supply of the last generation
#' y1 <- 8 # the initial product supply
#'
#' f <- function() {
#'   names.commodity <- c(paste0("prod", 1:(ng + 2)), paste0("lab", 1:(ng + 1)))
#'   names.agent <- c(paste0("firm", 1:(ng + 1)), paste0("consumer", 1:ng))
#'
#'   n <- length(names.commodity) # the number of commodity kinds
#'   m <- length(names.agent) # the number of agent kinds
#'
#'   # the exogenous supply matrix.
#'   S0Exg <- matrix(NA, n, m, dimnames = list(names.commodity, names.agent))
#'   for (k in 1:(ng - 1)) {
#'     S0Exg[paste0("lab", k:(k + 2)), paste0("consumer", k)] <- labor.first * (1 + gr.laborer)^(k - 1)
#'   }
#'   S0Exg[paste0("lab", ng:(ng + 1)), paste0("consumer", ng)] <- labor.last
#'   S0Exg["prod1", "consumer1"] <- y1
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
#'
#'   dstl.firm <- list()
#'   for (k in 1:(ng + 1)) {
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
#'     priceAdjustmentVelocity = 0.05
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
#' # the steady-state growth rate of prices in the Diamond model
#' beta.consumer[3] * (1 - beta.prod.firm) / beta.prod.firm / (1 + gr.laborer)
#'
#' # the output-labor ratios
#' ge$z[paste0("firm", 1:ng)] / rowSums(ge$S)[paste0("lab", 1:ng)]
#' # the steady-state output-labor ratio in the Diamond model
#' alpha.firm^(1 / (1 - beta.prod.firm)) * (beta.consumer[3] * (1 - beta.prod.firm) /
#'                                            (1 + gr.laborer))^(beta.prod.firm / (1 - beta.prod.firm))
#'
#' # the captial-labor ratios
#' rowSums(ge$D[paste0("prod", 1:ng), paste0("firm", 1:ng)]) /
#'   rowSums(ge$S[paste0("lab", 1:ng), paste0("consumer", 1:ng)])
#' # the steady-state captial-labor ratio in the Diamond model
#' alpha.firm^(1 / (1 - beta.prod.firm)) * (beta.consumer[3] * (1 - beta.prod.firm)
#'                                          / (1 + gr.laborer))^(1 / (1 - beta.prod.firm))
#'
#' ##
#' ng <- 15
#' alpha.firm <- 2
#' beta.prod.firm <- 0.5
#' beta.consumer <- c(1, 1, 1) / 3
#' labor.first <- c(100, 100, 0)
#' labor.last <- c(100, 100)
#' y1 <- 8
#' gr.laborer <- 0
#' f()
#'
#' #### Assume that consumers live for two periods and consume labor (i.e., leisure).
#' ng <- 10 # the number of generations
#' alpha.firm <- 2 # the efficient parameter of firms
#' beta.prod.firm <- 0.5 # the product (i.e. capital) share parameter of firms
#' beta.consumer <- prop.table(c(
#'   lab1 = 1, lab2 = 1,
#'   prod1 = 1, prod2 = 1
#' )) # the share parameter of consumers
#' labor <- c(100, 0) # the labor supply of each generation
#' y1 <- 8 # the initial product supply
#'
#' names.commodity <- c(paste0("lab", 1:(ng + 1)), paste0("prod", 1:(ng + 1)))
#' names.agent <- c(paste0("consumer", 1:ng), paste0("firm", 1:ng))
#'
#' n <- length(names.commodity) # the number of commodity kinds
#' m <- length(names.agent) # the number of agent kinds
#'
#' # the exogenous supply matrix.
#' S0Exg <- matrix(NA, n, m, dimnames = list(names.commodity, names.agent))
#' for (k in 1:ng) {
#'   S0Exg[paste0("lab", k:(k + 1)), paste0("consumer", k)] <- labor
#' }
#' if (labor[2] == 0) S0Exg[paste0("lab", ng + 1), paste0("consumer", ng)] <- labor[1]
#' S0Exg["prod1", "consumer1"] <- y1
#'
#'
#' B <- matrix(0, n, m, dimnames = list(names.commodity, names.agent))
#' for (k in 1:ng) {
#'   B[paste0("prod", k + 1), paste0("firm", k)] <- 1
#' }
#'
#' dstl.consumer <- list()
#' for (k in 1:ng) {
#'   dstl.consumer[[k]] <- node_new(
#'     "util",
#'     type = "CD", alpha = 1,
#'     beta = beta.consumer, # prop.table(c(1e-5,1e-5,0.5,0.5)),
#'     paste0("lab", k:(k + 1)), paste0("prod", k:(k + 1))
#'   )
#' }
#'
#' # Assume that consumers live for three periods.
#' # dstl.consumer <- list()
#' # for (k in 1:(ng - 1)) {
#' #   dstl.consumer[[k]] <- node_new(
#' #     "util",
#' #     type = "CD", alpha = 1,
#' #     beta = rep(1 / 6, 6),
#' #     paste0("lab", k:(k + 2)), paste0("prod", k:(k + 2))
#' #   )
#' # }
#' #
#' # dstl.consumer[[ng]] <- node_new(
#' #   "util",
#' #   type = "CD", alpha = 1,
#' #   beta = rep(1 / 4, 4),
#' #   paste0("lab", ng:(ng + 1)), paste0("prod", ng:(ng + 1))
#' # )
#'
#' dstl.firm <- list()
#' for (k in 1:ng) {
#'   dstl.firm[[k]] <- node_new(
#'     "prod",
#'     type = "CD", alpha = alpha.firm,
#'     beta = c(1 - beta.prod.firm, beta.prod.firm),
#'     paste0("lab", k), paste0("prod", k)
#'   )
#' }
#'
#' ge <- sdm2(
#'   A = c(dstl.consumer, dstl.firm),
#'   B = B,
#'   S0Exg = S0Exg,
#'   names.commodity = names.commodity,
#'   names.agent = names.agent,
#'   numeraire = "lab1"
#' )
#'
#' ge$z
#' growth_rate(ge$p[paste0("prod", 1:ng)]) + 1
#' growth_rate(ge$p[paste0("lab", 1:ng)]) + 1
#' ge$p[paste0("prod", 1:ng)] / ge$p[paste0("lab", 1:ng)]
#' }

gemOLG_Basic <- function(...) sdm2(...)
