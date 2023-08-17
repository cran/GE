#' @export
#' @title Some Timeline Equilibrium Models with CESAK Production Function
#' @aliases gemCESAK_Timeline_2_2
#' @description Some timeline general equilibrium models with CESAK production function.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' np <- 5
#' gr <- 0
#' initial.product.supply <- 20
#' beta.prod.firm <- 0.35
#' eis <- 1
#' rho.beta <- 0.97
#' gr <- 0.05
#' alphaK <- 1.05
#'
#' S0Exg <- matrix(NA, 2 * np - 1, np)
#' S0Exg[(np + 1):(2 * np - 1), np] <- 100 * (1 + gr)^(0:(np - 2))
#' S0Exg[1, np] <- initial.product.supply
#'
#' B <- matrix(0, 2 * np - 1, np)
#' B[2:np, 1:(np - 1)] <- diag(np - 1)
#'
#' dstl.firm <- list()
#' for (k in 1:(np - 1)) {
#'   dstl.firm[[k]] <- node_new("output",
#'                              type = "CESAK", es = 1,
#'                              alpha = 0.1, betaK = beta.prod.firm, alphaK = alphaK,
#'                              paste0("prod", k), paste0("lab", k)
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
#'   names.commodity = c(paste0("prod", 1:np), paste0("lab", 1:(np - 1))),
#'   names.agent = c(paste0("firm", 1:(np - 1)), "consumer"),
#'   numeraire = "lab1",
#'   priceAdjustmentVelocity = 0.03
#' )
#'
#' growth_rate(ge$p[1:np])
#' growth_rate(ge$z[1:np])
#' ge$D
#'
#' #### a AK model
#' np <- 5
#' initial.product.supply <- 20
#' eis <- 1
#' rho.beta <- 1
#' alphaK <- 1.25
#'
#' S0Exg <- matrix(NA, np, np)
#' S0Exg[1, np] <- initial.product.supply
#'
#' B <- matrix(0, np, np)
#' B[2:np, 1:(np - 1)] <- diag(np - 1)
#'
#' dstl.firm <- list()
#' for (k in 1:(np - 1)) {
#'   dstl.firm[[k]] <- node_new("output",
#'                              type = "Leontief", a = 1 / alphaK,
#'                              paste0("prod", k)
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
#'   names.commodity = paste0("prod", 1:np),
#'   names.agent = c(paste0("firm", 1:(np - 1)), "consumer"),
#'   numeraire = "prod1",
#'   priceAdjustmentVelocity = 0.03,
#'   policy = makePolicyMeanValue(50),
#' )
#'
#' growth_rate(ge$p)
#' growth_rate(ge$z[1:(np - 1)])
#' ge$D
#'
#' ## Simplify the production function in the model above.
#' dstl.firm <- list()
#' for (k in 1:(np - 1)) {
#'   dstl.firm[[k]] <- node_new("output",
#'                              type = "Leontief", a = 1 / alphaK^(k),
#'                              "prod1"
#'   )
#' }
#'
#' ge2 <- sdm2(
#'   A = c(dstl.firm, dst.consumer),
#'   B = B,
#'   S0Exg = S0Exg,
#'   names.commodity = paste0("prod", 1:np),
#'   names.agent = c(paste0("firm", 1:(np - 1)), "consumer"),
#'   numeraire = "prod1",
#'   priceAdjustmentVelocity = 0.03,
#'   policy = makePolicyMeanValue(50),
#' )
#'
#' growth_rate(ge2$p)
#' growth_rate(ge2$z[1:(np - 1)])
#' ge2$D
#' }

gemCESAK_Timeline_2_2 <- function(...) sdm2(...)
