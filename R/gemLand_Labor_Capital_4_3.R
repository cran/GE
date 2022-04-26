#' @export
#' @title Some Examples of Market Clearing Paths Involving Land, Labor and Capital
#' @aliases gemLand_Labor_Capital_4_3
#' @description Some examples of market clearing paths involving land, labor and capital.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' depreciation.rate <- 0.05
#'
#' dst.firm.production <- node_new("prod",
#'   type = "CD",
#'   alpha = 1, beta = c(0.4, 0.4, 0.2),
#'   "lab", "cap", "land"
#' )
#'
#' dst.firm.capital.leasing <- node_new("cap",
#'   type = "Leontief", a = 1,
#'   "prod"
#' )
#'
#' dst.consumer <- node_new("util",
#'   type = "CD",
#'   alpha = 1, beta = c(0.5, 0.5),
#'   "prod", "lab"
#' )
#'
#' dstl <- list(dst.firm.production, dst.consumer, dst.firm.capital.leasing)
#' f <- function(policy = policyMarketClearingPrice,
#'               p0 = c(1, 1, 1, 1),
#'               z0 = c(10, 10, 10),
#'               numberOfPeriods = 100) {
#'   sdm2(
#'     A = dstl,
#'     B = matrix(c(
#'       1, 0, 1 - depreciation.rate,
#'       0, 0, 0,
#'       0, 0, 1,
#'       0, 0, 0
#'     ), 4, 3, TRUE),
#'     S0Exg = {
#'       S0Exg <- matrix(NA, 4, 3)
#'       S0Exg[2, 2] <- S0Exg[4, 2] <- 1
#'       S0Exg
#'     },
#'     names.commodity = c("prod", "lab", "cap", "land"),
#'     names.agent = c("firm.production", "consumer", "firm.capital.leasing"),
#'     numeraire = "prod",
#'     maxIteration = 1,
#'     numberOfPeriods = numberOfPeriods,
#'     p0 = p0,
#'     z0 = z0,
#'     policy = policy,
#'     ts = TRUE
#'   )
#' }
#'
#' ge1 <- f()
#' ge1$p
#' ge1$DV
#' ge1$SV
#' matplot(ge1$ts.z, type = "l")
#'
#' ## a market clearing path with population growth
#' policy.population.growth <- function(time, state) {
#'   if (time >= 5) {
#'     state$S[2, 2] <- 1.01^(time - 4)
#'   }
#'   state
#' }
#'
#' ge2 <- f(
#'   policy = list(
#'     policy.population.growth,
#'     policyMarketClearingPrice
#'   ),
#'   p0 = ge1$p, z0 = ge1$z,
#'   numberOfPeriods = 30
#' )
#' matplot(ge2$ts.z, type = "o", pch = 40)
#' matplot(growth_rate(ge2$ts.z), type = "o", pch = 20)
#'
#' ## a market clearing path with technology progress
#' policy.technology.progress <- function(time, A) {
#'   if (time >= 5) {
#'     A[[1]]$alpha <- 1.02^(time - 4)
#'   }
#' }
#'
#' ge3 <- f(
#'   policy = list(
#'     policy.technology.progress,
#'     policyMarketClearingPrice
#'   ),
#'   p0 = ge1$p, z0 = ge1$z,
#'   numberOfPeriods = 30
#' )
#'
#' matplot(ge3$ts.z, type = "o", pch = 20)
#' matplot(growth_rate(ge3$ts.z), type = "o", pch = 20)
#'
#' ## a market clearing path with population growth and technology progress
#' ge4 <- f(
#'   policy = list(
#'     policy.population.growth,
#'     policy.technology.progress,
#'     policyMarketClearingPrice
#'   ),
#'   p0 = ge1$p, z0 = ge1$z,
#'   numberOfPeriods = 30
#' )
#'
#' matplot(ge4$ts.z, type = "o", pch = 20)
#' matplot(growth_rate(ge4$ts.z), type = "o", pch = 20)
#' }

gemLand_Labor_Capital_4_3 <- function(...) sdm2(...)
