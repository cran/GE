#' @export
#' @title Some General Equilibrium Models with Skill (i.e. Human Capital)
#' @aliases gemSkill
#' @description Some general equilibrium models with skill (i.e. human capital).
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' depreciation.rate <- 0.05 # the depreciation rate of skill
#' skill.density <- 10
#' relative.efficiency.coef <- 2
#' efficiency.coef <- skill.density * relative.efficiency.coef
#'
#' dst.efficiency.unit <- node_new("efficiency unit",
#'   type = "Leontief",
#'   a =  1 / efficiency.coef,
#'   "complex labor"
#' )
#'
#' dst.firm <- node_new(
#'   "product",
#'   type = "SCES", alpha = 1,
#'   beta = c(0.4, 0.6), es = 0.5,
#'   "product", "labor"
#' )
#' node_set(dst.firm, "labor",
#'   type = "SCES", alpha = 1,
#'   beta = c(0.5, 0.5), es = 1.5,
#'   "simple labor", dst.efficiency.unit
#' )
#'
#' dst.school <- node_new(
#'   "skill",
#'   type = "Leontief",
#'   a = c(0.1, 1, 0.1),
#'   "product", "simple labor", dst.efficiency.unit
#' )
#'
#' dst.complex.laborer <- node_new(
#'   "complex labor",
#'   type = "Leontief", a = c(skill.density, 1),
#'   "skill", "simple labor"
#' )
#'
#' dst.simple.laborer <- node_new(
#'   "util",
#'   type = "Leontief", a = 1,
#'   "product"
#' )
#'
#' ge <- sdm2(
#'   A = list(dst.firm, dst.school, dst.complex.laborer, dst.simple.laborer),
#'   B = matrix(c(
#'     1, 0, 0, 0,
#'     0, 1, skill.density * (1 - depreciation.rate), 0,
#'     0, 0, 1, 0,
#'     0, 0, 0, 0
#'   ), 4, 4, TRUE),
#'   S0Exg = {
#'     tmp <- matrix(NA, 4, 4)
#'     tmp[4, 4] <- 100
#'     tmp
#'   },
#'   names.commodity = c("product", "skill", "complex labor", "simple labor"),
#'   names.agent = c("firm", "school", "complex laborer", "simple laborer"),
#'   numeraire = "simple labor",
#'   policy = makePolicyMeanValue(50),
#'   priceAdjustmentVelocity = 0.05,
#'   maxIteration = 1,
#'   numberOfPeriods = 1000,
#'   ts = TRUE
#' )
#'
#' ge$p
#' ge$z
#' addmargins(ge$D, 2)
#' addmargins(ge$S, 2)
#' matplot(ge$ts.p, type = "l")
#'
#' #### Assumed that the amount of education and training purchased by laborers is
#' ## determined primarily by their preferences rather than their investment motives.
#' depreciation.rate <- 0.05
#' skill.density <- 10
#' relative.efficiency.coef <- 2
#' efficiency.coef <- skill.density * relative.efficiency.coef
#'
#' dst.efficiency.unit <- node_new("efficiency unit",
#'   type = "Leontief",
#'   a = c(skill.density / efficiency.coef, 1 / efficiency.coef),
#'   "skill service", "simple labor"
#' )
#'
#' dst.firm <- node_new(
#'   "product",
#'   type = "SCES", alpha = 1,
#'   beta = c(0.4, 0.6), es = 0.5,
#'   "product", "labor"
#' )
#' node_set(dst.firm, "labor",
#'   type = "SCES", alpha = 1,
#'   beta = c(0.5, 0.5), es = 1.5,
#'   "simple labor", dst.efficiency.unit
#' )
#'
#' dst.school <- node_new(
#'   "skill",
#'   type = "Leontief",
#'   a = c(0.1, 1, 0.1),
#'   "product", "simple labor", dst.efficiency.unit
#' )
#'
#' dst.laborer <- node_new(
#'   "util",
#'   type = "CD", alpha = 1,
#'   beta = c(0.7887, 0.2113),
#'   # beta <- c(0.9, 0.1),
#'   # beta <- c(0.6, 0.4),
#'   "product", "skill",
#'   skill.stock = 0
#' )
#'
#' ge <- sdm2(
#'   A = list(dst.firm, dst.school, dst.laborer),
#'   B = matrix(c(
#'     1, 0, 0,
#'     0, 1, 0,
#'     0, 0, 0,
#'     0, 0, 0
#'   ), 4, 3, TRUE),
#'   S0Exg = {
#'     tmp <- matrix(NA, 4, 3)
#'     tmp[4, 3] <- 100
#'     tmp
#'   },
#'   names.commodity = c("product", "skill", "skill service", "simple labor"),
#'   names.agent = c("firm", "school", "laborer"),
#'   numeraire = "simple labor",
#'   policy = function(A, state) {
#'     last.D <- state$last.A %*% dg(state$last.z)
#'     new.skill <- last.D[2, 3]
#'     state$S[3, 3] <- A[[3]]$skill.stock <-
#'       A[[3]]$skill.stock * (1 - depreciation.rate) + new.skill
#'     state
#'   },
#'   priceAdjustmentVelocity = 0.05,
#'   maxIteration = 1,
#'   numberOfPeriods = 1000,
#'   ts = TRUE
#' )
#'
#' ge$p
#' ge$z
#' addmargins(ge$D, 2)
#' addmargins(ge$S, 2)
#' matplot(log(ge$ts.p), type = "l")
#' }

gemSkill <- function(...) sdm2(...)
