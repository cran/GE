#' @export
#' @title Some Examples of Market-Clearing Paths Illustrating Research and Development Intensity
#' @aliases gemResearchDevelopmentIntensity
#' @description Some examples of market-clearing paths illustrating R&D intensity.
#' R&D intensity of a firm is the ratio of expenditures by the firm on R&D to the firm's sales.
#' @param ... arguments to be passed to the function sdm2.
#' @details The first example contains two kinds of commodities (namely product and labor) and three economic agents
#' (namely a firm, a R&D center of the firm and a laborer).
#' Since the R&D center does not produce products, the R&D center is regarded as a consumer-type agent in the model.
#' The utility level of the R&D center (that is, the R&D level) will affect the technological progress rate of the firm.
#' In the model, the firm allocates part of its output to the R&D centers for sale according to a given R&D intensity,
#' which is equivalent to allocating part of the firm's sales revenue to the R&D center.
#' At first, the economy is set in steady-state equilibrium without R&D activity. R&D activities begin in the fifth period.
#' @return  A market-clearing path.
#' @examples
#' \donttest{
#' #### a 2-by-3 example.
#' RDIntensity <- 0.3
#' RDEffectivenessCoefficient <- 0.001
#'
#' dst.firm <- node_new(
#'   "prod",
#'   type = "CD",
#'   alpha = 2, beta = c(0.5, 0.5),
#'   "prod", "cc1"
#' )
#' node_set(dst.firm, "cc1",
#'   type = "Leontief", a = 1,
#'   "lab"
#' )
#'
#' dst.RDCenter <- node_new(
#'   "util",
#'   type = "CD",
#'   alpha = 1, beta = c(0.5, 0.5),
#'   "prod", "lab"
#' )
#'
#' dst.laborer <- node_new(
#'   "util",
#'   type = "Leontief",
#'   a = 1,
#'   "prod"
#' )
#'
#' # a function calculating the rate of technological progress according to the level of R&D.
#' f.TPR <- function(RDLevel) RDEffectivenessCoefficient * RDLevel
#'
#' f <- function() {
#'   sdm2(
#'     A = list(dst.firm, dst.RDCenter, dst.laborer),
#'     B = matrix(c(
#'       1, 0, 0,
#'       0, 0, 0
#'     ), 2, 3, TRUE),
#'     S0Exg = matrix(c(
#'       NA, NA, NA,
#'       NA, NA, 100
#'     ), 2, 3, TRUE),
#'     names.commodity = c("prod", "lab"),
#'     names.agent = c("firm", "RDCenter", "laborer"),
#'     numeraire = "prod",
#'     z0 = c(200, 0, 100),
#'     policy = list(
#'       function(time, A, state) {
#'         if (time >= 5) {
#'           state$S[1, 2] <- state$S[1, 1] * RDIntensity
#'           state$S[1, 1] <- state$S[1, 1] * (1 - RDIntensity)
#'           last.a <- node_set(A[[1]], "cc1")$a
#'           last.RDLevel <- state$last.z[2]
#'           technology.progress.rate <- f.TPR(last.RDLevel)
#'           node_set(A[[1]], "cc1", a = last.a / (1 + technology.progress.rate))
#'         }
#'
#'         state
#'       },
#'       policyMarketClearingPrice
#'     ),
#'     maxIteration = 1,
#'     numberOfPeriods = 50,
#'     ts = TRUE
#'   )
#' }
#'
#' ge <- f()
#' matplot((ge$ts.z), type = "o", pch = 20)
#' ge$z
#'
#' ## change the R&D intensity.
#' node_set(dst.firm, "cc1", a = 1)
#' RDIntensity <- 0.8
#' ge <- f()
#' matplot((ge$ts.z), type = "o", pch = 20)
#' ge$z
#'
#' ## random rate of technological progress.
#' set.seed(1)
#' RDIntensity <- 0.3
#' node_set(dst.firm, "cc1", a = 1)
#' f.TPR <- function(RDLevel) max(0, rnorm(1, RDEffectivenessCoefficient * RDLevel,
#'   sqrt(RDEffectivenessCoefficient * RDLevel)))
#' ge <- f()
#' matplot((ge$ts.z), type = "o", pch = 20)
#' ge$z
#'
#' ## two firms with different R&D intensity.
#' node_set(dst.firm, "cc1", a = 1)
#' RDIntensity1 <- 0.1
#' RDIntensity2 <- 0.05
#' RDEffectivenessCoefficient <- 0.002
#'
#' dst.firm2 <- Clone(dst.firm)
#' dst.RDCenter2 <- Clone(dst.RDCenter)
#' ge <- sdm2(
#'   A = list(dst.firm, dst.RDCenter, dst.laborer, dst.firm2, dst.RDCenter2),
#'   B = matrix(c(
#'     1, 0, 0, 1, 0,
#'     0, 0, 0, 0, 0
#'   ), 2, 5, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA, NA, NA, NA,
#'     NA, NA, 200, NA, NA
#'   ), 2, 5, TRUE),
#'   names.commodity = c("prod", "lab"),
#'   names.agent = c("firm1", "RDCenter1", "laborer", "firm2", "RDCenter2"),
#'   numeraire = "prod",
#'   z0 = c(200, 0, 200, 200, 0),
#'   policy = list(
#'     function(time, A, state) {
#'       if (time >= 5) {
#'         state$S[1, 2] <- state$S[1, 1] * RDIntensity1
#'         state$S[1, 1] <- state$S[1, 1] * (1 - RDIntensity1)
#'         last.a1 <- node_set(A[[1]], "cc1")$a
#'         last.RDLevel1 <- state$last.z[2]
#'         technology.progress.rate1 <- RDEffectivenessCoefficient * last.RDLevel1
#'         node_set(A[[1]], "cc1", a = last.a1 / (1 + technology.progress.rate1))
#'
#'         state$S[1, 5] <- state$S[1, 4] * RDIntensity2
#'         state$S[1, 4] <- state$S[1, 4] * (1 - RDIntensity2)
#'         last.a2 <- node_set(A[[4]], "cc1")$a
#'         last.RDLevel2 <- state$last.z[5]
#'         technology.progress.rate2 <- RDEffectivenessCoefficient * last.RDLevel2
#'         node_set(A[[4]], "cc1", a = last.a2 / (1 + technology.progress.rate2))
#'       }
#'
#'       state
#'     },
#'     policyMarketClearingPrice
#'   ),
#'   maxIteration = 1,
#'   numberOfPeriods = 50,
#'   ts = TRUE
#' )
#'
#' matplot((ge$ts.z), type = "o", pch = 20)
#' ge$z
#'
#' ## Assume that the R&D center is owned by the government and
#' ## receives revenue through taxation on firms.
#' ## The technologies developed by the R&D center are public goods.
#' node_set(dst.firm, "cc1", a = 1)
#' RDIntensity <- 0.1
#' RDEffectivenessCoefficient <- 0.002
#'
#' dst.firm2 <- Clone(dst.firm)
#'
#' ge <- sdm2(
#'   A = list(dst.firm, dst.RDCenter, dst.laborer, dst.firm2),
#'   B = matrix(c(
#'     1, 0, 0, 1,
#'     0, 0, 0, 0
#'   ), 2, 4, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA, NA, NA,
#'     NA, NA, 200, NA
#'   ), 2, 4, TRUE),
#'   names.commodity = c("prod", "lab"),
#'   names.agent = c("firm1", "RDCenter", "laborer", "firm2"),
#'   numeraire = "prod",
#'   z0 = c(200, 0, 200, 200),
#'   policy = list(
#'     function(time, A, state) {
#'       if (time >= 5) {
#'         last.RDLevel <- state$last.z[2]
#'         technology.progress.rate <- RDEffectivenessCoefficient * last.RDLevel
#'
#'         state$S[1, 2] <- (state$S[1, 1] + state$S[1, 4]) * RDIntensity
#'         state$S[1, 1] <- state$S[1, 1] * (1 - RDIntensity)
#'         state$S[1, 4] <- state$S[1, 4] * (1 - RDIntensity)
#'         last.a1 <- node_set(A[[1]], "cc1")$a
#'         node_set(A[[1]], "cc1", a = last.a1 / (1 + technology.progress.rate))
#'
#'         last.a2 <- node_set(A[[4]], "cc1")$a
#'         node_set(A[[4]], "cc1", a = last.a2 / (1 + technology.progress.rate))
#'       }
#'
#'       state
#'     },
#'     policyMarketClearingPrice
#'   ),
#'   maxIteration = 1,
#'   numberOfPeriods = 30,
#'   ts = TRUE
#' )
#'
#' matplot((ge$ts.z), type = "o", pch = 20)
#' ge$z
#' }

gemResearchDevelopmentIntensity <- function(...) sdm2(...)
