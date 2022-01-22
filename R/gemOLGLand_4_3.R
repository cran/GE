#' @export
#' @title An Overlapping Generations Model with Land
#' @aliases gemOLGLand_4_3
#' @description Some examples of an overlapping generations model with land.
#' @param ... arguments to be passed to the function sdm2.
#' @details
#' In this model, consumers live for two periods. Age2 owns a unit of land and age1 owns a unit of labor.
#' Here the land use rights and land ownership are regarded as two commodities.
#' Age2 gets land rent by selling land use rights to the firm.
#' Then land ownership is sold to age1. Age1 saves by purchasing land ownership.
#'
#' Here, the ratio of land rent to wage (denoted as xi) is determined by the production function.
#' No matter what the saving rate of age1 is, and at what price the land ownership is transferred to age1,
#' the consumption ratio of age2 and age1 will not be less than xi.
#'
#' When the consumer's intertemporal utility function is min(c1, c2),
#' the intertemporal substitution elasticity is 0,
#' and each consumer desires the same amount of consumption in the two periods.
#' It can also be assumed that the consumer's intertemporal substitution elasticity is close to 0 rather than exactly zero.
#'
#' If xi>1, then this economy will inevitably have efficiency loss (dynamic inefficiency).
#' In this case, in order to achieve the optimal allocation, not only age2 should give land to age1 for free,
#' but age2 also needs to distribute part of the land rent to age1.
#' However, age2 has no incentive to do so.
#' @references Rhee, Changyong (1991) Dynamic Inefficiency in an Economy with Land. Review of Economic Studies. 58(4), pp:791-797.
#' @seealso {
#' \code{\link{gemOLGPureExchange_2_2}}
#' }
#' @examples
#' \donttest{
#' saving.rate <- 0.001
#' ratio.saving.consumption <- saving.rate / (1 - saving.rate)
#'
#' dst.firm <- node_new(
#'   "prod",
#'   type = "CD", alpha = 5,
#'   beta = c(1 / 6, 2 / 6, 3 / 6),
#'   "lab", "prod", "land.use.rights"
#' )
#'
#' dst.age1 <- node_new(
#'   "util",
#'   type = "FIN",
#'   rate = c(1, ratio.saving.consumption),
#'   "prod", "land.ownership"
#' )
#'
#' dst.age2 <- node_new(
#'   "util",
#'   type = "Leontief", a = 1,
#'   "prod"
#' )
#'
#' ge <- sdm2(
#'   A = list(
#'     dst.firm, dst.age1, dst.age2
#'   ),
#'   B = matrix(c(
#'     1, 0, 0,
#'     0, 0, 0,
#'     0, 0, 0,
#'     0, 0, 0
#'   ), 4, 3, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA, NA,
#'     NA, 1, NA,
#'     NA, NA, 1,
#'     NA, NA, 1
#'   ), 4, 3, TRUE),
#'   names.commodity = c("prod", "lab", "land.use.rights", "land.ownership"),
#'   names.agent = c("firm", "age1", "age2"),
#'   numeraire = "lab"
#' )
#'
#' ge$p
#' ge$D
#' ge$DV
#' ge$S
#' ge$SV
#'
#' ## Change the saving-consumption ratio.
#' dst.age1$rate <- c(1, ratio.saving.consumption = 99)
#'
#' ge <- sdm2(
#'   A = list(
#'     dst.firm, dst.age1, dst.age2
#'   ),
#'   B = matrix(c(
#'     1, 0, 0,
#'     0, 0, 0,
#'     0, 0, 0,
#'     0, 0, 0
#'   ), 4, 3, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA, NA,
#'     NA, 1, NA,
#'     NA, NA, 1,
#'     NA, NA, 1
#'   ), 4, 3, TRUE),
#'   names.commodity = c("prod", "lab", "land.use.rights", "land.ownership"),
#'   names.agent = c("firm", "age1", "age2"),
#'   numeraire = "lab"
#' )
#'
#' ge$p
#' ge$D
#' ge$DV
#' ge$S
#' ge$SV
#' }
#'

gemOLGLand_4_3 <- function(...) sdm2(...)
