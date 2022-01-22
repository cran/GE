#' @export
#' @title Overlapping Generations Financial Models for Pure Exchange Economies
#' @aliases gemOLGFPureExchange
#' @description Some examples of overlapping generations models with financial instrument for pure exchange economies.
#'
#' In these examples, there is a financial instrument (namely security) which serves as saving means and can be regarded as money, the shares of a firm, etc.
#' Consumers use this security for saving, and this is the only use of the security.
#' As Samuelson (1958) wrote, society by using money (i.e. security) will go from the non-optimal configuration
#' to the optimal configuration.
#'
#' Here financial demand structure trees are used, which contain financial nodes.
#' A financial demand structure tree reflects the demand structure of a consumer
#' who has a demand for financial instruments.
#' Although CD-type nodes can be used instead of financial-type nodes in the consumer's demand structure tree,
#' the use of financial-type nodes will make the demand structure tree easier to understand.
#'
#' When there is a population growth, we will take the security-split assumption.
#' That is, assume that in each period the security will be split just like share split,
#' and the growth rate of the quantity of the security is equal to the growth rate of the population.
#' Obviously, this assumption will not affect the calculation results essentially.
#' And with this assumption, the equilibrium price vector can keep constant in each period, and
#' the nominal rates of profit and interest will equal the real rates of profit and interest (i.e. the population growth rate).
#' In contrast, in the time circle model the nominal rates of profit and interest equal zero and the real rates of profit and interest
#' equal the population growth rate.
#' @param ... arguments to be passed to the function sdm2.
#' @note As can be seen from the first example below, in a pure exchange economy with two-period-lived consumers,
#' if age1 (i.e. young) has one unit of labor and age2 (i.e. old) does not,
#' then the optimal allocation can be obtained by introducing securities.
#' Here it is assumed that each consumer consumes one unit of labor in total.
#'
#' However, if age2 (i.e. adult) has one unit of labor and age1 (i.e. child) does not,
#' we cannot get the optimal allocation by introducing securities.
#' So we need the family system.
#' @seealso {
#' \code{\link{gemOLGPureExchange_2_2}}
#' }
#' @examples
#' \donttest{
#' #### an OLGF pure exchange economy with two-period-lived consumers
#' ## Suppose each consumer has one unit of labor in her first period
#' ## and she has a constant saving rate (i.e. a log instantaneous utility
#' ## function, a C-D type intertemporal utility function).
#' saving.rate <- 0.5
#' ratio.saving.consumption <- saving.rate / (1 - saving.rate)
#'
#' dst.age1 <- node_new(
#'   "util",
#'   type = "FIN",
#'   rate = c(1, ratio.saving.consumption),
#'   "lab", "secy" # security, the financial instrument
#' )
#'
#' dst.age2 <- node_new(
#'   "util",
#'   type = "Leontief", a = 1,
#'   "lab"
#' )
#'
#' ge <- sdm2(
#'   A = list(
#'     dst.age1, dst.age2
#'   ),
#'   B = matrix(0, 2, 2, TRUE),
#'   S0Exg = matrix(c(
#'     1, NA,
#'     NA, 1
#'   ), 2, 2, TRUE),
#'   names.commodity = c("lab", "secy"),
#'   names.agent = c("age1", "age2"),
#'   numeraire = "lab"
#' )
#'
#' ge$p
#' ge$D
#' ge$DV
#' ge$S
#'
#' #### the basic overlapping generations (inefficient) exchange model
#' ## Here the lab2 is regarded as a financial instrument (saving instrument).
#' ## See gemOLGPureExchange_2_2.
#' dst.age1 <- node_new(
#'   "util",
#'   type = "FIN",
#'   rate = c(1, ratio.totalSaving.consumption = 2),
#'   "lab1", "lab2"
#' )
#'
#' dst.age2 <- node_new(
#'   "util",
#'   type = "FIN",
#'   rate = c(1, ratio.saving.consumption = 1),
#'   "lab1", "lab2"
#' )
#'
#' ge <- sdm2(
#'   A = list(dst.age1, dst.age2),
#'   B = matrix(0, 2, 2),
#'   S0Exg = matrix(c(
#'     1, 1,
#'     1, 0
#'   ), 2, 2, TRUE),
#'   names.commodity = c("lab1", "lab2"),
#'   names.agent = c("age1", "age2"),
#'   numeraire = "lab1",
#'   policy = function(time, state) {
#'     pension <- (state$last.A[, 2] * state$last.z[2])[2]
#'     if (time > 1) state$S[1, 2] <- 1 - pension
#'     state
#'   }
#' )
#'
#' ge$p
#' ge$S
#' ge$D
#' ge$DV
#'
#' #### the basic financial overlapping generations exchange model (see Samuelson, 1958)
#' ## Suppose each consumer has a utility function log(c1) + log(c2) + log(c3).
#' GRExg <- 0.03 # the population growth rate
#' rho <- 1 / (1 + GRExg)
#'
#' dst.age1 <- node_new(
#'   "util",
#'   type = "FIN",
#'   rate = {
#'     saving.rate <- (2 - rho) / 3
#'     c(1, ratio.saving.consumption = saving.rate / (1 - saving.rate))
#'   },
#'   "lab", "secy"
#' )
#'
#' dst.age2 <- node_new(
#'   "util",
#'   type = "FIN",
#'   rate = c(1, ratio.saving.consumption = 1),
#'   "lab", "secy"
#' )
#'
#' dst.age3 <- node_new(
#'   "util",
#'   type = "Leontief", a = 1,
#'   "lab"
#' )
#'
#' ge <- sdm2(
#'   A = list(dst.age1, dst.age2, dst.age3),
#'   B = matrix(0, 2, 3),
#'   S0Exg = matrix(c(
#'     1 + GRExg, 1, 0,
#'     0, 0.5, 0.5
#'   ), 2, 3, TRUE),
#'   names.commodity = c("lab", "secy"),
#'   names.agent = c("age1", "age2", "age3"),
#'   numeraire = "lab",
#'   policy = function(time, state) {
#'     # Assume that unsold security will be void.
#'     last.Demand <- state$last.A %*% dg(state$last.z)
#'     secy.holding <- prop.table(last.Demand[2, ])
#'     if (time > 1) {
#'       state$S[2, 2:3] <- secy.holding[1:2]
#'     }
#'     state
#'   }
#' )
#'
#' ge$p
#' ge$S
#' ge$D
#'
#' #### a pure exchange economy with three-period-lived consumers
#' ## Suppose each consumer has a Leontief-type utility function min(c1, c2, c3).
#' GRExg <- 0.03 # the population growth rate
#' R <- 1 + GRExg
#'
#' dst.age1 <- node_new(
#'   "util",
#'   type = "FIN",
#'   rate = {
#'     saving.rate <- 1 / (1 + R + R^2)
#'     c(1, ratio.saving.consumption = saving.rate / (1 - saving.rate))
#'   },
#'   "lab", "secy"
#' )
#'
#' dst.age2 <- node_new(
#'   "util",
#'   type = "FIN",
#'   rate = {
#'     saving.rate <- 1 / (1 + R)
#'     c(1, ratio.saving.consumption = saving.rate / (1 - saving.rate))
#'   },
#'   "lab", "secy"
#' )
#'
#' dst.age3 <- node_new(
#'   "util",
#'   type = "Leontief", a = 1,
#'   "lab"
#' )
#'
#' ge <- sdm2(
#'   A = list(dst.age1, dst.age2, dst.age3),
#'   B = matrix(0, 2, 3),
#'   S0Exg = matrix(c(
#'     1 + GRExg, 1, 0,
#'     0, 0.5, 0.5
#'   ), 2, 3, TRUE),
#'   names.commodity = c("lab", "secy"),
#'   names.agent = c("age1", "age2", "age3"),
#'   numeraire = "lab",
#'   policy = function(time, state) {
#'     # Assume that unsold security will be void.
#'     last.Demand <- state$last.A %*% dg(state$last.z)
#'     secy.holding <- prop.table(last.Demand[2, ])
#'     if (time > 1) {
#'       state$S[2, 2:3] <- secy.holding[1:2]
#'     }
#'     state
#'   }
#' )
#'
#' ge$p
#' ge$S
#' ge$D
#'
#' ## Assume that the unsold security of age3 will be void.
#' ## The calculation results are the same as above.
#' ge <- sdm2(
#'   A = list(dst.age1, dst.age2, dst.age3),
#'   B = matrix(0, 2, 3),
#'   S0Exg = matrix(c(
#'     1 + GRExg, 1, 0,
#'     0, 0.5, 0.5
#'   ), 2, 3, TRUE),
#'   names.commodity = c("lab", "secy"),
#'   names.agent = c("age1", "age2", "age3"),
#'   numeraire = "lab",
#'   policy = function(time, state, state.history) {
#'     secy.unsold <- state.history$S[2, , time - 1] * (1 - state.history$q[time - 1, 2])
#'     last.Demand <- state$last.A %*% dg(state$last.z)
#'     secy.purchased <- last.Demand[2, ]
#'
#'     if (time > 1) {
#'       # Assume that the unsold security of age3 will be void.
#'       state$S[2, 2:3] <- prop.table(secy.purchased[1:2] + secy.unsold[1:2])
#'     }
#'     state
#'   },
#'   maxIteration = 1
#' )
#'
#' ge$p
#' ge$S
#' ge$D
#' }
#'

gemOLGFPureExchange <- function(...) sdm2(...)
