#' @export
#' @title Overlapping Generations Financial Sequential Models for Pure Exchange Economies
#' @aliases gemOLGF_PureExchange
#' @description
#' Some examples of overlapping generations sequential models with security for pure exchange economies.
#'
#' In these examples, a security (e.g., fiat money, public bond, shares of a firm, etc) serves as a means of saving (see Samuelson, 1958).
#' Consumers use these securities to transfer value across time, enabling intertemporal allocation of resources.
#'
#' Here financial demand structure trees are used, which contain financial nodes.
#' A financial demand structure tree reflects the demand structure of a consumer who has a demand for financial instruments.
#'
#' When experiencing population growth, we adopt the security-split assumption.
#' Specifically, we assume that in each period, the security undergoes a split similar to a stock split,
#' with the quantity of the security growing at a rate equal to the population growth rate.
#' Obviously, this assumption will not affect the calculation results essentially.
#' And with this assumption, the equilibrium price vector can keep constant in each period, and
#' the nominal rates of profit and interest will equal the real rates of profit and interest (i.e. the population growth rate).
#' In contrast, in the time circle model the nominal rates of profit and interest equal zero and the real rates of profit and interest
#' equal the population growth rate.

#' @param ... arguments to be passed to the function sdm2.
#' @note
#' As can be seen from the first example below, in a pure exchange economy with two-period-lived consumers,
#' if the young (i.e., age 1) has labor and the old (i.e., age 2) does not,
#' then the optimal steady-state allocation can be obtained by introducing securities as a store of value.
#'
#' However, if the old (i.e., age 2) has 1 unit of labor and the young (i.e., age 1) does not,
#' then the optimal steady-state allocation cannot be obtained by introducing securities as a store of value.
#' Therefore, we need to introduce the family system, or in other words, grant the young the right to tax the old.
#'
#' @seealso {
#' \code{\link{gemOLG_PureExchange}}
#' }
#' @examples
#' \donttest{
#' #### (A) An OLG pure exchange economy with two-period-lived consumers.
#' ## To simplify the analysis, we assume that the savings rate of the
#' ## young consumer is an exogenous value. The consumer's utility
#' ## function that corresponds to this savings rate can be constructed
#' ## based on the steady-state equilibrium results.
#'
#' sr <- 0.5
#'
#' dst.age1 <- node_new(
#'   "util",
#'   type = "FIN",
#'   rate = c(1, ratio.saving.consumption <- sr / (1 - sr)),
#'   "lab", "secy"
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
#'     100, 0,
#'     0, 100
#'   ), 2, 2, TRUE),
#'   names.commodity = c("lab", "secy"),
#'   names.agent = c("age1", "age2"),
#'   numeraire = "secy"
#' )
#'
#' ge$p
#' ge$D
#' ge$DV
#' ge$S
#'
#' ##
#' sr <- 0.375
#' dst.age1 <- node_new(
#'   "util",
#'   type = "FIN",
#'   rate = c(1, ratio.saving.consumption <- sr / (1 - sr)),
#'   "lab", "secy"
#' )
#'
#' ge <- sdm2(
#'   A = list(
#'     dst.age1, dst.age2
#'   ),
#'   B = matrix(0, 2, 2, TRUE),
#'   S0Exg = matrix(c(
#'     80, 20,
#'     0, 100
#'   ), 2, 2, TRUE),
#'   names.commodity = c("lab", "secy"),
#'   names.agent = c("age1", "age2"),
#'   numeraire = "secy"
#' )
#'
#' ge$p
#' ge$D
#' ge$DV
#' ge$S
#'
#' ##
#' sr <- 0.4
#' dst.age1 <- node_new(
#'   "util",
#'   type = "FIN",
#'   rate = c(1, ratio.saving.consumption <- sr / (1 - sr)),
#'   "lab", "secy"
#' )
#'
#' ge <- sdm2(
#'   A = list(
#'     dst.age1, dst.age2
#'   ),
#'   B = matrix(0, 2, 2, TRUE),
#'   S0Exg = matrix(c(
#'     100, 20,
#'     0, 100
#'   ), 2, 2, TRUE),
#'   names.commodity = c("lab", "secy"),
#'   names.agent = c("age1", "age2"),
#'   numeraire = "secy"
#' )
#'
#' ge$p
#' ge$D
#' ge$DV
#' ge$S
#'
#' ####  (B) The population growth and demographic dividend.
#' ## Suppose each consumer has a SCES intertemporal utility function.
#' gr.lab <- 0.03 # the growth rate of population and labor supply
#'
#' # share parameters of the SCES function
#' beta2 <- 0.4
#' beta1 <- 1 - beta2
#' es <- 0.5 # the elasticity of substitution in the SCES function
#'
#' dst.age1 <- node_new(
#'   "util",
#'   type = "FIN",
#'   rate = c(1, ratio.saving.consumption = 0.1),
#'   "lab", "secy",
#'   p.lab.last = 1,
#'   p.lab.ratio.predicted.last = 1,
#'   ts.saving.rate = numeric(0)
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
#'     100, NA,
#'     NA, 100
#'   ), 2, 2, TRUE),
#'   names.commodity = c("lab", "secy"),
#'   names.agent = c("age1", "age2"),
#'   numeraire = "secy",
#'   policy = list(function(time, A, state) {
#'     state$S[1, 1] <- 100 * (1 + gr.lab)^time
#'     p.lab.current <- state$p[1] / state$p[2]
#'
#'     lambda <- 0.6
#'     p.lab.ratio.predicted <- p.lab.current / A[[1]]$p.lab.last * lambda +
#'       A[[1]]$p.lab.ratio.predicted.last * (1 - lambda)
#'     A[[1]]$p.lab.last <- p.lab.current
#'     A[[1]]$p.lab.ratio.predicted.last <- p.lab.ratio.predicted
#'
#'     ratio.saving.consumption <- beta2 / beta1 * (p.lab.ratio.predicted)^(1 - es)
#'     A[[1]]$rate <- c(1, ratio.saving.consumption)
#'     A[[1]]$ts.saving.rate <- c(A[[1]]$ts.saving.rate, ratio.saving.consumption /
#'                                  (1 + ratio.saving.consumption))
#'
#'     state
#'   }, policyMarketClearingPrice),
#'   maxIteration = 1,
#'   numberOfPeriods = 50,
#'   ts = TRUE
#' )
#'
#' matplot(growth_rate(ge$ts.p), type = "o", pch = 20)
#' matplot(growth_rate(ge$ts.z), type = "o", pch = 20)
#' ge$p
#' dst.age1$rate[2] # beta2 / beta1 * (1 + gr.lab)^(es - 1)
#' dst.age1$p.lab.ratio.predicted.last
#'
#' plot(dst.age1$ts.saving.rate, type = "o", pch = 20)
#' tail(dst.age1$ts.saving.rate,1) # beta2 / (beta2 + beta1 * (1 + gr.lab)^(1 - es))
#'
#' #### (C) The basic overlapping generations (inefficient) exchange model.
#' ## Here the lab2 is regarded as a financial instrument (saving instrument).
#' ## See gemOLG_PureExchange.
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
#'     50, 50,
#'     50, 0
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
#' #### (D) the basic financial overlapping generations exchange model (see Samuelson, 1958).
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
#'     # Assume that unsold public bond will be void.
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
#' #### (E) a pure exchange economy with three-period-lived consumers.
#' ## Suppose each consumer has a Leontief-type utility function min(c1, c2, c3).
#' GRExg <- 0.03 # the population growth rate
#' igr <- 1 + GRExg
#'
#' dst.age1 <- node_new(
#'   "util",
#'   type = "FIN",
#'   rate = {
#'     saving.rate <- 1 / (1 + igr + igr^2)
#'     c(1, ratio.saving.consumption = saving.rate / (1 - saving.rate))
#'   },
#'   "lab", "secy"
#' )
#'
#' dst.age2 <- node_new(
#'   "util",
#'   type = "FIN",
#'   rate = {
#'     saving.rate <- 1 / (1 + igr)
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

gemOLGF_PureExchange <- function(...) sdm2(...)
