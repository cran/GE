#' @export
#' @title Make a Policy of Income Tax
#' @aliases makePolicyIncomeTax
#' @description This function returns a policy function that redistributes the supplies of economic agents,
#' and the effect is equivalent to the collection of income tax.
#' @param agent a vector specifying the indices or names of taxed agents.
#' @param tax.rate a vector specifying the income tax rates for agents, which has the same length with the argument agent.
#' @param redistribution a vector specifying the proportions of tax revenue received by agents, which has the same length with the argument agent.
#' @param time.win the time window vector, i.e. a 2-vector specifying the start time and end time of policy implementation.
#' @return A policy function, which is often used as an argument of the function sdm2.
#' @seealso \code{\link{gemTax_5_4}}
#' @references Manuel Alejandro Cardenete, Ana-Isabel Guerra, Ferran Sancho (2012, ISBN: 9783642247453) Applied General Equilibrium: An Introduction. Springer-Verlag Berlin Heidelberg.
#' @examples
#' \donttest{
#' ## an exmaple of income tax (see Cardenete et al., 2012, Table 4.3)
#' dst.consumer1 <- node_new("utility",
#'                           type = "CD",
#'                           alpha = 1,
#'                           beta = c(0.3, 0.7),
#'                           "prod1", "prod2"
#' )
#'
#' dst.consumer2 <- Clone(dst.consumer1)
#' dst.consumer2$beta <- c(0.6, 0.4)
#'
#' dst.firm1 <- node_new("output",
#'                       type = "Leontief",
#'                       a = c(0.5, 0.2, 0.3),
#'                       "VA", "prod1", "prod2"
#' )
#' node_set(dst.firm1, "VA",
#'          type = "CD",
#'          alpha = 0.8^-0.8 * 0.2^-0.2,
#'          beta = c(0.8, 0.2),
#'          "lab", "cap"
#' )
#'
#' dst.firm2 <- Clone(dst.firm1)
#' node_set(dst.firm2, "output",
#'          a = c(0.25, 0.5, 0.25)
#' )
#' node_set(dst.firm2, "VA",
#'          alpha = 0.4^-0.4 * 0.6^-0.6,
#'          beta = c(0.4, 0.6)
#' )
#' dstl <- list(dst.firm1, dst.firm2, dst.consumer1, dst.consumer2)
#' ge <- sdm2(dstl,
#'   names.commodity = c("prod1", "prod2", "lab", "cap"),
#'   names.agent = c("firm1", "firm2", "consumer1", "consumer2"),
#'   numeraire = "lab",
#'   B = {
#'     tmp <- matrix(0, 4, 4)
#'     tmp[1, 1] <- 1
#'     tmp[2, 2] <- 1
#'     tmp
#'   },
#'   S0Exg = {
#'     tmp <- matrix(NA, 4, 4)
#'     tmp[3:4, 3] <- c(30, 20)
#'     tmp[3:4, 4] <- c(20, 5)
#'     tmp
#'   },
#'   maxIteration = 1,
#'   policy = makePolicyIncomeTax(
#'     agent = c(3, 4),
#'     tax.rate = c(0.2, 0.2),
#'     redistribution = c(0.5, 0.5)
#'   )
#' )
#' }
#'
makePolicyIncomeTax <- function(agent,
                                tax.rate,
                                redistribution,
                                time.win = c(1, Inf)) {
  redistribution <- prop.table(redistribution)

  function(time, state) {
    if (is.character(agent)) {
      agent.index <- match(agent, state$names.agent)
    } else {
      agent.index <- agent
    }

    tax <- state$p * 0
    if (time >= time.win[1] && time <= time.win[2]) {
      for (k in seq_along(agent.index)) {
        tax <- tax + state$S[, agent.index[k]] * tax.rate[k]
        state$S[, agent.index[k]] <- state$S[, agent.index[k]] * (1 - tax.rate[k])
      }

      for (k in seq_along(agent.index)) {
        state$S[, agent.index[k]] <- state$S[, agent.index[k]] + tax * redistribution[k]
      }
    }

    state
  }
}



