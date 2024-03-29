#' @export
#' @title A General Equilibrium Model with Tax (see Cardenete et al., 2012).
#' @aliases gemTax_5_4
#' @description A general equilibrium model with tax (see chapter 4, Cardenete et al., 2012),
#' wherein there are 5 commodities (i.e. product 1, product 2, labor, capital goods,
#' and tax receipt) and 4 agents (i.e. 2 firms and 2 consumers).
#'
#' @param dstl the demand structure tree list.
#' @param names.commodity names of commodities.
#' @param names.agent names of agents.
#' @param delta the proportion of tax revenue allocated to consumer 1.
#' 1-delta is the proportion of tax revenue allocated to consumer 2.
#' @param supply.lab.consumer1 the labor supply of consumer 1.
#' @param supply.cap.consumer1 the capital supply of consumer 1.
#' @param supply.lab.consumer2 the labor supply of consumer 2.
#' @param supply.cap.consumer2 the capital supply of consumer 2.
#' @param policy.tax a tax policy function (see \code{\link{sdm2}}).
#' @return A general equilibrium (see \code{\link{sdm2}}), wherein labor is the numeraire.
#' @references Manuel Alejandro Cardenete, Ana-Isabel Guerra, Ferran Sancho (2012, ISBN: 9783642247453) Applied General Equilibrium: An Introduction. Springer-Verlag Berlin Heidelberg.
#' @examples
#' \donttest{
#' dst.consumer1 <- node_new("utility",
#'   type = "CD",
#'   alpha = 1,
#'   beta = c(0.3, 0.7),
#'   "prod1", "prod2"
#' )
#'
#' dst.consumer2 <- Clone(dst.consumer1)
#' dst.consumer2$beta <- c(0.6, 0.4)
#'
#' dst.firm1 <- node_new("output",
#'   type = "Leontief",
#'   a = c(0.5, 0.2, 0.3),
#'   "VA", "prod1", "prod2"
#' )
#' node_set(dst.firm1, "VA",
#'   type = "CD",
#'   alpha = 0.8^-0.8 * 0.2^-0.2,
#'   beta = c(0.8, 0.2),
#'   "lab", "cap"
#' )
#'
#' dst.firm2 <- Clone(dst.firm1)
#' node_set(dst.firm2, "output",
#'   a = c(0.25, 0.5, 0.25)
#' )
#' node_set(dst.firm2, "VA",
#'   alpha = 0.4^-0.4 * 0.6^-0.6,
#'   beta = c(0.4, 0.6)
#' )
#'
#' ## no taxation
#' dstl <- list(dst.firm1, dst.firm2, dst.consumer1, dst.consumer2)
#' ge <- gemTax_5_4(dstl, delta = 1)
#'
#' ## ad valorem output tax (see Table 4.1)
#' output.tax.rate <- 0.1
#' dst.taxed.firm1 <- node_new("taxed.output",
#'   type = "FIN", rate = c(1, output.tax.rate),
#'   dst.firm1, "tax"
#' )
#' node_plot(dst.taxed.firm1)
#'
#' dst.taxed.firm2 <- node_new("taxed.output",
#'   type = "FIN", rate = c(1, output.tax.rate),
#'   dst.firm2, "tax"
#' )
#' node_plot(dst.taxed.firm2)
#'
#' dstl <- list(dst.taxed.firm1, dst.taxed.firm2, dst.consumer1, dst.consumer2)
#'
#' ge.output.tax1 <- gemTax_5_4(dstl, delta = 1)
#' ge.output.tax2 <- gemTax_5_4(dstl, delta = 0.5)
#' ge.output.tax3 <- gemTax_5_4(dstl, delta = 0)
#'
#' ## labor tax (see Table 4.3)
#' lab.tax.rate <- 0.1
#'
#' dst.taxed.lab <- node_new("taxed.lab",
#'   type = "FIN",
#'   rate = c(1, lab.tax.rate),
#'   "lab",
#'   "tax"
#' )
#'
#' dst.labor.taxed.firm1 <- Clone(dst.firm1)
#' node_prune(dst.labor.taxed.firm1, "lab", "cap")
#' node_set(
#'   dst.labor.taxed.firm1, "VA",
#'   dst.taxed.lab,
#'   "cap"
#' )
#'
#' dst.labor.taxed.firm2 <- Clone(dst.labor.taxed.firm1)
#' node_set(dst.labor.taxed.firm2, "output",
#'   a = c(0.25, 0.5, 0.25)
#' )
#' node_set(dst.labor.taxed.firm2, "VA",
#'   alpha = 0.4^-0.4 * 0.6^-0.6,
#'   beta = c(0.4, 0.6)
#' )
#'
#' dstl.labor.tax <- list(dst.labor.taxed.firm1, dst.labor.taxed.firm2, dst.consumer1, dst.consumer2)
#'
#' ge.lab.tax <- gemTax_5_4(dstl.labor.tax, delta = 0.5)
#'
#' ge.lab.tax$p
#' ge.lab.tax$z / ge$z - 1
#'
#' ## income tax (see Table 4.3)
#' income.tax.rate <- 0.2
#' consumption.tax.rate <- income.tax.rate / (1 - income.tax.rate)
#' dst.taxed.consumer1 <- node_new("taxed.utility",
#'   type = "FIN",
#'   rate = c(1, consumption.tax.rate),
#'   dst.consumer1,
#'   "tax"
#' )
#'
#' dst.taxed.consumer2 <- node_new("taxed.utility",
#'   type = "FIN",
#'   rate = c(1, consumption.tax.rate),
#'   dst.consumer2,
#'   "tax"
#' )
#'
#' dstl <- list(dst.firm1, dst.firm2, dst.taxed.consumer1, dst.taxed.consumer2)
#'
#' ge.income.tax <- gemTax_5_4(dstl, delta = 0.5)
#' ge.income.tax$z / ge$z - 1
#'
#' ## labor tax (see Table 4.3)
#' lab.tax.rate <- 0.3742
#' node_set(dst.labor.taxed.firm1, "taxed.lab",
#'   rate = c(1, lab.tax.rate)
#' )
#' node_set(dst.labor.taxed.firm2, "taxed.lab",
#'   rate = c(1, lab.tax.rate)
#' )
#'
#' ge.lab.tax <- gemTax_5_4(list(
#'   dst.labor.taxed.firm1,
#'   dst.labor.taxed.firm2,
#'   dst.consumer1,
#'   dst.consumer2
#' ), delta = 0.5)
#' ge.lab.tax$z / ge$z - 1
#'
#' ## variable labor tax rate
#' policy.var.tax.rate <- function(time, A, state) {
#'   current.tax.rate <- NA
#'   if (time >= 200) {
#'     tax.amount <- (state$p / state$p[3])[5]
#'     adjustment.ratio <- ratio_adjust(tax.amount / 18.7132504, coef = 0.1)
#'     last.tax.rate <- node_set(A[[1]], "taxed.lab")$rate[2]
#'     current.tax.rate <- last.tax.rate / adjustment.ratio
#'   } else {
#'     current.tax.rate <- 0.1
#'   }
#'   node_set(A[[1]], "taxed.lab", rate = c(1, current.tax.rate))
#'   node_set(A[[2]], "taxed.lab", rate = c(1, current.tax.rate))
#'
#'   state$current.policy.data <- c(time, current.tax.rate)
#'   state
#' }
#'
#' ge.var.lab.tax <- gemTax_5_4(dstl.labor.tax, policy = policy.var.tax.rate)
#' matplot(ge.var.lab.tax$ts.z, type = "l")
#' matplot(ge.var.lab.tax$ts.p / ge.var.lab.tax$p[3], type = "l")
#' plot(ge.var.lab.tax$policy.data[, 1], ge.var.lab.tax$policy.data[, 2],
#'   ylab = "labor tax rate"
#' )
#' ge.var.lab.tax$p / ge.var.lab.tax$p[3]
#' }
#'
gemTax_5_4 <- function(dstl,
                       names.commodity = c("prod1", "prod2", "lab", "cap", "tax"),
                       names.agent = c("taxed.firm1", "taxed.firm2", "consumer1", "consumer2"),
                       delta = 1,
                       supply.lab.consumer1 = 30,
                       supply.cap.consumer1 = 20,
                       supply.lab.consumer2 = 20,
                       supply.cap.consumer2 = 5,
                       policy.tax = NULL) {
  ge <- sdm2(dstl,
    names.commodity = names.commodity,
    names.agent = names.agent,
    B = {
      tmp <- matrix(0, 5, 4, dimnames = list(names.commodity, names.agent))
      tmp[1, 1] <- 1
      tmp[2, 2] <- 1
      tmp
    },
    S0Exg = {
      tmp <- matrix(NA, 5, 4, dimnames = list(names.commodity, names.agent))
      tmp[3, 3] <- supply.lab.consumer1
      tmp[4, 3] <- supply.cap.consumer1
      tmp[3, 4] <- supply.lab.consumer2
      tmp[4, 4] <- supply.cap.consumer2

      tmp[5, 3] <- delta
      tmp[5, 4] <- 1 - delta
      tmp
    },
    ts = TRUE,
    maxIteration = 1,
    numberOfPeriods = 500,
    policy = policy.tax
  )

  ge$p <- ge$p / ge$p[3]
  ge_tidy(ge, names.commodity, names.agent)
}
