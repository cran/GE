#' @export
#' @title Some 3-by-3 General Equilibrium Models with Money and Exogenous Interest Rate
#' @aliases gemMoney_3_3
#' @description Some 3-by-3 general equilibrium models with money as a medium of exchange and a means of payment.
#' Here, the interest rate is an exogenous variable.
#'
#' In these examples, the price of money refers to its rental price, which is the interest amount generated per unit of money.
#' The value of a unit of currency (i.e., its selling price or asset price) is its rental price divided by the interest rate.
#' When the rental price of money equals the interest rate, the value of the currency equals 1, which implies that money is used as the numeraire (i.e. the unit of account).
#' @param ... arguments to be passed to the function sdm2.
#' @references LI Wu (2019, ISBN: 9787521804225) General Equilibrium and Structural Dynamics: Perspectives of New Structural Economics. Beijing: Economic Science Press. (In Chinese)
#' @examples
#' \donttest{
#' #### a monetary pure exchange model
#' interest.rate <- 0.25
#' vm <- 1 # the velocity of money
#'
#' dst.consumer1 <- node_new("util",
#'                           type = "CD",
#'                           alpha = 1, beta = c(0.5, 0.5),
#'                           "cc1", "wheat"
#' )
#' node_set(dst.consumer1, "cc1",
#'          type = "FIN", rate = c(1, interest.rate / vm),
#'          "iron", "money"
#' )
#'
#' dst.consumer2 <- node_new("util",
#'                           type = "CD",
#'                           alpha = 1, beta = c(0.5, 0.5),
#'                           "cc1", "iron"
#' )
#' node_set(dst.consumer2, "cc1",
#'          type = "FIN", rate = c(1, interest.rate / vm),
#'          "wheat", "money"
#' )
#'
#' dst.consumer3 <- node_new("util",
#'                           type = "FIN", rate = c(1, interest.rate / vm),
#'                           "cc1", "money"
#' )
#' node_set(dst.consumer3, "cc1",
#'          type = "CD",
#'          alpha = 1, beta = c(0.5, 0.5),
#'          "wheat", "iron"
#' )
#'
#' ge <- sdm2(
#'   A = list(dst.consumer1, dst.consumer2, dst.consumer3),
#'   B = matrix(0, 3, 3),
#'   S0Exg = matrix(c(
#'     100, 0, 0,
#'     0, 100, 0,
#'     0, 0, 100
#'   ), 3, 3, TRUE),
#'   names.commodity = c("wheat", "iron", "money"),
#'   names.agent = c("consumer1", "consumer2", "consumer3"),
#'   numeraire = c(money = interest.rate)
#' )

#' ge$p
#' ge$z
#' addmargins(ge$D, 2)
#' addmargins(ge$DV)
#' addmargins(ge$SV)
#'
#' ## Here are a few examples of calculating demand coefficients.
#' node_plot(dst.consumer1, TRUE)
#'
#' # In the following example, the rental price of money is 0.25,
#' # which equals the interest rate of the money,
#' # thus it is known that the value of one unit of money is 1.
#' demand_coefficient(dst.consumer1, p = c(wheat = 1, iron = 1, money = 0.25))
#'
#' demand_coefficient(dst.consumer1, p = c(wheat = 1, iron = 2, money = 0.25))
#'
#' # In the following example,  the rental price of money is 0.5,
#' # and the value of one unit of money is 0.5/0.25=2.
#' demand_coefficient(dst.consumer1, p = c(wheat = 1, iron = 2, money = 0.5))
#'
#' #### a monetary model with production
#' interest.rate <- 0.25
#' vm <- 1 # the velocity of money
#'
#' dst.firm <- node_new("prod",
#'                      type = "FIN", rate = c(1, interest.rate / vm),
#'                      "cc1", "money"
#' )
#' node_set(dst.firm, "cc1",
#'          type = "CD", alpha = 2, beta = c(0.5, 0.5),
#'          "prod", "lab"
#' )
#'
#' dst.laborer <- dst.moneyOwner <-
#'   node_new("util",
#'            type = "FIN", rate = c(1, interest.rate / vm),
#'            "prod", "money"
#'   )
#'
#' ge <- sdm2(
#'   A = list(dst.firm, dst.laborer, dst.moneyOwner),
#'   B = diag(c(1, 0, 0)),
#'   S0Exg = matrix(c(
#'     NA, NA, NA,
#'     NA, 100, NA,
#'     NA, NA, 100
#'   ), 3, 3, TRUE),
#'   names.commodity = c("prod", "lab", "money"),
#'   names.agent = c("firm", "laborer", "moneyOwner"),
#'   numeraire = "prod"
#' )
#'
#' ge$p
#' ge$z
#' addmargins(ge$D, 2)
#' addmargins(ge$S, 2)
#' addmargins(ge$DV)
#' addmargins(ge$SV)
#'
#' ## Take money as numeraire, that is, let the asset price of money equal to 1,
#' ## and let the interest per unit of money equal to the exogenous interest rate.
#' ge2 <- sdm2(
#'   A = list(dst.firm, dst.laborer, dst.moneyOwner),
#'   B = diag(c(1, 0, 0)),
#'   S0Exg = matrix(c(
#'     NA, NA, NA,
#'     NA, 100, NA,
#'     NA, NA, 100
#'   ), 3, 3, TRUE),
#'   names.commodity = c("prod", "lab", "money"),
#'   names.agent = c("firm", "laborer", "moneyOwner"),
#'   numeraire = c(money = interest.rate)
#' )
#'
#' ge2$p
#' ge2$z
#' addmargins(ge2$D, 2)
#' addmargins(ge2$S, 2)
#' addmargins(ge2$DV)
#' addmargins(ge2$SV)
#'
#' #### another model (Li, 2019, example 7.2)
#' interest.rate <- 0.25
#' vm <- 1 # the velocity of money
#'
#' dst <- node_new("demand",
#'                 type = "FIN", rate = c(1, interest.rate / vm),
#'                 "cc1", "money"
#' )
#' node_set(dst, "cc1",
#'          type = "CD", alpha = 1, beta = c(0.5, 0.5),
#'          "prod", "lab"
#' )
#'
#' ge <- sdm2(
#'   A = list(dst, dst, dst),
#'   B = diag(c(1, 0, 0)),
#'   S0Exg = matrix(c(
#'     NA, NA, NA,
#'     NA, 100, NA,
#'     NA, NA, 100
#'   ), 3, 3, TRUE),
#'   names.commodity = c("prod", "lab", "money"),
#'   names.agent = c("firm", "laborer", "money.lender"),
#'   numeraire = c(money = interest.rate)
#' )
#'
#' ge$p
#' ge$z
#' addmargins(ge$D, 2)
#' addmargins(ge$S, 2)
#' addmargins(ge$DV)
#' addmargins(ge$SV)
#' }

gemMoney_3_3 <- function(...) sdm2(...)
