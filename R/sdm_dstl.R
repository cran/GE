#' @export
#' @title Structural Dynamic Model (alias Structural Growth Model) with a Demand Structure Tree List
#' @aliases sdm_dstl
#' @description This is a wrapper of the function CGE::sdm.
#' The parameter A of CGE::sdm is replaced with a demand structure tree list.
#' This function can be replaced by the more comprehensive function \code{\link{sdm2}},
#' so it is not recommended.
#' @param dstl a demand structure tree list.
#' @param names.commodity names of commodities.
#' @param names.agent names of agents.
#' @param ... arguments to be to be passed to the function CGE::sdm.
#' @return A general equilibrium, which is a list with the following elements:
#' \itemize{
#' \item D - the demand matrix.
#' \item DV - the demand value matrix.
#' \item SV - the supply value matrix.
#' \item ... - some elements returned by the CGE::sdm function
#' }
#'
#' @references LI Wu (2019, ISBN: 9787521804225) General Equilibrium and Structural Dynamics: Perspectives of New Structural Economics. Beijing: Economic Science Press. (In Chinese)
#' @references LI Wu (2010) A Structural Growth Model and its Applications to Sraffa's System. http://www.iioa.org/conferences/18th/papers/files/104_20100729011_AStructuralGrowthModelanditsApplicationstoSraffasSstem.pdf
#' @references Manuel Alejandro Cardenete, Ana-Isabel Guerra, Ferran Sancho (2012, ISBN: 9783642247453) Applied General Equilibrium: An Introduction. Springer-Verlag Berlin Heidelberg.
#' @references Torres, Jose L. (2016, ISBN: 9781622730452) Introduction to Dynamic Macroeconomic General Equilibrium Models (Second Edition). Vernon Press.
#' @seealso \code{\link{sdm2}}
#' @examples
#' \donttest{
#' #### a pure exchange economy with two agents and two commodities
#' dst.CHN <- node_new("util.CHN",
#'                     type = "SCES", alpha = 1, beta = c(0.8, 0.2), es = 2,
#'                     "lab.CHN", "lab.ROW"
#' )
#' node_plot(dst.CHN)
#'
#' dst.ROW <- node_new("util.ROW",
#'                     type = "SCES", alpha = 1, beta = c(0.05, 0.95), es = 2,
#'                     "lab.CHN", "lab.ROW"
#' )
#'
#' dstl <- list(dst.CHN, dst.ROW)
#'
#' ge <- sdm_dstl(dstl,
#'                names.commodity = c("lab.CHN", "lab.ROW"),
#'                names.agent = c("CHN", "ROW"),
#'                B = matrix(0, 2, 2, TRUE),
#'                S0Exg = matrix(c(
#'                  100, 0,
#'                  0, 600
#'                ), 2, 2, TRUE)
#' )
#'
#' ## supply change
#' geSC <- sdm_dstl(dstl,
#'                  names.commodity = c("lab.CHN", "lab.ROW"),
#'                  names.agent = c("CHN", "ROW"),
#'                  B = matrix(0, 2, 2, TRUE),
#'                  S0Exg = matrix(c(
#'                    200, 0,
#'                    0, 600
#'                  ), 2, 2, TRUE)
#' )
#'
#' geSC$p / ge$p
#'
#' ## preference change
#' dst.CHN$beta <- c(0.9, 0.1)
#' gePC <- sdm_dstl(dstl,
#'                  names.commodity = c("lab.CHN", "lab.ROW"),
#'                  names.agent = c("CHN", "ROW"),
#'                  B = matrix(0, 2, 2, TRUE),
#'                  S0Exg = matrix(c(
#'                    100, 0,
#'                    0, 600
#'                  ), 2, 2, TRUE)
#' )
#'
#' gePC$p / ge$p
#'
#'
#' #### a pure exchange economy with two agents and four basic commodities
#' prod.CHN <- node_new("prod.CHN",
#'                      type = "SCES", alpha = 1, beta = c(0.5, 0.5), es = 0.75,
#'                      "lab.CHN", "cap.CHN"
#' )
#'
#' node_plot(prod.CHN)
#'
#' prod.ROW <- node_new("prod.ROW",
#'                      type = "SCES", alpha = 2, beta = c(0.4, 0.6), es = 0.75,
#'                      "lab.ROW", "cap.ROW"
#' )
#'
#' dst.CHN <- node_new("CHN",
#'                     type = "SCES", alpha = 1, beta = c(0.8, 0.2), es = 2,
#'                     prod.CHN, prod.ROW
#' )
#'
#' node_plot(dst.CHN)
#' node_print(dst.CHN)
#' p <- c("lab.CHN" = 1, "cap.CHN" = 1, "lab.ROW" = 1, "cap.ROW" = 1)
#' demand_coefficient(dst.CHN, p)
#'
#'
#' dst.ROW <- node_new("ROW",
#'                     type = "SCES", alpha = 1, beta = c(0.05, 0.95), es = 2,
#'                     prod.CHN, prod.ROW
#' )
#'
#' node_plot(dst.ROW)
#' node_print(dst.ROW)
#'
#' dstl <- list(dst.CHN, dst.ROW)
#'
#' ge <- sdm_dstl(dstl,
#'                names.commodity = c("lab.CHN", "cap.CHN", "lab.ROW", "cap.ROW"),
#'                names.agent = c("CHN", "ROW"),
#'                B = matrix(0, 4, 2, TRUE),
#'                S0Exg = matrix(c(
#'                  100, 0,
#'                  100, 0,
#'                  0, 600,
#'                  0, 800
#'                ), 4, 2, TRUE)
#' )
#'
#'
#' ## Add currencies to the example above.
#' prod_money.CHN <- node_new("prod_money.CHN",
#'                            type = "FIN", rate = c(1, 0.1), # 0.1 is the interest rate.
#'                            prod.CHN, "money.CHN"
#' )
#'
#' prod_money.ROW <- node_new("prod_money.ROW",
#'                            type = "FIN", rate = c(1, 0.1),
#'                            prod.ROW, "money.ROW"
#' )
#'
#' dst.CHN <- node_new("util.CHN",
#'                     type = "SCES", alpha = 1, beta = c(0.8, 0.2), es = 2,
#'                     prod_money.CHN, prod_money.ROW
#' )
#'
#' dst.ROW <- node_new("util.ROW",
#'                     type = "SCES", alpha = 1, beta = c(0.05, 0.95), es = 2,
#'                     prod_money.CHN, prod_money.ROW
#' )
#'
#' dstl <- list(dst.CHN, dst.ROW)
#'
#' ge <- sdm_dstl(dstl,
#'                names.commodity = c(
#'                  "lab.CHN", "cap.CHN", "money.CHN",
#'                  "lab.ROW", "cap.ROW", "money.ROW"
#'                ),
#'                names.agent = c("CHN", "ROW"),
#'                B = matrix(0, 6, 2, TRUE),
#'                S0Exg = matrix(c(
#'                  100, 0,
#'                  100, 0,
#'                  100, 0,
#'                  0, 600,
#'                  0, 800,
#'                  0, 100
#'                ), 6, 2, TRUE)
#' )
#'
#' ge$p["money.ROW"] / ge$p["money.CHN"] # the exchange rate
#'
#'
#' #### Example 7.6 in Li (2019), which illustrates foreign exchange rates.
#' interest.rate.CHN <- 0.1
#' interest.rate.ROW <- 0.1
#'
#' firm.CHN <- node_new("output.CHN",
#'                      type = "FIN", rate = c(1, interest.rate.CHN),
#'                      "cc1.CHN", "money.CHN"
#' )
#' node_set(firm.CHN, "cc1.CHN",
#'          type = "CD", alpha = 1, beta = c(0.5, 0.5),
#'          "lab.CHN", "iron"
#' )
#'
#' household.CHN <- node_new("util",
#'                           type = "FIN", rate = c(1, interest.rate.CHN),
#'                           "wheat", "money.CHN"
#' )
#'
#' moneylender.CHN <- Clone(household.CHN)
#'
#'
#' firm.ROW <- node_new("output.ROW",
#'                      type = "FIN", rate = c(1, interest.rate.ROW),
#'                      "cc1.ROW", "money.ROW"
#' )
#' node_set(firm.ROW, "cc1.ROW",
#'          type = "CD", alpha = 1, beta = c(0.5, 0.5),
#'          "iron", "lab.ROW"
#' )
#'
#' household.ROW <- node_new("util",
#'                           type = "FIN", rate = c(1, interest.rate.ROW),
#'                           "wheat", "money.ROW"
#' )
#'
#' moneylender.ROW <- Clone(household.ROW)
#'
#'
#' dstl <- list(
#'   firm.CHN, household.CHN, moneylender.CHN,
#'   firm.ROW, household.ROW, moneylender.ROW
#' )
#'
#' ge <- sdm_dstl(dstl,
#'                names.commodity = c(
#'                  "wheat", "lab.CHN", "money.CHN",
#'                  "iron", "lab.ROW", "money.ROW"
#'                ),
#'                names.agent = c(
#'                  "firm.CHN", "household.CHN", "moneylender.CHN",
#'                  "firm.ROW", "household.ROW", "moneylender.ROW"
#'                ),
#'                B = {
#'                  tmp <- matrix(0, 6, 6)
#'                  tmp[1, 1] <- 1
#'                  tmp[4, 4] <- 1
#'                  tmp
#'                },
#'                S0Exg = {
#'                  tmp <- matrix(NA, 6, 6)
#'                  tmp[2, 2] <- 100
#'                  tmp[3, 3] <- 600
#'                  tmp[5, 5] <- 100
#'                  tmp[6, 6] <- 100
#'                  tmp
#'                }
#' )
#'
#' ge$p.money <- ge$p
#' ge$p.money["money.CHN"] <- ge$p["money.CHN"] / interest.rate.CHN
#' ge$p.money["money.ROW"] <- ge$p["money.ROW"] / interest.rate.ROW
#' ge$p.money <- ge$p.money / ge$p.money["money.CHN"]
#'
#' ge$p.money["money.ROW"] / ge$p.money["money.CHN"] # the exchange rate
#'
#'
#' #### the example (see Table 2.1 and 2.2) of the canonical dynamic
#' #### macroeconomic general equilibrium model in Torres (2016).
#' discount.factor <- 0.97
#' return.rate <- 1 / discount.factor - 1
#' depreciation.rate <- 0.06
#'
#' production.firm <- node_new("output",
#'                             type = "CD", alpha = 1, beta = c(0.65, 0.35),
#'                             "labor", "capital.goods"
#' )
#'
#' household <- node_new("util",
#'                       type = "CD", alpha = 1, beta = c(0.4, 0.6),
#'                       "product", "labor"
#' )
#'
#' leasing.firm <- node_new("output",
#'                          type = "FIN", rate = c(1, return.rate),
#'                          "product", "dividend"
#' )
#'
#' dstl <- list(
#'   production.firm, household, leasing.firm
#' )
#'
#' ge <- sdm_dstl(dstl,
#'                names.commodity = c("product", "labor", "capital.goods", "dividend"),
#'                names.agent = c("production.firm", "household", "leasing.firm"),
#'                B = matrix(c(
#'                  1, 0, 1 - depreciation.rate,
#'                  0, 1, 0,
#'                  0, 0, 1,
#'                  0, 1, 0
#'                ), 4, 3, TRUE),
#'                S0Exg = {
#'                  tmp <- matrix(NA, 4, 3)
#'                  tmp[2, 2] <- 1
#'                  tmp[4, 2] <- 1
#'                  tmp
#'                },
#'                priceAdjustmentVelocity = 0.03,
#'                maxIteration = 1,
#'                numberOfPeriods = 15000,
#'                ts = TRUE
#' )
#'
#' ge$D # the demand matrix
#' ge$p / ge$p[1]
#'
#' plot(ge$ts.z[, 1], type = "l")
#'
#'
#' #### an example of applied general equilibrium (see section 3.4, Cardenete et al., 2012).
#' dst.consumer1 <- node_new("util",
#'                           type = "CD", alpha = 1, beta = c(0.3, 0.7),
#'                           "prod1", "prod2"
#' )
#'
#' dst.consumer2 <- node_new("util",
#'                           type = "CD", alpha = 1, beta = c(0.6, 0.4),
#'                           "prod1", "prod2"
#' )
#'
#' dst.firm1 <- node_new("output",
#'                       type = "Leontief", a = c(0.5, 0.2, 0.3),
#'                       "VA", "prod1", "prod2"
#' )
#' node_set(dst.firm1, "VA",
#'          type = "CD",
#'          alpha = 0.8^-0.8 * 0.2^-0.2, beta = c(0.8, 0.2),
#'          "lab", "cap"
#' )
#'
#' dst.firm2 <- Clone(dst.firm1)
#' dst.firm2$a <- c(0.25, 0.5, 0.25)
#' node_set(dst.firm2, "VA",
#'          alpha = 0.4^-0.4 * 0.6^-0.6, beta = c(0.4, 0.6)
#' )
#'
#' node_print(dst.firm2)
#'
#' dstl <- list(dst.firm1, dst.firm2, dst.consumer1, dst.consumer2)
#'
#' ge <- sdm_dstl(dstl,
#'                names.commodity = c("prod1", "prod2", "lab", "cap"),
#'                names.agent = c("firm1", "firm2", "consumer1", "consumer2"),
#'                B = {
#'                  tmp <- matrix(0, 4, 4)
#'                  tmp[1, 1] <- 1
#'                  tmp[2, 2] <- 1
#'                  tmp
#'                },
#'                S0Exg = {
#'                  tmp <- matrix(NA, 4, 4)
#'                  tmp[3, 3] <- 30
#'                  tmp[4, 3] <- 20
#'                  tmp[3, 4] <- 20
#'                  tmp[4, 4] <- 5
#'                  tmp
#'                }
#' )
#'
#' }




sdm_dstl <- function(dstl,
                     names.commodity,
                     names.agent,
                     ...) {
  ge <- sdm(
    A = function(state) {
      p <- c(state$p)
      names(p) <- names.commodity

      result <- sapply(dstl, demand_coefficient, p)
      return(result)
    },
    ...
  )

  ge_tidy(ge, names.commodity, names.agent)
}
