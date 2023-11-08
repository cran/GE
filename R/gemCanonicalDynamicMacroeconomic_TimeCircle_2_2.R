#' @export
#' @title A Canonical Dynamic Macroeconomic General Equilibrium Model in Time-circle Form (see Torres, 2016)
#' @aliases gemCanonicalDynamicMacroeconomic_TimeCircle_2_2
#' @description A canonical dynamic macroeconomic general equilibrium model in time-circle form (see Torres, 2016, Table 2.1 and 2.2).
#' @param alpha.firm a positive vector, indicating the efficiency parameters of the firm for each economic period.
#' The number of economic periods will be set to length(alpha.firm) .
#' @param es.prod.lab.firm the elasticity of substitution between product and labor in the production function of the firm.
#' @param beta.prod.firm  the share parameter of the product in the production function.
#' @param depreciation.rate the physical depreciation rate of capital stock.
#' @param eis the elasticity of intertemporal substitution of the consumer.
#' @param rho.beta the subjective discount factor of the consumer.
#' @param es.prod.lab.consumer the elasticity of substitution between product and labor in the CES-type period utility function of the consumer.
#' @param beta.prod.consumer the share parameter of the product in the period utility function.
#' @param gr the growth rate of the labor supply.
#' @param wage.payment a character string specifying the wage payment method, must be one of "pre" or "post".
#' @param ... arguments to be passed to the function sdm2.
#' @return A general equilibrium (see \code{\link{sdm2}}).
#' @references Torres, Jose L. (2016, ISBN: 9781622730452) Introduction to Dynamic Macroeconomic General Equilibrium Models (Second Edition). Vernon Press.
#' @seealso \code{\link{gemCanonicalDynamicMacroeconomic_Timeline_2_2}},\cr
#' \code{\link{gemCanonicalDynamicMacroeconomic_Sequential_3_2}},\cr
#' \code{\link{gemCanonicalDynamicMacroeconomic_Sequential_WagePostpayment_4_3}}.
#' @examples
#' \donttest{
#' #### Take the wage postpayment assumption.
#' ge <- gemCanonicalDynamicMacroeconomic_TimeCircle_2_2()
#' np <- 3
#' eis <- 1
#' rho.beta <- 0.97
#' gr <- 0
#' ge$p
#' growth_rate(ge$p[1:np])
#' 1 / (1 + sserr(eis = eis, rho.beta = rho.beta, gr = gr)) - 1
#' ge$z
#' growth_rate(ge$z[1:np])
#' ge$D
#' ge$S
#'
#' ##  Take the wage postpayment assumption.
#' eis <- 0.8
#' rho.beta <- 0.97
#' gr <- 0.03
#' ge <- gemCanonicalDynamicMacroeconomic_TimeCircle_2_2(
#'   es.prod.lab.firm = 0.8,
#'   eis = eis, rho.beta = rho.beta, es.prod.lab.consumer = 0.8,
#'   gr = gr
#' )
#'
#' ge$p
#' growth_rate(ge$p[1:np])
#' 1 / (1 + sserr(eis = eis, rho.beta = rho.beta, gr = gr)) - 1
#' ge$z
#' growth_rate(ge$z[1:np])
#' ge$D
#' ge$S
#'
#' #### an anticipated technology shock.
#' ## Warning: Running the program below takes about 4 minutes.
#' # np <- 120
#' # alpha.firm <- rep(1, np)
#' # alpha.firm[40] <- 1.05
#' # ge <- gemCanonicalDynamicMacroeconomic_TimeCircle_2_2(alpha.firm = alpha.firm)
#'
#' ## The steady state product supply is 343.92.
#' ## the (economic) time series of product supply
#' # plot(ge$z[1:np] / 343.92 - 1, type = "o", pch = 20)
#' ## The steady state product consumption is 57.27.
#' ## the (economic) time series of product consumption
#' # plot(ge$D[2:np, np + 1] / 57.27 - 1, type = "o", pch = 20)
#'
#' #### Take the wage prepayment assumption.
#' ge <- gemCanonicalDynamicMacroeconomic_TimeCircle_2_2(wage.payment = "pre")
#' np <- 3
#' eis <- 1
#' rho.beta <- 0.97
#' gr <- 0
#' ge$p
#' growth_rate(ge$p[1:np])
#' 1 / (1 + sserr(eis = eis, rho.beta = rho.beta, gr = gr)) - 1
#' ge$z
#' growth_rate(ge$z[1:np])
#' ge$D
#' ge$S
#'
#' ##  Take the wage prepayment assumption.
#' eis <- 0.8
#' rho.beta <- 0.97
#' gr <- 0.03
#'
#' ge <- gemCanonicalDynamicMacroeconomic_TimeCircle_2_2(
#'   es.prod.lab.firm = 0.8,
#'   eis = eis, es.prod.lab.consumer = 0.8,
#'   rho.beta = rho.beta, gr = gr,
#'   wage.payment = "pre"
#' )
#'
#' ge$p
#' growth_rate(ge$p[1:np])
#' 1 / (1 + sserr(eis = eis, rho.beta = rho.beta, gr = gr)) - 1
#' ge$z
#' growth_rate(ge$z[1:np])
#' ge$D
#' ge$S
#' }
gemCanonicalDynamicMacroeconomic_TimeCircle_2_2 <- function(alpha.firm = rep(1, 3),
                                                            es.prod.lab.firm = 1,
                                                            beta.prod.firm = 0.35,
                                                            depreciation.rate = 0.06,
                                                            eis = 1,
                                                            rho.beta = 0.97,
                                                            beta.prod.consumer = 0.4,
                                                            es.prod.lab.consumer = 1,
                                                            gr = 0,
                                                            wage.payment = "post",
                                                            ...) {
  np <- length(alpha.firm)
  yield <- sserr(eis = eis, rho.beta = rho.beta, gr = gr, prepaid = TRUE)
  zeta <- (1 + gr)^np # the ratio of repayments to loans

  n <- 2 * np + 2 # the number of commodity kinds
  m <- np + 2 # the number of agent kinds

  names.commodity <- c(paste0("prod", 1:np), paste0("lab", 1:np), "claim", paste0("prod", np + 1))
  names.agent <- c(paste0("firm", 1:np), "consumer", "bank")

  # the exogenous supply matrix.
  S0Exg <- matrix(NA, n, m, dimnames = list(names.commodity, names.agent))
  S0Exg[paste0("lab", 1:np), "consumer"] <- 100 * (1 + gr)^(0:(np - 1)) # the supply of labor
  S0Exg["claim", "consumer"] <- 100 # the supply of financial claim

  # the output coefficient matrix.
  B <- matrix(0, n, m, dimnames = list(names.commodity, names.agent))
  for (k in 1:np) {
    B[paste0("prod", k + 1), paste0("firm", k)] <- 1
  }
  B["prod1", "bank"] <- 1 / zeta

  dstl.firm <- list()
  for (k in 1:np) {
    dstl.firm[[k]] <- node_new("output",
      type = "CESAK", es = es.prod.lab.firm,
      alpha = alpha.firm[k], betaK = beta.prod.firm, alphaK = 1 - depreciation.rate,
      paste0("prod", k), paste0("lab", k)
    )
  }

  dst.bank <- node_new("prod",
    type = "FIN", rate = c(1, (1 + yield)^np - 1),
    "cc1", "claim"
  )
  node_set(dst.bank, "cc1",
    type = "Leontief", a = 1,
    paste0("prod", np + 1)
  )

  dst.consumer <- node_new(
    "util",
    type = "CES", es = eis,
    alpha = 1, beta = prop.table(rho.beta^(1:np)),
    paste0("cc", 1:np)
  )
  switch(wage.payment,
    "post" = {
      # Take the wage postpayment assumption.
      for (k in 1:np) {
        node_set(dst.consumer, paste0("cc", k),
          type = "CES", es = es.prod.lab.consumer,
          alpha = 1, beta = c(beta.prod.consumer, 1 - beta.prod.consumer),
          paste0("prod", k + 1), paste0("lab", k)
        )
      }
    },
    "pre" = {
      # Take the wage prepayment assumption.
      for (k in 1:np) {
        node_set(dst.consumer, paste0("cc", k),
          type = "CES", es = es.prod.lab.consumer,
          alpha = 1, beta = c(beta.prod.consumer, 1 - beta.prod.consumer),
          paste0("prod", k), paste0("lab", k)
        )
      }
    },
    stop("Li: Wrong wage payment method.")
  )

  ge <- sdm2(
    A = c(dstl.firm, dst.consumer, dst.bank),
    B = B,
    S0Exg = S0Exg,
    names.commodity = names.commodity,
    names.agent = names.agent,
    numeraire = "lab1",
    priceAdjustmentVelocity = 0.03,
    ts = TRUE,
    ...
  )

  ge$dstl.firm <- dstl.firm
  ge$dst.consumer <- dst.consumer
  ge$dst.bank <- dst.bank

  return(ge)
}
