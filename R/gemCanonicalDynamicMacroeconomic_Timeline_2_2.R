#' @export
#' @title A Canonical Dynamic Macroeconomic General Equilibrium Model in Timeline Form (see Torres, 2016)
#' @aliases gemCanonicalDynamicMacroeconomic_Timeline_2_2
#' @description A canonical dynamic macroeconomic general equilibrium model in timeline form (see Torres, 2016, Table 2.1 and 2.2).
#' The firm has a CESAK production function.
#' @param alpha.firm a positive vector, indicating the efficiency parameters of the firm for each planning period.
#' The number of planning periods will be set to length(alpha.firm) + 1.
#' @param es.prod.lab.firm the elasticity of substitution between product and labor in the production function of the firm.
#' @param beta.prod.firm  the share parameter of the product in the production function.
#' @param depreciation.rate the physical depreciation rate of capital stock.
#' @param eis a positive scalar indicating the elasticity of intertemporal substitution of the consumer.
#' @param rho.beta the subjective discount factor of the consumer.
#' @param es.prod.lab.consumer the elasticity of substitution between product and labor in the CES-type period utility function of the consumer.
#' @param beta.prod.consumer the share parameter of the product in the period utility function.
#' @param gr the growth rate of the labor supply.
#' @param initial.product.supply the initial product supply.
#' @param head.tail.adjustment  a character string specifying the type of the head-tail-adjustment policy, must be one of "both" (default), "head", "tail" or "none".
#' @param wage.payment a character string specifying the wage payment method, must be one of "pre" or "post".
#' @param beta.consumer NULL (the default) or a positive vector containing length(alpha.firm) + 1 elements specifying the consumer's intertemporal share parameter.
#' If beta.consumer is not NULL, rho.beta will be ignored.
#' @param ... arguments to be passed to the function sdm2.
#' @references Torres, Jose L. (2016, ISBN: 9781622730452) Introduction to Dynamic Macroeconomic General Equilibrium Models (Second Edition). Vernon Press.
#' @seealso \code{\link{gemCanonicalDynamicMacroeconomic_TimeCircle_2_2}},\cr
#' \code{\link{gemCanonicalDynamicMacroeconomic_Sequential_3_2}},\cr
#' \code{\link{gemCanonicalDynamicMacroeconomic_Sequential_WagePostpayment_4_3}}.
#' @examples
#' \donttest{
#' #### Take the wage postpayment assumption.
#' ge <- gemCanonicalDynamicMacroeconomic_Timeline_2_2()
#' np <- 5
#' eis <- 1
#' rho.beta <- 0.97
#' gr <- 0
#' ge$p
#' ge$p[1:(np - 1)] / ge$p[2:np] - 1
#' ge$p[(np + 1):(2 * np - 2)] / ge$p[(np + 2):(2 * np - 1)] - 1
#' sserr(eis = eis, rho.beta = rho.beta, gr = gr) # the steady-state equilibrium return rate
#' ge$z
#' ge$D
#' node_plot(ge$dst.consumer, TRUE)
#'
#' #### Take the wage postpayment assumption.
#' eis <- 0.8
#' rho.beta <- 0.97
#' gr <- 0.03
#' ge <- gemCanonicalDynamicMacroeconomic_Timeline_2_2(
#'   es.prod.lab.firm = 0.8,
#'   eis = eis, rho.beta = rho.beta, es.prod.lab.consumer = 0.8,
#'   gr = gr
#' )
#'
#' np <- 5
#' ge$p
#' growth_rate(ge$p[1:np])
#' 1 / (1 + sserr(eis = eis, rho.beta = rho.beta, gr = gr)) - 1
#' ge$z
#' growth_rate(ge$z[1:(np - 1)])
#' ge$D
#' ge$S
#'
#' ##### anticipated technology shock.
#' ## Warning: Running the program below takes several minutes.
#' # np <- 120
#' # alpha.firm <- rep(1, np - 1)
#' # alpha.firm[40] <- 1.05
#' # ge <- gemCanonicalDynamicMacroeconomic_Timeline_2_2(alpha.firm = alpha.firm)
#' #
#' ## The steady state product supply is 343.92.
#' ## the (planning) time series of product supply.
#' # plot(ge$z[1:(np - 1)] / 343.92 - 1, type = "o", pch = 20)
#' ## The steady state product consumption is 57.27.
#' ## the (planning) time series of product consumption.
#' # plot(ge$D[2:(np - 1), np] / 57.27 - 1, type = "o", pch = 20)
#' # plot(growth_rate(ge$p[1:(np)]), type = "o", pch = 20)
#' # plot(growth_rate(ge$p[(np + 1):(2 * np)]), type = "o", pch = 20)
#' #
#' ##### an unanticipated technology shock.
#' # np <- 50
#' # alpha.firm <- rep(1, np - 1)
#' # alpha.firm[1] <- 1.05
#' # ge <- gemCanonicalDynamicMacroeconomic_Timeline_2_2(
#' #   alpha.firm = alpha.firm,
#' #   initial.product.supply = 286.6341, # the steady state value
#' #   head.tail.adjustment = "tail"
#' # )
#' #
#' ## The steady state product supply is 343.92.
#' ## the (planning) time series of product supply.
#' # plot(ge$z[1:(np - 1)] / 343.92 - 1, type = "o", pch = 20)
#' ## The steady state product consumption is 57.27.
#' ## the (planning) time series of product consumption.
#' # plot(ge$D[2:(np - 1), np] / 57.27 - 1, type = "o", pch = 20)
#' # plot(growth_rate(ge$p[1:(np)]), type = "o", pch = 20)
#' # plot(growth_rate(ge$p[(np + 1):(2 * np)]), type = "o", pch = 20)
#' #
#' ##### a technology shock anticipated three periods in advance.
#' # np <- 50
#' # alpha.firm <- rep(1, np - 1)
#' # alpha.firm[4] <- 1.05
#' # ge <- gemCanonicalDynamicMacroeconomic_Timeline_2_2(
#' #   alpha.firm = alpha.firm,
#' #   initial.product.supply = 286.6341, # the steady state value
#' #   head.tail.adjustment = "tail"
#' # )
#' #
#' ## The steady state product supply is 343.92.
#' ## the (planning) time series of product supply
#' # plot(ge$z[1:(np - 1)] / 343.92 - 1, type = "o", pch = 20)
#' ## The steady state product consumption is 57.27.
#' ## the (planning) time series of product consumption
#' # plot(ge$D[2:(np - 1), np] / 57.27 - 1, type = "o", pch = 20)
#' # plot(growth_rate(ge$p[1:(np)]), type = "o", pch = 20)
#' # plot(growth_rate(ge$p[(np + 1):(2 * np)]), type = "o", pch = 20)
#' #
#' ##### an unanticipated technology shock.
#' ## Warning: Running the program below takes several minutes.
#' # np <- 100
#' # alpha.firm <- exp(0.01)
#' # for (t in 2:(np - 1)) {
#' #   alpha.firm[t] <- exp(0.9 * log(alpha.firm[t - 1]))
#' # }
#' # plot(alpha.firm)
#' #
#' # ge <- gemCanonicalDynamicMacroeconomic_Timeline_2_2(
#' #   alpha.firm = alpha.firm,
#' #   initial.product.supply = 286.6341, # the steady state value
#' #   head.tail.adjustment = "tail"
#' # )
#' #
#' ## The steady state product supply is 343.92.
#' ## the (planning) time series of product supply
#' # plot(ge$z[1:(np - 1)] / 343.92 - 1, type = "o", pch = 20)
#' ## The steady state product consumption is 57.27.
#' ## the (planning) time series of product consumption
#' # plot(ge$D[2:(np - 1), np] / 57.27 - 1, type = "o", pch = 20)
#' # plot(growth_rate(ge$p[1:(np)]), type = "o", pch = 20)
#' # plot(growth_rate(ge$p[(np + 1):(2 * np)]), type = "o", pch = 20)
#'
#' #### Take the wage prepayment assumption.
#' ge <- gemCanonicalDynamicMacroeconomic_Timeline_2_2(wage.payment = "pre")
#' np <- 5
#' eis <- 1
#' rho.beta <- 0.97
#' gr <- 0
#' ge$p
#' ge$p[1:(np - 1)] / ge$p[2:np] - 1
#' ge$p[(np + 1):(2 * np - 2)] / ge$p[(np + 2):(2 * np - 1)] - 1
#' sserr(eis = eis, rho.beta = rho.beta, gr = gr) # the steady-state equilibrium return rate
#' ge$z
#' ge$D
#' node_plot(ge$dst.consumer, TRUE)
#'
#' #### Take the wage prepayment assumption.
#' np <- 5
#' eis <- 0.8
#' rho.beta <- 0.97
#' gr <- 0.03
#' ge <- gemCanonicalDynamicMacroeconomic_Timeline_2_2(
#'   es.prod.lab.firm = 0.8,
#'   eis = eis, rho.beta = rho.beta, es.prod.lab.consumer = 0.8,
#'   gr = gr,
#'   wage.payment = "pre"
#' )
#'
#' ge$p
#' growth_rate(ge$p[1:np])
#' 1 / (1 + sserr(eis = eis, rho.beta = rho.beta, gr = gr)) - 1
#' ge$z
#' growth_rate(ge$z[1:(np - 1)])
#' ge$D
#' ge$S
#' }
#'
gemCanonicalDynamicMacroeconomic_Timeline_2_2 <- function(alpha.firm = rep(1, 4),
                                                          es.prod.lab.firm = 1,
                                                          beta.prod.firm = 0.35,
                                                          depreciation.rate = 0.06,
                                                          eis = 1,
                                                          rho.beta = 0.97,
                                                          beta.prod.consumer = 0.4,
                                                          es.prod.lab.consumer = 1,
                                                          gr = 0,
                                                          initial.product.supply = 200,
                                                          head.tail.adjustment = "both",
                                                          wage.payment = "post",
                                                          beta.consumer = NULL,
                                                          ...) {
  np <- length(alpha.firm) + 1

  n <- 2 * np - 1 # the number of commodity kinds
  m <- np # the number of agent kinds

  names.commodity <- c(paste0("prod", 1:np), paste0("lab", 1:(np - 1)))
  names.agent <- c(paste0("firm", 1:(np - 1)), "consumer")

  S0Exg <- matrix(NA, n, m, dimnames = list(names.commodity, names.agent))
  S0Exg[paste0("lab", 1:(np - 1)), "consumer"] <- 100 * (1 + gr)^(0:(np - 2))
  S0Exg["prod1", "consumer"] <- initial.product.supply # the product supply in the first period, which will be adjusted.

  B <- matrix(0, n, m, dimnames = list(names.commodity, names.agent))
  for (k in 1:(np - 1)) {
    B[paste0("prod", k + 1), paste0("firm", k)] <- 1
  }


  dstl.firm <- list()
  for (k in 1:(np - 1)) {
    dstl.firm[[k]] <- node_new("output",
      type = "CESAK", es = es.prod.lab.firm,
      alpha = alpha.firm[k], betaK = beta.prod.firm, alphaK = 1 - depreciation.rate,
      paste0("prod", k), paste0("lab", k)
    )
  }

  if (is.null(beta.consumer)) {
    beta.consumer <- prop.table(c(
      rho.beta^(1:(np - 1)),
      ifelse(head.tail.adjustment %in% c("both", "tail"), 1, 1e-100)
    ))
  }
  dst.consumer <- node_new(
    "util",
    type = "CES", es = eis,
    alpha = 1, beta = beta.consumer,
    paste0("cc", 1:(np - 1)), paste0("prod", np)
  )
  switch(wage.payment,
    "post" = {
      # Take the wage postpayment assumption.
      for (k in 1:(np - 1)) {
        node_set(dst.consumer, paste0("cc", k),
          type = "CES", es = es.prod.lab.consumer,
          alpha = 1, beta = c(beta.prod.consumer, 1 - beta.prod.consumer),
          paste0("prod", k + 1), paste0("lab", k)
        )
      }
    },
    "pre" = {
      # Take the wage prepayment assumption.
      for (k in 1:(np - 1)) {
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
    A = c(dstl.firm, dst.consumer),
    B = B,
    S0Exg = S0Exg,
    names.commodity = names.commodity,
    names.agent = names.agent,
    numeraire = "lab1",
    priceAdjustmentVelocity = 0.05,
    policy = list(
      makePolicyMeanValue(50),
      makePolicyHeadTailAdjustment(type = head.tail.adjustment, gr = gr, np = np)
    ),
    ...
  )

  ge$dstl.firm <- dstl.firm
  ge$dst.consumer <- dst.consumer

  return(ge)
}
