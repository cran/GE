#' @export
#' @title A Canonical Dynamic Macroeconomic General Equilibrium Model in Sequential Form under the Wage Postpayment Assumption (see Torres, 2016)
#' @aliases gemCanonicalDynamicMacroeconomic_Sequential_WagePostpayment_4_3
#' @description A canonical dynamic macroeconomic general equilibrium model in sequential form
#' under the wage postpayment assumption (see Torres, 2016, Table 2.1 and 2.2).
#' In this model, there are two firms and one consumer.
#' Under the wage postpayment assumption, the consumer actually consumes a kind of labor (that is, leisure)
#' and the products produced by this labor at the same time.
#' Firm 1 is a regular production firm.
#' Firm 2 can store labor from one period to the next period for consumption by the consumer.
#' @param alpha.firm a positive scalar, indicating the efficiency parameter of firm 1.
#' @param es.prod.lab.firm the elasticity of substitution between product and labor in the production function of firm 1.
#' @param beta.prod.firm  the share parameter of the product in the production function of firm 1.
#' @param depreciation.rate the physical depreciation rate of capital stock of firm 1.
#' @param eis the elasticity of intertemporal substitution of the consumer.
#' @param Gamma.beta the subjective discount factor of the consumer.
#' @param es.prod.lab.consumer the elasticity of substitution between product and labor in the CES-type period utility function of the consumer.
#' @param beta.prod.consumer the share parameter of the product in the period utility function.
#' @param gr the growth rate of the labor supply.
#' @param ... arguments to be passed to the function sdm2.
#' @return A general equilibrium (see \code{\link{sdm2}}).
#' @seealso \code{\link{gemCanonicalDynamicMacroeconomic_Timeline_2_2}},\cr
#' \code{\link{gemCanonicalDynamicMacroeconomic_TimeCircle_2_2}},\cr
#' \code{\link{gemCanonicalDynamicMacroeconomic_Sequential_3_2}}.
#' @examples
#' \donttest{
#' gemCanonicalDynamicMacroeconomic_Sequential_WagePostpayment_4_3()
#'
#' ####
#' eis <- 0.8
#' Gamma.beta <- 0.97
#' gr <- 0.03
#' ge <- gemCanonicalDynamicMacroeconomic_Sequential_WagePostpayment_4_3(
#'   es.prod.lab.firm = 0.8,
#'   eis = eis, Gamma.beta = Gamma.beta, es.prod.lab.consumer = 0.8,
#'   gr = gr
#' )
#'
#' ge$p
#' ge$p[1] * (sserr(eis = eis, Gamma.beta = Gamma.beta, gr = gr, prepaid = TRUE) + 1)
#' ge$z
#' addmargins(ge$D, 2)
#' addmargins(ge$S, 2)
#' ge$S[1, 1] * (1 + gr)
#' }
#'
gemCanonicalDynamicMacroeconomic_Sequential_WagePostpayment_4_3 <- function(alpha.firm = 1,
                                                                            es.prod.lab.firm = 1,
                                                                            beta.prod.firm = 0.35,
                                                                            depreciation.rate = 0.06,
                                                                            eis = 1,
                                                                            Gamma.beta = 0.97,
                                                                            es.prod.lab.consumer = 1,
                                                                            beta.prod.consumer = 0.4,
                                                                            gr = 0,
                                                                            ...) {
  yield <- sserr(eis = eis, Gamma.beta = Gamma.beta, gr = gr, prepaid = TRUE)

  # Take the wage postpayment assumption.
  dst.firm1 <- node_new("output",
    type = "CESAK", es = es.prod.lab.firm,
    alpha = alpha.firm, betaK = beta.prod.firm, alphaK = 1 - depreciation.rate,
    "cc1", "lab"
  )
  node_set(dst.firm1, "cc1",
    type = "FIN", rate = c(1, yield),
    "prod", "equity.share"
  )

  dst.firm2 <- node_new("output",
    type = "Leontief", a = 1,
    "lab"
  )

  dst.consumer <- node_new("util",
    type = "CES", es = es.prod.lab.consumer,
    alpha = 1, beta = c(beta.prod.consumer, 1 - beta.prod.consumer),
    "prod", "lab.stored"
  )

  ge <- sdm2(
    A = list(dst.firm1, dst.firm2, dst.consumer),
    B = matrix(c(
      1, 0, 0,
      0, 1, 0,
      0, 0, 0,
      0, 0, 0
    ), 4, 3, TRUE),
    S0Exg = matrix(c(
      NA, NA, NA,
      NA, NA, NA,
      NA, NA, 100,
      NA, NA, 100
    ), 4, 3, TRUE),
    names.commodity = c("prod", "lab.stored", "lab", "equity.share"),
    names.agent = c("firm1", "firm2", "consumer"),
    numeraire = "lab",
    GRExg = gr,
    ...
  )

  ge$dst.firm1 <- dst.firm1
  ge$dst.firm2 <- dst.firm2
  ge$dst.consumer <- dst.consumer

  return(ge)
}
