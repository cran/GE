#' @export
#' @title A Canonical Dynamic Macroeconomic General Equilibrium Model in Sequential Form (see Torres, 2016)
#' @aliases gemCanonicalDynamicMacroeconomic_Sequential_3_2
#' @description A canonical dynamic macroeconomic general equilibrium model in sequential form (see Torres, 2016, Table 2.1 and 2.2).
#' @param alpha.firm a positive scalar, indicating the efficiency parameter of the firm.
#' @param es.prod.lab.firm the elasticity of substitution between product and labor in the production function of firm 1.
#' @param beta.prod.firm  the share parameter of the product in the production function.
#' @param depreciation.rate the physical depreciation rate of capital stock.
#' @param eis the elasticity of intertemporal substitution of the consumer.
#' @param Gamma.beta the subjective discount factor of the consumer.
#' @param es.prod.lab.consumer the elasticity of substitution between product and labor in the CES-type period utility function of the consumer.
#' @param beta.prod.consumer the share parameter of the product in the period utility function.
#' @param gr the growth rate of the labor supply.
#' @param wage.payment a character string specifying the wage payment method, must be one of "pre" or "post".
#' See the note below.
#' @param ... arguments to be passed to the function sdm2.
#' @return A general equilibrium (see \code{\link{sdm2}}).
#' @note In the timeline model and the time-circle model, we refer to the labor provided in period t as labor t,
#' and the product produced by using labor t as product t+1.
#' When the consumer's period utility function simultaneously includes labor (or leisure) and product,
#' we can choose from one of two assumptions: it can be assumed that the period utility function of the consumer in period $t$
#' includes labor $t$ and product $t$, or it can be assumed that it includes labor $t$ and product $t+1$.
#' These two assumptions are respectively referred to as the wage prepayment assumption and the wage postpayment assumption.
#' @seealso \code{\link{gemCanonicalDynamicMacroeconomic_Timeline_2_2}},\cr
#' \code{\link{gemCanonicalDynamicMacroeconomic_TimeCircle_2_2}},\cr
#' \code{\link{gemCanonicalDynamicMacroeconomic_Sequential_WagePostpayment_4_3}}.
#' @examples
#' \donttest{
#' #### Take the wage postpayment assumption.
#' ge <- gemCanonicalDynamicMacroeconomic_Sequential_3_2()
#' ge$p
#' ge$z
#' addmargins(ge$D, 2)
#' addmargins(ge$S, 2)
#'
#' #### Take the wage prepayment assumption.
#' ge <- gemCanonicalDynamicMacroeconomic_Sequential_3_2(wage.payment  = "pre")
#' ge$p
#' ge$z
#' ge$D
#' ge$S
#'
#' #### Take the wage prepayment assumption.
#' ge <- gemCanonicalDynamicMacroeconomic_Sequential_3_2(
#'   es.prod.lab.firm = 0.8,
#'   eis = 0.8, es.prod.lab.consumer = 0.8, gr = 0.03,
#'   wage.payment  = "pre"
#' )
#' ge$p
#' ge$z
#' ge$D
#' ge$S
#'
#' #### an example of steady-state equilibrium at
#' # http://gecon.r-forge.r-project.org/models/rbc.pdf
#' ge <- gemCanonicalDynamicMacroeconomic_Sequential_3_2(
#'   beta.prod.firm = 0.36,
#'   depreciation.rate = 0.025,
#'   Gamma.beta = 0.99,
#'   eis = 0.5,
#'   beta.prod.consumer = 0.3,
#' )
#'
#' ge$p / ge$p[1]
#' addmargins(ge$D, 2)
#' addmargins(ge$S, 2)
#' }
gemCanonicalDynamicMacroeconomic_Sequential_3_2 <- function(alpha.firm = 1,
                                                            es.prod.lab.firm = 1,
                                                            beta.prod.firm = 0.35,
                                                            depreciation.rate = 0.06,
                                                            eis = 1,
                                                            Gamma.beta = 0.97,
                                                            es.prod.lab.consumer = 1,
                                                            beta.prod.consumer = 0.4,
                                                            gr = 0,
                                                            wage.payment  = "post",
                                                            ...) {
  if ((wage.payment  == "post") && (gr > 0) && (es.prod.lab.consumer != 1)) {
    warning("Li: In this case ((wage.payment  == 'post') && (gr > 0) && (es.prod.lab.consumer != 1)),
            the function gemCanonicalDynamicMacroeconomic_Sequential_WagePostpayment_4_3 should be used instead of this function.")
  }

  yield.rate <- sserr(eis = eis, Gamma.beta = Gamma.beta, gr = gr, prepaid = TRUE)

  switch(wage.payment ,
    "post" = {
      # Take the wage postpayment assumption.
      dst.firm <- node_new("output",
        type = "CESAK", es = es.prod.lab.firm,
        alpha = alpha.firm, betaK = beta.prod.firm, alphaK = 1 - depreciation.rate,
        "cc1", "lab"
      )
      node_set(dst.firm, "cc1",
        type = "FIN", rate = c(1, yield.rate),
        "prod", "equity.share"
      )
    },
    "pre" = {
      # Take the wage prepayment assumption.
      dst.firm <- node_new("output",
        type = "FIN", rate = c(1, yield.rate),
        "cc1", "equity.share"
      )
      node_set(dst.firm, "cc1",
        type = "CESAK", es = es.prod.lab.firm,
        alpha = alpha.firm, betaK = beta.prod.firm, alphaK = 1 - depreciation.rate,
        "prod", "lab"
      )
    },
    stop("Li: Wrong wage payment method.")
  )

  dst.consumer <- node_new("util",
    type = "CES", es = es.prod.lab.consumer,
    alpha = 1, beta = c(beta.prod.consumer, 1 - beta.prod.consumer),
    "prod", "lab"
  )

  ge <- sdm2(
    A = list(dst.firm, dst.consumer),
    B = matrix(c(
      1, 0,
      0, 0,
      0, 0
    ), 3, 2, TRUE),
    S0Exg = matrix(c(
      NA, NA,
      NA, 100,
      NA, 100
    ), 3, 2, TRUE),
    names.commodity = c("prod", "lab", "equity.share"),
    names.agent = c("firm", "consumer"),
    numeraire = "lab",
    GRExg = gr,
    ...
  )

  ge$dst.firm <- dst.firm
  ge$dst.consumer <- dst.consumer

  return(ge)
}
