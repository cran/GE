#' @export
#' @title A SCES-type General Equilibrium Model based on an Input-Output Table.
#' @aliases gemInputOutputTable_SCES_3_3
#' @description Given a 3×3 input-output table (e.g., see Zhang Xin, 2017, Table 2.2.2), this model can be
#' used to calculate the corresponding equilibrium.
#' This input-output table contains two firms and one household.
#' The household consumes products and supplies labor.
#' @param input the input matrix in the base period.
#' @param output a vector consisting of the product outputs and labor supply in the base period.
#' @param es a scalar, which is the elasticity of substitution between the
#' inputs.
#' @return A general equilibrium, which is a list with the following elements:
#' \itemize{
#' \item p - the price vector with labor as numeraire.
#' \item D - the demand matrix, also called the input table. Wherein the benchmark prices are used.
#' \item DV - the demand value matrix, also called the value input table. Wherein the current price is used.
#' \item SV - the supply value matrix, also called the value output table. Wherein the current price is used.
#' \item ... - some elements returned by the CGE::sdm function
#' }
#'
#' @references Zhang Xin. (2017, ISBN: 9787543227637). Principles of Computable General Equilibrium Modeling and Programming (Second Edition). Shanghai: Gezhi Press. (In Chinese)
#' @examples
#' x <- 75
#' gemInputOutputTable_SCES_3_3(
#'   input = matrix(c(
#'     200, 300, 100,
#'     x, 320, 530,
#'     250, 380, 0
#'   ), 3, 3, TRUE),
#'   output = c(600, 1000, 630),
#'   es = 0.5
#' )

gemInputOutputTable_SCES_3_3 <- function(input = matrix(c(
                                          200, 300, 100,
                                          150, 320, 530,
                                          250, 380, 0
                                        ), 3, 3, TRUE),
                                        output = c(600, 1000, 630),
                                        es = 0) {
  Beta <- prop.table(input, 2)
  alpha <- output / colSums(input)
  ge <- sdm(
    A = function(state) {
      SCES_A(alpha = alpha, Beta = Beta, p = state$p, es = rep(es, 3))
    },
    B = diag(3),
    S0Exg = {
      tmp <- matrix(NA, 3, 3)
      tmp[3, 3] <- output[3]
      tmp
    }
  )
  ge$p <- ge$p / ge$p[3]
  ge <- ge_tidy(ge,
    names.commodity = c("product1", "product2", "labor"),
    names.agent = c("firm1", "firm2", "household")
  )

  return(ge)
}
