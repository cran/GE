#' @export
#' @title Compute the Return Rate in the Steady State Equilibrium
#' @aliases sserr
#' @description Compute the (postpaid) return rate in the steady state equilibrium.
#' @param eis a positive scalar indicating the elasticity of intertemporal substitution in the intertemporal utility function.
#' @param Gamma.beta a positive scalar indicating the subjective discount factor, which is typically no greater than 1.
#' @param gr a non-negative scalar indicating the growth rate in the steady state equilibrium.
#' @param type a character indicating the type of the intertemporal utility function, which may be CES (i.e. CRRA) or SCES.
#' @param prepaid a logical value. If prepaid is FALSE, the return rate is returned.
#' Otherwise the prepaid steady-state equilibrium return rate (i.e. the current yield rate) is returned.
#' @examples
#' \donttest{
#' sserr(eis = 1, Gamma.beta = 0.97, gr = 0)
#' sserr(eis = 1, Gamma.beta = 1.25, gr = 0)
#' sserr(eis = 1, Gamma.beta = 0.97, gr = 0, type = "SCES")
#'
#' sserr(eis = 0.5, Gamma.beta = 0.97, gr = 0)
#' sserr(eis = 0.5, Gamma.beta = 0.97, gr = 0, type = "SCES")
#' }
#'
sserr <- function(eis, Gamma.beta, gr = 0, type = "CES", prepaid = FALSE) {
  if (!prepaid) {
    if (type == "CES") {
      return(
        (1 + gr)^(1 / eis) / Gamma.beta - 1
      )
    }

    if (type == "SCES") {
      return(
        ((1 + gr) / Gamma.beta)^(1 / eis) - 1
      )
    }
  } else {
    if (type == "CES") {
      return(
        (1 + gr)^(1 / eis - 1) / Gamma.beta - 1
      )
    }

    if (type == "SCES") {
      return(
        (1 + gr)^(1 / eis - 1) * Gamma.beta^(-1 / eis) - 1
      )
    }
  }
}
