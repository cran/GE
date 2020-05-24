#' @export
#' @title Tidy a General Equilibrium
#' @aliases ge_tidy
#' @description Add names to the matrices and vectors of a general equilibrium, and add
#' demand matrix, demand value matrix and supply value matrix to it.
#' @param ge a general equilibrium.
#' @param names.commodity a character vector consisting of
#' names of commodities.
#' @param names.agent a character vector consisting of
#' names of agents.
#' @return A tidied general equilibrium.

ge_tidy <- function(ge, names.commodity, names.agent) {
  names(ge$p) <- names.commodity
  names(ge$z) <- names.agent

  ge$D <- ge$A %*% dg(ge$z)
  ge$DV <- dg(ge$p) %*% ge$D

  ge$SV <- dg(ge$p) %*% ge$S


  dimnames(ge$A) <- dimnames(ge$S) <- dimnames(ge$SV) <- dimnames(ge$D) <-
    dimnames(ge$DV) <- list(names.commodity, names.agent)

  if (!is.null(ge$B)) dimnames(ge$B) <- list(names.commodity, names.agent)
  if (!is.null(ge$S0Exg)) dimnames(ge$S0Exg) <- list(names.commodity, names.agent)


  if (!is.null(ge$ts.p)) colnames(ge$ts.p) <- names.commodity
  if (!is.null(ge$ts.q)) colnames(ge$ts.q) <- names.commodity
  if (!is.null(ge$ts.z)) colnames(ge$ts.z) <- names.agent
  if (!is.null(ge$ts.S)) dimnames(ge$ts.S) <- list(names.commodity, names.agent, NULL)

  ge
}
