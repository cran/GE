#' @export
#' @title Tidy a General Equilibrium
#' @aliases ge_tidy
#' @description Add names to the matrices and vectors of a general equilibrium, and add
#' demand matrix, demand value matrix and supply value matrix to it.
#' @param ge the general equilibrium.
#' @param names.commodity a character vector consisting of
#' names of commodities.
#' @param names.agent a character vector consisting of
#' names of agents.
#' @return A tidyed general equilibrium.

ge_tidy <- function(ge, names.commodity, names.agent) {
  names(ge$p) <- names.commodity
  names(ge$z) <- names.agent

  ge$D <- ge$A %*% dg(ge$z)
  ge$DV <- dg(ge$p) %*% ge$D

  ge$SV <- dg(ge$p) %*% ge$S

  colnames(ge$A) <- colnames(ge$S) <- colnames(ge$D) <-
      colnames(ge$DV) <- colnames(ge$SV) <- names.agent

    rownames(ge$A) <- rownames(ge$S) <- rownames(ge$D) <-
      rownames(ge$DV) <- rownames(ge$SV) <- names.commodity


  ge
}
