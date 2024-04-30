#' @export
#' @title Make a Policy of Head Adjustment for a Timeline Model
#' @aliases makePolicyHeadAdjustment
#' @description Make a policy of head adjustment for a timeline model.
#' Head adjustment refers to the adjustment of the initial product supply to a steady-state value.
#' @param ind a 4-column matrix or a numeric 4-vector that will be converted into a 4-column matrix.
#' In each row of the matrix, the first element corresponds to the index number of a type of product supplied in the first period,
#' the second element corresponds to the index number of its supplier,
#' the third element corresponds to the index number of the type of product supplied in the second period,
#' and the fourth element corresponds corresponds to the index number of its supplier.
#'
#' Head adjustments are usually made simultaneously with tail adjustments to compute the steady-state equilibrium path.
#' There is usually no need to make head adjustments alone.
#' @param gr the growth rate.
#' @return A policy, which is often used as an argument of the function sdm2.
#' @seealso {
#' \code{\link{makePolicyHeadTailAdjustment}};
#' \code{\link{makePolicyTailAdjustment}}
#' }

makePolicyHeadAdjustment <- function(ind, gr = 0) {
  ind <- rbind(ind)

  policyHeadAdjustment <- function(state) {
    for (k in 1:nrow(ind)) {
      state$S[ind[k, 1], ind[k, 2]] <- state$B[ind[k, 3], ind[k, 4]] * state$last.z[ind[k, 4]] / (1 + gr)
    }

    state
  }

  return(policyHeadAdjustment)
}
