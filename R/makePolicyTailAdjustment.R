#' @export
#' @title Make a Policy of Tail Adjustment for a Timeline Model
#' @aliases makePolicyTailAdjustment
#' @description Make a policy of tail adjustment for a timeline model.
#' Tail adjustment refers to the adjustment of the share coefficient of the last period of the consumer in the timeline model in order to let the model run in a steady-state equilibrium path.
#' @param ind a 2-column matrix or a numeric 2-vector that will be converted into a 2-column matrix.
#' In each row of the matrix, the first element corresponds to the index number of the last activity level of producing the product,
#' and the second element corresponds to the index number of a consumer who demands the product in the final period.
#' @param gr the growth rate.
#' @return A policy, which is often used as an argument of the function sdm2.
#' @seealso {
#' \code{\link{makePolicyHeadTailAdjustment}};
#' \code{\link{makePolicyHeadAdjustment}}
#' }

makePolicyTailAdjustment <- function(ind, gr = 0) {
  ind <- rbind(ind)

  policyTailAdjustment <- function(A, state) {
    for (k in 1:nrow(ind)) {
      ind.z <- ind[k,1]
      ind.consumer <- ind[k,2]

      ratio.output.tail <- state$last.z[ind.z ] / (state$last.z[ind.z- 1] * (1 + gr))


      if (A[[ind.consumer]]$type == "FIN") {
        tmp.node <- node_set(A[[ind.consumer]], "cc1")
      } else {
        tmp.node <- A[[ind.consumer]]
      }

      tmp.n <- length(tmp.node$beta)
      tail.beta <- tmp.node$beta[tmp.n]
      if (tail.beta == 0) tail.beta <- 1 / tmp.n
      tail.beta <- tail.beta / ratio.output.tail
      tmp.node$beta <- prop.table(c(tmp.node$beta[1:(tmp.n - 1)], tail.beta))
    }

  }

  return(policyTailAdjustment)
}
