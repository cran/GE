#' @export
#' @title Make a Policy of Head and/or Tail Adjustment for a Timeline Model
#' @aliases makePolicyHeadTailAdjustment
#' @description Make a policy of head and/or tail adjustment for a timeline model.
#' A timeline model is an intertemporal non-sequential model that includes production and a given initial product supply.
#' Head adjustment refers to the adjustment of the initial product supply to a steady-state value.
#' Similarly, tail adjustment refers to the adjustment of the share coefficient of the last period of the consumer in the timeline model in order to let the model run in a steady-state equilibrium path.
#' @param type a character string specifying the type of the policy, must be one of "both" (default), "head", "tail" or "none".
#' If type=="none", NULL will be returned.
#' @param gr the growth rate.
#' @param np the number of planning periods.
#' @return A policy, which is often used as an argument of the function sdm2.
#' @seealso {
#' \code{\link{gemIntertemporal_Dividend}};
#' \code{\link{gemIntertemporal_Money_Dividend_Example7.5.1}}
#' }

makePolicyHeadTailAdjustment <- function(type = c("both", "tail", "head", "none"), gr = 0, np) {
  policyHeadAdjustment <- function(A, state) {
    ratio.output.head <- state$last.z[2] / (state$last.z[1] * (1 + gr))
    if (is.null(A[[1]]$y1)) {
      A[[1]]$y1 <- state$S[1, np]
    } else {
      A[[1]]$y1 <- state$S[1, np] <- A[[1]]$y1 * ratio.output.head
    }

    state
  }

  policyTailAdjustment <- function(A, state) {
    ratio.output.tail <- state$last.z[np - 1] / (state$last.z[np - 2] * (1 + gr))
    if (A[[np]]$type == "FIN") {
      tmp.node <- node_set(A[[np]], "cc1")
    } else {
      tmp.node <- A[[np]]
    }

    tmp.n <- length(tmp.node$beta)
    tail.beta <- tmp.node$beta[tmp.n]
    if (tail.beta == 0) tail.beta <- 1 / tmp.n
    tail.beta <- tail.beta / ratio.output.tail
    tmp.node$beta <- prop.table(c(tmp.node$beta[1:(tmp.n - 1)], tail.beta))
  }

  switch(type[1],
    "both" = {
      return(list(policyHeadAdjustment, policyTailAdjustment))
    },
    "tail" = {
      return(policyTailAdjustment)
    },
    "head" = {
      return(policyHeadAdjustment)
    },
    "none" = NULL,
    stop("Wrong type.")
  )
}
