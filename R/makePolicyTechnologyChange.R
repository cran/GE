#' @export
#' @title Make a Policy of Technology Change
#' @aliases makePolicyTechnologyChange
#' @description This function returns a policy function that changes the attributes alpha and a of the demand structure trees of agents specified.
#' An attribute alpha is usually a parameter of a CES or CD function.
#' An attribute a is usually a parameter of a Leontief function.
#' For demand structure trees that do not contain these two attributes, this function has no effect.
#' @param adjumentment.ratio a scalar. The attributes alpha will be multiplied by adjumentment.ratio.
#' The attributes a will be divided by adjumentment.ratio.
#' @param agent a vector specifying the indices or names of agents.
#' @param time.win the time window vector, i.e. a 2-vector specifying the start time and end time of policy implementation.
#' @return A policy function, which is often used as an argument of the function sdm2.
#' @seealso \code{\link{sdm2}}
#' @examples
#' \donttest{
#' dst.firm <- node_new("output",
#'   type = "CD", alpha = 1, beta = c(0.5, 0.5),
#'   "prod", "lab"
#' )
#'
#' dst.consumer <- node_new("utility",
#'   type = "Leontief", a = 1, "prod"
#' )
#'
#' B <- matrix(c(
#'   1, 0,
#'   0, 0
#' ), 2, 2, TRUE)
#' S0Exg <- matrix(c(
#'   NA, NA,
#'   NA, 100
#' ), 2, 2, TRUE)
#'
#' ge <- sdm2(
#'   A = list(dst.firm, dst.consumer), B = B, S0Exg = S0Exg,
#'   names.commodity = c("prod", "lab"),
#'   names.agent = c("firm", "consumer"),
#'   priceAdjustmentFunction = function(p, q) p,
#'   policy = list(
#'     makePolicyTechnologyChange(agent = "firm"),
#'     makePolicyStickyPrice(stickiness = 0, time.win = c(1, 20)),
#'     makePolicyStickyPrice(stickiness = 0.9, time.win = c(20, Inf))
#'   ),
#'   ts = TRUE,
#'   maxIteration = 1,
#'   numberOfPeriods = 40
#' )
#'
#' par(mfrow = c(1, 2))
#' matplot(ge$ts.z, type = "b", pch = 20)
#' matplot(ge$ts.p, type = "b", pch = 20)
#' }
#'
makePolicyTechnologyChange <- function(adjumentment.ratio = 1.1,
                                       agent = 1,
                                       time.win = c(20, 20)) {
  function(time, dstl, state) {
    if (is.character(agent)) {
      agent.index <- match(agent, state$names.agent)
    } else {
      agent.index <- agent
    }

    if (time >= time.win[1] && time <= time.win[2]) {
      for (index.k in agent.index) {
        dstl[[index.k]]$Do(function(node) {
          if (!is.null(node$alpha)) node$alpha <- node$alpha * adjumentment.ratio
          if (!is.null(node$a)) node$a <- node$a / adjumentment.ratio
        },
        filterFun = isNotLeaf
        )
      }
    }
  }
}
