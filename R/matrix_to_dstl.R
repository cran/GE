#' @export
#' @title Convert a Matrix into a Demand Structural Tree List
#' @aliases matrix_to_dstl
#' @description Convert a demand coefficient matrix into a demand structural tree list.
#' @param x a matrix.
#' @param names.commodity names of commodities.
#' They will be the names of leaf nodes of each demand structural tree.
#' @param names.agent names of agents.
#' They will be the names of root nodes of those demand structural trees.
#' @return  A demand structural tree list.
#' @examples
#' \donttest{
#' A <- matrix(c(
#'   0, 0, 0, 1,
#'   8, 6, 1, 0,
#'   4, 2, 1.5, 0,
#'   2, 1.5, 0.5, 0
#' ), 4, 4, TRUE)
#'
#' dstl <- matrix_to_dstl(A)
#' node_print(dstl[[1]])
#' }
#'
matrix_to_dstl <- function(x,
                           names.commodity = paste("comm", 1:nrow(x), sep = ""),
                           names.agent = paste("agt", 1:ncol(x), sep = "")) {
  result <- list()
  dst <- node_new("agent",
    type = "Leontief"
  )
  for (k in seq_along(names.commodity)) node_set(dst, "agent", names.commodity[k])

  for (k in 1:(ncol(x))) {
    result[[k]] <- Clone(dst)
    result[[k]]$name <- names.agent[k]
    result[[k]]$a <- x[, k]
  }
  result
}
