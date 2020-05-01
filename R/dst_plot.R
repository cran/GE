#' @import stringr
#' @importFrom graphics plot
#' @export
#' @title Plot a Demand Structure Tree
#' @aliases dst_plot
#' @description Plot a demand structure tree by the package DiagrammeR
#' and the type attribute of each node is shown.
#' @param dst a demand structural tree.
#' @seealso \code{\link{demand_coefficient}}

dst_plot <- function(dst) {
  tmp.dst <- Clone(dst)
  SetNodeStyle(tmp.dst, shape = "box")
  tmp.dst$Do(function(node) {
    if (!is.null(node$type)) {
      node$name <-
        str_c(node$name, "(", node$type, ")")
    }
  }, filterFun = isNotLeaf)
  plot(tmp.dst)
}
