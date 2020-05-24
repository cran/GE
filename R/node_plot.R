#' @importFrom graphics plot
#' @export
#' @title Plot a Tree and Show the Type Attribute
#' @aliases node_plot
#' @description A wrapper of the function plot.Node of the packages data.tree.
#' If a non-leaf node has a type attribute, then the attribute will be shown.
#' And if a type attribute is CES, it will be shown as SCES.
#' @param node a tree (i.e. a Node object).
#' @param ... arguments to be to be passed to the function plot.Node.
#' @seealso \code{\link{demand_coefficient}}

node_plot <- function(node, ...) {
  tmp.node <- Clone(node)
  SetNodeStyle(tmp.node, shape = "box")
  tmp.node$Do(function(the.node) {
    if (!is.null(the.node$type)) {
      if (the.node$type == "CES") {
        the.node$name <-
          paste(the.node$name, "(SCES)", sep = "")
      } else {
        the.node$name <-
          paste(the.node$name, "(", the.node$type, ")", sep = "")
      }
    }
  }, filterFun = isNotLeaf)
  plot(tmp.node, ...)
}
