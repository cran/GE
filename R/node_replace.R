#' @export
#' @title Replace a Node of a Tree
#' @aliases node_replace
#' @description  Scan the tree and replace the first non-root node having the name specified. \cr
#' This function is based on the package data.tree and has side-effects.
#' It modifies the tree given by the argument (see the package data.tree).
#' @param tree a tree (i.e. a Node object).
#' @param node.name a character string specifying the name of the node to be pruned off.
#' @param ... some Node objects or character strings.
#' A character string will be treated as the name of a new node to be created.
#' Those nodes will be added to the tree.
#' @return Invisibly returns the parent node of those new nodes.
#' @examples
#' \donttest{
#' dst.firm <- node_new(
#'   "output",
#'   "prod1", "prod2"
#' )
#' plot(dst.firm)
#'
#' dst.VA <- node_new(
#'   "VA",
#'   "lab", "cap"
#' )
#'
#' node_replace(
#'   dst.firm, "prod2",
#'   dst.VA, "prod3"
#' )
#' plot(dst.firm)
#'
#' node_replace(
#'   dst.firm, "lab",
#'   "labor"
#' )
#' plot(dst.firm)
#'
#' node_replace(
#'   dst.firm, "VA",
#'   "prod2"
#' )
#' plot(dst.firm)
#' }
#'
node_replace <- function(tree, node.name, ...) {
  the.parent <- node_set(tree, node.name)$parent
  if (is.null(the.parent)) {
    warning("Li: the non-root node having the name specified is not found.")
    return()
  }

  tmp.parent <- Clone(the.parent)
  node.names <- sapply(the.parent$children, function(x) x$name)
  for (name.k in node.names) {
    node_prune(the.parent, name.k)
  }

  for (name.k in node.names) {
    if (name.k == node.name) {
      new.nodes <- list(...)

      lapply(new.nodes, function(x) {
        switch(class(x)[1],
          "character" = {
            the.parent$AddChild(x)
          },
          "R6" = ,
          "Node" = {
            the.parent$AddChildNode(x)
          },
          stop("Li: the class of the new node is wrong.")
        )
      })
    } else {
      the.parent$AddChildNode(node_set(tmp.parent, name.k))
    }
  }

  invisible(the.parent)
}
