#' @export
#' @title Insert Nodes into a Tree
#' @aliases node_insert
#' @description  Scan the tree and insert nodes before the first non-root node having the name specified. \cr
#' This function is based on the package data.tree and has side-effects.
#' It modifies the tree given by the argument (see the package data.tree).
#' @param tree a tree (i.e. a Node object).
#' @param node.name a character string specifying the name of a node. Some nodes will be inserted before it.
#' @param ... some Node objects or character strings.
#' A character string will be treated as the name of a new node to be created. Those nodes will be inserted into the tree.
#' @return Invisibly returns the parent node of those new nodes.
#' @examples
#' \donttest{
#' dst.firm <- node_set(
#'   "output", NA,
#'   "prod1",
#'   "prod2"
#' )
#' plot(dst.firm)
#' 
#' dst.VA <- node_set(
#'   "VA", NA,
#'   "lab",
#'   "cap"
#' )
#' 
#' node_insert(dst.firm, "prod1", dst.VA, "prod3")
#' plot(dst.firm)
#' }
node_insert <- function(tree, node.name, ...) {
  the.parent <- node_set(tree, node.name)$parent
  if (is.null(the.parent)) {
    warning("Li: the non-root node having the name specified is not found.")
    return()
  }

  tmp.parent <- Clone(the.parent)
  sibling.node.names <- sapply(the.parent$children, function(x) x$name)
  for (name.k in sibling.node.names) {
    node_prune(the.parent, name.k)
  }

  for (name.k in sibling.node.names) {
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
    }

    the.parent$AddChildNode(node_set(tmp.parent, name.k))
  }
  
  invisible(the.parent)
}
