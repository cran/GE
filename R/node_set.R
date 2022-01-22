#' @export
#' @title Create a Tree or Set Attributes for a Node
#' @aliases node_set
#' @description Create a tree or set attributes for a node by the package data.tree.
#' This function can also be used to add child nodes to a node.
#' This function has side-effects, it modifies the tree given by the argument (see the package data.tree).
#' @param tree a tree (i.e. a Node object) or a character string.
#' If it is a character string, a tree will be created and the string will be the name of the root.
#' And in this case, if you need to use the following parameters to set the attributes of the root node,
#' then the second parameter node.name should be set to NA.
#' @param node.name a character string, the name of a node.
#' If the first parameter is a tree, the value of this parameter should be the name of a node in the tree.
#' @param ... attribute names and values (e.g. alpha=1).
#' The parameter name of a value will be treated as the name of an attribute.
#' If a value is NULL, the corresponding attribute should exist and will be deleted. \cr
#'
#' A value without a parameter name will be treated as a child node or the name of a child node.
#' If the class of the value is Node, it will be added as a child.
#' If the class of the value is character, a child node (or some child nodes) will be created with the value
#' as the name (or names).
#' @return Invisibly returns the node.
#' @seealso \code{\link{node_new}}
#' @examples
#' \donttest{
#' #### create a tree
#' dst1 <- node_set("firm1")
#' print(dst1)
#'
#' ## create a tree with children
#' dst <- node_set(
#'   "firm", NA,
#'   "lab", "cap", dst1
#' )
#' print(dst)
#'
#' # the same as above
#' dst <- node_set(
#'   "firm", NA,
#'   c("lab", "cap"), dst1
#' )
#' print(dst)
#'
#' #### create a tree with attributes
#' dst <- node_set("firm", NA,
#'   type = "CD", alpha = 1, beta = c(0.5, 0.5)
#' )
#' print(dst, "type", "alpha", "beta")
#'
#' #### create a tree with attributes and children
#' dst <- node_set("firm", NA,
#'   type = "CD", alpha = 1, beta = c(0.5, 0.5),
#'   "lab", "cap"
#' )
#' print(dst, "type", "alpha", "beta")
#'
#' #### set attributes for a node
#' dst.firm <- node_set("firm", NA, "VA")
#' node_set(dst.firm, "VA",
#'   type = "CD",
#'   alpha = 0.8^-0.8 * 0.2^-0.2,
#'   beta = c(0.8, 0.2),
#'   "lab",
#'   "cap"
#' )
#' print(dst.firm, "alpha", "beta")
#'
#' ## set attributes and add a child for a node
#' node_set(dst.firm, "VA",
#'   type = "SCES",
#'   alpha = 1,
#'   beta = c(0.1, 0.8, 0.1),
#'   es = 0,
#'   "land"
#' )
#' print(dst.firm, "type", "alpha", "beta", "es")
#'
#' ## find a node
#' x <- node_set(dst.firm, "VA")
#' node_print(x)
#' }
#'
node_set <- function(tree, node.name = NA, ...) {
  if (is.na(node.name)) {
    if (is.character(tree)) {
      the.node <- Node$new(tree)
    } else {
      stop("Li: The first argument is expected to be a character string.")
    }
  } else {
    the.node <- FindNode(tree, node.name)
    if (is.null(the.node)) {
      warning("Li: node not found.")
      return(NULL)
    }
  }

  arg.list <- list(...)

  arg.names <- names(arg.list)

  for (k in seq_along(arg.list)) {
    if (is.null(arg.names) || arg.names[k] == "") {
      switch(class(arg.list[[k]])[1],
        "character" = {
          tmp <- arg.list[[k]]
          for (tmp.k in seq_along(tmp)) {
            the.node$AddChild(tmp[tmp.k])
          }
        },
        "R6" = ,
        "Node" = {
          the.node$AddChildNode(arg.list[[k]])
        }
      )
    } else {
      if (is.null(arg.list[[k]])) {
        the.node$RemoveAttribute(arg.names[k])
      } else {
        eval(parse(text = paste("the.node$", arg.names[k], " <- arg.list[[k]]")))
      }
    }
  }

  invisible(the.node)
}
