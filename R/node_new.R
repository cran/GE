#' @export
#' @title Create a Tree
#' @aliases node_new
#' @description Create a tree by the \code{\link{node_set}} function.
#' @param root.name a character string specifying the name of the root node.
#' @param ... attribute names and values (e.g. alpha=1).
#' The parameter name of a value will be treated as the name of an attribute.\cr
#' A value without a parameter name will be treated as a child node or the name of a child node.
#' If the class of the value is Node, it will be added as a child.
#' If the class of the value is character, a child node (or some child nodes) will be created with the value
#' as the name (or names).
#' @return A tree (i.e. a Node object).
#' @examples
#' \donttest{
#' #### create a tree
#' dst1 <- node_new("firm1")
#' print(dst1)
#'
#' ## create a tree with children
#' dst <- node_new(
#'   "firm",
#'   "lab", "cap", dst1
#' )
#' print(dst)
#'
#' # the same as above
#' dst <- node_new(
#'   "firm",
#'   c("lab", "cap"), dst1
#' )
#' print(dst)
#'
#' #### create a tree with attributes
#' dst <- node_new("firm",
#'   type = "CD", alpha = 1, beta = c(0.5, 0.5)
#' )
#' node_print(dst)
#'
#' #### create a tree with attributes and children
#' dst <- node_new("firm",
#'   type = "CD", alpha = 1, beta = c(0.5, 0.5),
#'   "lab", "cap"
#' )
#' node_print(dst)
#'
#' }
#'
node_new <- function(root.name, ...) {

  the.node <- node_set(root.name, NA, ...)

  the.node
}
