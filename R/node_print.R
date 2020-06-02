#' @export
#' @title Print a Tree and Its Fields
#' @aliases node_print
#' @description A wrapper of the function print.Node of the package data.tree.
#' Print a tree and its fields except the func field.
#' @param node a Node object.
#' @param ... arguments passed to print.Node.
#' @examples
#' \donttest{
#' dst <- node_new("firm",
#'                 type = "SCES",
#'                 alpha = 2, beta = c(0.8, 0.2),
#'                 es = 0.5,
#'                 "wheat", "iron"
#' )
#'
#' node_print(dst)
#'
#' ####
#' dst <- node_new("firm",
#'                 type = "FUNC",
#'                 func = min,
#'                 "wheat", "iron"
#' )
#'
#' node_print(dst)
#' }
#'
node_print <- function(node, ...) {
  the.fields <- node$fieldsAll
  the.fields <- setdiff(the.fields, "func")
  args <- c(node, as.list(the.fields), list(...))
  do.call(print, args)
}


