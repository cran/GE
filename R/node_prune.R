#' @export
#' @title Prune Nodes off a Tree by Names
#' @aliases node_prune
#' @description A wrapper of data.tree::Prunes. Prune nodes off a tree by names.
#' This function has side-effects, it modifies the tree given by the argument (see the package data.tree).
#' @param tree a tree (i.e. a Node object).
#' @param ... some character strings specifies the names of nodes to be pruned.
#' @return Invisibly returns the tree.
#' @examples
#' \donttest{
#' dst <- node_new(
#'   "firm",
#'   "lab", "cap", "land"
#' )
#' node_prune(
#'   dst,
#'   "cap", "land"
#' )
#' plot(dst)
#' }


node_prune <- function(tree, ...) {
  arg.list <- list(...)

  for (k in seq_along(arg.list)) {
    Prune(tree, function(x) x$name != arg.list[[k]])
  }

  invisible(tree)
}
