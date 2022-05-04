#' @importFrom graphics plot
#' @export
#' @title Plot a Tree and Show the Type Attribute
#' @aliases node_plot
#' @description A wrapper of the function plot.Node of the packages data.tree.
#' If a non-leaf node has a type attribute, then the attribute will be shown.
#' @param node a tree (i.e. a Node object).
#' @param param If TRUE, those parameters such as alpha, beta, es etc. will be shown.
#' @param ... arguments to be to be passed to the function plot.Node.
#' @seealso \code{\link{demand_coefficient}}

node_plot <- function(node, param = FALSE, ...) {
  tmp.node <- Clone(node)
  SetNodeStyle(tmp.node, shape = "box")
  tmp.node$Do(function(the.node) {
    if (!is.null(the.node$type)) {
      the.name <- paste0(the.node$name, "(", the.node$type, ")")
      if (param) {
        switch(the.node$type,
          "CD" = {
            the.name <- paste0(
              the.name, "\n", "alpha=", round(the.node$alpha, 4), "\n",
              "beta=(", paste0(round(the.node$beta, 4), collapse = ", "), ")"
            )
          },
          "CES" = ,
          "SCES" = {
            if (is.null(the.node$es) && !is.null(the.node$sigma)) the.node$es <- 1 / (1 - the.node$sigma)
            the.name <- paste0(
              the.name, "\n",
              "es=", round(the.node$es, 4),
              ", alpha=", round(the.node$alpha, 4), "\n",
              "beta=(", paste0(round(the.node$beta, 4), collapse = ", "), ")"
            )
          },
          "Leontief" = {
            if (length(the.node$a) > 1) {
              the.name <- paste0(
                the.name, "\n",
                "a=(", paste0(round(the.node$a, 4), collapse = ", "), ")"
              )
            } else {
              the.name <- paste0(
                the.name, "\n",
                "a=", round(the.node$a, 4)
              )
            }
          },
          "FIN" = {
            if (!is.null(the.node$rate)) {
              the.name <- paste0(
                the.name, "\n",
                "rate=(", paste0(round(the.node$rate, 4), collapse = ", "), ")"
              )
            } else {
              the.name <- paste0(
                the.name, "\n",
                "beta=(", paste0(round(the.node$beta, 4), collapse = ", "), ")"
              )
            }
          },
          "StickyLinear" = ,
          "SL" = {
            the.name <- paste0(
              the.name, "\n",
              "beta=(", paste0(round(the.node$beta, 4), collapse = ", "), ")"
            )
          }
        )
      }

      the.node$name <- the.name
    }
  }, filterFun = isNotLeaf)
  plot(tmp.node, ...)
}
