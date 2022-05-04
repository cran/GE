#' @import CGE data.tree DiagrammeR
#' @export
#' @title Compute the Utility of a Consumer or the Output of a Firm by the Demand Structural Tree
#' @aliases output
#' @description Given a demand structural tree and an input vector,
#' this function computes the utility of a consumer or the output of a firm.
#' If the demand structural tree has a FUNC-type node,
#' the node should has an attribute named fun that is a function computing the output.
#' @param node a demand structural tree.
#' @param input an input vector with names of commodities.
#' @return A scalar.
#' @examples
#' dst <- node_new("output",
#'                 type = "SCES", es = 0, alpha = 1, beta = c(0.5, 0.5),
#'                 "cc1", "cc2"
#' )
#' node_set(dst, "cc1",
#'          type = "Leontief", a = c(0.6, 0.4),
#'          "wheat", "iron"
#' )
#' node_set(dst, "cc2",
#'          type = "SCES", sigma = -1, alpha = 1, beta = c(0.5, 0.5),
#'          "labor", "capital"
#' )
#'
#' node_plot(dst, TRUE)
#'
#' p <- c(wheat = 1, iron = 3, labor = 2, capital = 4)
#' x <-demand_coefficient(dst, p)
#' output(dst, x)

output <- function(node, input) {
  if (isLeaf(node)) {
    return(input[node$name])
  }

  the.input <- sapply(node$children, output, input)

  switch(node$type,
         "SCES" = {
           if (!is.null(node$es)) {
             the.es <-  node$es
           } else {
             the.es <- 1/(1-node$sigma)
           }

           return(SCES(alpha=node$alpha, beta=node$beta, x=the.input, es = the.es))
         },
         "CES" = {
           if (!is.null(node$es)) {
             the.es <-  node$es
           } else {
             the.es <- 1/(1-node$sigma)
           }

           return(CES(alpha=node$alpha, beta=node$beta, x=the.input, es = the.es))
         },
         "CD" = {
           return(node$alpha*prod(the.input^node$beta))
         },
         "Leontief" = {
           return(min(the.input/node$a))
         },
         "FIN" = {
           if (!is.null(node$beta)) {
             tmp.rate <- beta_to_rate(node$beta)
           } else {
             tmp.rate <- node$rate
           }

           return(unname(the.input[1]/tmp.rate[1]))

         },
         "FUNC" = {
           if (is.null(node$fun)) {
             stop("Li: A FUNC-type node should has a function (namely fun) to compute output.")
           } else {
             return(node$fun(the.input))
           }

         },
         "SL" = ,
         "StickyLinear" = {
           return(sum(the.input * node$beta))
         },

         stop("Li: wrong node$type.")
  )
}
