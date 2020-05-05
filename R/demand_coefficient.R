#' @import CGE data.tree DiagrammeR
#' @export
#' @title Compute Demand Coefficients of an Agent (or a Sector) with a Demand Sturctural Tree
#' @aliases demand_coefficient
#' @description Given a price vector, this function computes the demand coefficients of a sector with a demand structural tree. The class of a demand structural tree is Node defined by the package data.tree.
#' For a CES function, this function always assume that it has a standard form such as
#' alpha * (beta1 * (x1 / beta1)^sigma + beta2 * (x2 / beta2)^sigma)^(1 / sigma) wherein beta1 + beta2 == 1 (see \code{\link{SCES_A}}).
#' @param node the demand structural tree.
#' @param p the price vector with names of commodities.
#' @return A vector consisting of demand coefficients.
#' @details The demand for various commodities by an economic agent can be expressed by a demand structure tree.
#' Each non-leaf node can be regarded as the output of all its child nodes.
#' Each node can be regarded as an input of its parent node.
#' Each non-leaf node usually has an attribute named type.
#' This attribute describes the input-output relationship between the child nodes and the parent node.
#' This relationship can sometimes be represented by a production function or a utility function.
#' The type attribute of each non-leaf node can take the following values.
#' \itemize{
#' \item CES. In this case, this node also has parameters alpha, beta and es (or sigma = 1-1 / es).
#' alpha and es are scalars. beta is a vector. These parameters are parameters of a standard CES function.
#' \item Leontief. In this case, this node also has the parameter a,
#' which is a vector and is the parameter of a Leontief function.
#' \item CD. CD is Cobb-Douglas. In this case, this node also has parameters alpha and beta.
#' These parameters are parameters of a Cobb-Douglas function.
#' \item FIN. That is the financial type. FIN can also be written as money, dividends, bonds and taxes.
#' In this case, this node also has the parameter rate or beta.
#' If the parameter beta is not NULL, then the parameter rate will be ignored.
#' The parameter rate applies to all situations, while the parameter beta only applies for some special cases.
#' For FIN nodes, the first child node should represent for a physical commodity or a composite commodity
#' containing a physical commodity, and other child nodes represent for financial instruments.
#' The parameter beta indicates the proportion of each child node's expenditure.
#' The parameter rate indicates the expenditure ratios between financial-instrument-type child nodes
#' and the first child node.
#' The first element of the parameter rate indicates the amount of the first child node needed to get a unit of output.
#' }
#' If a non-leaf node does not have an attribute named type, then it should have an attribute named func.
#' The value of that attribute is a function which calculates the demand coefficient for the child nodes.
#' The argument of that function is a price vector. The length of that price vector is equal to the number of the child nodes.
#' @examples
#' #### a Leontief-type node
#' dst <- Node$new("firm", type = "Leontief", a = c(0.5, 0.1))
#' dst$AddChild("wheat")$AddSibling("iron")
#' print(dst, "type")
#' plot(dst)
#'
#' demand_coefficient(dst, p = c(wheat = 1, iron = 2)) # the same as a = c(0.5, 0.1)
#'
#' #### a CD-type node
#' dst <- Node$new("firm", type = "CD", alpha = 1, beta = c(0.5, 0.5))
#' dst$AddChild("wheat")$AddSibling("iron")
#'
#' demand_coefficient(dst, p = c(wheat = 1, iron = 2))
#' # the same as the following
#' CD_A(1, c(0.5, 0.5), c(1, 2))
#'
#' #### a CES-type node
#' dst <- Node$new("firm",
#'                 type = "CES",
#'                 alpha = 2, beta = c(0.8, 0.2),
#'                 es = 0.5,
#' )
#' dst$AddChild("wheat")$AddSibling("iron")
#'
#' demand_coefficient(dst, p = c(wheat = 1, iron = 2))
#'
#' # the same as the following
#' SCES_A(alpha = 2, Beta = c(0.8, 0.2), p = c(1, 2), es = 0.5)
#' CES_A(sigma = 1 - 1 / 0.5, alpha = 2, Beta = c(0.8, 0.2), p = c(1, 2), Theta = c(0.8, 0.2))
#'
#' #### a func-type node
#' dst <- Node$new("firm",
#'   func = function(p) CES_A(sigma = -1, alpha = 2, Beta = c(0.8, 0.2), p, Theta = c(0.8, 0.2))
#' )
#' dst$AddChild("wheat")$AddSibling("iron")
#' demand_coefficient(dst, p = c(wheat = 1, iron = 2))
#'
#' # the same as the following
#' CES_A(sigma = -1, alpha = 2, Beta = c(0.8, 0.2), p = c(1, 2), Theta = c(0.8, 0.2))
#'
#' ####
#' p <- c(wheat = 1, iron = 3, labor = 2, capital = 4)
#'
#' dst <- Node$new("firm 1", type = "CES", sigma = -1, alpha = 1, beta = c(1, 1))
#' dst$AddChild("cc1", type = "Leontief", a = c(0.6, 0.4))$
#'   AddChild("wheat")$AddSibling("iron")
#' dst$AddChild("cc2", type = "CES", sigma = -1, alpha = 1, beta = c(1, 1))$
#'   AddChild("labor")$AddSibling("capital")
#' plot(dst)
#' demand_coefficient(dst, p)
#'
#' ####
#' p <- c(product = 1, labor = 1, money = 1)
#' dst <- Node$new("firm", type = "FIN", rate = c(0.75, 1 / 3)) # a financial-type node
#' dst$AddChild("cc1", type = "Leontief", a = c(0.8, 0.2))$
#'   AddChild("product")$AddSibling("labor")$
#'   parent$
#'   AddSibling("money")
#' dst_plot(dst)
#' demand_coefficient(dst, p)
#'
#' ## the same as above
#' p <- c(product = 1, labor = 1, money = 1)
#' dst <- Node$new("firm", type = "Leontief", a = c(0.8, 0.2))
#' dst$AddChild("cc1", type = "FIN", rate = c(0.75, 1 / 3))$
#'   AddChild("product")$AddSibling("money")
#' dst$AddChild("cc2", type = "FIN", rate = c(0.75, 1 / 3))$
#'   AddChild("labor")$AddSibling("money")
#' dst_plot(dst)
#' demand_coefficient(dst, p)
#'
#' ## the same as above
#' p <- c(product = 1, labor = 1, money = 1)
#' dst <- Node$new("firm", type = "FIN", rate = c(1, 1 / 3)) # Financial-type Demand Structure
#' dst$AddChild("cc1", type = "Leontief", a = c(0.6, 0.15))$
#'   AddChild("product")$AddSibling("labor")$
#'   parent$
#'   AddSibling("money")
#' dst_plot(dst)
#' demand_coefficient(dst, p)


demand_coefficient <- function(node, p) {
  compute.price_dc <- function(node, p) {
    if (isLeaf(node)) {
      tmp.name <- node$name
      dc <- 1
      names(dc) <- tmp.name
      return(list(
        price = p[tmp.name],
        dc = dc
      ))
    }

    p_dc <- lapply(node$children, compute.price_dc, p)
    the.input.p <- sapply(p_dc, function(x) x$p)
    child.dc <- lapply(p_dc, function(x) x$dc)

    if (!is.null(node$func)) {
      the.input.coef <- node$func(the.input.p) # the.input.coef is the direct demand coefficient for children.
    } else {
      switch(
        {
          node$type
        },
        "CES" = {
          if (!is.null(node$es)) {
            the.input.coef <- SCES_A(
              alpha = node$alpha, Beta = node$beta, p = the.input.p, es = node$es
            )
          } else {
            the.input.coef <- SCES_A(
              node$sigma, node$alpha, node$beta, the.input.p
            )
          }
        },
        "CD" = {
          the.input.coef <- CD_A(node$alpha, node$beta, the.input.p)
        },
        "Leontief" = {
          the.input.coef <- node$a
        },
        "FIN" = ,
        "money" = ,
        "dividend" = ,
        "bond" = ,
        "tax" = {
          if (!is.null(node$beta)) tmp.rate <- beta_to_rate(node$beta) else
            tmp.rate <- node$rate
          if (length(tmp.rate) == length(the.input.p)) {
            tmp.input.value <- the.input.p[1] * tmp.rate[1]
            the.input.coef <- c(
              tmp.rate[1],
              tmp.input.value * tmp.rate[-1] / the.input.p[-1]
            )
          } else if (length(tmp.rate) == length(the.input.p) - 1) {
            the.input.coef <- c(1, the.input.p[1] * tmp.rate / the.input.p[-1])
          } else {
            stop("Li: wrong length of tmp.rate.")
          }
        },
        stop("Li: wrong type.")
      )
    }

    price <- sum(the.input.p * the.input.coef)

    dc <- p * 0
    for (k in 1:length(the.input.coef)) {
      tmp <- unlist(child.dc[[k]]) * the.input.coef[k]
      dc[names(tmp)] <- dc[names(tmp)] + tmp
    }

    return(list(
      price = price,
      dc = dc
    ))
  }

  p_dc <- compute.price_dc(node, p)
  return(p_dc$dc)
}
