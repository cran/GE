#' @import CGE data.tree DiagrammeR
#' @export
#' @title Compute Demand Coefficients of a Sector with a Demand Sturctural Tree
#' @aliases demand_coefficient
#' @description Given a price vector, this function computes the demand coefficients of a sector with a demand structural tree. The class of a demand structural tree is Node defined by the package data.tree.
#' For a CES function, this function always assume that it has a standard form such as
#' alpha * (beta1 * (x1 / beta1)^sigma + beta2 * (x2 / beta2)^sigma)^(1 / sigma) wherein beta1 + beta2 == 1.
#' @param node the demand structural tree.
#' @param p the price vector with names of commodities.
#' @return A vector consisting of demand coefficients.
#' @examples
#' dst <- Node$new("firm", type = "Leontief", a = c(0.5, 0.1))
#' dst$AddChild("wheat")$AddSibling("iron")
#' plot(dst)
#'
#' demand_coefficient(dst, p = c(wheat = 1, iron = 2)) # the same as a = c(0.5, 0.1)
#'
#' ####
#' dst <- Node$new("firm", type = "CD", alpha = 1, beta = c(0.5, 0.5))
#' dst$AddChild("wheat")$AddSibling("iron")
#'
#' demand_coefficient(dst, p = c(wheat = 1, iron = 2))
#' # the same as the following
#' CD_A(1, c(0.5, 0.5), c(1, 2))
#'
#' ####
#'
#' dst <- Node$new("firm",
#'   type = "CES", sigma = -1,
#'   alpha = 2, beta = c(0.8, 0.2), theta = c(0.8, 0.2)
#' )
#' dst$AddChild("wheat")$AddSibling("iron")
#'
#' demand_coefficient(dst, p = c(wheat = 1, iron = 2))
#' # the same as the following
#' CES_A(sigma = -1, alpha = 2, Beta = c(0.8, 0.2), p = c(1, 2), Theta = c(0.8, 0.2))
#'
#' ####
#' dst <- Node$new("firm",
#'   func = function(p) CES_A(sigma = -1, alpha = 2, Beta = c(0.8, 0.2), p, Theta = c(0.8, 0.2))
#' )
#' dst$AddChild("wheat")$AddSibling("iron")
#' demand_coefficient(dst, p = c(wheat = 1, iron = 2))
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
#' dst <- Node$new("firm", type = "FIN", rate = c(1, 1 / 3)) # Financial-type Demand Structure
#' dst$AddChild("cc1", type = "Leontief", a = c(0.6, 0.15))$
#'   AddChild("product")$AddSibling("labor")$
#'   parent$
#'   AddSibling("money")
#' plot(dst)
#' demand_coefficient(dst, p)
#'
#' ## the same as above
#' dst <- Node$new("firm", type = "Leontief", a = c(0.8, 0.2))
#' dst$AddChild("cc1", type = "FIN", rate = c(0.75, 1/3))$
#'   AddChild("product")$AddSibling("money")
#' dst$AddChild("cc2", type = "FIN", rate = c(0.75, 1/3))$
#'   AddChild("labor")$AddSibling("money")
#' plot(dst)
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
          the.input.coef <- SCES_A(
            node$sigma, node$alpha, node$beta, the.input.p
          )
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
          if (length(node$rate) == length(the.input.p)) {
            tmp.input.value <- the.input.p[1] * node$rate[1]
            the.input.coef <- c(
              node$rate[1],
              tmp.input.value * node$rate[-1] / the.input.p[-1]
            )
          } else if (length(node$rate) == length(the.input.p) - 1) {
            the.input.coef <- c(1, the.input.p[1] * node$rate / the.input.p[-1])
          } else {
            stop("Li: wrong length of node$rate.")
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
