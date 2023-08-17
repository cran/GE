#' @export
#' @title Applying a Function to All Combinations of the Supplied Vectors
#' @aliases apply_expand.grid
#' @description  A wrapper of the functions apply and expand.grid.
#' Returns a data frame of values obtained by applying a function
#' to all combinations of the supplied vectors.
#' Firstly, the function expand.grid will be used for the supplied vectors in ... and
#' we will get a data frame containing one row for each combination of the supplied vectors.
#' Then the function will be applied to each row of the data frame.
#' The values of the data frame will also be included in the returned data frame.
#' @param FUN the function to be applied. The argument is a numeric vector.
#' @param ... numeric vectors.
#' @return A data frame.
#' @examples
#' \donttest{
#' apply_expand.grid(prod, a = 1:9, b = 1:9)
#'
#' ####
#' f <- function(x) c(r1 = sum(x), r2 = unname(x["b"] - x["a"]))
#' apply_expand.grid(f, a = c(1, 2), b = c(3, 4))
#'
#' ####
#' f <- function(x) list(list(sum(x)), prod(x))
#' apply_expand.grid(f, a = c(1, 2), b = c(3, 4))
#'
#' ####
#' f <- function(x) {
#'   result <- SCES_A(alpha = 1, Beta = c(0.5, 0.5), p = c(x["p1"], 1), es = x["es"])
#'   names(result) <- c("dc1", "dc2")
#'   result
#' }
#'
#' apply_expand.grid(f, p1 = seq(0.1, 10, 0.1), es = c(0.3, 0.5, 1))
#' }
#'
apply_expand.grid <- function(FUN, ...) {
  param.list <- match.call(expand.dots = FALSE)$`...`
  first.names <- names(param.list)

  param <- expand.grid(...)

  for (k in seq_along(first.names)) {
    if (first.names[k] == "" && is.name(param.list[k])) {
      first.names[k] <- as.character(param.list[k])
    }
  }

  colnames(param) <- first.names

  result <- apply(param, 1, FUN)

  if (is.vector(result)) {
    dim(result) <- c(length(result), 1)
    return(cbind(result, param))
  } else {
    return(cbind(t(result), param))
  }
}
