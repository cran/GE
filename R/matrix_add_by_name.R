#' @export
#' @title Add Matrices by Names of Columns and Rows
#' @aliases matrix_add_by_name
#' @description Add together some matrices by names of columns and rows.
#' Those matrices may have distinct sizes. All matrices should not have column names and row names other than those of the first matrix.
#' @param M a matrix with column names and row names.
#' @param ... some matrices with column names and row names which constitute subsets of those of M.
#' If there is a vector, it will be converted to a matrix of one column and the column will be named after the vector.
#' @return A matirx.
#' @examples
#' M <- matrix(0, 5, 5)
#' colnames(M) <- paste("c", 1:5, sep = "")
#' rownames(M) <- paste("r", 1:5, sep = "")
#'
#' M2 <- matrix(1:9, 3, 3)
#' colnames(M2) <- c("c2", "c3", "c5")
#' rownames(M2) <- c("r1", "r2", "r4")
#'
#' matrix_add_by_name(M, M2)
#'
#' c1 <- c(r1 = 1, r3 = 2)
#' matrix_add_by_name(M, c1)
#' matrix_add_by_name(M, c1, M2)

matrix_add_by_name <- function(M, ...) {
  add.by.name <- function(M, M2) {
    if (is.null(colnames(M2)) && ncol(M2) == 1) colnames(M2) <- name.M2
    if (is.null(rownames(M2)) && nrow(M2) == 1) rownames(M2) <- name.M2

    the.colnames <- colnames(M2)
    the.rownames <- rownames(M2)

    if (is.null(colnames(M)) || is.null(rownames(M)) ||
      is.null(colnames(M2)) || is.null(rownames(M2))) {
      stop("Li, matrix.add.by.name: null name of the first matrix")
    }

    if (any(!(the.colnames %in% colnames(M)))) {
      print(paste(the.colnames[!(the.colnames %in% colnames(M))]))
      stop("Li: wrong colnames")
    }

    if (any(!(the.rownames %in% rownames(M)))) {
      print(paste(the.rownames[!(the.rownames %in% rownames(M))]))
      stop("Li: wrong rownames")
    }

    M[the.rownames, the.colnames] <- M[the.rownames, the.colnames] + M2

    return(M)
  }


  result <- as.matrix(M)

  M.list <- list(...)

  if (length(M.list) > 0) {
    name.list <- match.call(expand.dots = FALSE)$`...`

    for (k in seq_along(M.list)) {
      name.M2 <- as.character(name.list[[k]])
      M2 <- as.matrix(M.list[[k]])
      result <- add.by.name(result, M2)
    }
  }

  return(result)
}
