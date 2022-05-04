#' @export
#' @title Aggregate Some Rows and Columns of a Matrix
#' @aliases matrix_aggregate
#' @description Aggregate some rows and columns of a matrix to obtain a matrix with smaller dimensions.
#' This function can be used for aggregating some rows and columns of an input-output table.
#' @param M a numeric matrix without NA.
#' @param row.index a numeric vector or a list of numeric vectors indicating the index numbers of rows to be aggregated.
#' The default value is is NULL.
#' @param col.index a numeric vector or a list of numeric vectors indicating the index numbers of columns to be aggregated.
#' The default value is is NULL.
#' @param row.name a character vector or a list of character vectors indicating the names of rows to be aggregated.
#' The default value is NULL.
#' If row.index or col.index is not NULL, row.name and col.name will be ignored.
#' @param col.name a character vector or a list of character vectors indicating the names of columns to be aggregated.
#' The default value is NULL.
#' @examples
#' \donttest{
#' M <- matrix(1:16,4,4,TRUE)
#' colnames(M) <- paste0("c",1:4)
#' rownames(M) <- paste0("r",1:4)
#' addmargins(M)
#'
#' M2 <- matrix_aggregate(M, list(c(1,3),c(2, 4)), 2:3)
#' addmargins(M2)
#'
#' M3 <- matrix_aggregate(M, row.name = list(c("r1","r3"),c("r2","r4")), col.name = c("c2","c3"))
#' addmargins(M3)
#' }

matrix_aggregate <-function(M, row.index=NULL, col.index=NULL,
                            row.name=NULL, col.name=NULL){

  col.merge<-function(M, index){
    first.of.index <- min(index)
    other.index <- index[index!=first.of.index]

    M[, first.of.index] <- rowSums(M[,index, drop=FALSE])

    M[, other.index] <- NA

    return(M)
  }

  M<-as.matrix(M)

  if((!is.null(row.index))||(!is.null(col.index))){
    if(!is.null(col.index)){
        if (!is.list(col.index)) col.index<-list(col.index)
        for (k in seq_along(col.index)){
          M <- col.merge(M, col.index[[k]])
        }
      }

    if(!is.null(row.index)){
      if (!is.list(row.index)) row.index<-list(row.index)
      for (k in seq_along(row.index)){
        M <- t(col.merge(t(M), row.index[[k]]))
      }
    }

    #delete all-na rows and columns
    matrix.delete.na.row<-function(M){
      flag.to.delete <- apply(M,1,function(the.row) if (all(is.na(the.row))) return(TRUE) else return(FALSE))
      return(M[!flag.to.delete,])
    }

    M <- matrix.delete.na.row(M)
    M <- t(matrix.delete.na.row(t(M)))

    return(M)

  } else # aggregate by name
  {
    if (!is.null(row.name)){
      if (!is.list(row.name)) row.name <- list(row.name)
      row.index.list <- row.name
      for (k in seq_along(row.index.list)){
        row.index.list[[k]] <- unique(match(row.index.list[[k]], rownames(M)))
      }
    }

    if (!is.null(col.name)){
      if (!is.list(col.name)) col.name <- list(col.name)
      col.index.list <- col.name
      for (k in seq_along(col.index.list)){
        col.index.list[[k]] <- unique(match(col.index.list[[k]], colnames(M)))
      }
    }

    return(matrix_aggregate(M, row.index.list, col.index.list))
  }
}
