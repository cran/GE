#' @export
#' @title Ratio Adjustment
#' @aliases ratio_adjust
#' @description Adjust ratios to new values.
#' @details For a positive ratio and the following methods, the return values are as follows:
#' \itemize{
#' \item log : coef * log(ratio) + 1, if ratio >= 1; 1 / (coef * log(1 / ratio) + 1), if ratio < 1.
#' \item left.linear : 1 / (coef * (1 / ratio - 1) + 1), if ratio >= 1; 1 + coef * (ratio - 1), if ratio < 1.
#' \item max : max(coef * log(ratio) + 1, 0).
#' \item linear : coef * (ratio - 1) + 1.
#' }
#' @param ratio a positive numeric vector.
#' @param coef a positive number.
#' @param method a character string specifying the adjustment method.
#' @return A vector.
#' @examples
#' ratio_adjust(10, 0.8)
#' ratio_adjust(0.1, 0.8)
#'
#' x <- seq(0.01, 2, 0.01)
#' plot(x, x, type = "l")
#' lines(x, ratio_adjust(x, 0.8, method = "log"), col = "red")
#' lines(x, ratio_adjust(x, 0.8, method = "left.linear"), col = "blue")
#' lines(x, ratio_adjust(x, 0.8, method = "max"), col = "green")
#'

ratio_adjust <- function(ratio, coef = 0.8,
                            method = c("log","left.linear", "max", "linear")) {
  switch(method[1],
    log = { #symmetric
      result <- ifelse(ratio >= 1,
        coef * log(ratio) + 1,
        1 / (coef * log(1 / ratio) + 1)
      )
    },
    left.linear = { #symmetric
      result <- ifelse(ratio >= 1,
        1 / (coef * (1 / ratio - 1) + 1),
        coef * (ratio - 1) + 1
      )
    },
    max = { # asymmetric
      result <- pmax(coef * log(ratio) + 1, 0)
    },
    linear = { #asymmetric
      result <- coef * (ratio - 1) + 1
    }
  )

  return(result)
}


