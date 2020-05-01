#' @export
#' @title Structural Dynamic Model (alias Structural Growth Model) with a Demand Structure Tree List
#' @aliases sdm_dstl
#' @description This is a wrapper of the function CGE::sdm.
#' The parameter A of CGE::sdm is replaced with a demand structure tree list.
#' @param dstl a demand structure tree list.
#' @param names.commodity names of commodities.
#' @param names.agent names of agents.
#' @param ... arguments to be to be passed to the function CGE::sdm.
#' @return A general equilibrium.
#' @examples
#' #### a pure exchange economy with two agents and two commodities
#' dst.CHN <- Node$new("CHN",
#'                     type = "CES",
#'                     alpha = 1,
#'                     beta = c(0.8, 0.2),
#'                     es = 2
#' )
#' dst.CHN$AddChild("lab.CHN")$AddSibling("lab.ROW")
#' dst_plot(dst.CHN)
#'
#' dst.ROW <- Node$new("ROW",
#'                     type = "CES",
#'                     alpha = 1,
#'                     beta = c(0.05, 0.95),
#'                     es = 2
#' )
#' dst.ROW$AddChild("lab.CHN")$AddSibling("lab.ROW")
#'
#' dstl <- list(dst.CHN, dst.ROW)
#'
#' ge <- sdm_dstl(dstl,
#'                names.commodity = c("lab.CHN", "lab.ROW"),
#'                names.agent = c("CHN", "ROW"),
#'                B = matrix(0, 2, 2, TRUE),
#'                S0Exg = matrix(c(
#'                  100, 0,
#'                  0, 600
#'                ), 2, 2, TRUE)
#' )
#'
#' ## supply change
#' geSC <- sdm_dstl(dstl,
#'                  names.commodity = c("lab.CHN", "lab.ROW"),
#'                  names.agent = c("CHN", "ROW"),
#'                  B = matrix(0, 2, 2, TRUE),
#'                  S0Exg = matrix(c(
#'                    200, 0,
#'                    0, 600
#'                  ), 2, 2, TRUE)
#' )
#'
#' geSC$p / ge$p
#'
#' ## preference change
#' dst.CHN$beta <- c(0.9, 0.1)
#' gePC <- sdm_dstl(dstl,
#'                  names.commodity = c("lab.CHN", "lab.ROW"),
#'                  names.agent = c("CHN", "ROW"),
#'                  B = matrix(0, 2, 2, TRUE),
#'                  S0Exg = matrix(c(
#'                    100, 0,
#'                    0, 600
#'                  ), 2, 2, TRUE)
#' )
#'
#' gePC$p / ge$p
#'
#'
#' #### a pure exchange economy with two agents and four commodities
#' prod.CHN <- Node$new("prod.CHN",
#'                      type = "CES",
#'                      alpha = 1,
#'                      beta = c(0.5, 0.5),
#'                      es = 0.75
#' )
#' prod.CHN$AddChild("lab.CHN")$AddSibling("cap.CHN")
#'
#' plot(prod.CHN)
#'
#' prod.ROW <- Node$new("prod.ROW",
#'                      type = "CES",
#'                      alpha = 2,
#'                      beta = c(0.4, 0.6),
#'                      es = 0.75
#' )
#' prod.ROW$AddChild("lab.ROW")$AddSibling("cap.ROW")
#'
#' dst.CHN <- Node$new("CHN",
#'                     type = "CES",
#'                     alpha = 1,
#'                     beta = c(0.8, 0.2),
#'                     es = 2
#' )
#' dst.CHN$AddChildNode(prod.CHN)
#' dst.CHN$AddChildNode(prod.ROW)
#'
#' dst_plot(dst.CHN)
#' print(dst.CHN, "beta")
#' p <- c("lab.CHN" = 1, "cap.CHN" = 1, "lab.ROW" = 1, "cap.ROW" = 1)
#' demand_coefficient(dst.CHN, p)
#'
#'
#' dst.ROW <- Node$new("ROW",
#'                     type = "CES",
#'                     alpha = 1,
#'                     beta = c(0.05, 0.95),
#'                     es = 2
#' )
#' dst.ROW$AddChildNode(prod.CHN)
#' dst.ROW$AddChildNode(prod.ROW)
#'
#' dst_plot(dst.ROW)
#' print(dst.ROW, "beta")
#'
#'
#' dstl <- list(dst.CHN, dst.ROW)
#'
#' ge <- sdm_dstl(dstl,
#'                names.commodity = c("lab.CHN", "cap.CHN", "lab.ROW", "cap.ROW"),
#'                names.agent = c("CHN", "ROW"),
#'                B = matrix(0, 4, 2, TRUE),
#'                S0Exg = matrix(c(
#'                  100, 0,
#'                  100, 0,
#'                  0, 600,
#'                  0, 800
#'                ), 4, 2, TRUE)
#' )

sdm_dstl <- function(dstl,
                     names.commodity,
                     names.agent,
                     ...) {
  ge <- sdm(
    A = function(state) {
      p <- c(state$p)
      names(p) <- names.commodity

      result <- sapply(dstl, demand_coefficient, p)
      return(result)
    },
    ...
  )

  ge_tidy(ge, names.commodity, names.agent)
}