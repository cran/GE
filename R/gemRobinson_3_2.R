#' @export
#' @title A Robinson Crusoe Economy
#' @aliases gemRobinson_3_2
#' @description Compute the general equilibrium of a Robinson Crusoe economy.
#' @details A general equilibrium model with 3 commodities (i.e. product, labor,
#' and land) and 2 agents (i.e. a firm and a consumer). The numeraire is labor.
#' @param dstl the demand structure tree list.
#' @param endowment the endowment 3-vector.
#' The endowment of the product is a non-negative number.
#' The endowments of labor and land are positive numbers.
#' @return A general equilibrium.
#' @references http://essentialmicroeconomics.com/ChapterY5/SlideChapter5-1.pdf
#' @references http://homepage.ntu.edu.tw/~josephw/MicroTheory_Lecture_11a_RobinsonCrusoeEconomy.pdf
#' @examples
#' \donttest{
#' #### a general equilibrium model with 2 basic commodities (i.e. labor and land) and 1 agent
#' dst.Robinson <- node_new("util",
#'   type = "CD", alpha = 1, beta = c(0.5, 0.5),
#'   "prod", "lab"
#' )
#' node_set(dst.Robinson, "prod",
#'   type = "CD", alpha = 8, beta = c(0.5, 0.5),
#'   "lab", "land"
#' )
#'
#' node_plot(dst.Robinson)
#'
#' ge <- sdm2(
#'   A = list(dst.Robinson),
#'   names.commodity = c("lab", "land"),
#'   names.agent = c("Robinson"),
#'   B = matrix(0, 2, 1),
#'   S0Exg = matrix(c(
#'     12,
#'     1
#'   ), 2, 1, TRUE),
#'   numeraire = "lab"
#' )
#' ge
#'
#'
#' ## the same economy as above
#' dst.Robinson <- node_new("util",
#'   type = "CD", alpha = 1, beta = c(0.5, 0.5),
#'   "prod", "lab"
#' )
#' dst.firm <- node_new("output",
#'   type = "CD", alpha = 8, beta = c(0.5, 0.5),
#'   "lab", "land"
#' )
#'
#' dstl <- list(dst.firm, dst.Robinson)
#'
#' ge <- gemRobinson_3_2(dstl, endowment = c(0, 12, 1))
#' ge
#'
#' ## another example
#' dst.firm$alpha <- 1
#'
#' ge <- gemRobinson_3_2(dstl, endowment = c(3, 144, 1))
#' ge
#'
#'
#' #### a Robinson Crusoe economy with labor and two types of land
#' dst.Robinson <- node_new("util",
#'   type = "CD", alpha = 1, beta = c(0.5, 0.5),
#'   "prod1", "prod2"
#' )
#' node_set(dst.Robinson, "prod1",
#'   type = "CD", alpha = 1, beta = c(0.2, 0.8),
#'   "lab", "land1"
#' )
#' node_set(dst.Robinson, "prod2",
#'   type = "CD", alpha = 1, beta = c(0.8, 0.2),
#'   "lab", "land2"
#' )
#' node_plot(dst.Robinson)
#'
#' dstl <- list(dst.Robinson)
#'
#' ge.3_1 <- sdm2(dstl,
#'   names.commodity = c("lab", "land1", "land2"),
#'   names.agent = c("Robinson"),
#'   B = matrix(0, 3, 1),
#'   S0Exg = matrix(c(
#'     100,
#'     100,
#'     100
#'   ), 3, 1, TRUE),
#'   numeraire = "lab"
#' )
#' ge.3_1
#'
#' #### the same economy as above
#' ge.5_3 <- sdm2(
#'   A = list(
#'     dst.firm1 = node_new("output",
#'                          type = "CD", alpha = 1, beta = c(0.2, 0.8),
#'                          "lab", "land1"
#'     ),
#'     dst.firm2 = node_new("output",
#'                          type = "CD", alpha = 1, beta = c(0.8, 0.2),
#'                          "lab", "land2"
#'     ),
#'     dst.Robinson = node_new("util",
#'                             type = "CD", alpha = 1, beta = c(0.5, 0.5),
#'                             "prod1", "prod2"
#'     )
#'   ),
#'   names.commodity = c("prod1", "prod2", "lab", "land1", "land2"),
#'   names.agent = c("firm1", "firm2", "Robinson"),
#'   B = {
#'     B <- matrix(0, 5, 3)
#'     B[1, 1] <- B[2, 2] <- 1
#'     B
#'   },
#'   S0Exg = {
#'     S0Exg <- matrix(NA, 5, 3)
#'     S0Exg[3:5, 3] <- 100
#'     S0Exg
#'   },
#'   numeraire = "lab"
#' )
#' ge.5_3
#' }
#'
gemRobinson_3_2 <- function(dstl, endowment) {
  sdm2(dstl,
    names.commodity = c("prod", "lab", "land"),
    names.agent = c("firm", "Robinson"),
    B = {
      B <- matrix(0, 3, 2)
      B[1, 1] <- 1
      B
    },
    S0Exg = {
      S0Exg <- matrix(NA, 3, 2)
      S0Exg[1:3, 2] <- endowment
      S0Exg
    },
    numeraire = "lab"
  )
}
