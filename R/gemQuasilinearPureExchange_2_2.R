#' @export
#' @title A Pure Exchange Economy with a Quasilinear Utility Function
#' @aliases gemQuasilinearPureExchange_2_2
#' @description An example of a pure exchange economy with a quasilinear utility function (Karaivanov, see the reference).
#' @details Suppose there are only two goods (bananas and fish) and 2 consumers (Annie and Ben) in an exchange economy.
#' Annie has a utility function x_1^(1/3) * x_2^(2/3) where x_1 is the amount of fish she eats and x_2 is the amount of
#' bananas she eats.
#' Annie has an endowment of 3 kilos of fish and 7 bananas.
#' Ben has a utility function x_1 + 1.5 * log(x_2) and endowments of 4 kilos of fish and 0 bananas.
#' Assume the price of bananas is 1.
#' See the reference for more details.
#' @param A a demand structure tree list, a demand coefficient 2-by-2 matrix (alias demand structure matrix)
#' or a function A(state) which returns a 2-by-2 matrix (see \code{\link{sdm2}}).
#' @param Endowment a 2-by-2 matrix.
#' @param policy a policy function (see \code{\link{sdm2}}).
#' @return A general equilibrium.
#' @references http://www.sfu.ca/~akaraiva/CE_example.pdf
#' @examples
#' \donttest{
#' demand_consumer2 <- function(p, w) {
#'   a <- 1.5
#'   d <- rbind(0, 0)
#'   w <- w / p[1]
#'   p <- p / p[1] # normalize the wealth and prices
#'   d[2] <- a / p[2]
#'   if (d[2] * p[2] > w) {
#'     d[2] <- w / p[2]
#'     d[1] <- 0
#'   } else {
#'     d[1] <- w - d[2] * p[2]
#'   }
#'
#'   d
#' }
#'
#' A <- function(state) {
#'   a1 <- CD_A(1, rbind(1 / 3, 2 / 3), state$p)
#'   a2 <- demand_consumer2(state$p, state$w[2])
#'   cbind(a1, a2)
#' }
#'
#' ge.mat <- gemQuasilinearPureExchange_2_2(A = A)
#' ge.mat
#'
#' ## Use a dstl and a policy function to compute the general equilibrium above.
#' dst.consumer1 <- node_new("util",
#'                           type = "CD", alpha = 1, beta = c(1 / 3, 2 / 3),
#'                           "fish", "banana"
#' )
#' dst.consumer2 <- node_new("util",
#'                           type = "Leontief", a = c(1, 1),
#'                           "fish", "banana"
#' )
#'
#' dstl <- list(dst.consumer1, dst.consumer2)
#'
#' policy.quasilinear <- function(A, state) {
#'   wealth <- t(state$p) %*% state$S
#'   A[[2]]$a <- demand_consumer2(state$p, wealth[2])
#' }
#'
#' ge.dstl <- gemQuasilinearPureExchange_2_2(
#'   A = dstl,
#'   policy = policy.quasilinear
#' )
#' ge.dstl
#'
#' #### Another example. Now Ben has a utility function x_1 + sqrt(x_2).
#' demand_consumer2 <- function(p, w) {
#'   d <- rbind(0, 0)
#'   w <- w / p[1]
#'   p <- p / p[1] # normalize the wealth and prices
#'   d[2] <- (2 * p[2])^-2
#'   if (d[2] * p[2] > w) {
#'     d[2] <- w / p[2]
#'     d[1] <- 0
#'   } else {
#'     d[1] <- w - d[2] * p[2]
#'   }
#'
#'   d
#' }
#'
#' A <- function(state) {
#'   a1 <- CD_A(1, rbind(1 / 3, 2 / 3), state$p)
#'   a2 <- demand_consumer2(state$p, state$w[2])
#'   cbind(a1, a2)
#' }
#'
#' ge.2_2 <- gemQuasilinearPureExchange_2_2(A = A)
#' ge.2_2
#'
#' ## another computation method for the economy above
#' A <- function(state) {
#'   a1 <- CD_A(1, rbind(1 / 3, 2 / 3, 0, 0), state$p)
#'   a2 <- c(0, 0, 1, 0)
#'   a3 <- c(1, 0, 0, 0) # firm 1
#'   a4 <- CD_A(1, rbind(0, 1 / 2, 0, 1 / 2), state$p) # firm 2
#'   cbind(a1, a2, a3, a4)
#' }
#'
#' ge.4_4 <- sdm2(
#'   A = A,
#'   B = {
#'     B <- matrix(0, 4, 4)
#'     B[3, 3] <- 1
#'     B[3, 4] <- 1
#'     B
#'   },
#'   S0Exg = {
#'     S0Exg <- matrix(NA, 4, 4)
#'     S0Exg[1:2, 1] <- c(3, 7)
#'     S0Exg[1:2, 2] <- c(4, 0)
#'     S0Exg[4, 1:2] <- c(0, 1)
#'     S0Exg
#'   },
#'   names.commodity = c("fish", "banana", "util2", "land"),
#'   names.agent = c("Annie", "Ben", "firm1", "firm2"),
#'   numeraire = "banana"
#' )
#' ge.4_4
#'
#' #### an example with a LES (linear expenditure system) utility function
#' demand_LES <- function(p, w,
#'                        gamma = c(0.1, 0.2),
#'                        beta = c(0.4, 0.6)) {
#'   d <- c()
#'   discretionary.income <- w - sum(p * gamma)
#'   for (k in seq_along(gamma)) {
#'     d[k] <- gamma[k] + beta[k] * discretionary.income / p[k]
#'   }
#'
#'   d
#' }
#'
#' A <- function(state) {
#'   a1 <- CD_A(1, rbind(1 / 3, 2 / 3), state$p)
#'   a2 <- demand_LES(state$p, state$w[2])
#'   cbind(a1, a2)
#' }
#'
#' sdm2(
#'   A = A,
#'   B = matrix(0, 2, 2),
#'   S0Exg = matrix(c(
#'     3, 4,
#'     7, 0
#'   ), 2, 2, TRUE),
#'   names.commodity = c("fish", "banana"),
#'   names.agent = c("Annie", "Ben"),
#'   numeraire = "banana"
#' )
#' }




gemQuasilinearPureExchange_2_2 <- function(A,
                                           Endowment = matrix(c(
                                             3, 4,
                                             7, 0
                                           ), 2, 2, TRUE),
                                           policy = NULL) {
  ge <- sdm2(
    A = A,
    B = matrix(0, 2, 2),
    S0Exg = Endowment,
    names.commodity = c("fish", "banana"),
    names.agent = c("Annie", "Ben"),
    numeraire = "banana",
    policy = policy
  )
}

