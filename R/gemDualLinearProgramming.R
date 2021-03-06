#' @export
#' @title General Equilibrium Models and Linear Programming Problems (see Winston, 2003)
#' @aliases gemDualLinearProgramming
#' @description Some examples illustrating the relationship between general equilibrium problems and (dual) linear programming problems.
#' Some linear programming problems can be transformed into general equilibrium problems and vice versa.
#' @param ... arguments to be passed to the function CGE::sdm.
#' @details These examples are similar and let us explain briefly the first example (Winston, 2003).\cr
#' The Dakota Furniture Company manufactures desks, tables, and chairs.
#' The manufacture of each type of furniture requires lumber and two types of skilled labor: finishing and carpentry.
#' The amount of each resource needed to make each type of furniture is as follows: \cr
#'   desk: c(8, 4, 2)\cr
#'   table: c(6, 2, 1.5)\cr
#'   chair: c(1, 1.5, 0.5)\cr
#' Currently, 48 board feet of lumber, 20 finishing hours, and 8 carpentry hours are available.
#' A desk sells for $60, a table for $30, and a chair for $20.
#' Because the available resources have already been purchased, Dakota wants to maximize total revenue.
#' This problem can be solved by the linear programming method.\cr
#' Now let us regard the problem above as a general equilibrium problem.
#' The Dakota Furniture Company can be regarded as a consumer who obtains 1 unit of utility from 1 dollar and owns lumber and two types of skilled labor.
#' There are four commodities (i.e. dollar, desk, table and chair) and four agents (i.e. a desk producer, a table producer, a chair producer and the consumer Dakota) in this problem.
#' We need to compute the equilibrium activity levels and the equilibrium prices,
#' which are also the solutions of the (dual) linear programming problems (i.e. the utility-maximizing problem of the consumer and the cost-minimizing problem of the producers).
#' @return  A general equilibrium.
#' @references LI Wu (2019, ISBN: 9787521804225) General Equilibrium and Structural Dynamics: Perspectives of New Structural Economics. Beijing: Economic Science Press. (In Chinese)
#' @references Winston, Wayne L. (2003, ISBN: 9780534380588) Operations Research: Applications and Algorithms. Cengage Learning.
#' @references http://web.mit.edu/15.053/www/AMP-Chapter-04.pdf
#' @references https://web.stanford.edu/~ashishg/msande111/notes/chapter4.pdf
#' @references https://www.me.utexas.edu/~jensen/ORMM/supplements/methods/lpmethod/S3_dual.pdf
#' @references Stapel, Elizabeth. Linear Programming: Introduction. Purplemath. Available from https://www.purplemath.com/modules/linprog.htm
#' @examples
#' \donttest{
#' #### the Dakota example of Winston (2003, section 6.3, 6.6 and 6.8)
#' A <- matrix(c(
#'   0, 0, 0, 1,
#'   8, 6, 1, 0,
#'   4, 2, 1.5, 0,
#'   2, 1.5, 0.5, 0
#' ), 4, 4, TRUE)
#' B <- matrix(c(
#'   60, 30, 20, 0,
#'   0, 0, 0, 0,
#'   0, 0, 0, 0,
#'   0, 0, 0, 0
#' ), 4, 4, TRUE)
#' S0Exg <- {
#'   S0Exg <- matrix(NA, 4, 4)
#'   S0Exg[2:4, 4] <- c(48, 20, 8)
#'   S0Exg
#' }
#'
#' ## Compute the equilibrium by the function CGE::sdm.
#' gemDualLinearProgramming(A = A, B = B, S0Exg = S0Exg)
#'
#' ## Compute the equilibrium by the function sdm2.
#' ## The function policyMeanValue is used to accelerate convergence.
#' ge <- sdm2(
#'   A = A, B = B, S0Exg = S0Exg,
#'   policy = policyMeanValue
#' )
#'
#' ge$z
#' ge$p / ge$p[1]
#'
#' ## Compute the general equilibrium above and
#' ## the market-clearing path by the function sdm2.
#' ## Warning: time consuming.
#' ge2 <- sdm2(
#'   A = matrix_to_dstl(A), B = B, S0Exg = S0Exg,
#'   policy = makePolicyStickyPrice(stickiness = 0, tolCond = 1e-4),
#'   maxIteration = 1,
#'   numberOfPeriods = 60,
#'   ts = TRUE
#' )
#'
#' matplot(ge2$ts.p, type = "l")
#' ge2$z
#' ge2$p / ge2$p[1]
#'
#' #### an example in the mit reference
#' A <- matrix(c(
#'   0, 0, 0, 1,
#'   0.5, 2, 1, 0,
#'   1, 2, 4, 0
#' ), 3, 4, TRUE)
#' B <- matrix(c(
#'   6, 14, 13, 0,
#'   0, 0, 0, 0,
#'   0, 0, 0, 0
#' ), 3, 4, TRUE)
#' S0Exg <- {
#'   S0Exg <- matrix(NA, 3, 4)
#'   S0Exg[2:3, 4] <- c(24, 60)
#'   S0Exg
#' }
#'
#' ge <- gemDualLinearProgramming(
#'   A = A, B = B, S0Exg = S0Exg
#' )
#'
#' ge$z
#' ge$p / ge$p[1]
#'
#' #### an example in the stanford reference
#' A <- matrix(c(
#'   0, 0, 1,
#'   4.44, 0, 0,
#'   0, 6.67, 0,
#'   4, 2.86, 0,
#'   3, 6, 0
#' ), 5, 3, TRUE)
#' B <- matrix(c(
#'   3, 2.5, 0,
#'   0, 0, 0,
#'   0, 0, 0,
#'   0, 0, 0,
#'   0, 0, 0
#' ), 5, 3, TRUE)
#' S0Exg <- {
#'   S0Exg <- matrix(NA, 5, 3)
#'   S0Exg[2:5, 3] <- 100
#'   S0Exg
#' }
#'
#' ge <- gemDualLinearProgramming(
#'   A = A, B = B, S0Exg = S0Exg
#' )
#'
#' ge$z
#' ge$p / ge$p[1]
#'
#' #### an example in the utexas reference
#' A <- matrix(c(
#'   0, 0, 1,
#'   0, 1, 0,
#'   1, 3, 0,
#'   1, 0, 0
#' ), 4, 3, TRUE)
#' B <- matrix(c(
#'   2, 3, 0,
#'   1, 0, 0,
#'   0, 0, 0,
#'   0, 0, 0
#' ), 4, 3, TRUE)
#' S0Exg <- {
#'   S0Exg <- matrix(NA, 4, 3)
#'   S0Exg[2:4, 3] <- c(5, 35, 20)
#'   S0Exg
#' }
#'
#' ge <- gemDualLinearProgramming(
#'   A = A, B = B, S0Exg = S0Exg
#' )
#'
#' ge$z
#' ge$p / ge$p[1]
#'
#' #### the Giapetto example of Winston (2003, section 3.1)
#' A <- matrix(c(
#'   0, 0, 1,
#'   2, 1, 0,
#'   1, 1, 0,
#'   1, 0, 0
#' ), 4, 3, TRUE)
#' B <- matrix(c(
#'   27 - 10 - 14, 21 - 9 - 10, 0,
#'   0, 0, 0,
#'   0, 0, 0,
#'   0, 0, 0
#' ), 4, 3, TRUE)
#' S0Exg <- {
#'   S0Exg <- matrix(NA, 4, 3)
#'   S0Exg[2:4, 3] <- c(100, 80, 40)
#'   S0Exg
#' }
#'
#' ge <- sdm2(
#'   A = A, B = B, S0Exg = S0Exg,
#'   policy = policyMeanValue,
#'   numeraire = 1
#' )
#'
#' ge$z
#' ge$p
#'
#' #### the Dorian example (a minimization problem) of Winston (2003, section 3.2)
#' A <- matrix(c(
#'   0, 0, 1,
#'   7, 2, 0,
#'   2, 12, 0
#' ), 3, 3, TRUE)
#' B <- matrix(c(
#'   28, 24, 0,
#'   0, 0, 0,
#'   0, 0, 0
#' ), 3, 3, TRUE)
#' S0Exg <- {
#'   S0Exg <- matrix(NA, 3, 3)
#'   S0Exg[2:3, 3] <- c(50, 100)
#'   S0Exg
#' }
#'
#' ge <- sdm2(
#'   A = A, B = B, S0Exg = S0Exg,
#'   policy = policyMeanValue,
#'   numeraire = 1
#' )
#'
#' ge$p
#' ge$z
#'
#' #### the diet example (a minimization problem) of Winston (2003, section 3.4)
#' A <- matrix(c(
#'   0, 0, 0, 0, 1,
#'   400, 3, 2, 2, 0,
#'   200, 2, 2, 4, 0,
#'   150, 0, 4, 1, 0,
#'   500, 0, 4, 5, 0
#' ), 5, 5, TRUE)
#' B <- matrix(c(
#'   500, 6, 10, 8, 0,
#'   0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0
#' ), 5, 5, TRUE)
#' S0Exg <- {
#'   S0Exg <- matrix(NA, 5, 5)
#'   S0Exg[2:5, 5] <- c(50, 20, 30, 80)
#'   S0Exg
#' }
#'
#' ge <- sdm2(
#'   A = A, B = B, S0Exg = S0Exg,
#'   policy = policyMeanValue,
#'   numeraire = 1
#' )
#'
#' ge$p
#' ge$z
#'
#' #### An example of Stapel (see the reference):
#' ## Find the maximal value of 3x + 4y subject to the following constraints:
#' ## x + 2y <= 14, 3x - y >= 0, x - y <= 2, x >= 0, y >= 0
#'
#' A <- matrix(c(
#'   0, 0, 1,
#'   1, 2, 0,
#'   0, 1, 0,
#'   1, 0, 0
#' ), 4, 3, TRUE)
#' B <- matrix(c(
#'   3, 4, 0,
#'   0, 0, 0,
#'   3, 0, 0,
#'   0, 1, 0
#' ), 4, 3, TRUE)
#' S0Exg <- {
#'   S0Exg <- matrix(NA, 4, 3)
#'   S0Exg[2:4, 3] <- c(14, 0, 2)
#'   S0Exg
#' }
#'
#' ge <- sdm2(
#'   A = A, B = B, S0Exg = S0Exg,
#'   policy = policyMeanValue,
#'   priceAdjustmentVelocity = 0.03,
#'   numeraire = 1
#' )
#'
#' ge$z
#' ge$p
#' }

gemDualLinearProgramming <- function(...) sdm(...)
