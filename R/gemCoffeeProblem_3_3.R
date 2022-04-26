#' @export
#' @title Coffee Problem: Some Examples of Equilibrium and Disequilibrium Pure Exchange Economies
#' @aliases gemCoffeeProblem_3_3
#' @description Some examples of equilibrium and disequilibrium pure exchange economies.
#' @param ... arguments to be passed to the function sdm2.
#' @references Bapat, R. B., Raghavan, T. E. S. (1997, ISBN: 9780521571678) Nonnegative Matrices and Applications. Cambridge University Press.
#' @references LI Wu (2019, ISBN: 9787521804225) General Equilibrium and Structural Dynamics: Perspectives of New Structural Economics. Beijing: Economic Science Press. (In Chinese)
#' @examples
#' \donttest{
#' #### equilibrium coffee problem (Bapat, Raghavan, 1997, example 7.1; Li, 2019, example 8.1)
#' ge <- sdm2(
#'   A = matrix(c(
#'     0.05, 0.05, 0.1,
#'     0.1, 0, 0.1,
#'     0, 0.15, 0.05
#'   ), 3, 3, TRUE),
#'   B = matrix(0, 3, 3),
#'   S0Exg = diag(3),
#'   names.commodity = c("coffee powder", "milk", "sugar"),
#'   names.agent = c("consumer1", "consumer2", "consumer3"),
#'   numeraire = "sugar"
#' )
#'
#' ge$p
#'
#' #### disequilibrium coffee problem with exogenous prices (Li, 2019, example 8.3).
#' de <- sdm2(
#'   A = matrix(c(
#'     0.05, 0.05, 0.1,
#'     0.1, 0, 0.1,
#'     0, 0.15, 0.05
#'   ), 3, 3, TRUE),
#'   B = matrix(0, 3, 3),
#'   S0Exg = diag(3),
#'   names.commodity = c("coffee powder", "milk", "sugar"),
#'   names.agent = c("consumer1", "consumer2", "consumer3"),
#'   pExg = c(1, 1, 1),
#'   maxIteration = 1,
#'   numberOfPeriods = 50,
#'   ts = TRUE
#' )
#'
#' de$z
#' addmargins(de$D)
#' addmargins(de$S)
#'
#' matplot(de$ts.z, type = "o", pch = 20)
#' matplot(de$ts.q, type = "o", pch = 20)
#' }

gemCoffeeProblem_3_3 <- function(...) sdm2(...)
