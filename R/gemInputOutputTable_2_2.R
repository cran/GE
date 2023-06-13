#' @export
#' @title A General Equilibrium Model based on a 2×2 (Unbalanced) Input-Output Table
#' @aliases gemInputOutputTable_2_2
#' @description A general equilibrium model based on a 2×2 (unbalanced) input-output table (unit: yuan).
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' names.commodity <- c("prod", "lab")
#' names.agent <- c("firm", "laborer")
#'
#' IT <- matrix(c(
#'   40, 40,
#'   40, 60
#' ), 2, 2, TRUE)
#'
#' OT <- matrix(c(
#'   100, 0,
#'   0, 100
#' ), 2, 2, TRUE)
#'
#' dimnames(IT) <- dimnames(OT) <- list(names.commodity, names.agent)
#'
#' addmargins(IT)
#' addmargins(OT)
#'
#' #### the model
#' dst.firm <- node_new(
#'   "prod",
#'   type = "SCES",
#'   es = 1,
#'   alpha = 1.25, # 100 / (40 + 40)
#'   beta = prop.table(c(40, 40)),
#'   "prod", "lab"
#' )
#'
#' dst.consumer <- node_new(
#'   "util",
#'   type = "SCES",
#'   es = 1, alpha = 1,
#'   beta = prop.table(c(40, 60)),
#'   "prod", "lab"
#' )
#'
#' dstl <- list(dst.firm, dst.consumer)
#'
#' ge.benchmark <- sdm2(
#'   A = dstl,
#'   B = matrix(c(
#'     1, 0,
#'     0, 0
#'   ), 2, 2, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA,
#'     NA, 100
#'   ), 2, 2, TRUE),
#'   names.commodity = c("prod", "lab"),
#'   names.agent = c("firm", "consumer"),
#'   numeraire = "lab"
#' )
#'
#' ge.benchmark$p
#' ge.benchmark$D
#' ge.benchmark$S
#'
#' addmargins(ge.benchmark$DV)
#' addmargins(ge.benchmark$SV)
#'
#' ## the same as above
#' ge <- sdm2(
#'   A = function(state) {
#'     a.firm <- SCES_A(es = 1, alpha = 1.25, Beta = prop.table(c(40, 40)), p = state$p)
#'     a.consumer <- SCES_A(es = 1, alpha = 1.25, Beta = prop.table(c(40, 60)), p = state$p)
#'     cbind(a.firm, a.consumer)
#'   },
#'   B = matrix(c(
#'     1, 0,
#'     0, 0
#'   ), 2, 2, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA,
#'     NA, 100
#'   ), 2, 2, TRUE),
#'   names.commodity = c("prod", "lab"),
#'   names.agent = c("firm", "consumer"),
#'   numeraire = "lab"
#' )
#'
#' ## technology progress
#' dstl[[1]]$alpha <- 2.5
#'
#' ge.TP <- sdm2(
#'   A = dstl,
#'   B = matrix(c(
#'     1, 0,
#'     0, 0
#'   ), 2, 2, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA,
#'     NA, 100
#'   ), 2, 2, TRUE),
#'   names.commodity = c("prod", "lab"),
#'   names.agent = c("firm", "consumer"),
#'   numeraire = "lab"
#' )
#'
#' ge.TP$p
#' ge.TP$D
#' ge.TP$S
#'
#' addmargins(ge.TP$DV)
#' addmargins(ge.TP$SV)
#' }

gemInputOutputTable_2_2 <- function(...) sdm2(...)
