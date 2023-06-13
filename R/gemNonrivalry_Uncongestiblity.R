#' @export
#' @title Some Examples Illustrating Uncongestible Non-rival Goods
#' @aliases gemNonrivalry_Uncongestiblity
#' @description Some examples illustrating (uncongestible) non-rival goods (or services), Lindahl prices and the uniform price.
#' In general equilibrium models, non-rival services can be regarded as personalized services,
#' which are joint products of a production process (see Mas-Colell, Whinston, and Green, 1995, section 16.G).
#' @param ... arguments to be passed to the function sdm2.
#' @references Mas-Colell, Andreu and Whinston, Michael Dennis and Green, Jerry R. (1995, ISBN: 0195073401) Microeconomic Theory. Oxford University Press (New York).
#' @examples
#' \donttest{
#' ## The firm supplies non-rival services.
#' dst.firm <- node_new(
#'   "non-rival services",
#'   type = "Leontief", a = 1,
#'   "labor"
#' )
#'
#' dst.consumer1 <- node_new(
#'   "util",
#'   type = "SCES", es = 1, # es = 0
#'   alpha = 1, beta = c(0.75, 0.25),
#'   "serv1", "labor"
#' )
#'
#' dst.consumer2 <- node_new(
#'   "util",
#'   type = "SCES", es = 1, # es = 0
#'   alpha = 1, beta = c(0.5, 0.5),
#'   "serv2", "labor"
#' )
#'
#' ge <- sdm2(
#'   A = list(dst.firm, dst.consumer1, dst.consumer2),
#'   B = matrix(c(
#'     1, 0, 0,
#'     1, 0, 0,
#'     0, 0, 0
#'   ), 3, 3, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA, NA,
#'     NA, NA, NA,
#'     NA, 60, 60
#'   ), 3, 3, TRUE),
#'   names.commodity = c("serv1", "serv2", "labor"),
#'   names.agent = c("firm", "consumer1", "consumer2"),
#'   numeraire = "labor"
#' )
#'
#' ge$p # Lindahl prices
#' addmargins(ge$D, 2)
#' addmargins(ge$S, 2)
#' addmargins(ge$DV)
#'
#' ## Computing the uniform price of the non-rival services
#' ## by transfer payment between consumers.
#' ge <- sdm2(
#'   A = list(dst.firm, dst.consumer1, dst.consumer2),
#'   B = matrix(c(
#'     1, 0, 0,
#'     1, 0, 0,
#'     0, 0, 0
#'   ), 3, 3, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA, NA,
#'     NA, NA, NA,
#'     NA, 60, 60
#'   ), 3, 3, TRUE),
#'   names.commodity = c("serv1", "serv2", "labor"),
#'   names.agent = c("firm", "consumer1", "consumer2"),
#'   numeraire = "labor",
#'   policy = function(A, state) {
#'     # A[[1]]$last.s is the previous labor supply of consumer1.
#'     if (is.null(A[[1]]$last.s)) A[[1]]$last.s <- 60
#'
#'     p <- state$p / state$p[3]
#'     last.DV <- dg(p) %*% state$last.A %*% dg(state$last.z)
#'     transfer.payment <- last.DV[1, 2] - mean(c(last.DV[1, 2], last.DV[2, 3]))
#'
#'     A[[1]]$last.s <- state$S[3, 2] <- A[[1]]$last.s *
#'       ratio_adjust((60 + transfer.payment) / A[[1]]$last.s, 0.2)
#'     state$S[3, 3] <- 120 - state$S[3, 2]
#'
#'     state
#'   }
#' )
#'
#' # Taking transfer payment into account, the uniform price of the non-rival services is 0.5.
#' ge$p
#' addmargins(ge$D, 2)
#' addmargins(ge$S, 2)
#' addmargins(ge$DV)
#'
#' ge2 <- sdm2(
#'   A = list(dst.firm, dst.consumer1, dst.consumer2),
#'   B = matrix(c(
#'     1, 0, 0,
#'     1, 0, 0,
#'     0, 0, 0
#'   ), 3, 3, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA, NA,
#'     NA, NA, NA,
#'     NA, 80, 40
#'   ), 3, 3, TRUE),
#'   names.commodity = c("serv1", "serv2", "labor"),
#'   names.agent = c("firm", "consumer1", "consumer2"),
#'   numeraire = "labor"
#' )
#'
#' ge2$p
#' addmargins(ge2$D, 2)
#' addmargins(ge2$S, 2)
#' addmargins(ge2$DV)
#'
#' ## Calculate a stationary state with price regulation.
#' ## Both services have the same price and service 2 is oversupplied.
#' ss <- sdm2(
#'   A = list(dst.firm, dst.consumer1, dst.consumer2),
#'   B = matrix(c(
#'     1, 0, 0,
#'     1, 0, 0,
#'     0, 0, 0
#'   ), 3, 3, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA, NA,
#'     NA, NA, NA,
#'     NA, 60, 60
#'   ), 3, 3, TRUE),
#'   names.commodity = c("serv1", "serv2", "labor"),
#'   names.agent = c("firm", "consumer1", "consumer2"),
#'   numeraire = "labor",
#'   policy = function(state) {
#'     state$p[2] <- state$p[1]
#'     state
#'   },
#'   maxIteration = 1,
#'   numberOfPeriods = 200,
#'   depreciationCoef = 0,
#'   ts = TRUE
#' )
#'
#' ss$p
#' addmargins(ss$D, 2)
#' addmargins(ss$S, 2)
#' matplot(ss$ts.q, type = "l")
#' matplot(ss$ts.z, type = "l")
#' matplot(ss$ts.p, type = "l")
#'
#' ##
#' ss <- sdm2(
#'   A = list(dst.firm, dst.consumer1, dst.consumer2),
#'   B = matrix(c(
#'     1, 0, 0,
#'     1, 0, 0,
#'     0, 0, 0
#'   ), 3, 3, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA, NA,
#'     NA, NA, NA,
#'     NA, 50, 50
#'   ), 3, 3, TRUE),
#'   names.commodity = c("serv1", "serv2", "labor"),
#'   names.agent = c("firm", "consumer1", "consumer2"),
#'   numeraire = "labor",
#'   policy = list(
#'     function(state) {
#'       state$p[1:2] <- sum(state$p[1:2] * c(0.8, 0.2))
#'       state
#'     },
#'     makePolicyMeanValue()
#'   ),
#'   maxIteration = 1,
#'   numberOfPeriods = 1000,
#'   ts = TRUE
#' )
#'
#' ss$p
#' addmargins(ss$D, 2)
#' addmargins(ss$S, 2)
#' addmargins(ss$DV)
#' addmargins(ss$SV)
#' matplot(ss$ts.q, type = "l")
#' matplot(ss$ts.z, type = "l")
#' matplot(ss$ts.p, type = "l")
#' }

gemNonrivalry_Uncongestiblity <- function(...) sdm2(...)
