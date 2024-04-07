#' @export
#' @title Overlapping Generations Financial Sequential Models with One Firm
#' @aliases gemOLGF_OneFirm
#' @description Some examples of overlapping generations financial sequential models with one firm.
#'
#' When there is a population growth, we will take the security-split assumption (see \code{\link{gemOLGF_PureExchange}}).
#' @param ... arguments to be passed to the function sdm2.
#' @references Samuelson, P. A. (1958) An Exact Consumption-Loan Model of Interest with or without the Social Contrivance of Money. Journal of Political Economy, vol. 66(6): 467-482.
#' @references de la Croix, David and Philippe Michel (2002, ISBN: 9780521001151) A Theory of Economic Growth: Dynamics and Policy in Overlapping Generations. Cambridge University Press.
#' @seealso {
#' \code{\link{gemOLG_PureExchange}}
#' \code{\link{gemOLG_TimeCircle}}
#' }
#' @examples
#' \donttest{
#' #### an OLGF economy with a firm and two-period-lived consumers
#' beta.firm <- c(1 / 3, 2 / 3)
#' # the population growth rate
#' GRExg <- 0.03
#' saving.rate <- 0.5
#' ratio.saving.consumption <- saving.rate / (1 - saving.rate)
#'
#' dst.firm <- node_new(
#'   "prod",
#'   type = "CD", alpha = 5,
#'   beta = beta.firm,
#'   "lab", "prod"
#' )
#'
#' dst.age1 <- node_new(
#'   "util",
#'   type = "FIN",
#'   rate = c(1, ratio.saving.consumption),
#'   "prod", "secy" # security, the financial instrument
#' )
#'
#' dst.age2 <- node_new(
#'   "util",
#'   type = "Leontief", a = 1,
#'   "prod"
#' )
#'
#' ge <- sdm2(
#'   A = list(
#'     dst.firm, dst.age1, dst.age2
#'   ),
#'   B = matrix(c(
#'     1, 0, 0,
#'     0, 0, 0,
#'     0, 0, 0
#'   ), 3, 3, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA, NA,
#'     NA, 1, NA,
#'     NA, NA, 1
#'   ), 3, 3, TRUE),
#'   names.commodity = c("prod", "lab", "secy"),
#'   names.agent = c("firm", "age1", "age2"),
#'   numeraire = "lab",
#'   GRExg = GRExg,
#'   maxIteration = 1,
#'   ts = TRUE
#' )
#'
#' ge$p
#' matplot(ge$ts.p, type = "l")
#' matplot(growth_rate(ge$ts.z), type = "l") # GRExg
#' addmargins(ge$D, 2) # the demand matrix of the current period
#' addmargins(ge$S, 2) # the supply matrix of the current period
#' addmargins(ge$S * (1 + GRExg), 2) # the supply matrix of the next period
#' addmargins(ge$DV)
#' addmargins(ge$SV)
#'
#' ## Suppose consumers consume product and labor (i.e. service) and
#' ## age1 and age2 may have different instantaneous utility functions.
#' dst.age1 <- node_new(
#'   "util",
#'   type = "FIN",
#'   rate = c(1, ratio.saving.consumption),
#'   "cc1", "secy" # security, the financial instrument
#' )
#' node_set(dst.age1,  "cc1",
#'   type = "Leontief",
#'   a = c(0.5, 0.5),
#'   "prod", "lab"
#' )
#' node_plot(dst.age1)
#'
#' dst.age2 <- node_new("util",
#'   type = "Leontief",
#'   a = c(0.2, 0.8),
#'   "prod", "lab"
#' )
#'
#' ge <- sdm2(
#'   A = list(
#'     dst.firm, dst.age1, dst.age2
#'   ),
#'   B = matrix(c(
#'     1, 0, 0,
#'     0, 0, 0,
#'     0, 0, 0
#'   ), 3, 3, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA, NA,
#'     NA, 1, NA,
#'     NA, NA, 1
#'   ), 3, 3, TRUE),
#'   names.commodity = c("prod", "lab", "secy"),
#'   names.agent = c("firm", "age1", "age2"),
#'   numeraire = "lab",
#'   GRExg = GRExg,
#'   priceAdjustmentVelocity = 0.05
#' )
#'
#' ge$p
#' addmargins(ge$D, 2)
#' addmargins(ge$S, 2)
#' addmargins(ge$DV)
#' addmargins(ge$SV)
#'
#' ## Aggregate the above consumers into one infinite-lived consumer,
#' ## who always spends the same amount on cc1 and cc2.
#' dst.consumer <- node_new("util",
#'                          type = "CD", alpha = 1,
#'                          beta = c(0.5, 0.5),
#'                          "cc1", "cc2"
#' )
#' node_set(dst.consumer, "cc1",
#'          type = "Leontief",
#'          a = c(0.5, 0.5),
#'          "prod", "lab"
#' )
#' node_set(dst.consumer,  "cc2",
#'          type = "Leontief",
#'          a = c(0.2, 0.8),
#'          "prod", "lab"
#' )
#'
#' ge <- sdm2(
#'   A = list(
#'     dst.firm, dst.consumer
#'   ),
#'   B = matrix(c(
#'     1, 0,
#'     0, 0
#'   ), 2, 2, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA,
#'     NA, 1
#'   ), 2, 2, TRUE),
#'   names.commodity = c("prod", "lab"),
#'   names.agent = c("firm", "consumer"),
#'   numeraire = "lab",
#'   GRExg = GRExg,
#'   priceAdjustmentVelocity = 0.05
#' )
#'
#' ge$p
#' addmargins(ge$D, 2)
#' addmargins(ge$S, 2)
#' addmargins(ge$DV)
#' addmargins(ge$SV)
#'
#' #### an OLGF economy with a firm and two-period-lived consumers
#' ## Suppose each consumer has a Leontief-type utility function min(c1, c2/a).
#' beta.firm <- c(1 / 3, 2 / 3)
#' # the population growth rate, the equilibrium interest rate and profit rate
#' GRExg <- 0.03
#' rho <- 1 / (1 + GRExg)
#' a <- 0.9
#'
#' dst.firm <- node_new(
#'   "prod",
#'   type = "CD", alpha = 5,
#'   beta = beta.firm,
#'   "lab", "prod"
#' )
#'
#' dst.age1 <- node_new(
#'   "util",
#'   type = "FIN",
#'   rate = c(1, ratio.saving.consumption = a * rho),
#'   "prod", "secy" # security, the financial instrument
#' )
#'
#' dst.age2 <- node_new(
#'   "util",
#'   type = "Leontief", a = 1,
#'   "prod"
#' )
#'
#' ge <- sdm2(
#'   A = list(
#'     dst.firm, dst.age1, dst.age2
#'   ),
#'   B = matrix(c(
#'     1, 0, 0,
#'     0, 0, 0,
#'     0, 0, 0
#'   ), 3, 3, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA, NA,
#'     NA, 1, NA,
#'     NA, NA, 1
#'   ), 3, 3, TRUE),
#'   names.commodity = c("prod", "lab", "secy"),
#'   names.agent = c("firm", "age1", "age2"),
#'   numeraire = "lab",
#'   GRExg = GRExg,
#'   maxIteration = 1,
#'   ts = TRUE
#' )
#'
#' ge$p
#' matplot(ge$ts.p, type = "l")
#' matplot(growth_rate(ge$ts.z), type = "l") # GRExg
#' addmargins(ge$D, 2)
#' addmargins(ge$S, 2)
#' addmargins(ge$DV)
#' addmargins(ge$SV)
#'
#' ## the corresponding time-cycle model
#' n <- 5 # the number of periods, consumers and firms.
#' S <- matrix(NA, 2 * n, 2 * n)
#'
#' S.lab.consumer <- diag((1 + GRExg)^(0:(n - 1)), n)
#' S[(n + 1):(2 * n), (n + 1):(2 * n)] <- S.lab.consumer
#'
#' B <- matrix(0, 2 * n, 2 * n)
#' B[1:n, 1:n] <- diag(n)[, c(2:n, 1)]
#' B[1, n] <- rho^n
#'
#' dstl.firm <- list()
#' for (k in 1:n) {
#'   dstl.firm[[k]] <- node_new(
#'     "prod",
#'     type = "CD", alpha = 5,
#'     beta = beta.firm,
#'     paste0("lab", k), paste0("prod", k)
#'   )
#' }
#'
#' dstl.consumer <- list()
#' for (k in 1:(n - 1)) {
#'   dstl.consumer[[k]] <- node_new(
#'     "util",
#'     type = "FIN",
#'     rate = c(1, ratio.saving.consumption = a * rho),
#'     paste0("prod", k), paste0("prod", k + 1)
#'   )
#' }
#'
#' dstl.consumer[[n]] <- node_new(
#'   "util",
#'   type = "FIN",
#'   rate = c(1, ratio.saving.consumption = a * rho),
#'   paste0("prod", n), "cc1"
#' )
#' node_set(dstl.consumer[[n]], "cc1",
#'          type = "Leontief", a = rho^n, # a discounting factor
#'          "prod1"
#' )
#'
#' ge2 <- sdm2(
#'   A = c(dstl.firm, dstl.consumer),
#'   B = B,
#'   S0Exg = S,
#'   names.commodity = c(paste0("prod", 1:n), paste0("lab", 1:n)),
#'   names.agent = c(paste0("firm", 1:n), paste0("consumer", 1:n)),
#'   numeraire = "lab1",
#'   policy = makePolicyMeanValue(30),
#'   maxIteration = 1,
#'   numberOfPeriods = 600,
#'   ts = TRUE
#' )
#'
#' ge2$p
#' growth_rate(ge2$p[1:n]) + 1 # rho
#' growth_rate(ge2$p[(n + 1):(2 * n)]) + 1 # rho
#' ge2$D
#' }
#'

gemOLGF_OneFirm <- function(...) sdm2(...)
