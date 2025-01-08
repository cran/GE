#' @export
#' @title An Example Illustrating Endogenous Equilibrium Interest Rates and the Velocity of Money in a (Timeline) Transitional Equilibrium Path
#' @aliases gemIntertemporal_EndogenousEquilibriumInterestRate_MoneyVelocity
#' @description This example illustrates (endogenous) equilibrium primitive interest rates and the velocity of money in a transitional equilibrium path.
#' There are two types of interest rates involved here, namely the (primitive) period interest rate and the (primitive) intraperiod interest rate
#' (see \code{\link{convert_ir}}).
#' @param ... arguments to be passed to the function sdm2.
#' @seealso \code{\link{gemIntertemporal_EndogenousEquilibriumInterestRate}}
#' @examples
#' \donttest{
#' vm <- 4 # the velocity of money
#' eis <- 0.8 # the elasticity of intertemporal substitution
#' Gamma.beta <- 0.8 # the subjective discount factor
#' gr <- 0 # the steady-state growth rate
#' np <- 20 # the number of economic periods
#'
#' f <- function(ir.intraperiod = rep(0.25, np - 1), return.ge = FALSE,
#'               y1 = 10, # the product supply in the first period
#'               alpha.firm = rep(2, np - 1) # the efficiency parameters of firms
#' ) {
#'   n <- 2 * np # the number of commodity kinds
#'   m <- np + 1 # the number of agent kinds
#'
#'   names.commodity <- c(
#'     paste0("prod", 1:np),
#'     paste0("lab", 1:(np - 1)),
#'     "money"
#'   )
#'   names.agent <- c(
#'     paste0("firm", 1:(np - 1)),
#'     "laborer", "moneyOwner"
#'   )
#'
#'   # the exogenous supply matrix.
#'   S0Exg <- matrix(NA, n, m, dimnames = list(names.commodity, names.agent))
#'   S0Exg[paste0("lab", 1:(np - 1)), "laborer"] <- 100 * (1 + gr)^(0:(np - 2))
#'   S0Exg["money", "moneyOwner"] <- 100
#'   S0Exg["prod1", "laborer"] <- y1
#'
#'   # the output coefficient matrix.
#'   B <- matrix(0, n, m, dimnames = list(names.commodity, names.agent))
#'   for (k in 1:(np - 1)) {
#'     B[paste0("prod", k + 1), paste0("firm", k)] <- 1
#'   }
#'
#'   dstl.firm <- list()
#'   for (k in 1:(np - 1)) {
#'     dstl.firm[[k]] <- node_new(
#'       "prod",
#'       type = "FIN", rate = c(1, ir.intraperiod[k] / vm),
#'       "cc1", "money"
#'     )
#'     node_set(dstl.firm[[k]], "cc1",
#'       type = "CD", alpha = alpha.firm[k], beta = c(0.5, 0.5),
#'       paste0("prod", k), paste0("lab", k)
#'     )
#'   }
#'
#'   dst.laborer <- node_new(
#'     "util",
#'     type = "CES", es = eis,
#'     alpha = 1, beta = prop.table(Gamma.beta^(1:np)),
#'     paste0("cc", 1:(np - 1)), paste0("prod", np)
#'   )
#'
#'   for (k in 1:(np - 1)) {
#'     node_set(dst.laborer, paste0("cc", k),
#'       type = "FIN", rate = c(1, ir.intraperiod[k] / vm),
#'       paste0("prod", k), "money"
#'     )
#'   }
#'
#'   dst.moneyOwner <- node_new(
#'     "util",
#'     type = "CES", es = eis,
#'     alpha = 1, beta = prop.table(Gamma.beta^(1:(np - 1))),
#'     paste0("cc", 1:(np - 1))
#'   )
#'   for (k in 1:(np - 1)) {
#'     node_set(dst.moneyOwner, paste0("cc", k),
#'       type = "FIN", rate = c(1, ir.intraperiod[k] / vm),
#'       paste0("prod", k), "money"
#'     )
#'   }
#'
#'   ge <- sdm2(
#'     A = c(dstl.firm, dst.laborer, dst.moneyOwner),
#'     B = B,
#'     S0Exg = S0Exg,
#'     names.commodity = names.commodity,
#'     names.agent = names.agent,
#'     numeraire = "prod1",
#'     policy = makePolicyHeadTailAdjustment(gr = gr, np = np, type = c("tail"))
#'   )
#'
#'   tmp <- rowSums(ge$SV)
#'   ts.exchange.value <- tmp[paste0("prod", 1:(np - 1))] + tmp[paste0("lab", 1:(np - 1))]
#'   # The period interest rate equals the yield rate of money.
#'   ir.period <- ts.exchange.value[1:(np - 2)] / ts.exchange.value[2:(np - 1)] - 1
#'   ir.period <- pmax(1e-6, ir.period)
#'   ir.period[np - 1] <- ir.period[np - 2]
#'   ir.intraperiod.new <- convert_ir(ir.period, vm, "intraperiod")
#'
#'   ir.intraperiod <- c(ir.intraperiod * ratio_adjust(ir.intraperiod.new / ir.intraperiod, 0.3))
#'   cat("ir.intraperiod: ", ir.intraperiod, "\n")
#'
#'   if (return.ge) {
#'     ge$ts.exchange.value <- ts.exchange.value
#'     return(ge)
#'   } else {
#'     return(ir.intraperiod)
#'   }
#' }
#'
#' ## Calculate equilibrium intraperiod interest rates.
#' ## Warning: Running the program below takes about several minutes.
#' ## mat.ir.intraperiod <- iterate(rep(0.1, np - 1), f, tol = 1e-4)
#' ## # the period interest rate in the steady-state equilibrium.
#' ## ir.period.steadyState <- sserr(eis, Gamma.beta, gr, prepaid = TRUE)
#' ## # the intraperiod interest rate in the steady-state equilibrium.
#' ## convert_ir(ir.period.steadyState, vm, "intraperiod")
#'
#' ## Below are the calculated equilibrium intraperiod interest rates.
#' (ir.intraperiod <- c(
#'   0.3723, 0.2981, 0.2580, 0.2363, 0.2245, 0.2181, 0.2147,
#'   0.2128, 0.2118, 0.2112, 0.2109, 0.2107, 0.2106, 0.2106,
#'   0.2106, 0.2105, 0.2105, 0.2106, 0.2106
#' ))
#'
#' ge <- f(ir.intraperiod, TRUE)
#'
#' plot(ge$z[1:(np - 1)], type = "o")
#'
#' # The yield rate of money is equal to the primitive period interest rate.
#' (ir.period <- ge$ts.exchange.value[1:(np - 2)] / ge$ts.exchange.value[2:(np - 1)] - 1)
#' convert_ir(ir.period, vm, "intraperiod") # consistent with ir.intraperiod
#' # consistent with ir.period.
#' convert_ir(ir.intraperiod, vm, "period")
#'
#' ## Calculate the growth rate of the money supply and the equilibrium nominal
#' ## period interest rates when the current price of the product remains constant.
#' price.money <- 1 / c(1, cumprod(ir.intraperiod + 1))
#' currentPrice.prod <- ge$p[1:np] / price.money
#' gr.moneySupply <- unname(growth_rate(1 / currentPrice.prod))
#' # the equilibrium nominal period interest rates
#' (1 + ir.period) * (gr.moneySupply[2:(np - 1)] + 1) - 1
#' (1 + convert_ir(ir.intraperiod, vm, "period")) * (gr.moneySupply[2:np] + 1) - 1
#'
#' ## the corresponding sequential model with the same steady-state equilibrium.
#' dividend.rate <- sserr(eis, Gamma.beta, prepaid = TRUE)
#' ir.intraperiod <- convert_ir(dividend.rate, vm, "intraperiod")
#'
#' np <- 5
#' ge.ss <- f(ir.intraperiod = rep(ir.intraperiod, np - 1), return.ge = TRUE, y1 = 152)
#'
#' dst.firm <- node_new("prod",
#'   type = "FIN", rate = c(1, ir.intraperiod / vm, (1 + ir.intraperiod / vm) * dividend.rate),
#'   "cc1", "money", "equity.share"
#' )
#' node_set(dst.firm, "cc1",
#'   type = "CD",
#'   alpha = 2, beta = c(0.5, 0.5),
#'   "prod", "lab"
#' )
#'
#' dst.laborer <- node_new("util",
#'   type = "FIN", rate = c(1, ir.intraperiod / vm),
#'   "prod", "money"
#' )
#'
#' dst.moneyOwner <- node_new("util",
#'   type = "FIN", rate = c(1, ir.intraperiod / vm),
#'   "prod", "money"
#' )
#'
#' ge2 <- sdm2(
#'   A = list(dst.firm, dst.laborer, dst.moneyOwner),
#'   B = matrix(c(
#'     1, 0, 0,
#'     0, 0, 0,
#'     0, 0, 0,
#'     0, 0, 0
#'   ), 4, 3, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA, NA,
#'     NA, 100, NA,
#'     NA, NA, 100,
#'     NA, 100, NA
#'   ), 4, 3, TRUE),
#'   names.commodity = c(
#'     "prod", "lab", "money", "equity.share"
#'   ),
#'   names.agent = c("firm", "laborer", "moneyOwner"),
#'   numeraire = c("money" = ir.intraperiod)
#' )
#'
#' ge2$p
#' ge.ss$p
#' ge2$p / ge2$p["prod"]
#'
#' ge.ss$z[np - 1]
#' ge2$z
#'
#' ge.ss$D[paste0("prod", np - 1), c("laborer", "moneyOwner")]
#' ge2$D
#'
#' # The total value of the product and labor is equal to 400, which
#' # is equal to the money stock multiplied by the velocity of money.
#' addmargins(ge2$SV)
#' addmargins(ge2$DV)
#' }

gemIntertemporal_EndogenousEquilibriumInterestRate_MoneyVelocity <- function(...) sdm2(...)
