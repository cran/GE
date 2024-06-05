#' @export
#' @title An Example Illustrating Endogenous Equilibrium Interest Rates and the Velocity of Money in a (Timeline) Transitional Equilibrium Path
#' @aliases gemIntertemporal_EndogenousEquilibriumInterestRate_MoneyVelocity
#' @description This example illustrates (endogenous) equilibrium primitive interest rates and the velocity of money in a transitional equilibrium path.
#' The (primitive) interest rate here can be divided into two types, namely the interest rate in the case of lump-sum interest payment and the interest rate in the case of installment interest payment.
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
#' f <- function(ir.installment = rep(0.25, np - 1), return.ge = FALSE,
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
#'       type = "FIN", rate = c(1, ir.installment[k] / vm),
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
#'       type = "FIN", rate = c(1, ir.installment[k] / vm),
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
#'       type = "FIN", rate = c(1, ir.installment[k] / vm),
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
#'   # The lump-sum interest rate equals the yield rate of money.
#'   ir.lumpsum <- ts.exchange.value[1:(np - 2)] / ts.exchange.value[2:(np - 1)] - 1
#'   ir.lumpsum <- pmax(1e-6, ir.lumpsum)
#'   ir.lumpsum[np - 1] <- ir.lumpsum[np - 2]
#'   ir.installment.new <- 1 / (1 / ir.lumpsum + 1 - 1 / vm)
#'
#'   ir.installment <- c(ir.installment * ratio_adjust(ir.installment.new / ir.installment, 0.3))
#'   cat("ir.installment: ", ir.installment, "\n")
#'
#'   if (return.ge) {
#'     ge$ts.exchange.value <- ts.exchange.value
#'     return(ge)
#'   } else {
#'     return(ir.installment)
#'   }
#' }
#'
#' ## Calculate equilibrium installment interest rates.
#' ## Warning: Running the program below takes about several minutes.
#' ## mat.ir.installment <- iterate(rep(0.1, np - 1), f, tol = 1e-4)
#' ## # the lump-sum interest rate in the steady-state equilibrium.
#' ## ir.lumpsum.steadyState <- sserr(eis, Gamma.beta, gr, prepaid = TRUE)
#' ## # the installment interest rate in the steady-state equilibrium.
#' ## 1 / (1 / ir.lumpsum.steadyState + 1 - 1 / vm)
#'
#' ## Below are the calculated equilibrium installment interest rates.
#' (ir.installment <- c(
#'   0.3723, 0.2981, 0.2580, 0.2363, 0.2245, 0.2181, 0.2147,
#'   0.2128, 0.2118, 0.2112, 0.2109, 0.2107, 0.2106, 0.2106,
#'   0.2106, 0.2105, 0.2105, 0.2106, 0.2106
#' ))
#'
#' ge <- f(ir.installment, TRUE)
#'
#' plot(ge$z[1:(np - 1)], type = "o")
#'
#' # The yield rate is the interest rate for a lump-sum payment.
#' (ir.lumpsum <- ge$ts.exchange.value[1:(np - 2)] / ge$ts.exchange.value[2:(np - 1)] - 1)
#' 1 / (1 / ir.lumpsum + 1 - 1 / vm) # consistent with the installment interest rates
#' # consistent with the lump-sum interest rates.
#' ir.installment / (1 + ir.installment / vm - ir.installment)
#'
#' ## Calculate the growth rate of the money supply and the equilibrium nominal
#' ## lump-sum interest rates when the current price of the product remains constant.
#' price.money <- 1 / c(1, cumprod(ir.installment + 1))
#' currentPrice.prod <- ge$p[1:np] / price.money
#' gr.moneySupply <- unname(growth_rate(1 / currentPrice.prod))
#' # the equilibrium nominal lump-sum interest rates
#' (1 + ir.lumpsum) * (gr.moneySupply[2:(np - 1)] + 1) - 1
#' (1 + ir.installment / (1 + ir.installment / vm - ir.installment)) * (gr.moneySupply[2:np] + 1) - 1
#'
#' ## the corresponding sequential model with the same steady-state equilibrium.
#' dividend.rate <- sserr(eis, Gamma.beta, prepaid = TRUE)
#' ir.installment <- 1 / (1 / dividend.rate + 1 - 1 / vm)
#'
#' np <- 5
#' ge.ss <- f(ir.installment = rep(ir.installment, np - 1), return.ge = TRUE, y1 = 152)
#'
#' dst.firm <- node_new("prod",
#'   type = "FIN", rate = c(1, ir.installment / vm, (1 + ir.installment / vm) * dividend.rate),
#'   "cc1", "money", "equity.share"
#' )
#' node_set(dst.firm, "cc1",
#'   type = "CD",
#'   alpha = 2, beta = c(0.5, 0.5),
#'   "prod", "lab"
#' )
#'
#' dst.laborer <- node_new("util",
#'   type = "FIN", rate = c(1, ir.installment / vm),
#'   "prod", "money"
#' )
#'
#' dst.moneyOwner <- node_new("util",
#'   type = "FIN", rate = c(1, ir.installment / vm),
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
#'   numeraire = "prod"
#' )
#'
#' ge2$p
#' ge.ss$z[np - 1]
#' ge2$z
#' ge.ss$D[paste0("prod", np - 1), c("laborer", "moneyOwner")]
#' ge2$D
#' }

gemIntertemporal_EndogenousEquilibriumInterestRate_MoneyVelocity <- function(...) sdm2(...)
