#' @export
#' @title An Example Illustrating Endogenous Equilibrium Interest Rates in a (Timeline) Transitional Equilibrium Path
#' @aliases gemIntertemporal_EndogenousEquilibriumInterestRate
#' @description This example illustrates (endogenous) equilibrium primitive interest rates in a transitional equilibrium path,
#' which is an intertemporal path distinct from a steady-state equilibrium.
#' Assume that the velocity of money is equal to one, that is, money circulates once per period.
#'
#' The interest rate calculated here is adjusted from the nominal interest rate based on the growth rate of the money supply,
#' which is equal to the nominal interest rate when the money stock remains unchanged.
#' We refer to this kind of interest rate as the primitive interest rate,
#' which usually differs from the real interest rate obtained by adjusting the nominal rate based on the inflation rate.
#'
#' There are three types of economic agents in the model: firms, a laborer, and a money owner.
#' Suppose the laborer and the money owner need to use money to buy products, and firms need to use money to buy products and labor.
#' Formally, the money owner borrows money from himself and pays interest to himself.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' eis <- 0.8 # the elasticity of intertemporal substitution
#' Gamma.beta <- 0.8 # the subjective discount factor
#' gr <- 0 # the steady-state growth rate
#' np <- 20 # the number of economic periods
#'
#' f <- function(ir = rep(0.25, np - 1), return.ge = FALSE,
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
#'       type = "FIN", rate = c(1, ir[k]),
#'       "cc1", "money"
#'     )
#'     node_set(dstl.firm[[k]], "cc1",
#'              type = "CD", alpha = alpha.firm[k], beta = c(0.5, 0.5),
#'              paste0("prod", k), paste0("lab", k)
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
#'              type = "FIN", rate = c(1, ir[k]),
#'              paste0("prod", k), "money"
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
#'              type = "FIN", rate = c(1, ir[k]),
#'              paste0("prod", k), "money"
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
#'   ir.new <- ts.exchange.value[1:(np - 2)] / ts.exchange.value[2:(np - 1)] - 1
#'   ir.new <- pmax(1e-6, ir.new)
#'   ir.new[np - 1] <- ir.new[np - 2]
#'
#'   ir <- c(ir * ratio_adjust(ir.new / ir, 0.3))
#'   cat("ir: ", ir, "\n")
#'
#'   if (return.ge) {
#'     ge$ts.exchange.value <- ts.exchange.value
#'     return(ge)
#'   } else {
#'     return(ir)
#'   }
#' }
#'
#' ## Calculate equilibrium interest rates.
#' ## Warning: Running the program below takes about several minutes.
#' # mat.ir <- iterate(rep(0.1, np - 1), f, tol = 1e-4)
#' # sserr(eis, Gamma.beta, gr, prepaid = TRUE)
#'
#' ## Below are the calculated equilibrium interest rates.
#' ir <- rep(0.25, np - 1)
#' ir[1:14] <- c(0.4301, 0.3443, 0.3007, 0.2776, 0.2652, 0.2584, 0.2546,
#'   0.2526, 0.2514, 0.2508, 0.2504, 0.2502, 0.2501, 0.2501)
#'
#' ge <- f(ir, TRUE)
#'
#' plot(ge$z[1:(np - 1)], type = "o")
#' ge$ts.exchange.value[1:(np - 2)] / ge$ts.exchange.value[2:(np - 1)] - 1
#' ir
#'
#' ## Calculate the growth rate of the money supply and the equilibrium nominal
#' ## interest rate when the current price of the product remains constant.
#' price.money <- 1 / c(1, cumprod(ir + 1))
#' currentPrice.prod <- ge$p[1:np] / price.money
#' gr.moneySupply <- unname(growth_rate(1 / currentPrice.prod))
#' (ir + 1) * (gr.moneySupply[2:np] + 1) - 1
#'
#' ## the corresponding sequential model with the same steady-state equilibrium.
#' np <- 5
#' ge.ss <- f(return.ge = TRUE, y1 = 128)
#'
#' dividend.rate <- ir <- sserr(eis, Gamma.beta, prepaid = TRUE)
#'
#' dst.firm <- node_new("prod",
#'                      type = "FIN", rate = c(1, ir, (1 + ir) * dividend.rate),
#'                      "cc1", "money", "equity.share"
#' )
#' node_set(dst.firm, "cc1",
#'          type = "CD",
#'          alpha = 2, beta = c(0.5, 0.5),
#'          "prod", "lab"
#' )
#'
#' dst.laborer <- node_new("util",
#'                         type = "FIN", rate = c(1, ir),
#'                         "prod", "money"
#' )
#'
#' dst.moneyOwner <- node_new("util",
#'                            type = "FIN", rate = c(1, ir),
#'                            "prod", "money"
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
#'
#' ## a technology shock.
#' ## Warning: Running the program below takes about several minutes.
#' # np <- 50
#' # f2 <- function(x) {
#' #  f(
#' #     ir = x, return.ge = FALSE,
#' #     y1 = 128, alpha.firm = {
#' #       tmp <- rep(2, np - 1)
#' #       tmp[25] <- 1.8
#' #       tmp
#' #     }
#' #   )
#' # }
#' #
#' # mat.ir <- iterate(rep(0.25, np - 1), f2, tol = 1e-4)
#' # tail(mat.ir, 1) # the equilibrium interest rates
#'
#' ## Calculate equilibrium interest rates.
#' ## Warning: Running the program below takes about several minutes.
#' # np <- 20
#' # gr <- 0.03
#' # mat.ir <- iterate(rep(0.1, np - 1), f, tol = 1e-4)
#' # sserr(eis, Gamma.beta, gr, prepaid = TRUE)
#' }

gemIntertemporal_EndogenousEquilibriumInterestRate <- function(...) sdm2(...)
