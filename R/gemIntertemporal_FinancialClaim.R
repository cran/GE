#' @export
#' @title Some Intertemporal Models with Financial Claim
#' @aliases gemIntertemporal_FinancialClaim
#' @description  Some intertemporal models with financial claim (or long-term held securities).
#' Financial claims may be equities, bonds, ad valorem taxation rights (ad valorem tax receipt), fiat money etc,
#' which can be treated in the same way in models.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' #### a model with tax.
#' np <- 5 # the number of (internal) periods.
#' gr.lab <- 0.03 # the growth rate of the labor supply.
#' tax.rate <- 0.25
#'
#' S <- matrix(NA, 2 * np + 1, np + 1) # the exogenous supply matrix.
#' S[(np + 1):(2 * np), np] <- 100 * (1 + gr.lab)^(0:(np - 1)) # the labor supply.
#' S[1, np] <- 10 # the product supply in the first period.
#' S[2 * np + 1, np + 1] <- np * 100 # the supply of tax receipt
#'
#' B <- matrix(0, 2 * np + 1, np + 1)
#' B[2:np, 1:(np - 1)] <- diag(np - 1)
#'
#' dstl.firm <- list()
#' for (k in 1:(np - 1)) {
#'   dstl.firm[[k]] <- node_new(
#'     "prod",
#'     type = "FIN", rate = c(1, tax.rate),
#'     "cc1", "tax receipt"
#'   )
#'   node_set(dstl.firm[[k]], "cc1",
#'     type = "CD",
#'     alpha = 1, beta = c(0.5, 0.5),
#'     paste0("prod", k), paste0("lab", k)
#'   )
#' }
#'
#' dst.laborer <- node_new(
#'   "util",
#'   type = "FIN", rate = c(1, tax.rate),
#'   "cc1", "tax receipt"
#' )
#' node_set(dst.laborer, "cc1",
#'   type = "CES", es = 0.5,
#'   alpha = 1, beta = rep(1 / np, np),
#'   paste0("cc1.", 1:np)
#' )
#' for (k in 1:np) {
#'   node_set(dst.laborer, paste0("cc1.", k),
#'     type = "CD", alpha = 1, beta = c(0.5, 0.5),
#'     paste0("prod", k), paste0("lab", k)
#'   )
#' }
#'
#' dst.government <- node_new(
#'   "util",
#'   type = "CD",
#'   alpha = 1, beta = rep(1 / np, np),
#'   paste0("cc1.", 1:np)
#' )
#' for (k in 1:np) {
#'   node_set(dst.government, paste0("cc1.", k),
#'     type = "CD", alpha = 1, beta = c(0.5, 0.5),
#'     paste0("prod", k), paste0("lab", k)
#'   )
#' }
#' node_plot(dst.government, TRUE)
#'
#' ge <- sdm2(
#'   A = c(dstl.firm, dst.laborer, dst.government),
#'   B = B,
#'   S0Exg = S,
#'   names.commodity = c(paste0("prod", 1:np), paste0("lab", 1:np), "tax receipt"),
#'   names.agent = c(paste0("firm", 1:(np - 1)), "laborer", "government"),
#'   numeraire = "prod1",
#'   # policy = makePolicyHeadTailAdjustment(gr = gr.lab, np = np)
#' )
#'
#' ge$D
#' ge$z
#' ge$DV
#'
#' #### a pure exchange model with money.
#' np <- 3 # the number of (internal) periods.
#' gr.lab <- 0.03 # the growth rate of endowments (i.e. labor supply)
#' eis <- 0.8 # the elasticity of intertemporal substitution
#' rho.beta <- 0.8 # the subjective discount factor
#' interest.rate <- 0.2593 # (1 + gr.lab)^(1 / eis - 1) / rho.beta - 1
#'
#' dst.laborer <- node_new(
#'   "util",
#'   type = "CES", es = eis,
#'   alpha = 1, beta = rho.beta^(0:(np - 1)),
#'   paste0("cc", 1:np)
#' )
#'
#' for (k in 1:np) {
#'   node_set(dst.laborer, paste0("cc", k),
#'     type = "FIN",
#'     rate = c(1, interest.rate),
#'     paste0("lab", k), paste0("money", k)
#'   )
#' }
#'
#' node_plot(dst.laborer, TRUE)
#'
#' dst.money.owner <- Clone(dst.laborer)
#'
#' ge <- sdm2(
#'   A = list(dst.laborer, dst.money.owner),
#'   B = matrix(0, 2 * np, 2),
#'   S0Exg = {
#'     tmp <- matrix(0, 2 * np, 2)
#'     tmp[1:np, 1] <- 100 * (1 + gr.lab)^(0:(np - 1))
#'     tmp[(np + 1):(2 * np), 2] <- 200
#'     tmp
#'   },
#'   names.commodity = c(paste0("lab", 1:np), paste0("money", 1:np)),
#'   names.agent = c("laborer", "money.owner"),
#'   numeraire = c(money1 = interest.rate)
#' )
#'
#' #### a two-period model with production and money.
#' interest.rate1 <- 0.25
#' interest.rate2 <- 0.1
#'
#' dst.firm <- node_new(
#'   "prod",
#'   type = "FIN",
#'   rate = c(1, interest.rate1),
#'   "cc1", "money1"
#' )
#' node_set(dst.firm, "cc1",
#'   type = "CES",
#'   es = 1, alpha = 2, beta = c(0.5, 0.5),
#'   "prod1", "lab1"
#' )
#'
#' dst.laborer <- node_new(
#'   "util",
#'   type = "CES",
#'   es = 0.5, alpha = 1, beta = c(2 / 3, 1 / 3),
#'   "cc1", "cc2"
#' )
#' node_set(dst.laborer, "cc1",
#'   type = "FIN",
#'   rate = c(1, interest.rate1),
#'   "prod1", "money1"
#' )
#'
#' node_set(dst.laborer, "cc2",
#'   type = "FIN",
#'   rate = c(1, interest.rate2),
#'   "prod2", "money2"
#' )
#'
#' dst.money.owner <- Clone(dst.laborer)
#'
#' ge <- sdm2(
#'   A = list(dst.firm, dst.laborer, dst.money.owner),
#'   B = matrix(c(
#'     0, 0, 0,
#'     0, 0, 0,
#'     0, 0, 0,
#'     1, 0, 0,
#'     0, 0, 0
#'   ), 5, 3, TRUE),
#'   S0Exg = matrix(c(
#'     NA, 200, NA,
#'     NA, 100, NA,
#'     NA, NA, 100,
#'     NA, NA, NA,
#'     NA, NA, 100
#'   ), 5, 3, TRUE),
#'   names.commodity = c("prod1", "lab1", "money1", "prod2", "money2"),
#'   names.agent = c("firm", "laborer", "money.owner"),
#'   numeraire = c(money1 = interest.rate1)
#' )
#'
#' ge$p
#' ge$DV
#' }

gemIntertemporal_FinancialClaim <- function(...) sdm2(...)
