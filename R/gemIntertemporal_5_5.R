#' @export
#' @title An Intertemporal Model with Land, Two Consumers, and Three Types of Firms
#' @aliases gemIntertemporal_5_5
#' @description An (intertemporal) timeline model with two consumers (i.e. a laborer and a landowner) and three types of firms
#' (i.e. wheat producers, iron producers and iron leaser).
#' Here the iron leasing firm is actually a quasi-firm, which does not require primary factors such as labor and land in its production process.
#' There are four commodities in the model, namely wheat, iron, iron leased out as a capital good, labor and land.
#' @param ... arguments to be passed to the function sdm2.
#' @examples
#' \donttest{
#' np <- 15 # the number of economic periods
#' gr <- 0 # the growth rate in the steady state equilibrium
#' eis <- 1 # the elasticity of intertemporal substitution  of consumers
#' rho.beta <- 0.97 # the subjective discount factor of consumers
#' last.beta.laborer <- 0
#' last.beta.landowner <- 0
#' depreciation.rate <- 0.06
#'
#' alpha.firm.wheat <- rep(5, np - 1)
#' alpha.firm.iron <- rep(5, np - 1)
#'
#' y1.wheat <- 200
#' y1.iron <- 100
#'
#' n <- 5 * np - 3 # the number of commodity kinds
#' m <- 3 * np - 1 # the number of agent kinds
#'
#' names.commodity <- c(
#'   paste0("wheat", 1:np),
#'   paste0("iron", 1:np),
#'   paste0("cap", 1:(np - 1)),
#'   paste0("lab", 1:(np - 1)),
#'   paste0("land", 1:(np - 1))
#' )
#' names.agent <- c(
#'   paste0("firm.wheat", 1:(np - 1)), paste0("firm.iron", 1:(np - 1)),
#'   paste0("quasifirm.cap", 1:(np - 1)), # a quasifirm
#'   "laborer", "landowner"
#' )
#'
#' # the exogenous supply matrix.
#' S0Exg <- matrix(NA, n, m, dimnames = list(names.commodity, names.agent))
#' S0Exg["wheat1", "laborer"] <- y1.wheat
#' S0Exg["iron1", "landowner"] <- y1.iron
#' S0Exg[paste0("lab", 1:(np - 1)), "laborer"] <- 100 * (1 + gr)^(0:(np - 2)) # the supply of labor
#' S0Exg[paste0("land", 1:(np - 1)), "landowner"] <- 100 * (1 + gr)^(0:(np - 2)) # the supply of land
#'
#' # the output coefficient matrix.
#' B <- matrix(0, n, m, dimnames = list(names.commodity, names.agent))
#' for (k in 1:(np - 1)) {
#'   B[paste0("wheat", k + 1), paste0("firm.wheat", k)] <- 1
#'   B[paste0("iron", k + 1), paste0("firm.iron", k)] <- 1
#'   B[paste0("cap", k), paste0("quasifirm.cap", k)] <- 1
#'   B[paste0("iron", k + 1), paste0("quasifirm.cap", k)] <- 1 - depreciation.rate
#' }
#'
#' dstl.firm.wheat <- dstl.firm.iron <- dstl.quasifirm.cap <- list()
#' for (k in 1:(np - 1)) {
#'   dstl.firm.wheat[[k]] <- node_new(
#'     "prod",
#'     type = "CES", es = 1,
#'     alpha = alpha.firm.wheat[k], beta = c(0.2, 0.4, 0.4),
#'     paste0("cap", k), paste0("lab", k), paste0("land", k)
#'   )
#'
#'   dstl.firm.iron[[k]] <- node_new(
#'     "prod",
#'     type = "CES", es = 1,
#'     alpha = alpha.firm.iron[k], beta = c(0.4, 0.4, 0.2),
#'     paste0("cap", k), paste0("lab", k), paste0("land", k)
#'   )
#'
#'   dstl.quasifirm.cap[[k]] <- node_new(
#'     "output",
#'     type = "Leontief", a = 1,
#'     paste0("iron", k)
#'   )
#' }
#'
#' tmp.beta <- rho.beta^(1:(np - 1))
#' tmp.beta <- tmp.beta / tmp.beta[np - 1]
#' tmp.beta <- c(tmp.beta, last.beta.laborer)
#' dst.laborer <- node_new(
#'   "util",
#'   type = "CES", es = eis,
#'   alpha = 1, beta = prop.table(tmp.beta),
#'   paste0("cc", 1:(np - 1)), paste0("wheat", np)
#' )
#' for (k in 1:(np - 1)) {
#'   node_set(dst.laborer, paste0("cc", k),
#'     type = "CES", es = 1,
#'     alpha = 1, beta = c(0.4, 0.4, 0.2),
#'     paste0("wheat", k), paste0("lab", k), paste0("land", k)
#'   )
#' }
#'
#' tmp.beta <- rho.beta^(1:(np - 1))
#' tmp.beta <- tmp.beta / tmp.beta[np - 1]
#' tmp.beta <- c(tmp.beta, last.beta.landowner)
#' dst.landowner <- node_new(
#'   "util",
#'   type = "CES", es = eis,
#'   alpha = 1, beta = prop.table(tmp.beta),
#'   paste0("cc", 1:(np - 1)), paste0("iron", np)
#' )
#' for (k in 1:(np - 1)) {
#'   node_set(dst.landowner, paste0("cc", k),
#'     type = "CES", es = 1,
#'     alpha = 1, beta = c(0.2, 0.4, 0.4),
#'     paste0("wheat", k), paste0("lab", k), paste0("land", k)
#'   )
#' }
#'
#' ge <- sdm2(
#'   A = c(
#'     dstl.firm.wheat, dstl.firm.iron, dstl.quasifirm.cap,
#'     dst.laborer, dst.landowner
#'   ),
#'   B = B,
#'   S0Exg = S0Exg,
#'   names.commodity = names.commodity,
#'   names.agent = names.agent,
#'   numeraire = "lab1",
#'   policy = makePolicyMeanValue(50),
#'   ts = TRUE,
#'   priceAdjustmentVelocity = 0.03
#' )
#'
#' ge$p
#' ge$z
#' plot(ge$z[2 * (np - 1) + (1:(np - 1))], type = "b", pch = 20)
#' lines(1:(np - 1), ge$z[1:(np - 1)], type = "b", pch = 21)
#' lines(1:(np - 1), ge$z[np - 1 + (1:(np - 1))], type = "b", pch = 22)
#' legend("topleft", c("cap","wheat", "iron"), pch = 20:21)
#' }

gemIntertemporal_5_5 <- function(...) sdm2(...)
