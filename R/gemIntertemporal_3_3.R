#' @export
#' @title Some Examples of Intertemporal Models with One Consumer and Two Types of Firms
#' @aliases gemIntertemporal_3_3
#' @description Some examples of intertemporal models with one consumer and two types of firms.
#' There are three commodities (i.e. corn, iron and labor).
#' The consumer may consume corn and iron in each period, and may have a nested intertemporal utility function.
#' @param ... arguments to be passed to the function sdm2.
#' @references Zen Xiangjin (1995, ISBN: 7030046560). Basics of Economic Cybernetics. Beijing: Science Press. (In Chinese)
#' @examples
#' \donttest{
#' #### an example with a consumer with a nested intertemporal utility function
#' np <- 5 # the number of periods, firms.
#'
#' ## exogenous supply matrix
#' n <- 3 * np - 1
#' m <- 2 * (np - 1) + 1
#' S <- matrix(NA, n, m)
#' S[(2 * np + 1):(3 * np - 1), m] <- 100
#' S[1, m] <- 25 #corn1
#' S[np + 1, m] <- 100 #iron1
#'
#' B <- matrix(0, n, m)
#' B[2:np, 1:(np - 1)] <- B[(np + 2):(2 * np), np:(m - 1)] <-
#'   diag(np - 1)
#'
#' dstl.firm.corn <- dstl.firm.iron  <- list()
#' for (k in 1:(np - 1)) {
#'   dstl.firm.corn[[k]] <- node_new(
#'     "prod",
#'     type = "CD",
#'     alpha = 1,
#'     beta = c(0.5, 0.5),
#'     paste0("iron", k),
#'     paste0("lab", k)
#'   )
#'
#'   dstl.firm.iron[[k]] <- node_new(
#'     "prod",
#'     type = "CD",
#'     alpha = 2,
#'     beta = c(0.5, 0.5),
#'     paste0("iron", k),
#'     paste0("lab", k)
#'   )
#' }
#'
#' dst.consumer <- node_new(
#'   "util",
#'   type = "CD",
#'   alpha = 1,
#'   beta = prop.table(rep(1, np)),
#'   paste0("cc", 1:np)
#' )
#' for (k in 1:np) {
#'   node_set(
#'     dst.consumer,
#'     paste0("cc", k),
#'     type = "CD",
#'     alpha = 1,
#'     beta = c(0.5, 0.5),
#'     paste0("corn", k),
#'     paste0("iron", k)
#'   )
#' }
#'
#' ge <-   sdm2(
#'   A = c(dstl.firm.corn, dstl.firm.iron, dst.consumer),
#'   B = B,
#'   S0Exg = S,
#'   names.commodity = c(paste0("corn", 1:np),
#'                       paste0("iron", 1:np),
#'                       paste0("lab", 1:(np - 1))),
#'   names.agent = c(paste0("firm.corn", 1:(np - 1)),
#'                   paste0("firm.iron", 1:(np - 1)),
#'                   "consumer"),
#'   numeraire = "lab1",
#'   ts = TRUE
#' )
#'
#' ge$p
#' ge$z
#' ge$D
#' ge$S
#' ge$DV
#' ge$SV
#'
#' #### an example with a consumer with a non-nested intertemporal utility function
#' np <- 3 # the number of periods, firms.
#'
#' ## There are np types of corn, np-1 types of iron and np-1 types of labor.
#' ## There are np-1 corn firms, np-2 iron firms and one consumer.
#' n <- 3 * np - 2
#' m <- 2 * np - 2
#'
#' ## exogenous supply matrix
#' S <- matrix(NA, n, m)
#' S[(2 * np):n, m] <- 100
#' S[1, m] <- 25 #corn1
#' S[np + 1, m] <- 100 #iron1
#'
#' B <- matrix(0, n, m)
#' B[2:np, 1:(np - 1)]  <- diag(np - 1)
#' B[(np + 2):(2 * np - 1), np:(m - 1)] <- diag(np - 2)
#'
#' dstl.firm.corn <- dstl.firm.iron  <- list()
#' for (k in 1:(np - 1)) {
#'   dstl.firm.corn[[k]] <- node_new(
#'     "prod",
#'     type = "CD",
#'     alpha = 1,
#'     beta = c(0.5, 0.5),
#'     paste0("iron", k),
#'     paste0("lab", k)
#'   )
#' }
#'
#' for (k in seq_along(np:(2 * np - 3))) {
#'   dstl.firm.iron[[k]] <- node_new(
#'     "prod",
#'     type = "CD",
#'     alpha = 2,
#'     beta = c(0.5, 0.5),
#'     paste0("iron", k),
#'     paste0("lab", k)
#'   )
#' }
#'
#' dst.consumer <- node_new(
#'   "util",
#'   type = "CD",
#'   alpha = 1,
#'   beta = prop.table(rep(1, np)),
#'   paste0("corn", 1:np)
#' )
#'
#' ge <-   sdm2(
#'   A = c(dstl.firm.corn, dstl.firm.iron, dst.consumer),
#'   B = B,
#'   S0Exg = S,
#'   names.commodity = c(paste0("corn", 1:np),
#'                       paste0("iron", 1:(np - 1)),
#'                       paste0("lab", 1:(np - 1))),
#'   names.agent = c(paste0("firm.corn", 1:(np - 1)),
#'                   paste0("firm.iron", 1:(np - 2)),
#'                   "consumer"),
#'   numeraire = "lab1",
#'   ts = TRUE
#' )
#'
#' ge$p
#' ge$z
#' ge$D
#' ge$S
#' ge$DV
#' ge$SV
#'
#' #### an example of Zeng (1995, page 227)
#' ic1 <- 1 / 10 #input coefficient
#' ic2 <- 1 / 7
#' dc1 <- 2 / 3 #depreciation coefficient
#' dc2 <- 9 / 10
#'
#' ge <- sdm2(
#'   A = {
#'     #corn, iron1, iron2, iron3, iron4
#'     a1.1 <- c(0, ic1, 0, 0, 0)
#'     a1.2 <- c(0, ic2, 0, 0, 0)
#'     a2.1 <- c(0, 0, ic1, 0, 0)
#'     a2.2 <- c(0, 0, ic2, 0, 0)
#'     a3.1 <- c(0, 0, 0, ic1, 0)
#'     a3.2 <- c(0, 0, 0, ic2, 0)
#'     a4.1 <- c(0, 0, 0, 0, ic1)
#'     a4.2 <- c(0, 0, 0, 0, ic2)
#'
#'     a.consumer <- c(1, 0, 0, 0, 0)
#'
#'     cbind(a1.1, a1.2, a2.1, a2.2, a3.1, a3.2, a4.1, a4.2, a.consumer)
#'   },
#'   B = {
#'     b1.1 <- c(1, 0, ic1 * dc1, 0, 0)
#'     b1.2 <- c(1, 0, ic2 * dc2, 0, 0)
#'     b2.1 <- c(1, 0, 0, ic1 * dc1, 0)
#'     b2.2 <- c(1, 0, 0, ic2 * dc2, 0)
#'     b3.1 <- c(1, 0, 0, 0, ic1 * dc1)
#'     b3.2 <- c(1, 0, 0, 0, ic2 * dc2)
#'     b4.1 <- c(1, 0, 0, 0, 0)
#'     b4.2 <- c(1, 0, 0, 0, 0)
#'     b.consumer <- c(0, 0, 0, 0, 0)
#'
#'     cbind(b1.1, b1.2, b2.1, b2.2, b3.1, b3.2, b4.1, b4.2, b.consumer)
#'   },
#'   S0Exg = {
#'     tmp <- matrix(NA, 5, 9)
#'     tmp[2, 9] <- 100
#'     tmp
#'   },
#'   names.commodity = c("corn", paste0("iron", 1:4)),
#'   names.agent = c(paste0("firm", 1:8), "consumer"),
#'   numeraire = "corn",
#'   policy = makePolicyMeanValue(30),
#'   priceAdjustmentVelocity = 0.05,
#'   maxIteration = 1,
#'   numberOfPeriods = 1000,
#'   ts = TRUE
#' )
#'
#' matplot(ge$ts.z, type = "l")
#'
#' ge$p
#' ge$z
#' ge$D
#' ge$S
#' }

gemIntertemporal_3_3 <- function(...) sdm2(...)
