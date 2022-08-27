#' @export
#' @title Some Examples of Market-Clearing Paths with Capital Accumulation
#' @aliases gemCapitalAccumulation
#' @description Some examples of market-clearing paths (MCP) with capital accumulation.
#' The economy contains a production firm, a capital-leasing firm and a consumer.
#' @param ... arguments to be passed to the function sdm2.
#' @seealso {
#' \code{\link{gemPersistentTechnologicalProgress}}
#' }
#' @examples
#' \donttest{
#' #### a 3-by-3 example
#' dst.firm1 <- node_new(
#'   "prod",
#'   type = "CD",
#'   alpha=2, beta = c(0.5, 0.5),
#'   "cap", "cc1"
#' )
#' node_set(dst.firm1, "cc1",
#'          type="Leontief", a=1,
#'          "lab")
#'
#' dst.consumer <- dst.firm2 <- node_new(
#'   "util",
#'   type = "Leontief",
#'   a= 1,
#'   "prod"
#' )
#'
#' dstl <- list(dst.firm1, dst.consumer, dst.firm2)
#'
#' B <-  matrix(c(
#'   1, 0, 0.5,
#'   0, 0, 1,
#'   0, 0, 0
#' ), 3, 3, TRUE)
#'
#' S0Exg <-  matrix(c(
#'   NA, NA, NA,
#'   NA, NA, NA,
#'   NA, 100,NA
#' ), 3, 3, TRUE)
#'
#' ge <- sdm2(
#'   A = dstl,
#'   B = B,
#'   S0Exg = S0Exg,
#'   names.commodity = c("prod", "cap", "lab"),
#'   names.agent = c("firm1", "laborer","firm2"),
#'   numeraire = "prod",
#'   z0 = c(100, 0, 50),
#'   policy = policyMarketClearingPrice,
#'   maxIteration = 1,
#'   numberOfPeriods = 30,
#'   ts = TRUE
#' )
#'
#' matplot((ge$ts.z), type="o",pch=20)
#' matplot((ge$ts.p), type="o",pch=20)
#'
#' ## a MCP with labor supply change
#' ge <- sdm2(
#'   A = dstl,
#'   B = B,
#'   S0Exg = S0Exg,
#'   names.commodity = c("prod", "cap", "lab"),
#'   names.agent = c("firm1", "laborer","firm2"),
#'   numeraire = "prod",
#'   z0 = c(400,200,400),
#'   policy = list(
#'     function(time, state) {
#'       if (time >= 5) state$S[3, 2] <- 150
#'       state
#'     },
#'     policyMarketClearingPrice
#'   ),
#'   maxIteration = 1,
#'   numberOfPeriods = 30,
#'   ts = TRUE
#' )
#'
#' matplot((ge$ts.z), type="o",pch=20)
#' matplot((ge$ts.p), type="o",pch=20)
#'
#' ## a MCP with transitory technological progress
#' ge <- sdm2(
#'   A = dstl,
#'   B = B,
#'   S0Exg = S0Exg,
#'   names.commodity = c("prod", "cap", "lab"),
#'   names.agent = c("firm1", "laborer","firm2"),
#'   numeraire = "prod",
#'   z0 = c(400,200,400),
#'   policy = list(
#'     function(time, A) {
#'       if (time == 5) {
#'         A[[1]]$alpha = 3
#'       } else {
#'         A[[1]]$alpha = 2
#'       }
#'     },
#'     policyMarketClearingPrice
#'   ),
#'   maxIteration = 1,
#'   numberOfPeriods = 30,
#'   ts = TRUE
#' )
#'
#' matplot((ge$ts.z), type="o",pch=20)
#' matplot((ge$ts.p), type="o",pch=20)
#'
#' ## a MCP with permanent technological progress
#' ge <- sdm2(
#'   A = dstl,
#'   B = B,
#'   S0Exg = S0Exg,
#'   names.commodity = c("prod", "cap", "lab"),
#'   names.agent = c("firm1", "laborer","firm2"),
#'   numeraire = "prod",
#'   z0 = c(400,200,400),
#'   policy = list(
#'     function(time, A) {
#'       if (time >= 5) {
#'         A[[1]]$alpha = 3
#'       } else {
#'         A[[1]]$alpha = 2
#'       }
#'     },
#'     policyMarketClearingPrice
#'   ),
#'   maxIteration = 1,
#'   numberOfPeriods = 30,
#'   ts = TRUE
#' )
#'
#' matplot((ge$ts.z), type="o",pch=20)
#' matplot((ge$ts.p), type="o",pch=20)
#'
#' #### A 4-by-4 example wherein the capital goods
#' #### have a useful life of two periods.
#' ge <- sdm2(
#'   A = function(state) {
#'     a.firm1 <- CD_A(alpha = 2, Beta = c(0, 0.5, 0.5, 0), state$p)
#'     a.consumer <- c(1, 0, 0, 0)
#'     a.firm2 <- c(1, 0, 0, 0)
#'     a.firm3 <- c(0, 0, 0, 1)
#'     cbind(a.firm1, a.consumer, a.firm2, a.firm3)
#'   },
#'   B = matrix(c(
#'     1, 0, 0, 0,
#'     0, 0, 1, 1,
#'     0, 0, 0, 0,
#'     0, 0, 1, 0
#'   ), 4, 4, TRUE),
#'   S0Exg = matrix(c(
#'     NA, NA, NA,NA,
#'     NA, NA, NA,NA,
#'     NA, 100,NA,NA,
#'     NA, NA, NA,NA
#'   ), 4, 4, TRUE),
#'   names.commodity = c("prod", "cap", "lab","prod.used"),
#'   names.agent = c("firm1", "consumer","firm2","firm3"),
#'   numeraire = "prod",
#'   policy = policyMarketClearingPrice,
#'   z0 = c(100, 100, 100, 100),
#'   ts = TRUE,
#'   numberOfPeriods = 30,
#'   maxIteration = 1
#' )
#'
#' matplot(ge$ts.z, type = "o", pch = 20)
#' }

gemCapitalAccumulation <- function(...) sdm2(...)
