#' @export
#' @title Structural Dynamic Model (alias Structural Growth Model) Version 2
#' @aliases sdm2
#' @description A new version of the sdm function in the package CGE.
#' Now the  parameter A can be a demand structure tree list.
#' Hence we actually no longer need the function \code{\link{sdm_dstl}}.
#' Some rarely used parameters in the function sdm have been deleted.
#' This function is the core of this package.
#' @param A a demand structure tree list (i.e. dstl, see \code{\link{demand_coefficient}}), a demand coefficient n-by-m matrix (alias demand structure matrix)
#' or a function A(state) which returns an n-by-m matrix (see the sdm function).
#' When the calculation process involves policy functions, dstl is strongly recommended.
#' Some policy functions in this package only support dstl.
#' @param B	a supply coefficient n-by-m matrix (alias supply structure matrix).
#' If the (i,j)-th element of S0Exg is not NA, the value of the (i,j)-th element of B will be useless and ignored.
#' @param S0Exg an initial exogenous supply n-by-m matrix.
#' If the (i,j)-th element of S0Exg is zero, it means there is no supply,
#' and NA means the exogenous part of the supply is zero
#' and there may be an endogenous supply part.
#' In most cases, this matrix contains NA values but no zeros.
#' @param names.commodity names of commodities.
#' If the parameter A is a demand structure tree list, the values in names.commodity should be the names of those leaf nodes.
#' @param names.agent names of agents.
#' @param p0 an initial price n-vector.
#' @param z0 an m-vector consisting of the initial exchange levels (i.e. activity levels, production levels or utility levels).
#' @param GRExg an exogenous growth rate of the exogenous supplies in S0Exg.
#' If GRExg is NA and some commodities have exogenous supply, then GRExg will be set to 0.
#' @param pExg an n-vector indicating the exogenous prices (if any).
#' @param numeraire the name, index or price of the numeraire commodity.
#' If it is a character string, then it is assumed to be the name of the numeraire commodity.
#' If it is a number without a name, then it is assumed to be the index of the numeraire commodity.
#' If it is a number with a name, e.g. c("lab" = 0.5), then the name is assumed to be the name of the numeraire commodity
#' and the number is assumed to be the price of the numeraire commodity,
#' even though the price of the numeraire commodity usually is 1.
#' @param tolCond the tolerance condition.
#' @param maxIteration the maximum iteration count. If the main purpose of running this function is to do simulation instead of calculating equilibrium, then maxIteration should be set to 1.
#' @param numberOfPeriods the period number in each iteration, which should not be less than 20.
#' @param depreciationCoef the depreciation coefficient (i.e. 1 minus the depreciation rate) of the unsold products.
#' @param priceAdjustmentFunction the price adjustment function. The arguments are a price n-vector p and a sales rate n-vector q.
#' The return value is a price n-vector. The default price adjustment method is p * (1 - priceAdjustmentVelocity * (1 - q)).
#' @param priceAdjustmentVelocity the price adjustment velocity.
#' @param trace if TRUE, information is printed during the running of sdm.
#' @param ts if TRUE, the time series of the last iteration are returned.
#' @param policy a policy function or a policy function list. A policy function has the following optional parameters:
#' \itemize{
#' \item time - the current time.
#' \item dstl - the demand structure tree list in the model, which can be adjusted in the policy function
#' and it need not be returned.
#' \item state - the current state, which is a list.
#' state$p is the current price vector with names.
#' state$S is the current supply matrix.
#' state$last.z is the last output and utility vector.
#' state$B is the current supply coefficient matrix.
#' state$names.commodity contains the names of commodities.
#' state$names.agent contains the names of agents.
#' \item state.history - the state history, which is a list consisting of the time series of p, S, q, and z.
#' }
#' The return value of the policy function other than a list will be ignored.
#' If the return value is a list, it should have elements p, S and B which specify the prices, supplies and supply coefficient matrix after adjustment.
#' A vector with the name current.policy.data can be put into the state list as well,
#' which will be put into the return value of the sdm2.
#' @param exchangeFunction the exchange function.
#' @details In each period of the structural dynamic model, the economy runs as follows. \cr
#' Firstly, the new price vector emerges on the basis of the price vector and sales rates of the previous period, which indicates the current market prices. \cr
#' Secondly, outputs and depreciated inventories of the previous period constitute the current supplies.\cr
#' Thirdly, policy functions (if any) are implemented.\cr
#' Fourthly, the current input coefficient matrix is computed and the supplies are exchanged under market prices. The exchange vector and sales rate vector are obtained. Unsold goods constitute the inventories, which will undergo depreciation and become a portion of the supplies of the next period. The exchange vector determines the current outputs and utility levels.
#' @return  A list usually containing the following components:
#' \itemize{
#' \item tolerance - the tolerance of the results.
#' \item p - equilibrium prices.
#' \item z- equilibrium exchange levels (i.e. activity levels, output levels or utility levels).
#' \item S - the equilibrium supply matrix at the initial period.
#' \item growthRate	- the endogenous equilibrium growth rate in a pure production economy.
#' \item A - the equilibrium demand coefficient matrix.
#' \item B - the supply coefficient matrix.
#' \item S0Exg - the initial exogenous supply n-by-m matrix.
#' \item D - the demand matrix.
#' \item DV - the demand value matrix.
#' \item SV - the supply value matrix.
#' \item ts.p	- the time series of prices in the last iteration.
#' \item ts.z - the time series of exchange levels (i.e. activity levels, production levels or utility levels) in the last iteration.
#' \item ts.S - the time series of supply matrix in the last iteration.
#' \item ts.q - the time series of sales rates in the last iteration.
#' \item policy.data - the policy data.
#' }
#' @note In the package CGE, the instantaneous equilibrium path (alias market clearing path) is computed by the function iep.
#' In this package, the instantaneous equilibrium path can be computed by the function sdm2 with the parameter policy equal to \code{\link{policyMarketClearingPrice}}.\cr
#' @note The order of implementation of various policies is critical.
#' When a policy list contains a supply policy, a technology (i.e. dstl) policy, a price policy (e.g. a market-clearing-price policy) and a B policy
#' (i.e. a policy adjusting the argument B), both the supply policy and the technology policy should be placed before the price policy,
#' and the B policy should be placed after the price policy.
#' The reason is that the calculation of the current prices may require the use of supply and technology,
#' while the calculation of B may require the use of the current prices.
#' @references LI Wu (2019, ISBN: 9787521804225) General Equilibrium and Structural Dynamics: Perspectives of New Structural Economics. Beijing: Economic Science Press. (In Chinese)
#' @references LI Wu (2010) A Structural Growth Model and its Applications to Sraffa's System. http://www.iioa.org/conferences/18th/papers/files/104_20100729011_AStructuralGrowthModelanditsApplicationstoSraffasSstem.pdf
#' @examples
#' \donttest{
#' dst.firm <- node_new("output",
#'   type = "Leontief", a = c(0.5, 1),
#'   "prod", "lab"
#' )
#'
#' dst.consumer <- node_new("utility",
#'   type = "Leontief", a = 1, "prod"
#' )
#'
#' dstl <- list(dst.firm, dst.consumer)
#'
#' B <- matrix(c(
#'   1, 0,
#'   0, 0
#' ), 2, 2, TRUE)
#' S0Exg <- matrix(c(
#'   NA, NA,
#'   NA, 100
#' ), 2, 2, TRUE)
#'
#' ## variable dst and technology progress
#' policy.TP <- function(time, state, dstl) {
#'   if (time >= 200) {
#'     dstl[[1]]$a <- c(0.5, 0.8)
#'   } else {
#'     dstl[[1]]$a <- c(0.5, 1)
#'   }
#'   state
#' }
#'
#' ge.TP <- sdm2(
#'   A = dstl, B = B, S0Exg = S0Exg,
#'   names.commodity = c("prod", "lab"),
#'   names.agent = c("firm", "consumer"),
#'   policy = policy.TP,
#'   ts = TRUE,
#'   maxIteration = 1,
#'   numberOfPeriods = 1000
#' )
#' matplot(ge.TP$ts.z, type = "l")
#' plot(ge.TP$ts.p[, 1] / ge.TP$ts.p[, 2], type = "l")
#'
#' ## variable supply coefficient matrix and technology progress
#' policy.TP <- function(time, state) {
#'   if (time >= 200) {
#'     state$B[1, 1] <- 2
#'   } else {
#'     state$B[1, 1] <- 1
#'   }
#'   state
#' }
#'
#' ge.TP <- sdm2(
#'   A = dstl, B = B, S0Exg = S0Exg,
#'   names.commodity = c("prod", "lab"),
#'   names.agent = c("firm", "consumer"),
#'   policy = policy.TP,
#'   ts = TRUE,
#'   maxIteration = 1,
#'   numberOfPeriods = 1000
#' )
#' matplot(ge.TP$ts.z, type = "l")
#' plot(ge.TP$ts.p[, 1] / ge.TP$ts.p[, 2], type = "l")
#'
#' ## variable dst and disequilibrium
#' policy.DE <- function(time, dstl) {
#'   if (time >= 200) {
#'     dstl[[1]]$a[2] <- dstl[[1]]$a[2] * 0.999
#'   } else {
#'     dstl[[1]]$a[2] <- 1
#'   }
#' }
#'
#' ge.DE <- sdm2(
#'   A = dstl, B = B, S0Exg = S0Exg,
#'   names.commodity = c("prod", "lab"),
#'   names.agent = c("firm", "consumer"),
#'   policy = policy.DE,
#'   ts = TRUE,
#'   maxIteration = 1,
#'   numberOfPeriods = 1000
#' )
#' matplot(ge.DE$ts.z, type = "l")
#' plot(ge.DE$ts.p[, 1] / ge.DE$ts.p[, 2], type = "l")
#'
#'
#' ## structural equilibria and structural transition
#' policy.SE <- function(time, state, dstl) {
#'   dstl[[1]]$a[2] <- structural_function(state$last.z[1], c(105, 125), 1, 0.5)
#' }
#'
#' ge.low.level <- sdm2(
#'   A = dstl, B = B, S0Exg = S0Exg,
#'   names.commodity = c("prod", "lab"),
#'   names.agent = c("firm", "consumer"),
#'   policy = policy.SE,
#'   ts = TRUE,
#'   maxIteration = 1,
#'   numberOfPeriods = 1000,
#'   z0 = c(100, 100)
#' )
#' matplot(ge.low.level$ts.z, type = "l")
#'
#' ge.high.level <- sdm2(
#'   A = dstl, B = B, S0Exg = S0Exg,
#'   names.commodity = c("prod", "lab"),
#'   names.agent = c("firm", "consumer"),
#'   policy = policy.SE,
#'   ts = TRUE,
#'   maxIteration = 1,
#'   numberOfPeriods = 1000,
#'   z0 = c(150, 100)
#' )
#' matplot(ge.high.level$ts.z, type = "l")
#'
#' policy.ST <- function(time, state, dstl) {
#'   dstl[[1]]$a[2] <- structural_function(state$last.z[1], c(105, 125), 1, 0.5)
#'   if (time >= 200 && time <= 210) state$S[2, 2] <- 125 # Introduce foreign labor.
#'   state
#' }
#'
#' ge.ST <- sdm2(
#'   A = dstl, B = B, S0Exg = S0Exg,
#'   names.commodity = c("prod", "lab"),
#'   names.agent = c("firm", "consumer"),
#'   policy = policy.ST,
#'   ts = TRUE,
#'   maxIteration = 1,
#'   numberOfPeriods = 1000,
#'   z0 = c(100, 100)
#' )
#' matplot(ge.ST$ts.z, type = "l")
#'
#' #### economic cycles and an interest rate policy for the firm
#' dst.firm <- node_new("cc", # composite commodity
#'   type = "FIN",
#'   rate = c(1, 0.25),
#'   "cc1", "money"
#' )
#' node_set(dst.firm, "cc1",
#'   type = "Leontief",
#'   a = c(0.5, 0.5),
#'   "wheat", "labor"
#' )
#'
#' dst.laborer <- Clone(dst.firm)
#' dst.money.lender <- Clone(dst.firm)
#'
#' dstl <- list(dst.firm, dst.laborer, dst.money.lender)
#'
#' policy.interest.rate <- function(time, state, dstl, state.history) {
#'   upsilon <- NA
#'   if (time >= 600) {
#'     upsilon <- state.history$z[time - 1, 1] / mean(state.history$z[(time - 50):(time - 1), 1])
#'     dstl[[1]]$rate[2] <- max(0.25 + 0.5 * log(upsilon), 0)
#'   } else {
#'     dstl[[1]]$rate[2] <- 0.25
#'   }
#'
#'   state$current.policy.data <- c(time, dstl[[1]]$rate[2], upsilon)
#'   state
#' }
#'
#' B <- matrix(0, 3, 3)
#' B[1, 1] <- 1
#'
#' S0Exg <- matrix(NA, 3, 3)
#' S0Exg[2, 2] <- 100
#' S0Exg[3, 3] <- 100
#'
#' de <- sdm2(
#'   A = dstl, B = B, S0Exg = S0Exg,
#'   names.commodity = c("wheat", "labor", "money"),
#'   names.agent = c("firm", "laborer", "money.lender"),
#'   p0 = rbind(0.625, 0.375, 0.25),
#'   z0 = rbind(95, 100, 100),
#'   priceAdjustmentVelocity = 0.3,
#'   numberOfPeriods = 1000,
#'   maxIteration = 1,
#'   trace = FALSE,
#'   ts = TRUE
#' )
#' matplot(de$ts.z, type = "l")
#'
#' ge.policy <- sdm2(
#'   A = dstl, B = B, S0Exg = S0Exg,
#'   names.commodity = c("wheat", "labor", "money"),
#'   names.agent = c("firm", "laborer", "money.lender"),
#'   p0 = rbind(0.625, 0.375, 0.25),
#'   z0 = rbind(95, 100, 100),
#'   priceAdjustmentVelocity = 0.3,
#'   numberOfPeriods = 1000,
#'   maxIteration = 1,
#'   trace = FALSE,
#'   ts = TRUE,
#'   policy = policy.interest.rate
#' )
#' matplot(ge.policy$ts.z, type = "l")
#'
#' #### Example 9.3 in Li (2019): fixed-ratio price adjustment method
#' #### and disequilibrium (business cycles) in a pure production economy
#' fixedRatioPriceAdjustmentFunction <- function(p, q) {
#'   thresholdForPriceAdjustment <- 0.99
#'   priceAdjustmentVelocity <- 0.02
#'   result <- ifelse(q <= thresholdForPriceAdjustment,
#'     p * (1 - priceAdjustmentVelocity),
#'     p
#'   )
#'   return(prop.table(result))
#' }
#'
#' de.Sraffa <- sdm2(
#'   A = matrix(c(
#'     56 / 115, 6,
#'     12 / 575, 2 / 5
#'   ), 2, 2, TRUE),
#'   B = diag(2),
#'   maxIteration = 1,
#'   numberOfPeriods = 100,
#'   p0 = rbind(1 / 15, 1),
#'   z0 = rbind(575, 20),
#'   priceAdjustmentFunction = fixedRatioPriceAdjustmentFunction,
#'   ts = TRUE
#' )
#' matplot(growth_rate(de.Sraffa$ts.z), type = "l")
#' }
#'
sdm2 <- function(A,
                 B,
                 S0Exg = matrix(NA, nrow(B), ncol(B)),
                 names.commodity = paste("comm", 1:nrow(B), sep = ""),
                 names.agent = paste("agt", 1:ncol(B), sep = ""),
                 p0 = matrix(1, nrow = nrow(B), ncol = 1),
                 z0 = matrix(100, nrow = ncol(B), ncol = 1),
                 GRExg = NA,
                 pExg = NULL,
                 numeraire = NULL,
                 tolCond = 1e-5,
                 maxIteration = 200,
                 numberOfPeriods = 300,
                 depreciationCoef = 0.8,
                 priceAdjustmentFunction = NULL,
                 priceAdjustmentVelocity = 0.15,
                 trace = TRUE,
                 ts = FALSE,
                 policy = NULL,
                 exchangeFunction = F_Z) {
  ##### definition of the economic transition function xNext
  xNext <- function(xt) {
    p_t <- xt$p
    S_t <- xt$S
    q_t <- xt$q
    z_t <- xt$z
    Y_t <- xt$Y

    # price adjustment
    if (is.null(priceAdjustmentFunction)) {
      p_tp1 <- p_t * (1 - priceAdjustmentVelocity * (1 - q_t))
    } else {
      p_tp1 <- priceAdjustmentFunction(p_t, q_t)
    }

    if (any(!is.na(pExg))) {
      tmpIndex <- which(!is.na(pExg))
      tmpIndex <- tmpIndex[1]
      p_tp1 <- p_tp1 / p_tp1[tmpIndex] * pExg[tmpIndex]
      p_tp1[!is.na(pExg)] <- pExg[!is.na(pExg)]
    }
    else {
      p_tp1 <- prop.table(p_tp1)
    }


    S_tp1 <- Y_t + dg(depreciationCoef * (1 - q_t)) %*% S_t

    if (!all(is.na(S0Exg))) {
      SupplyExogenous <<- SupplyExogenous * (1 + GRExg)
      S_tp1[!is.na(S0Exg)] <- SupplyExogenous[!is.na(S0Exg)]
    } # Set exogenous supply


    if (!is.null(policy)) {
      ## 20200527
      if (is.function(policy)) policy <- list(policy)

      for (kth.policy in policy) {
        kth.policy.arg.names <- names(formals(kth.policy))

        kth.policy.arg <- list()
        if ("time" %in% kth.policy.arg.names) kth.policy.arg$time <- time
        if ("state" %in% kth.policy.arg.names) {
          tmp.p <- as.vector(p_tp1)
          names(tmp.p) <- names.commodity
          kth.policy.arg$state <- list(
            p = tmp.p, S = S_tp1, last.z = z_t, B = B,
            names.commodity = names.commodity,
            names.agent = names.agent
          )
        }
        if (is.list(A) && ("dstl" %in% kth.policy.arg.names)) kth.policy.arg$dstl <- A
        if ("state.history" %in% kth.policy.arg.names) {
          kth.policy.arg$state.history <- list(
            p = t(p),
            S = S,
            q = t(q),
            z = t(z)
          )
        }

        kth.policy.result <- do.call(kth.policy, kth.policy.arg)
        if (is.list(kth.policy.result)) {
          p_tp1 <- as.matrix(kth.policy.result$p)
          S_tp1 <- kth.policy.result$S
          B <<- kth.policy.result$B

          if (!is.null(kth.policy.result$current.policy.data)) {
            policy.data <<- rbind(policy.data, kth.policy.result$current.policy.data)
          }
        }
      }
    }

    switch(class(A)[1],
      "matrix" = {
        A_tp1 <- A
      },
      "function" = {
        A_tp1 <-
          A(list(
            p = p_tp1,
            z = z_t,
            w = t(p_tp1) %*% S_tp1,
            t = time
          ))
      },
      "list" = {
        tmp.p <- c(p_tp1)
        names(tmp.p) <- names.commodity
        A_tp1 <- sapply(A, demand_coefficient, tmp.p)
      }
    )

    tmp <- exchangeFunction(A_tp1, p_tp1, S_tp1)
    q_tp1 <- tmp$q
    z_tp1 <- tmp$z

    if (any(z_tp1 < 0)) {
      if (any(z_tp1[z_tp1 < 0] > -0.01)) {
        z_tp1[z_tp1 < 0] <- 0

        warning("LI: negative_z, z_tp1<0")
      }
      else {
        message(z_tp1)
        stop("Li: negative_z")
      }
    }

    list(
      p = p_tp1,
      S = S_tp1,
      q = q_tp1,
      z = z_tp1,
      Y = B %*% dg(z_tp1)
    )
  } # xNext

  # beginning ---------------------------------------------------------------
  n <- nrow(B)
  m <- ncol(B)

  if (!all(is.na(S0Exg))) SupplyExogenous <- S0Exg

  # check names.commodity
  if (is.list(A)) {
    names.leaf <- lapply(A, function(tree) {
      tree$Get("name", filterFun = isLeaf)
    })
    names.leaf <- unlist(names.leaf)
    if (!all(is.element(names.leaf, names.commodity))) {
      message("Li: The names of some leaf nodes are not included in names.commodity:")
      message(paste(setdiff(names.leaf, names.commodity), sep = ", "))
      stop()
    } else
    if (!setequal(names.leaf, names.commodity)) {
      message("Li: Some names in names.commodity do not have corresponding leaf nodes: ")
      message(paste(setdiff(names.commodity, names.leaf), sep = ", "))
    }
  }

  result <- c()

  if (is.na(GRExg) && !all(is.na(S0Exg))) GRExg <- 0

  if (trace) {
    message(paste("tolCond: ", tolCond))
  }

  p <- matrix(0, n, numberOfPeriods)
  S <- array(0, dim = c(n, m, numberOfPeriods))
  q <- matrix(0, n, numberOfPeriods)
  z <- matrix(0, m, numberOfPeriods)

  if (!is.null(policy)) policy.data <- data.frame()

  if (all(is.na(S0Exg))) {
    S0 <- matrix(0, n, m)
  } else {
    S0 <- S0Exg
    S0[is.na(S0)] <- 0
  }


  time <- 1

  xtp1 <- xNext(list(
    p = p0,
    S = S0,
    q = matrix(1, n, 1),
    z = z0,
    Y = B %*% dg(z0)
  ))

  p[, 1] <- xtp1$p
  S[, , 1] <- xtp1$S
  q[, 1] <- xtp1$q
  z[, 1] <- xtp1$z


  toleranceRec <- matrix(1, maxIteration, 1)

  for (k.iteration in 1:maxIteration) {
    time <- 1
    for (t in 2:numberOfPeriods) {
      time <- time + 1
      xtp1 <- xNext(xtp1)

      p[, t] <- xtp1$p
      S[, , t] <- xtp1$S
      q[, t] <- xtp1$q
      z[, t] <- xtp1$z
    } # for (t in 2:numberOfPeriods)
    ##### the end of an iteration

    if (ts) {
      result$ts.p <- t(p)
      result$ts.z <- t(z)
      result$ts.S <- S
      result$ts.q <- t(q)
    }

    tmp1 <- z[, ncol(z)] / max(z[, ncol(z)])
    tmp2 <- z[, ncol(z) - 1] / max(z[, ncol(z) - 1])
    toleranceZ <- max(abs(tmp1 - tmp2))

    tmpU <- apply(q[, (ncol(q) - 20):ncol(q)], 1, min)
    tmpU <- tmpU[p[, ncol(p)] > tolCond]
    toleranceU <- max(1 - tmpU)
    tolerance <- max(c(toleranceU, toleranceZ))
    toleranceRec[k.iteration] <- tolerance

    p0 <- p[, ncol(p)]
    z0 <- z[, ncol(z)]
    S0 <- S[, , dim(S)[3]]
    dim(S0) <- c(n, m)

    if (!all(is.na(S0Exg))) {
      # There are exogenous supplies.
      if (GRExg != 0) {
        z0 <- z0 / (1 + GRExg)^numberOfPeriods
        S0 <- S0 / (1 + GRExg)^numberOfPeriods
        SupplyExogenous <<- S0Exg
      }
    }
    else {
      S0 <- S0 / max(z0)
      z0 <- z0 / max(z0)
    }

    if (trace) {
      message(paste("Iteration number ", k.iteration, ": tolerance coefficient ", tolerance))
    }

    if (tolerance < tolCond) {
      break
    }

    if (k.iteration < maxIteration) {
      xtp1 <- xNext(list(
        p = p0,
        S = S0,
        q = matrix(1, n, 1),
        z = z0,
        Y = B %*% dg(z0)
      ))
      p[, 1] <- xtp1$p
      S[, , 1] <- xtp1$S
      q[, 1] <- xtp1$q
      z[, 1] <- xtp1$z
    }
  } # for (k.iteration in 1:maxIteration)


  # result ------------------------------------------------------------------
  result$tolerance <- tolerance
  result$p <- p0
  result$z <- z0
  result$S <- S0

  if (all(is.na(S0Exg))) {
    result$growthRate <- max(z[, ncol(z)]) / max(z[, ncol(z) - 1]) - 1
  }

  switch(mode(A),
    "numeric" = {
      result$A <- A
    },
    "function" = {
      tmpS <- S0Exg
      tmpS[is.na(tmpS)] <- 0
      result$A <-
        A(list(
          p = result$p,
          z = result$z,
          w = result$p %*% tmpS,
          t = 1
        ))
    },
    "list" = {
      tmp.p <- c(result$p)
      names(tmp.p) <- names.commodity
      result$A <- sapply(A, demand_coefficient, tmp.p)
    }
  )

  result$B <- B
  result$S0Exg <- S0Exg

  if (!is.null(policy) && length(policy.data) != 0) {
    result$policy.data <- policy.data
  }

  if (!is.null(numeraire)) {
    if (!is.null(names(numeraire))) {
      numeraire.index <- which(names.commodity == names(numeraire))
      numeraire.price <- numeraire
    } else if (is.character(numeraire)) {
      numeraire.index <- which(names.commodity == numeraire)
      numeraire.price <- 1
    } else {
      numeraire.index <- numeraire
      numeraire.price <- 1
    }

    result$p <- result$p / result$p[numeraire.index] * numeraire.price
    if (ts) {
      result$ts.p <- apply(result$ts.p, 2, function(x) x / result$ts.p[, numeraire.index] * numeraire.price)
    }
  }

  result <- ge_tidy(result, names.commodity, names.agent)

  return(result)
}
