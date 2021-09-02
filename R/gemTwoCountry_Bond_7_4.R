#' @export
#' @title An Example of Two-Country Economy with Bond
#' @aliases gemTwoCountry_Bond_7_4
#' @description A general equilibrium example of two-country economy with bond.
#' @param ... arguments to be passed to the function sdm2.
#' @return  A general equilibrium.
#' @seealso \cite{\link{gemTwoCountry_Tariff_9_5}}
#' @examples
#' \donttest{
#' es.DFProd <- 0.8 # substitution elasticity between domestic and foreign products
#' es.CL <- 0.8 # substitution elasticity between capital and labor
#'
#' dst.firm.CHN <- node_new("output",
#'   type = "SCES", alpha = 1, beta = c(0.78, 0.22), es = es.CL,
#'   "lab.CHN", "cap.CHN"
#' )
#'
#' dst.household.CHN <- node_new("util",
#'   type = "FIN", rate = c(1, 0.028),
#'   "cc1", "bond.ROW"
#' ) # 0.1 is the amount of foreign investment corresponding to
#' # each unit of cc1 (i.e. composite commodity 1).
#'
#' node_set(dst.household.CHN, "cc1",
#'   type = "SCES", alpha = 1, beta = c(0.93, 0.07), es = es.DFProd,
#'   "prod.CHN", "prod.ROW"
#' )
#'
#' node_plot(dst.household.CHN)
#'
#' dst.firm.ROW <- node_new("output",
#'   type = "SCES", alpha = 1, beta = c(0.75, 0.25), es = es.CL,
#'   "lab.ROW", "cap.ROW"
#' )
#'
#' dst.household.ROW <- node_new("util",
#'   type = "SCES", alpha = 1, beta = c(0.02, 0.98), es = es.DFProd,
#'   "prod.CHN", "prod.ROW"
#' )
#'
#' dstl <- list(dst.firm.CHN, dst.household.CHN, dst.firm.ROW, dst.household.ROW)
#'
#' ge <- gemTwoCountry_Bond_7_4(dstl,
#'   names.commodity = c(
#'     "prod.CHN", "lab.CHN", "cap.CHN",
#'     "prod.ROW", "lab.ROW", "cap.ROW", "bond.ROW"
#'   ),
#'   names.agent = c(
#'     "firm.CHN", "household.CHN",
#'     "firm.ROW", "household.ROW"
#'   ),
#'   B = {
#'     tmp <- matrix(0, 7, 4, TRUE)
#'     tmp[1, 1] <- tmp[4, 3] <- 1
#'     tmp
#'   },
#'   S0Exg = {
#'     tmp <- matrix(NA, 7, 4, TRUE)
#'     tmp[2, 2] <- 53 # the supply of lab.CHN
#'     tmp[3, 2] <- 15 # the supply of cap.CHN
#'     tmp[5, 4] <- 240 # the supply of lab.ROW
#'     tmp[6, 4] <- 77 # the supply of cap.ROW
#'     tmp[7, 4] <- 2 # the supply of bond.ROW
#'     tmp
#'   },
#'   numeraire = "lab.CHN"
#' )
#'
#' ge$p
#' ge$z
#'
#' # Determine the parameters in the
#' # example based on an input-output table.
#' IT <- matrix(c(
#'   0, 61.44, 0, 6.498,
#'   53, 0, 0, 0,
#'   14.94, 0, 0, 0,
#'   0, 4.647, 0, 320,
#'   0, 0, 242.9, 0,
#'   0, 0, 81.74, 0,
#'   0, 1.85, 0, 0
#' ), 7, 4, TRUE)
#'
#' OT <- matrix(c(
#'   67.94, 0, 0, 0,
#'   0, 53, 0, 0,
#'   0, 14.94, 0, 0,
#'   0, 0, 324.64, 0,
#'   0, 0, 0, 242.9,
#'   0, 0, 0, 81.74,
#'   0, 0, 0, 1.85
#' ), 7, 4, TRUE)
#'
#' dimnames(IT) <- dimnames(OT) <- list(
#'   c("prod.CHN", "lab.CHN", "cap.CHN", "prod.ROW", "lab.ROW", "cap.ROW", "bond.ROW"),
#'   c("firm.CHN", "household.CHN", "firm.ROW", "household.ROW")
#' )
#'
#' es.DFProd <- 0.8 # substitution elasticity between domestic and foreign products
#' es.CL <- 0.8 # substitution elasticity between capital and labor
#'
#' dst.firm.CHN <- node_new("output",
#'                          type = "SCES",
#'                          alpha = OT["prod.CHN", "firm.CHN"] /
#'                            sum(IT[c("lab.CHN", "cap.CHN"), "firm.CHN"]),
#'                          beta = prop.table(IT[c("lab.CHN", "cap.CHN"), "firm.CHN"]),
#'                          es = es.CL,
#'                          "lab.CHN", "cap.CHN"
#' )
#'
#' # the amount of foreign investment corresponding to
#' # each unit of composite commodity 1 used by household.
#' foreign.investment.rate <- IT["bond.ROW", "household.CHN"] /
#'   sum(IT[c("prod.CHN", "prod.ROW"), "household.CHN"])
#'
#' dst.household.CHN <- node_new("util",
#'                              type = "FIN",
#'                              rate = c(1, foreign.investment.rate),
#'                              "cc1", "bond.ROW"
#' )
#'
#' node_set(dst.household.CHN, "cc1",
#'          type = "SCES", alpha = 1,
#'          beta = prop.table(IT[c("prod.CHN", "prod.ROW"), "household.CHN"]),
#'          es = es.DFProd,
#'          "prod.CHN", "prod.ROW"
#' )
#'
#' dst.firm.ROW <- node_new("output",
#'                          type = "SCES", alpha = 1,
#'                          beta = prop.table(IT[c("lab.ROW", "cap.ROW"), "firm.ROW"]),
#'                          es = es.CL,
#'                          "lab.ROW", "cap.ROW"
#' )
#'
#' dst.household.ROW <- node_new("util",
#'                              type = "SCES", alpha = 1,
#'                              beta = prop.table(IT[c("prod.CHN", "prod.ROW"), "household.ROW"]),
#'                              es = es.DFProd,
#'                              "prod.CHN", "prod.ROW"
#' )
#'
#' dstl <- list(dst.firm.CHN, dst.household.CHN, dst.firm.ROW, dst.household.ROW)
#'
#' ge <- gemTwoCountry_Bond_7_4(dstl,
#'                              names.commodity = c(
#'                                "prod.CHN", "lab.CHN", "cap.CHN",
#'                                "prod.ROW", "lab.ROW", "cap.ROW", "bond.ROW"
#'                              ),
#'                              names.agent = c(
#'                                "firm.CHN", "household.CHN",
#'                                "firm.ROW", "household.ROW"
#'                              ),
#'                              B = {
#'                                tmp <- matrix(0, 7, 4, TRUE)
#'                                tmp[1, 1] <- tmp[4, 3] <- 1
#'                                tmp
#'                              },
#'                              S0Exg = {
#'                                tmp <- matrix(NA, 7, 4, TRUE)
#'                                tmp[2, 2] <- OT["lab.CHN", "household.CHN"]
#'                                tmp[3, 2] <- OT["cap.CHN", "household.CHN"]
#'                                tmp[5, 4] <- OT["lab.ROW", "household.ROW"]
#'                                tmp[6, 4] <- OT["cap.ROW", "household.ROW"]
#'                                tmp[7, 4] <- OT["bond.ROW", "household.ROW"]
#'                                tmp
#'                              },
#'                              numeraire = "lab.CHN"
#' )
#'
#' ge$p
#' ge$z
#' }

gemTwoCountry_Bond_7_4 <- function(...) sdm2(...)
