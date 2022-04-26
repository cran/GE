#' @export
#' @title General Equilibrium Models based on a 5x5 Input-Output Table (see Zhang Xin, 2017, Table 3.2.1)
#' @aliases gemInputOutputTable_5_5
#' @description Some general equilibrium models based on a 5x5 input-output table (see Zhang Xin, 2017, Table 3.2.1).
#' @param ... arguments to be passed to the function sdm2.
#' @references Zhang Xin (2017, ISBN: 9787543227637) Principles of Computable General Equilibrium Modeling and Programming (Second Edition). Shanghai: Gezhi Press. (In Chinese)
#' @examples
#' \donttest{
#' names.commodity <- c("agri", "manu", "serv", "lab", "cap")
#' names.agent <- c("agri", "manu", "serv", "consumer", "investor")
#'
#' IT <- matrix(c(
#'   200, 300, 150, 280, 70,
#'   80, 400, 250, 550, 320,
#'   30, 420, 240, 350, 110,
#'   500, 250, 330, 0, 0,
#'   190, 230, 180, 0, 0
#' ), 5, 5, TRUE)
#'
#' OT <- matrix(c(
#'   1000, 0, 0, 0, 0,
#'   0, 1600, 0, 0, 0,
#'   0, 0, 1150, 0, 0,
#'   0, 0, 0, 758.5714, 321.4286,
#'   0, 0, 0, 421.4286, 178.5714
#' ), 5, 5, TRUE)
#'
#' dimnames(IT) <- dimnames(OT) <- list(names.commodity, names.agent)
#'
#' addmargins(IT)
#' addmargins(OT)
#'
#' #### a model with non-nested production functions (demand structure trees)
#' dst.agri <- node_new("output",
#'   type = "SCES", es = 1, alpha = 1,
#'   beta = prop.table(c(200, 80, 30, 500, 190)),
#'   "agri", "manu", "serv", "lab", "cap"
#' )
#'
#' dst.manu <- node_new("output",
#'   type = "SCES", es = 1, alpha = 1,
#'   beta = prop.table(c(300, 400, 420, 250, 230)),
#'   "agri", "manu", "serv", "lab", "cap"
#' )
#'
#' dst.serv <- node_new("output",
#'   type = "SCES", es = 1, alpha = 1,
#'   beta = prop.table(c(150, 250, 240, 330, 180)),
#'   "agri", "manu", "serv", "lab", "cap"
#' )
#'
#' dst.consumer <- node_new("util",
#'   type = "SCES", es = 0.5, alpha = 1,
#'   beta = prop.table(c(280, 550, 350)),
#'   "agri", "manu", "serv"
#' )
#'
#' dst.investor <- node_new("util",
#'   type = "SCES", es = 0.5, alpha = 1,
#'   beta = prop.table(c(70, 320, 110)),
#'   "agri", "manu", "serv"
#' )
#'
#' ge1.benchmark <- sdm2(list(dst.agri, dst.manu, dst.serv, dst.consumer, dst.investor),
#'   B = matrix(c(
#'     1, 0, 0, 0, 0,
#'     0, 1, 0, 0, 0,
#'     0, 0, 1, 0, 0,
#'     0, 0, 0, 0, 0,
#'     0, 0, 0, 0, 0
#'   ), 5, 5, TRUE),
#'   S0Exg = {
#'     S0Exg <- matrix(NA, 5, 5)
#'     S0Exg[4:5, 4] <- c(1080, 600) * (1180 / (1180 + 500))
#'     S0Exg[4:5, 5] <- c(1080, 600) * (500 / (1180 + 500))
#'     S0Exg
#'   },
#'   names.commodity = c("agri", "manu", "serv", "lab", "cap"),
#'   names.agent = c("agri", "manu", "serv", "consumer", "investor"),
#'   numeraire = c("lab")
#' )
#'
#' addmargins(ge1.benchmark$D)
#' addmargins(ge1.benchmark$S)
#'
#' #### a model with nested production functions (demand structure trees)
#' dst.agri <- node_new("output",
#'   type = "SCES", es = 0, alpha = 1,
#'   beta = prop.table(c(200 + 80 + 30, 500 + 190)),
#'   "cc.II", "cc.VA"
#' )
#' node_set(dst.agri, "cc.II",
#'   type = "SCES", es = 0, alpha = 1,
#'   beta = prop.table(c(200, 80, 30)),
#'   "agri", "manu", "serv"
#' )
#' node_set(dst.agri, "cc.VA",
#'   type = "SCES", es = 0.5, alpha = 1,
#'   beta = prop.table(c(500, 190)),
#'   "lab", "cap"
#' )
#'
#' dst.manu <- node_new("output",
#'   type = "SCES", es = 0, alpha = 1,
#'   beta = prop.table(c(300 + 400 + 420, 250 + 230)),
#'   "cc.II", "cc.VA"
#' )
#' node_set(dst.manu, "cc.II",
#'   type = "SCES", es = 0, alpha = 1,
#'   beta = prop.table(c(300, 400, 420)),
#'   "agri", "manu", "serv"
#' )
#' node_set(dst.manu, "cc.VA",
#'   type = "SCES", es = 0.5, alpha = 1,
#'   beta = prop.table(c(250, 230)),
#'   "lab", "cap"
#' )
#'
#' dst.serv <- node_new("output",
#'   type = "SCES", es = 0, alpha = 1,
#'   beta = prop.table(c(150 + 250 + 240, 330 + 180)),
#'   "cc.II", "cc.VA"
#' )
#' node_set(dst.serv, "cc.II",
#'   type = "SCES", es = 0, alpha = 1,
#'   beta = prop.table(c(150, 250, 240)),
#'   "agri", "manu", "serv"
#' )
#' node_set(dst.serv, "cc.VA",
#'   type = "SCES", es = 0.5, alpha = 1,
#'   beta = prop.table(c(330, 180)),
#'   "lab", "cap"
#' )
#'
#' dst.consumer <- node_new("util",
#'   type = "SCES", es = 0.5, alpha = 1,
#'   beta = prop.table(c(280, 550, 350)),
#'   "agri", "manu", "serv"
#' )
#'
#' dst.investor <- node_new("util",
#'   type = "SCES", es = 0.5, alpha = 1,
#'   beta = prop.table(c(70, 320, 110)),
#'   "agri", "manu", "serv"
#' )
#'
#' ge2.benchmark <- sdm2(list(dst.agri, dst.manu, dst.serv, dst.consumer, dst.investor),
#'   B = matrix(c(
#'     1, 0, 0, 0, 0,
#'     0, 1, 0, 0, 0,
#'     0, 0, 1, 0, 0,
#'     0, 0, 0, 0, 0,
#'     0, 0, 0, 0, 0
#'   ), 5, 5, TRUE),
#'   S0Exg = {
#'     S0Exg <- matrix(NA, 5, 5)
#'     S0Exg[4:5, 4] <- c(1080, 600) * (1180 / (1180 + 500))
#'     S0Exg[4:5, 5] <- c(1080, 600) * (500 / (1180 + 500))
#'     S0Exg
#'   },
#'   names.commodity = c("agri", "manu", "serv", "lab", "cap"),
#'   names.agent = c("agri", "manu", "serv", "consumer", "investor"),
#'   numeraire = c("lab")
#' )
#'
#' addmargins(ge2.benchmark$D)
#' addmargins(ge2.benchmark$S)
#' }

gemInputOutputTable_5_5 <- function(...) sdm2(...)
