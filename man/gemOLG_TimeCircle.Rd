% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gemOLG_TimeCircle.R
\name{gemOLG_TimeCircle}
\alias{gemOLG_TimeCircle}
\title{Time-Circle Models (Closed Loop Overlapping Generations Models)}
\usage{
gemOLG_TimeCircle(...)
}
\arguments{
\item{...}{arguments to be passed to the function sdm2.}
}
\description{
Some examples of time-circle models, which usually contain overlapping generations.
When we connect together the head and tail of time of a dynamic model, we get a time-circle model which
treats time as a circle of finite length instead of a straight line of infinite length.
The (discounted) output of the final period will enter the utility function and the production function
of the first period, which implies the influence mechanism of the past on the present is
just like the influence mechanism of the present on the future.
A time-circle OLG model may be called a reincarnation model.

In a time-circle model, time can be thought of as having no beginning and no end.
And we can assume all souls meet in a single market (see Shell, 1971),
or there are implicit financial instruments that facilitate transactions between adjacent generations.

As in the Arrow–Debreu approach, in the following examples with production, we assume that firms operate independently and are not owned by consumers.
That is, there is no explicit property of firms as the firms do not make pure profit at equilibrium (constant return to scale)
(see de la Croix and Michel, 2002, page 292).
}
\examples{
\donttest{
#### a time-circle pure exchange economy with two-period-lived consumers.
# In this example, each agent sells some payoff to the previous
# generation and buys some payoff from the next generation.

# Here np can tend to infinity.
np <- 4 # the number of economic periods, commodity kinds and generations

names.commodity <- c(paste0("payoff", 1:np))
names.agent <- paste0("gen", 1:np)

index.comm <- c(1:np, 1)
payoff <- c(100, 0) # the payoffs of lifetime
# the exogenous supply matrix.
S0Exg <- matrix(0, np, np, dimnames = list(names.commodity, names.agent))
for (k in 1:np) {
  S0Exg[index.comm[k:(k + 1)], k] <- payoff
}

# Suppose each consumer has a utility function log(c1) + log(c2).
beta.consumer <- c(0.5, 0.5)
index.comm <- c(1:np, 1)
dstl.consumer <- list()
for (k in 1:np) {
  dstl.consumer[[k]] <- node_new(
    "util",
    type = "CD", alpha = 1,
    beta = beta.consumer,
    paste0("payoff", index.comm[k]), paste0("payoff", index.comm[k + 1])
  )
}

ge <- sdm2(
  A = dstl.consumer,
  B = matrix(0, np, np),
  S0Exg = S0Exg,
  names.commodity = names.commodity,
  names.agent = names.agent,
  numeraire = "payoff1"
)

ge$p
ge$D
ge$S

## Introduce population growth into the above pure exchange economy.
GRExg <- 0.03 # the population growth rate

S0Exg <- S0Exg \%*\% diag((1 + GRExg)^(0:(np - 1)))
S0Exg[1, np] <- S0Exg[1, np] * (1 + GRExg)^-np

dstl.consumer[[np]] <- node_new(
  "util",
  type = "CD", alpha = 1,
  beta = beta.consumer,
  paste0("payoff", np), "cc1"
)
node_set(dstl.consumer[[np]],
  "cc1",
  type = "Leontief", a = (1 + GRExg)^(-np), # a discounting factor
  "payoff1"
)

ge2 <- sdm2(
  A = dstl.consumer,
  B = matrix(0, np, np),
  S0Exg = S0Exg,
  names.commodity = names.commodity,
  names.agent = names.agent,
  numeraire = "payoff1"
)

ge2$p
ge2$D
ge2$DV
ge2$S

#### a time-circle pure exchange economy with three-period-lived consumers
# Suppose each consumer has a utility function log(c1) + log(c2) + log(c3).
# See gemOLG_PureExchange.
np <- 5 # the number of economic periods, commodity kinds and generations

names.commodity <- c(paste0("payoff", 1:np))
names.agent <- paste0("gen", 1:np)

index.comm <- c(1:np, 1:2)
payoff <- c(50, 50, 0) # the payoffs of lifetime
# the exogenous supply matrix.
S0Exg <- matrix(0, np, np, dimnames = list(names.commodity, names.agent))
for (k in 1:np) {
  S0Exg[index.comm[k:(k + 2)], k] <- payoff
}

dstl.consumer <- list()
for (k in 1:np) {
  dstl.consumer[[k]] <- node_new(
    "util",
    type = "CD", alpha = 1,
    beta = rep(1 / 3, 3),
    paste0("payoff", index.comm[k:(k + 2)])
  )
}

ge <- sdm2(
  A = dstl.consumer,
  B = matrix(0, np, np),
  S0Exg = S0Exg,
  names.commodity = names.commodity,
  names.agent = names.agent,
  numeraire = "payoff1"
)

ge$p
ge$D

## Introduce population growth into the above pure exchange economy.
GRExg <- 0.03 # the population growth rate
df <- (1 + GRExg)^-np # a discounting factor

# the exogenous supply matrix.
S0Exg <- matrix(0, np, np, dimnames = list(names.commodity, names.agent))
for (k in 1:np) {
  S0Exg[paste0("payoff", index.comm[k:(k + 2)]), paste0("gen", k)] <-
    payoff * (1 + GRExg)^(k - 1)
}
S0Exg[paste0("payoff", 1:2), paste0("gen", np)] <-
  S0Exg[paste0("payoff", 1:2), paste0("gen", np)] * df
S0Exg[paste0("payoff", 1), paste0("gen", np - 1)] <-
  S0Exg[paste0("payoff", 1), paste0("gen", np - 1)] * df

dstl.consumer[[np - 1]] <- node_new(
  "util",
  type = "CD", alpha = 1,
  beta = rep(1 / 3, 3),
  paste0("payoff", (np - 1):(np + 1))
)
node_set(dstl.consumer[[np - 1]], paste0("payoff", np + 1),
  type = "Leontief", a = df,
  "payoff1"
)

dstl.consumer[[np]] <- node_new(
  "util",
  type = "CD", alpha = 1,
  beta = rep(1 / 3, 3),
  paste0("payoff", np:(np + 2))
)
node_set(dstl.consumer[[np]], paste0("payoff", np + 1),
  type = "Leontief", a = df,
  "payoff1"
)
node_set(dstl.consumer[[np]], paste0("payoff", np + 2),
  type = "Leontief", a = df,
  "payoff2"
)

ge2 <- sdm2(
  A = dstl.consumer,
  B = matrix(0, np, np),
  S0Exg = S0Exg,
  names.commodity = names.commodity,
  names.agent = names.agent,
  numeraire = "payoff1"
)

ge2$p
growth_rate(ge2$p) + 1 # 1 / (1 + GRExg)
ge2$D
ge2$DV

#### a time-circle model with production and two-period-lived consumers
# Suppose each consumer has a utility function log(c1) + log(c2).
np <- 5 # the number of economic periods, consumers and firms.
names.commodity <- c(paste0("prod", 1:np), paste0("lab", 1:np))
names.agent <- c(paste0("firm", 1:np), paste0("consumer", 1:np))

index.comm <- c(1:np, 1)
labor.supply <- c(100, 0) # the labor supply of lifetime
# the exogenous supply matrix.
S0Exg <- matrix(NA, 2 * np, 2 * np, dimnames = list(names.commodity, names.agent))
for (k in 1:np) {
  S0Exg[paste0("lab", index.comm[k:(k + 1)]), paste0("consumer", k)] <- labor.supply
}

B <- matrix(0, 2 * np, 2 * np, dimnames = list(names.commodity, names.agent))
for (k in 1:np) {
  B[paste0("prod", index.comm[k + 1]), paste0("firm", k)] <- 1
}

dstl.firm <- list()
for (k in 1:np) {
  dstl.firm[[k]] <- node_new(
    "prod",
    type = "CD", alpha = 5,
    beta = c(1 / 3, 2 / 3),
    paste0("lab", k), paste0("prod", k)
  )
}

dstl.consumer <- list()
for (k in 1:np) {
  dstl.consumer[[k]] <- node_new(
    "util",
    type = "CD", alpha = 1,
    beta = c(1 / 2, 1 / 2),
    paste0("prod", index.comm[k:(k + 1)])
  )
}

ge <- sdm2(
  A = c(dstl.firm, dstl.consumer),
  B = B,
  S0Exg = S0Exg,
  names.commodity = names.commodity,
  names.agent = names.agent,
  numeraire = "lab1"
)

ge$p
ge$D

## Introduce population growth into the above economy.
GRExg <- 0.03 # the population growth rate
df <- (1 + GRExg)^-np # a discounting factor

for (k in 1:np) {
  S0Exg[paste0("lab", index.comm[k:(k + 1)]), paste0("consumer", k)] <-
    labor.supply * (1 + GRExg)^(k - 1)
}

B[1, np] <- df

dstl.consumer[[np]] <- node_new(
  "util",
  type = "CD", alpha = 1,
  beta = c(1 / 2, 1 / 2),
  paste0("prod", np), "cc1"
)
node_set(dstl.consumer[[np]], "cc1",
  type = "Leontief", a = df,
  "prod1"
)

ge2 <- sdm2(
  A = c(dstl.firm, dstl.consumer),
  B = B,
  S0Exg = S0Exg,
  names.commodity = names.commodity,
  names.agent = names.agent,
  numeraire = "lab1",
  policy = makePolicyMeanValue(30),
  maxIteration = 1,
  numberOfPeriods = 600,
  ts = TRUE
)

ge2$p
growth_rate(ge2$p[1:np]) + 1 # 1 / (1 + GRExg)
growth_rate(ge2$p[(np + 1):(2 * np)]) + 1 # 1 / (1 + GRExg)
ge2$D
ge2$DV

#### a time-circle model with production and one-period-lived consumers.
# These consumers also can be regarded as infinite-lived agents maximizing
# their per period utility subject to their disposable income per period.
np <- 5 # the number of economic periods, consumers and firms.
GRExg <- 0.03 # the population growth rate
# df <- (1 + GRExg)^-np # a discounting factor
df <- 0.5

names.commodity <- c(paste0("prod", 1:np), paste0("lab", 1:np))
names.agent <- c(paste0("firm", 1:np), paste0("consumer", 1:np))

# the exogenous supply matrix.
S0Exg <- matrix(NA, 2 * np, 2 * np, dimnames = list(names.commodity, names.agent))
for (k in 1:np) {
  S0Exg[paste0("lab", k), paste0("consumer", k)] <- 100 * (1 + GRExg)^(k - 1)
}

B <- matrix(0, 2 * np, 2 * np, dimnames = list(names.commodity, names.agent))
for (k in 1:(np - 1)) {
  B[paste0("prod", index.comm[k + 1]), paste0("firm", k)] <- 1
}
B["prod1", paste0("firm", np)] <- df

beta.firm <- c(1 / 3, 2 / 3)
beta.consumer <- c(1 / 2, 1 / 2)

dstl.firm <- list()
for (k in 1:np) {
  dstl.firm[[k]] <- node_new(
    "prod",
    type = "CD", alpha = 5,
    beta = beta.firm,
    paste0("lab", k), paste0("prod", k)
  )
}

dstl.consumer <- list()
for (k in 1:np) {
  dstl.consumer[[k]] <- node_new(
    "util",
    type = "CD", alpha = 1,
    beta = beta.consumer,
    paste0("lab", k), paste0("prod", k)
  )
}

ge <- sdm2(
  A = c(dstl.firm, dstl.consumer),
  B = B,
  S0Exg = S0Exg,
  names.commodity = names.commodity,
  names.agent = names.agent,
  numeraire = "lab1",
  policy = makePolicyMeanValue(30),
  maxIteration = 1,
  numberOfPeriods = 600,
  ts = TRUE
)

ge$p
growth_rate(ge$p[1:np]) + 1 # 1 / (1 + GRExg)
growth_rate(ge$p[(np + 1):(2 * np)]) + 1 # 1 / (1 + GRExg)
ge$D

## the sequential form of the above model.
dst.firm <- node_new(
  "prod",
  type = "CD", alpha = 5,
  beta = beta.firm,
  "lab", "prod"
)

dst.consumer <- node_new(
  "util",
  type = "CD", alpha = 1,
  beta = beta.consumer,
  "lab", "prod"
)

ge2 <- sdm2(
  A = list(
    dst.firm,
    dst.consumer
  ),
  B = matrix(c(
    1, 0,
    0, 0
  ), 2, 2, TRUE),
  S0Exg = matrix(c(
    NA, NA,
    NA, 100 / (1 + GRExg)
  ), 2, 2, TRUE),
  names.commodity = c("prod", "lab"),
  names.agent = c("firm", "consumer"),
  numeraire = "lab",
  z0 = c(ge$S[1, np], 0),
  GRExg = GRExg,
  policy = policyMarketClearingPrice,
  maxIteration = 1,
  numberOfPeriods = 20,
  ts = TRUE
)

ge2$p
ge2$D
ge2$ts.z[, 1]
ge$z

#### a time-circle OLG model with production and three-period-lived consumers.
np <- 6 # the number of economic periods, consumers and firms
gr.laborer <- 0.03 # the population growth rate
df <- (1 + gr.laborer)^-np # a discounting factor
alpha.firm <- 2 # the efficient parameter of firms
beta.prod.firm <- 0.4 # the product (i.e. capital) share parameter of firms
beta.consumer <- c(0, 0.8, 0.2) # the share parameter of consumers
labor.supply <- c(100, 0, 0) # the labor supply of lifetime

f <- function() {
  names.commodity <- c(paste0("prod", 1:np), paste0("lab", 1:np))
  names.agent <- c(paste0("firm", 1:np), paste0("consumer", 1:np))

  index.comm <- c(1:np, 1:2)

  # the exogenous supply matrix.
  S0Exg <- matrix(NA, 2 * np, 2 * np, dimnames = list(names.commodity, names.agent))
  for (k in 1:np) {
    S0Exg[paste0("lab", index.comm[k:(k + 2)]), paste0("consumer", k)] <-
      labor.supply * (1 + gr.laborer)^(k - 1)
  }
  S0Exg[paste0("lab", 1:2), paste0("consumer", np)] <-
    S0Exg[paste0("lab", 1:2), paste0("consumer", np)] * df
  S0Exg[paste0("lab", 1), paste0("consumer", np - 1)] <-
    S0Exg[paste0("lab", 1), paste0("consumer", np - 1)] * df

  B <- matrix(0, 2 * np, 2 * np, dimnames = list(names.commodity, names.agent))
  for (k in 1:np) {
    B[paste0("prod", index.comm[k + 1]), paste0("firm", k)] <- 1
  }
  B["prod1", paste0("firm", np)] <- df


  dstl.firm <- list()
  for (k in 1:np) {
    dstl.firm[[k]] <- node_new(
      "prod",
      type = "CD", alpha = alpha.firm,
      beta = c(beta.prod.firm, 1 - beta.prod.firm),
      paste0("prod", k), paste0("lab", k)
    )
  }

  dstl.consumer <- list()
  for (k in 1:np) {
    dstl.consumer[[k]] <- node_new(
      "util",
      type = "CD", alpha = 1,
      beta = beta.consumer,
      paste0("prod", k:(k + 2))
    )
  }

  node_set(dstl.consumer[[np - 1]], paste0("prod", np + 1),
           type = "Leontief", a = df,
           "prod1"
  )

  node_set(dstl.consumer[[np]], paste0("prod", np + 1),
           type = "Leontief", a = df,
           "prod1"
  )
  node_set(dstl.consumer[[np]], paste0("prod", np + 2),
           type = "Leontief", a = df,
           "prod2"
  )

  ge <- sdm2(
    A = c(dstl.firm, dstl.consumer),
    B = B,
    S0Exg = S0Exg,
    names.commodity = names.commodity,
    names.agent = names.agent,
    numeraire = "lab1"
  )
  invisible(ge)
}

ge <- f()
growth_rate(ge$p[1:np]) + 1 # 1 / (1 + gr.laborer)
growth_rate(ge$p[(np + 1):(2 * np)]) + 1 # 1 / (1 + gr.laborer)
ge$D
ge$DV

##
beta.consumer <- c(1 / 3, 1 / 3, 1 / 3) # the share parameter of consumers
labor.supply <- c(50, 50, 0) # the labor supply of lifetime
ge <- f()
ge$D
ge$DV

##
gr.laborer <- 0
df <- (1 + gr.laborer)^-np
beta.prod.firm <- 0.5
ge <- f()
ge$z
}
}
\references{
de la Croix, David and Philippe Michel (2002, ISBN: 9780521001151) A Theory of Economic Growth: Dynamics and Policy in Overlapping Generations. Cambridge University Press.

Diamond, Peter (1965) National Debt in a Neoclassical Growth Model. American Economic Review. 55 (5): 1126-1150.

Samuelson, P. A. (1958) An Exact Consumption-Loan Model of Interest with or without the Social Contrivance of Money. Journal of Political Economy, vol. 66(6): 467-482.

Shell, Karl (1971) Notes on the Economics of Infinity. Journal of Political Economy. 79 (5): 1002–1011.
}
\seealso{
{
\code{\link{gemOLG_PureExchange}}
}
}
