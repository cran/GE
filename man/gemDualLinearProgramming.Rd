% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gemDualLinearProgramming.R
\name{gemDualLinearProgramming}
\alias{gemDualLinearProgramming}
\title{General Equilibrium Models and Linear Programming Problems (see Winston, 2003)}
\usage{
gemDualLinearProgramming(...)
}
\arguments{
\item{...}{arguments to be passed to the function CGE::sdm.}
}
\value{
A general equilibrium.
}
\description{
Some examples illustrating the relationship between general equilibrium problems and (dual) linear programming problems.
Some linear programming problems can be transformed into general equilibrium problems and vice versa.
}
\details{
These examples are similar and let us explain briefly the first example (Winston, 2003).\cr
The Dakota Furniture Company manufactures desks, tables, and chairs.
The manufacture of each type of furniture requires lumber and two types of skilled labor: finishing and carpentry.
The amount of each resource needed to make each type of furniture is as follows: \cr
  desk: (8, 4, 2)\cr
  table: (6, 2, 1.5)\cr
  chair: (1, 1.5, 0.5)\cr
Currently, 48 board feet of lumber, 20 finishing hours, and 8 carpentry hours are available.
A desk sells for $60, a table for $30, and a chair for $20.
Because the available resources have already been purchased, Dakota wants to maximize total revenue.
This problem can be solved by the linear programming method.\cr
Now let us regard the problem above as a general equilibrium problem.
The Dakota Furniture Company can be regarded as a consumer who obtains 1 unit of utility from 1 dollar and owns lumber and two types of skilled labor.
There are four commodities (i.e. dollar, lumber and two types of skilled labor) and four agents (i.e. a desk producer, a table producer, a chair producer and the consumer Dakota) in this problem.
We need to compute the equilibrium activity levels and the equilibrium prices,
which are also the solutions of the (dual) linear programming problems (i.e. the utility-maximizing problem of the consumer and the cost-minimizing problem of the producers).
}
\note{
Below is a simplified form of the von Neumann general equilibrium model (von Neumann, 1945; Kemeny, Morgenstern, Thompson, 1956):
\deqn{\mathbf p^{T}\mathbf{A}  \geq \rho \mathbf p^{T}\mathbf{B} }
\deqn{\mathbf{Az} \leq \rho \mathbf  {Bz}}
The above model can be extended to the following general equilibrium model, namely the structural equilibrium model (Li, 2019, section 3.4):
\deqn{\mathbf p^{T}\mathbf{A(p,u,z)}  \geq \rho \mathbf p^{T}\mathbf{B(p,u,z)} }
\deqn{\mathbf{A(p,u,z)z} \leq \rho \mathbf  {B(p,u,z)z} }

We explain the structural equilibrium model as follows:\cr
(i) The vectors \eqn{\mathbf p} and \eqn{\mathbf z} reflect the prices of various commodities and the activity levels of various economic agents, respectively.\cr
(ii) The vector \eqn{\mathbf u} reflects the utility levels of various consumers.
In this model, the matrices \eqn{\mathbf A} and  \eqn{\mathbf B} are functions of prices, utilities, and activity levels.\cr
(iii) When describing a static general equilibrium and a steady-state equilibrium without intertemporal decisions,
the structural equilibrium model usually does not explicitly include time, while when describing an intertemporal general equilibrium,
variables such as prices and activity levels explicitly include time, that is, they are labeled with time.\cr
(iv) In a time-independent model, \eqn{\rho} is the discount factor \eqn{\frac{1}{1+\gamma}} corresponding to the steady-state growth rate \eqn{\gamma}.
In a time-dependent model, \eqn{\rho} is usually equal to 1.\cr
(v) The unit demand matrix \eqn{\mathbf{A(p,u,z)}}, the unit supply matrix \eqn{\mathbf {B(p,u,z)}} and the activity level vector \eqn{\mathbf z} in the structural equilibrium model are different from the input coefficient matrix A, the output coefficient matrix B and the purchase level vector z in the structural dynamic model.
The input coefficient matrix A is equivalent to the unit demand matrix with utility levels equal to 1.
The output coefficient matrix B, unlike the unit supply matrix, does not contain the exogenous supplies.
In the structural equilibrium model, the elements corresponding to consumers in \eqn{\mathbf z} usually reflect the number of consumers,
while in the structural dynamic model, they usually reflect the utility levels. \cr

Now consider the following linear programming problem:
\deqn{\max \quad \mathbf b^T\mathbf z \quad \text{s.t.} \quad \mathbf{Az\le e},\quad \mathbf{z \ge 0} }
The dual linear programming problem is
\deqn{\min \quad \mathbf p^T\mathbf e \quad \text{s.t.} \quad \mathbf p^T \mathbf{A\ge b},\quad \mathbf{p \ge 0} }
In the example of Winston (2003), we have \eqn{\mathbf e=(48,20,8)^T}, \eqn{\mathbf b=(60,30,20)^T} and
\deqn{
  \mathbf A=\left[\begin{matrix}
               8  &6&  1\\
               4& 2& 1.5 \\
               2& 1.5& 0.5\\
               \end{matrix}\right]
}
The corresponding structural equilibrium model is
\deqn{\mathbf p^{T}\mathbf A(u) \geq \mathbf p^{T}\mathbf B}
\deqn{\mathbf A(u) \mathbf z \leq \mathbf  {Bz} }
wherein \eqn{\mathbf p=(1,p_2,p_3,p_4)^T}, \eqn{\mathbf z=(z_1,z_2,z_3,1)^T},
\deqn{
  \mathbf A(u)=\left[\begin{matrix}
               0& 0& 0& u\\
               8  &6&  1&0 \\
               4& 2& 1.5&0 \\
               2& 1.5& 0.5&0\\
               \end{matrix}\right]
}
and
\deqn{
  \mathbf B=\left[\begin{matrix}
            60& 30& 20& 0\\
            0 & 0&  0&48 \\
            0 & 0&  0&20 \\
            0 & 0&  0&8\\
            \end{matrix}\right]
}

The following results are obtained by solving the above structural equilibrium model:

\deqn{\mathbf p^*=(1, 0, 10, 10)^T, \quad \mathbf z^*=(2, 0, 8, 1)^T, \quad u^*=280}
}
\examples{
\donttest{
#### the Dakota example of Winston (2003, section 6.3, 6.6 and 6.8)
A <- matrix(c(
  0, 0, 0, 1,
  8, 6, 1, 0,
  4, 2, 1.5, 0,
  2, 1.5, 0.5, 0
), 4, 4, TRUE)
B <- matrix(c(
  60, 30, 20, 0,
  0, 0, 0, 0,
  0, 0, 0, 0,
  0, 0, 0, 0
), 4, 4, TRUE)
S0Exg <- {
  S0Exg <- matrix(NA, 4, 4)
  S0Exg[2:4, 4] <- c(48, 20, 8)
  S0Exg
}

## Compute the equilibrium by the function CGE::sdm.
ge <- CGE::sdm(A = A, B = B, S0Exg = S0Exg)

ge$p / ge$p[1]
ge$z

## Compute the equilibrium by the function sdm2.
## The function policyMeanValue is used to accelerate convergence.
ge <- sdm2(
  A = A, B = B, S0Exg = S0Exg,
  policy = policyMeanValue,
  names.commodity = c("dollar", "lumber", "lab1", "lab2"),
  names.agent = c("desk producer", "table producer", "chair producer", "consumer"),
  numeraire = "dollar"
)

ge$z
ge$p

#### an example at http://web.mit.edu/15.053/www/AMP-Chapter-04.pdf.
A <- matrix(c(
  0, 0, 0, 1,
  0.5, 2, 1, 0,
  1, 2, 4, 0
), 3, 4, TRUE)
B <- matrix(c(
  6, 14, 13, 0,
  0, 0, 0, 0,
  0, 0, 0, 0
), 3, 4, TRUE)
S0Exg <- {
  S0Exg <- matrix(NA, 3, 4)
  S0Exg[2:3, 4] <- c(24, 60)
  S0Exg
}

ge <- CGE::sdm(
  A = A, B = B, S0Exg = S0Exg
)

ge$z
ge$p / ge$p[1]

#### an example at https://web.stanford.edu/~ashishg/msande111/notes/chapter4.pdf.
A <- matrix(c(
  0, 0, 1,
  4.44, 0, 0,
  0, 6.67, 0,
  4, 2.86, 0,
  3, 6, 0
), 5, 3, TRUE)
B <- matrix(c(
  3, 2.5, 0,
  0, 0, 0,
  0, 0, 0,
  0, 0, 0,
  0, 0, 0
), 5, 3, TRUE)
S0Exg <- {
  S0Exg <- matrix(NA, 5, 3)
  S0Exg[2:5, 3] <- 100
  S0Exg
}

ge <- CGE::sdm(
  A = A, B = B, S0Exg = S0Exg
)

ge$z
ge$p / ge$p[1]

#### an example at https://utw11041.utweb.utexas.edu/ORMM/supplements/methods/lpmethod/S3_dual.pdf.
A <- matrix(c(
  0, 0, 1,
  0, 1, 0,
  1, 3, 0,
  1, 0, 0
), 4, 3, TRUE)
B <- matrix(c(
  2, 3, 0,
  1, 0, 0,
  0, 0, 0,
  0, 0, 0
), 4, 3, TRUE)
S0Exg <- {
  S0Exg <- matrix(NA, 4, 3)
  S0Exg[2:4, 3] <- c(5, 35, 20)
  S0Exg
}

ge <- CGE::sdm(
  A = A, B = B, S0Exg = S0Exg
)

ge$z
ge$p / ge$p[1]

#### the Giapetto example of Winston (2003, section 3.1)
A <- matrix(c(
  0, 0, 1,
  2, 1, 0,
  1, 1, 0,
  1, 0, 0
), 4, 3, TRUE)
B <- matrix(c(
  27 - 10 - 14, 21 - 9 - 10, 0,
  0, 0, 0,
  0, 0, 0,
  0, 0, 0
), 4, 3, TRUE)
S0Exg <- {
  S0Exg <- matrix(NA, 4, 3)
  S0Exg[2:4, 3] <- c(100, 80, 40)
  S0Exg
}

ge <- sdm2(
  A = A, B = B, S0Exg = S0Exg,
  policy = policyMeanValue,
  numeraire = 1
)

ge$z
ge$p

#### the Dorian example (a minimization problem) of Winston (2003, section 3.2)
A <- matrix(c(
  0, 0, 1,
  7, 2, 0,
  2, 12, 0
), 3, 3, TRUE)
B <- matrix(c(
  28, 24, 0,
  0, 0, 0,
  0, 0, 0
), 3, 3, TRUE)
S0Exg <- {
  S0Exg <- matrix(NA, 3, 3)
  S0Exg[2:3, 3] <- c(50, 100)
  S0Exg
}

ge <- sdm2(
  A = A, B = B, S0Exg = S0Exg,
  policy = policyMeanValue,
  numeraire = 1
)

ge$p
ge$z

#### the diet example (a minimization problem) of Winston (2003, section 3.4)
A <- matrix(c(
  0, 0, 0, 0, 1,
  400, 3, 2, 2, 0,
  200, 2, 2, 4, 0,
  150, 0, 4, 1, 0,
  500, 0, 4, 5, 0
), 5, 5, TRUE)
B <- matrix(c(
  500, 6, 10, 8, 0,
  0, 0, 0, 0, 0,
  0, 0, 0, 0, 0,
  0, 0, 0, 0, 0,
  0, 0, 0, 0, 0
), 5, 5, TRUE)
S0Exg <- {
  S0Exg <- matrix(NA, 5, 5)
  S0Exg[2:5, 5] <- c(50, 20, 30, 80)
  S0Exg
}

ge <- sdm2(
  A = A, B = B, S0Exg = S0Exg,
  policy = policyMeanValue,
  numeraire = 1
)

ge$p
ge$z

#### An example of Elizabeth Stapel (Linear Programming: Introduction. Purplemath.
## Available from https://www.purplemath.com/modules/linprog.htm):
## Find the maximal value of 3x + 4y subject to the following constraints:
## x + 2y <= 14, 3x - y >= 0, x - y <= 2, x >= 0, y >= 0

A <- matrix(c(
  0, 0, 1,
  1, 2, 0,
  0, 1, 0,
  1, 0, 0
), 4, 3, TRUE)
B <- matrix(c(
  3, 4, 0,
  0, 0, 0,
  3, 0, 0,
  0, 1, 0
), 4, 3, TRUE)
S0Exg <- {
  S0Exg <- matrix(NA, 4, 3)
  S0Exg[2:4, 3] <- c(14, 0, 2)
  S0Exg
}

ge <- sdm2(
  A = A, B = B, S0Exg = S0Exg,
  policy = policyMeanValue,
  priceAdjustmentVelocity = 0.03,
  numeraire = 1
)

ge$z
ge$p
}
}
\references{
Kemeny, J. G., O. Morgenstern and G. L. Thompson (1956) A Generalization of the von Neumann Model of an Expanding Economy, Econometrica, 24, pp. 115-135.

LI Wu (2019, ISBN: 9787521804225) General Equilibrium and Structural Dynamics: Perspectives of New Structural Economics. Beijing: Economic Science Press. (In Chinese)

von Neumann, J. (1945) A Model of General Economic Equilibrium. The Review of Economic Studies, 13. pp. 1-9.

Winston, Wayne L. (2003, ISBN: 9780534380588) Operations Research: Applications and Algorithms. Cengage Learning.
}
