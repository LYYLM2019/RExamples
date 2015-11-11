library(tidyr)
library(nloptr)
library(assertthat)
library(dplyr)
library(ggplot2)

# starting parameter values
x0.hs100 = c(1, 2, 0, 4, 0, 1, 1)

# objective function
fn.hs100 = function(x) {
  (x[1]-10)^2 + 5*(x[2]-12)^2 + x[3]^4 + 3*(x[4]-11)^2 + 10*x[5]^6 +
    7*x[6]^2 + x[7]^4 - 4*x[6]*x[7] - 10*x[6] - 8*x[7]
}

# inequality constraints
# NOTE: nloptr with algorithm = NLOPT_LN_COBYLA takes g(x) <= 0
hin.hs100 = function(x) {
  h = numeric(4)
  h[1] = 127 - 2*x[1]^2 - 3*x[2]^4 - x[3] - 4*x[4]^2 - 5*x[5]
  h[2] = 282 - 7*x[1] - 3*x[2] - 10*x[3]^2 - x[4] + x[5]
  h[3] = 196 - 23*x[1] - x[2]^2 - 6*x[6]^2 + 8*x[7]
  h[4] = -4*x[1]^2 - x[2]^2 + 3*x[1]*x[2] -2*x[3]^2 - 5*x[6]	+11*x[7]
  return(-h)
}

# compute the solution
sink(file = "Output/cobyla_trace.txt", type = "output")
S1 = nloptr(x0 = x0.hs100, 
            eval_f = fn.hs100,
            eval_g_ineq = hin.hs100,
            opts = list(algorithm = "NLOPT_LN_COBYLA", 
                        xtol_rel = 1e-8, maxeval = 2000, print_level = 3))
sink()

# inequality constraints
# NOTE: cobyla takes g(x) >= 0
hin.hs100_minus = function(x) {
  h = numeric(4)
  h[1] = 127 - 2*x[1]^2 - 3*x[2]^4 - x[3] - 4*x[4]^2 - 5*x[5]
  h[2] = 282 - 7*x[1] - 3*x[2] - 10*x[3]^2 - x[4] + x[5]
  h[3] = 196 - 23*x[1] - x[2]^2 - 6*x[6]^2 + 8*x[7]
  h[4] = -4*x[1]^2 - x[2]^2 + 3*x[1]*x[2] -2*x[3]^2 - 5*x[6]	+11*x[7]
  return(h)
}

# compute the solution
S2 = cobyla(x0.hs100, fn.hs100, hin = hin.hs100_minus,
            nl.info = TRUE, control = list(xtol_rel = 1e-8, maxeval = 2000))

# check that both return the same solution
assertthat::are_equal(S1$solution, S2$par)

# get the solutions from the output file
solutionPath = readLines(con = file("Output/cobyla_trace.txt"))

# extract the solution path data out of the raw output
solutionPathParamRaw = solutionPath[grepl("^\tx", solutionPath)]
solutionPathParamMatch = gregexpr("(-)?[0-9]+(\\.[0-9]+)?", solutionPathParamRaw, perl = TRUE)
solutionPathParam = as.data.frame(
  t(
    sapply(
      regmatches(
        solutionPathParamRaw, solutionPathParamMatch
      ), 
      as.numeric, simplify = TRUE
    )
  )
)

# give the columns some names
names(solutionPathParam) = paste("x", seq(1, 7), sep = "")
solutionPathParam$IterationNum = seq(1, dim(solutionPathParam)[1])

# plot the solutions
solutionPathParam %>%
  gather(Parameter, Solution, -IterationNum) %>%
  ggplot(aes(x = IterationNum, y = Solution, group = Parameter, color = Parameter)) +
  geom_line() + 
  theme_bw()

ggsave("Output/SolutionPath.png")
  