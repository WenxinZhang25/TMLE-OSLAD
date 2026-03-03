# Data Generating Functions for simulation studies
# 
# Arguments:
# - ti: current entry time
# - batch_size: number of enrolled participants
# Return: 
# - a data table of covariates and counterfactual outcomes of enrolled participants

scenario_2 <- function(ti, batch_size, ...){
  D <- DAG.empty() +
    node("ti", distr = "rconst", const = ti) +
    node("W1", distr="runif", min = -4, max = 4) +
    node("W2", distr="runif", min = -4, max = 4) +
    node("W3", distr="runif", min = -4, max = 4) +
    node("Z", distr="rconst", const = 0.4 * W1 + 0.4 * W2 + 0.2 * W3) +
    node("EY1", t=1:5, distr="rconst", const = 0.5 - 1 / (1 + exp(-ifelse(t == 5, 0.25, ifelse(t == 4, 0.5, 4 - t))*Z))) +
    node("EY0", t=1:5, distr="rconst", const = -0.5 + 1 / (1 + exp(-ifelse(t == 5, 0.25, ifelse(t == 4, 0.5, 4 - t))*Z)))
  D <- set.DAG(D)
  return (data.table::as.data.table(sim(D, n = batch_size)))
}
