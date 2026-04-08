# Code for running simulation studies

# ---- Load libraries ----
library(hash)         
library(future)       
library(data.table)  
library(sl3)          
library(dplyr)        
library(simcausal)    
library(ggplot2)      
library(tmle3)        # implement TMLE: devtools::install_github("https://github.com/tlverse/tmle3", ref = "adapt_parameters")
library(tmle3sim)     # implement adaptive experiment: devtools::install_github("tlverse/tmle3sim")  

options(tmle3sim.verbose = FALSE)
options(sl3.verbose = FALSE)

# ---- Source all modules ----
source("code/utils/simulator.R")      # simulation module built on tmle3sim
source("code/utils/estimator.R")      # design and estimation module
source("code/utils/reporter.R")       # reporter module
source("code/utils/fit_CATE.R")       # CATE estimation module
source("code/utils/CATE_helper.R")    # helper function for CATE estimation

args <- commandArgs(trailingOnly = TRUE)
print(args)

# ---- Parse command-line arguments or set defaults ----
## Default values used when not running via commandArgs

t_end <- as.numeric(args[1]) # Last enrollment time point, e.g. t_end <- 20
batch_size <- as.numeric(args[2]) # Number of samples per enrollment step, e.g. batch_size <- 50
total_runs <- as.numeric(args[3]) # Total Monte Carlo repetitions, e.g. total_runs <- 10
n_rounds <- as.numeric(args[4]) # Number of parallel rounds, e.g. n_rounds <- 10
round <- as.numeric(args[5]) # Current round number, e.g. round <- 1
treat_func_type <- as.character(args[6]) # Type of treatment assignment function, e.g. treat_func_type <- "default"
design <- as.character(args[7]) # design = "Y1"
DGD_name <-  as.character(args[8]) # "DGD_hetero_d1" / "DGD_realSim" / "DGD_hetero_d3_scenario1" / "DGD_hetero_d3_scenario2"
conf_level <- as.numeric(args[9]) # CATE CI level
if (is.na(conf_level)) conf_level <- 0.95

b_learner_args <- list(smoothness_orders = 1) # 1st-order HAL for CATE estimation

# Function to source DGD script based on the simulation scenario
my_source <- function(..., type = "function", local=NULL) {
  tmp <- new.env(parent=parent.frame())
  source(..., local = tmp)
  funs <- names(tmp)[unlist(eapply(tmp, function(x){type %in% class(x)}))]
  for(x in names(tmp)) {
    assign(x, tmp[[x]], envir = parent.frame())
  }
  as.list(sort(funs))
}

define_spec <- function(simulator, params, sim_fun){
  params$name <- sim_fun
  sim_fun <- get(sim_fun)
  return (tmle3sim::make_spec(simulator, params = params, sim_fun = sim_fun))
}

# ---- Set simulation specs ----
if (conf_level == 0.95){
  sim_name <- sprintf("%s/%s/%s", DGD_name, treat_func_type, design)
} else{
  sim_name <- sprintf("%s-conf-%s/%s/%s", DGD_name, conf_level, treat_func_type, design)   # sensitivity for different conf levels for CATE CIs
}
sim_funs_all <- my_source(sprintf("code/%s.R", DGD_name), type = "function")

# ---- Load and define simulation specifications ---- 
source("code/utils/sim_params.R") 
sim_param <- list(t_end = t_end,
                  node_list = node_list,
                  E_t = E_t,
                  n_steps = t_end + 1,
                  sd = sd_noise)
sim_specs_all <- lapply(sim_funs_all,
                        FUN = define_spec,
                        simulator = adaptSim,
                        params = sim_param)

# ---- Set estimation and design updating mechanisms ----
source("code/utils/estimation_functions.R")  # adaptive design with TMLE estimation

est_specs_all <- my_source("code/utils/est_specs.R", type = "t3s_Spec")
est_specs_all <- lapply(est_specs_all, get)

if (design == "all") {
  selected_est_specs <- est_specs_all[sapply(est_specs_all, function(x) x$params$name %in% design_map)]
} else {
  est_name <- design_map[[design]]
  selected_est_specs <- est_specs_all[sapply(est_specs_all, function(x) x$params$name == est_name)]
}

# ---- Set results folder ----
ResultsDir <-sprintf("output/Results/%s/t_end_%s_batch_size_%s_n_runs_%s/round_%s",
                     sim_name, t_end, batch_size, total_runs, round)
if(!dir.exists(ResultsDir)) dir.create(ResultsDir, recursive = T)

# ---- Create a reporter to save simulation results ----
reporter <- adaptReporter$new(params = list(path = file.path(ResultsDir)))

# ---- Setup parallel computing ----
plan(multicore, workers=future::availableCores()-1)
cat("workers number:", future::availableCores()-1, "\n")
print(paste("workers number:", future::availableCores()-1, "\n"))
set.seed(round)

# ---- Run simulation ----
if (round == n_rounds){
  results <- run_sims(sim_specs_all, selected_est_specs,
                      reporter, n_runs = total_runs - (n_rounds-1)*ceiling(total_runs/n_rounds))
} else {
  results <- run_sims(sim_specs_all, selected_est_specs,
                      reporter, n_runs = ceiling(total_runs/n_rounds))
}

writeLines(capture.output(sessionInfo()), "output/sessionInfo_sim.txt")

