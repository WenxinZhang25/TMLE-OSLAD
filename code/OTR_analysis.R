# Code for running CV-TMLE for Mean of Final Outcome under its OTR

library(tmle3)
library(here)
library(hash)
library(sl3)
library(dplyr)
library(future)
library(future.apply)
here()

source(here("code/utils/OTR_utils.R"))
source(here("code/utils/CATE_helper.R"))

# Input arguments of results folder
args <- commandArgs(trailingOnly = TRUE)
sim_name <- args[1] # "DGD_hetero_d1" 
subfolder_name <- args[2] # subfolder_name = "default/all"
dir_name <- args[3] # "t_end_50_batch_size_50_n_runs_500"
round <- args[4] # 1

dir_name_split <- stringr::str_split(dir_name, "_")[[1]]
t_end <- as.numeric(dir_name_split[3])
batch_size <- as.numeric(dir_name_split[6])

resultsDir <- file.path("output/Results", sim_name, subfolder_name, dir_name, paste0("round_",round))
reportDir <- file.path("output/Results_CVTMLE", sim_name, subfolder_name, dir_name)

if(! exists(reportDir)) dir.create(reportDir, recursive = T)

# Idnetify result files
results_files <- dir(resultsDir, pattern = "rdata$", full.names = TRUE, recursive = T)
if (length(results_files)==0){
  stop(sprintf("No result file is found in %s", resultsDir))
}
sim_specs <- as.vector(sapply(results_files, get_spec, resultsDir = resultsDir, spec = "sim_spec"))
est_specs <- as.vector(sapply(results_files, get_spec, resultsDir = resultsDir, spec = "est_spec"))

# Set up node list
source("code/utils/sim_params.R") 
SY_node <- c(node_list$S, node_list$Y)
SY_tp <- c(node_list$S_tp, node_list$Y_tp)
SY1_node <- mapply(function(x,y){gsub(paste0("_",y),paste0("1_",y),x)}, SY_node, SY_tp)
SY0_node <- mapply(function(x,y){gsub(paste0("_",y),paste0("0_",y),x)}, SY_node, SY_tp)
ESY1_node <- paste0("E", SY1_node)
ESY0_node <- paste0("E", SY0_node)
dict_SY_tp <- hash(SY_node, SY_tp)
dict_SY1 <- hash(SY_node, SY1_node)
dict_SY0 <- hash(SY_node, SY0_node)
dict_ESY1 <- hash(SY_node, ESY1_node)
dict_ESY0 <- hash(SY_node, ESY0_node)

# Set up Q learners
lrnr_ols <- Lrnr_glm$new()
lrnr_rf <- Lrnr_ranger$new()
if (is.null(hal_num_knots)){
  lrnr_hal <- Lrnr_hal9001$new(return_x_basis = TRUE, smoothness_orders = 1,
                               max_degree = hal_max_degree,
                               fit_control = list(nfolds = hal_n_folds))
} else{
  lrnr_hal <- Lrnr_hal9001$new(return_x_basis = TRUE, smoothness_orders = 1,
                               max_degree = hal_max_degree,
                               num_knots = hal_num_knots,
                               fit_control = list(nfolds = hal_n_folds))
}
lrnr_mean <- Lrnr_mean$new()
Q_learner <- Lrnr_sl$new(
  learners = c(lrnr_ols, lrnr_rf, lrnr_mean, lrnr_hal),
  metalearner = Lrnr_cv_selector$new()
)

# Run CV-TMLE for OTR of final outcome in each setting
for (sim_spec in unique(sim_specs)) {
  sim_spec_idx <- sim_specs == sim_spec
  results_subset <- load_results(results_files, sim_spec_idx)
  est_specs_subset <- est_specs[sim_spec_idx]
  reports_subset <- Reduce(rbind, lapply(results_subset, `[[`, "report"))
  sim_spec_name <- unique(reports_subset$simulation_name)
  est_tmle_idx <- sapply(results_subset, function(result) {return(ifelse(grepl("TMLE", unique(result$report$estimator_name)), TRUE, FALSE))})
  M_rep <- length(results_subset[est_tmle_idx])
  final_data_list <- lapply(1:M_rep, function(m) {
    return(results_subset[est_tmle_idx][[m]]$data_full %>% filter(ti <= t_end))
  })
  plan("multisession", workers = parallelly::availableCores()-1)
  CV_TMLE_results <- future_lapply(final_data_list, CV_TMLE_for_OTR,
                                   Q_learner = Q_learner, outcome_node = node_list$Y, V = 2,
                                   future.seed = TRUE, future.packages = c("tmle3", "dplyr"))
  CV_TMLE_results_all <- bind_rows(CV_TMLE_results)
  output_dir <- file.path(reportDir, sim_spec_name, paste0("round_",round))
  if(! exists(output_dir)) dir.create(output_dir, recursive = T)
  write.csv(CV_TMLE_results_all, file = file.path(output_dir, "CV_TMLE-OTR.csv"))
}

