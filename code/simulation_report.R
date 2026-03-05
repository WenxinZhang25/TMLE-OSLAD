# Sensitivity report

library(ggplot2)
library(dplyr)
library(simcausal)
library(gridExtra)
library(hash)
library(tidyr)
library(latex2exp)

theme_set(theme_minimal(base_size = 15))
theme_update(
  plot.title = element_blank(),
  plot.subtitle = element_blank()
)
options(
  warn = -1,                        
  dplyr.summarise.inform = FALSE,   
  lifecycle_verbosity = "quiet"     
)
pdf.options(width = 10, height = 8)

# simulation configuration
args <- commandArgs(trailingOnly = TRUE)
sim_name <- args[1] # "DGD_hetero_d1"
treat_func_type <- args[2] # "default"
dir_name <- args[3] # "t_end_50_batch_size_50_n_runs_500"

dir_name_split <- stringr::str_split(dir_name, "_")[[1]]
t_end <- as.numeric(dir_name_split[3])
batch_size <- as.numeric(dir_name_split[6])
source("code/utils/report_utils.R")
resultsDir <- file.path("output/Results", sim_name, treat_func_type)

# functions to load results and get simulation names
load_results <- function(results_files, load_or_not){
  results <- lapply(results_files[load_or_not], 
                    function(results_file) {
                      var <- load(results_file)
                      get(var)
                    })
  return (results)
}

get_spec <- function(result_file, spec = c("sim_spec", "est_spec")) {
  fname <- basename(result_file)
  stem <- sub("^results_", "", fname)
  stem <- sub("\\.rdata$", "", stem)
  splitted_name <- strsplit(stem, "_")[[1]]
  if (spec == "sim_spec") {
    return(splitted_name[1])
  }
  if (spec == "est_spec") {
    return(splitted_name[2])
  }
}

# Extract result files
results_files <- dir(resultsDir, pattern = "rdata$", full.names = TRUE, recursive = T)
results_files <- sort(results_files) # make sure the order is the same across machines
results_files <- results_files[grep(dir_name,results_files)] 

sim_specs <- as.vector(sapply(results_files, get_spec, spec = "sim_spec"))
est_specs <- as.vector(sapply(results_files, get_spec, spec = "est_spec"))
source("code/utils/report_utils.R")

if (sim_name == "DGD_hetero_d1"){
  source(sprintf("code/%s.R",sim_name))
}

# Set time
S_tp <- c(1,2,3,4)
Y_tp <- 5
SY_tp <- c(S_tp,Y_tp)
n_S <- length(S_tp)
n_SY <- length(SY_tp)
ann_colors <- c(scales::hue_pal()(length(SY_tp)+2), "#808080")
ann_colors <- setNames(ann_colors, c(paste0("Y", S_tp," (Surrogate Outcome)"), "Y5 (Final Outcome)", "fair_coin", "TMLE-OSLAD", "naive"))
ann_colors_Y_k <- scales::hue_pal()(length(SY_tp))
ann_colors_Y_k <- setNames(ann_colors, c(paste0("Y_", SY_tp)))

plot_outcome_labels <- c(
  sapply(S_tp, function(i) 
    bquote(Y[.(i)] ~ "(surrogate outcome)")
  ),
  bquote(Y[.(Y_tp)] ~ "(final outcome)")
)

design_dict <- hash(c("fair_coin", paste0("Y_", 1:length(S_tp)), "Y_5"), 
                    c("Non-adaptive randomization", 
                      paste0(rep("Adaptive design (based on surrogate Y", length(S_tp)), S_tp, ")"),
                      "Adaptive design (based on final outcome Y5)"))
design_tp_dict <- hash(c("fair_coin", paste0("Y_", SY_tp)), c(1, SY_tp))
design_colors <- ann_colors
design_levels <- c(paste0(
  rep("Adaptive design (based on surrogate Y", length(S_tp)), S_tp, ")"),
  "Adaptive design (based on final outcome Y5)",
  "Adaptive design (TMLE-OSLAD)",
  "Non-adaptive randomization",
  "Adaptive design (based on the naive policy)")

design_colors <- setNames(design_colors, 
                          c(design_levels[1:length(SY_tp)],
                            "Non-adaptive randomization",
                            "Adaptive design (TMLE-OSLAD)",
                            "Adaptive design (based on the naive policy)"))

for (sim_spec in sort(unique(sim_specs))) {
  sim_spec_idx <- sim_specs == sim_spec
  system.time(results_subset <- load_results(results_files, sim_spec_idx))
  est_specs_subset <- est_specs[sim_spec_idx]
  system.time(reports_subset <- Reduce(rbind, lapply(results_subset, `[[`, "report")))
  original_est_name <- c(
    "est_RCT_hal",
    "est_static_Y_1_hal",
    "est_static_Y_2_hal",
    "est_static_Y_3_hal",
    "est_static_Y_4_hal",
    "est_static_Y_5_hal",
    "est_TMLE_hal",
    "est_naive_hal"
  )
  report_est_name <-  c("Non-adaptive randomization",
    paste0(rep("Adaptive design (based on surrogate Y", 4), 1:4, ")"),
    "Adaptive design (based on final outcome Y5)",
    "Adaptive design (TMLE-OSLAD)",
    "Adaptive design (based on the naive policy)"
  )
  est_dict <- hash(original_est_name, report_est_name)
  reports_subset$estimator_name = as.character(sapply(
    reports_subset$estimator_name,
    FUN = function(x) {
      values(est_dict, x)
    }
  ))
  sim_spec_name <- unique(reports_subset$simulation_name)
  imageDir <- file.path("output/Images", sim_name, treat_func_type, dir_name, sim_spec_name)
  tableDir <- file.path("output/Tables", sim_name, treat_func_type, dir_name, sim_spec_name)
  
  OTR_report_Dir <- file.path("output/Results_CVTMLE", sim_name, treat_func_type)
  if (!dir.exists(imageDir))  dir.create(imageDir, recursive = T)
  if (!dir.exists(tableDir)) dir.create(tableDir, recursive = T)
  source("code/utils/report_template.R")
  rm(results_subset)
  rm(reports_subset)
}

