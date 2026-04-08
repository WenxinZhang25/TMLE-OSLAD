# Set configuration parameters for different Data-Generating Designs (DGDs)
#   - sim_name: string specifying which simulation setting is used
#   - t_end: last enrollment time
#   - E_t: vector of sample size per enrollment time
#   - sd_noise: standard deviation of noise
#   - node_list: list specifying covariates W, treatment A, surrogates Y_k, and primary Y_K
#   - hal_n_folds: number of CV folds in HAL learner
#   - do_tmle_tps: vector of time points when TMLE estimation of causal estimands is triggered in designs other than TMLE-OSLAD
#   - hal_max_degree: max degree for HAL basis
#   - hal_num_knots: number of knots in HAL
#   - df_realSim: dataset with counterfactual outcomes for ADAPT-R simulation

# Prepare node structure for different datasets
node_list_d1 <-
  list(
    W = c("W"),
    A = "A",
    S = paste("Y", 1:4, sep = "_"),
    S_tp = 1:4,
    Y = "Y_5",
    Y_tp = 5
  )

node_list_d2 <-
  list(
    W = c("W1", "W2"),
    A = "A",
    S = paste("Y", 1:4, sep = "_"),
    S_tp = 1:4,
    Y = "Y_5",
    Y_tp = 5
  )

node_list_d3 <-
  list(
    W = c("W1", "W2", "W3"),
    A = "A",
    S = paste("Y", 1:4, sep = "_"),
    S_tp = 1:4,
    Y = "Y_5",
    Y_tp = 5
  )

design_map <- c(
  "TMLE" = "est_TMLE_hal",
  "RCT" = "est_RCT_hal",
  "Y1" = "est_static_Y_1_hal",
  "Y2" = "est_static_Y_2_hal",
  "Y3" = "est_static_Y_3_hal",
  "Y4" = "est_static_Y_4_hal",
  "Y5" = "est_static_Y_5_hal",
  "naive" = "est_naive_hal"
)

# Define simulation parameters
if (grepl("DGD_hetero_d1",sim_name)){
  E_t <- rep(batch_size, t_end + 1) # the last entry is a placeholder to support post-experiment estimation, applied to enrolled data from 1 to t_end
  sd_noise <- 1
  node_list <- node_list_d1
  hal_n_folds <- 10
  do_tmle_tps <- c(11, 21, 31, 41, 50)
  hal_max_degree <- 2
  hal_num_knots <- NULL
  skip_if_not_prespecified <- FALSE
} else if (grepl("DGD_realSim",sim_name)){
  print(sim_name)
  df_realSim <- read.csv("data/realSimData/realSim_data.csv")
  E_t <- c(as.numeric(table(df_realSim$ti)),1) # follow the enrollment schedule
  t_end <- max(df_realSim$ti)
  sd_noise <- 0.1
  node_list <- node_list_d2
  hal_n_folds <- 10
  do_tmle_tps <- c(7, 11, 15, 19)
  hal_max_degree <- 2
  hal_num_knots <- NULL
  skip_if_not_prespecified <- TRUE
  design_map <- c(design_map, "naive" = "est_naive_hal")
} else if (grepl("DGD_hetero_d3",sim_name)){
  E_t <- rep(batch_size, t_end + 1)
  sd_noise <- 1
  node_list <- node_list_d3
  hal_n_folds <- 5
  do_tmle_tps <- c(11, 21, 31, 41, 50)
  hal_max_degree <- 1
  hal_num_knots <- 25
  skip_if_not_prespecified <- TRUE
}

