# Define Q_learner based on Superlearner for outcome regression

# Ordinary Least Squares (OLS)
lrnr_ols <- Lrnr_glm$new()

# Random Forest
lrnr_rf <- Lrnr_ranger$new()

# Highly Adaptive Lasso (HAL)
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

# Constant mean
lrnr_mean <- Lrnr_mean$new()

# Super Learner combining the above learners via cross-validated selection
Q_learner <- Lrnr_sl$new(
  learners = c(lrnr_ols, lrnr_rf, lrnr_mean, lrnr_hal),
  metalearner = Lrnr_cv_selector$new()
)

# Define configurations under different adaptive designs
# Each uses HAL for CATE function estimation

# Non-adaptive design flipping a fair coin
est_spec_rct <- make_spec(
  adaptEst,
  params = list(
    node_selection = "fair_coin",
    name = "est_RCT_hal",
    Q_learner = Q_learner,
    b_learner = "hal",
    b_learner_args = b_learner_args,
    do_tmle_tps = do_tmle_tps,
    treat_func_type = treat_func_type,
    skip_if_not_prespecified = skip_if_not_prespecified,
    bound = bound
  ),
  est_fun = est_fun_adapt
)

# Adaptive designs based on surrogate Y_1 to primary outcome Y_5

est_spec_Y1_hal <- make_spec(
  adaptEst,
  params = list(
    node_selection = "Y_1",
    name = "est_static_Y_1_hal",
    Q_learner = Q_learner,
    b_learner = "hal",
    b_learner_args = b_learner_args,
    do_tmle_tps = do_tmle_tps,
    treat_func_type = treat_func_type,
    conf_level = conf_level,
    skip_if_not_prespecified = skip_if_not_prespecified,
    bound = bound
  ),
  est_fun = est_fun_adapt
)

est_spec_Y2_hal <- make_spec(
  adaptEst,
  params = list(
    node_selection = "Y_2",
    name = "est_static_Y_2_hal",
    Q_learner = Q_learner,
    b_learner = "hal",
    b_learner_args = b_learner_args,
    do_tmle_tps = do_tmle_tps,
    treat_func_type = treat_func_type,
    conf_level = conf_level,
    skip_if_not_prespecified = skip_if_not_prespecified,
    bound = bound
  ),
  est_fun = est_fun_adapt
)

est_spec_Y3_hal <- make_spec(
  adaptEst,
  params = list(
    node_selection = "Y_3",
    name = "est_static_Y_3_hal",
    Q_learner = Q_learner,
    b_learner = "hal",
    b_learner_args = b_learner_args,
    do_tmle_tps = do_tmle_tps,
    treat_func_type = treat_func_type,
    conf_level = conf_level,
    skip_if_not_prespecified = skip_if_not_prespecified,
    bound = bound
  ),
  est_fun = est_fun_adapt
)

est_spec_Y4_hal <- make_spec(
  adaptEst,
  params = list(
    node_selection = "Y_4",
    name = "est_static_Y_4_hal",
    Q_learner = Q_learner,
    b_learner = "hal",
    b_learner_args = b_learner_args,
    do_tmle_tps = do_tmle_tps,
    treat_func_type = treat_func_type,
    conf_level = conf_level,
    skip_if_not_prespecified = skip_if_not_prespecified,
    bound = bound
  ),
  est_fun = est_fun_adapt
)

est_spec_Y5_hal <- make_spec(
  adaptEst,
  params = list(
    node_selection = "Y_5",
    name = "est_static_Y_5_hal",
    Q_learner = Q_learner,
    b_learner = "hal",
    b_learner_args = b_learner_args,
    do_tmle_tps = do_tmle_tps,
    treat_func_type = treat_func_type,
    conf_level = conf_level,
    skip_if_not_prespecified = skip_if_not_prespecified,
    bound = bound
  ),
  est_fun = est_fun_adapt
)

# Online-Superlearner Adaptive Design using TMLE to evaluate and select surrogates
est_spec_tmle_hal <- make_spec(
  adaptEst,
  params = list(
    node_selection = "TMLE",
    name = "est_TMLE_hal",
    Q_learner = Q_learner,
    b_learner = "hal",
    b_learner_args = b_learner_args,
    do_tmle_tps = do_tmle_tps,
    treat_func_type = treat_func_type,
    conf_level = conf_level,
    skip_if_not_prespecified = skip_if_not_prespecified,
    bound = bound
  ),
  est_fun = est_fun_adapt
)

# Naive policy: selecting latest available outcome
est_spec_naive_hal <- make_spec(
  adaptEst,
  params = list(
    node_selection = "naive",
    name = "est_naive_hal",
    Q_learner = Q_learner,
    b_learner = "hal",
    b_learner_args = b_learner_args,
    do_tmle_tps = do_tmle_tps,
    treat_func_type = treat_func_type,
    conf_level = conf_level,
    skip_if_not_prespecified = skip_if_not_prespecified,
    bound = bound
  ),
  est_fun = est_fun_adapt
)

design_map <- c(
  "TMLE" = "est_TMLE_hal",
  "RCT" = "est_RCT_hal",
  "Y1" = "est_static_Y_1_hal",
  "Y2" = "est_static_Y_2_hal",
  "Y3" = "est_static_Y_3_hal",
  "Y4" = "est_static_Y_4_hal",
  "Y5" = "est_static_Y_5_hal"
)

if (grepl("DGD_realSim",sim_name)) {
  design_map <- c(design_map, "naive" = "est_naive_hal")
}