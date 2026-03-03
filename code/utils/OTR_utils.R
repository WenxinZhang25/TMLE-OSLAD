load_results <- function(results_files, load_or_not){
  results <- lapply(results_files[load_or_not],
                    function(results_file) {
                      var <- load(results_file)
                      get(var)
                    })
  return (results)
}

get_spec <- function(resultsDir, result_file, spec = c("sim_spec", "est_spec")){
  splitted_name <- strsplit(gsub(resultsDir,"",result_file), "_")[[1]]
  if (spec == "sim_spec"){
    return (splitted_name[2])
  }
  if (spec == "est_spec"){
    return (splitted_name[3])
  }
}

CV_TMLE_for_OTR <- function(data, Q_learner, outcome_node, V = 2){
  b_learner <- "hal"
  b_learner_args <- list(smoothness_orders = 1)
  n_total <- nrow(data)
  fold_id <- sample(rep(1:V, length.out = n_total))
  data[["Y"]] <- data[[outcome_node]]
  individual_EY_OTR_truth_1_to_V <- rep(NA,n_total)
  individual_IC_1_to_V <- rep(NA, n_total)
  TMLE_1_to_V <- rep(NA,n_total)
  for (v in 1:V){
    cat("CV TMLE, fold:", v, "\n")
    data_obs_node <- data[fold_id!=v,]
    data_new_node <- data[fold_id==v,]
    # Make sl task for Q learner
    sl_task <- make_sl3_Task(data = data_obs_node,
                             covariates = c(node_list$W, node_list$A),
                             outcome = "Y")

    # Fit Q model
    Q_fit <- Q_learner$train(sl_task)

    data_new_node$Y <- 0

    # Predictive Task on obs data
    predict_task_1 <-
      make_sl3_Task(
        data = data_obs_node %>% mutate(A = 1),
        covariates = c(node_list$W, node_list$A),
        outcome = "Y"
      )
    predict_task_0 <-
      make_sl3_Task(
        data = data_obs_node %>% mutate(A = 0),
        covariates = c(node_list$W, node_list$A),
        outcome = "Y"
      )
    Q_pred_1 <- Q_fit$predict(predict_task_1)
    Q_pred_0 <- Q_fit$predict(predict_task_0)

    # Generate Psuedo outcome
    data_obs_node_ps <- data_obs_node %>%
      mutate(Y = (A / pA * (Y - Q_pred_1) + Q_pred_1) -
               ((1 - A) / (1 - pA) * (Y - Q_pred_0) + Q_pred_0))

    # Fit CATE functions
    blip_task <- make_sl3_Task(
      data = data_obs_node_ps,
      covariates = c(node_list$W),
      outcome = "Y"
    )

    blip_learner <- do.call(Lrnr_hal9001$new, b_learner_args)
    blip_fit <- blip_learner$train(blip_task)
    blip_predict_task <- make_sl3_Task(
      data = data_new_node,
      covariates = c(node_list$W),
      outcome = "Y"
    )
    blip_pred_result <- data.frame(predictCATE(blip_fit, blip_predict_task, b_learner))
    blip_predict_task_obs_node_target <- make_sl3_Task(
      data = data_new_node,
      covariates = c(node_list$W),
      outcome = "Y"
    )

    blip_pred_result_node_target <- data.frame(predictCATE(blip_fit, blip_predict_task_obs_node_target, b_learner))
    OTR_for_data_new <- as.numeric(blip_pred_result_node_target$fit >= 0)

    target_EY1 <- data_new_node[[dict_ESY1[[outcome_node]]]]
    target_EY0 <- data_new_node[[dict_ESY0[[outcome_node]]]]

    individual_EY_OTR_truth_v <- OTR_for_data_new*target_EY1 + (1-OTR_for_data_new)*target_EY0
    individual_EY_OTR_truth_1_to_V[which(fold_id == v)] <- individual_EY_OTR_truth_v
    tmle_spec <- tmle3_Spec_ADSM$new(
      treatment_level = 1,
      control_level = 0,
      g_treat = data_new_node$pA,
      g_adapt = OTR_for_data_new
    )
    
    ## Define tmle task
    tmle_task <- tmle_spec$make_tmle_task(data_new_node, node_list[c("W","A","Y")])

    learner_list <- list(A = Lrnr_mean$new(), Y = Q_learner)

    ## Make initial likelihood
    initial_likelihood <- tmle_spec$make_initial_likelihood(
      tmle_task,
      learner_list
    )

    ## Create targeted_likelihood object
    targeted_likelihood <- Targeted_Likelihood$new(initial_likelihood)

    ## Define tmle param
    tmle_params <- tmle_spec$make_params(tmle_task, targeted_likelihood, initial_likelihood = initial_likelihood)

    ## Run TMLE
    tmle_fit <- fit_tmle3(
      tmle_task, targeted_likelihood, tmle_params,
      targeted_likelihood$updater
    )
    TMLE_1_to_V[fold_id == v] <- tmle_fit$estimates[[1]]$psi
    individual_IC_1_to_V[fold_id == v] <- tmle_fit$estimates[[1]]$IC[,1]
  }

  cv_tmle_truth <- mean(individual_EY_OTR_truth_1_to_V)
  cv_tmle_est <- mean(TMLE_1_to_V)
  cv_tmle_var <- mean(individual_IC_1_to_V^2)/n_total
  cv_tmle_se <- sqrt(cv_tmle_var)
  lower <- cv_tmle_est - 1.96*cv_tmle_se
  upper <- cv_tmle_est + 1.96*cv_tmle_se
  fit_result <- data.frame(estimator = "CV-TMLE",
                           V = V,
                           tmle_est = cv_tmle_est,
                           se = cv_tmle_se,
                           lower = lower,
                           upper = upper,
                           truth = cv_tmle_truth)
  return(fit_result)
}
