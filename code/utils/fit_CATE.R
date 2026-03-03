fit_CATE_for_node <- function(node, node_list, t, target_tp, dict_SY_tp, dict_ESY1, dict_ESY0, 
                              data_obs, data_new, Q_learner,
                              col_pretreat, 
                              do_OTR = TRUE, b_learner = "hal", 
                              b_learner_args = list(),
                              conf_level = 0.95, treat_func_type = "default"
){
  # ---- Step 1: Data preparation----
  n_available_target <- sum(data_obs$ti <= (t - target_tp))
  g_OTR <- rep(NA, n_available_target)
  node_tp <- dict_SY_tp[[node]]
  data_obs_node <- data_obs
  data_obs_node[["Y"]] <- data_obs[[node]]
  available_idx_node <- which(data_obs_node$ti <= (t - node_tp)) # rows with outcome
  output <- list(fitted = FALSE, 
                 pA_new = NULL,
                 g_OTR = rep(NA, nrow(data_new)),
                 blip_result = NULL
  )
  if (length(available_idx_node) == 0) return(output) # no data available for this node
  
  data_obs_node <- data_obs_node[available_idx_node, c(col_pretreat, "pA", node_list$A, "Y"), with=FALSE]
  
  # ---- Step 2: CATE ("blip") function prediction----
  ## Obtain psuedo outcome
  sl_task <- make_sl3_Task(data = data_obs_node,
                           covariates = c(node_list$W, node_list$A),
                           outcome = "Y")
  Q_fit <- Q_learner$train(sl_task)
  data_new_node <- data_new[, c(col_pretreat), with = FALSE]
  data_new_node$Y <- 0 # to be overwritten by blip prediction
  predict_task_1 <-
    make_sl3_Task(
      data = data_obs_node %>% mutate(A = 1),
      covariates = c(node_list$W, node_list$A),
      outcome = "Y")
  predict_task_0 <-
    make_sl3_Task(
      data = data_obs_node %>% mutate(A = 0),
      covariates = c(node_list$W, node_list$A),
      outcome = "Y"
    )
  Q_pred_1 <- Q_fit$predict(predict_task_1)
  Q_pred_0 <- Q_fit$predict(predict_task_0)
  if ((length(Q_pred_1)==0)|(length(Q_pred_0)==0)) return(output) 
  
  ## Generate Psuedo outcome
  data_obs_node_ps <- data_obs_node %>%
    mutate(Y = (A / pA * (Y - Q_pred_1) + Q_pred_1) -
             ((1 - A) / (1 - pA) * (Y - Q_pred_0) + Q_pred_0))
  
  ## Fit CATE
  blip_task <- make_sl3_Task(
    data = data_obs_node_ps,
    covariates = c(node_list$W),
    outcome = "Y"
  )
  blip_learner <- do.call(Lrnr_hal9001$new, b_learner_args)
  blip_fit <- tryCatch(blip_learner$train(blip_task), error = function(e) return(NULL))
  if (is.null(blip_fit)) return(output) 
  
  blip_predict_task <- make_sl3_Task(
    data = data_new_node,
    covariates = c(node_list$W),
    outcome = "Y"
  )
  blip_pred_result <- tryCatch(
    data.frame(predictCATE(blip_fit, blip_predict_task, b_learner)),
    error = function(e) return(NULL)
  )
  if (is.null(blip_pred_result)) {
    return(list(pA_new=rep(0.5, nrow(data_new)), g_OTR=g_OTR, blip_result=NULL))
  }

  colnames(blip_pred_result) <- paste0(colnames(blip_pred_result), "_", node_tp)
  
  # Predict blip and return its se
  pA_new <- treatByCATE(fit = blip_pred_result$fit,
                        se.fit = blip_pred_result$se,
                        conf_level = conf_level,
                        treat_func_type = treat_func_type)
  if (do_OTR & (n_available_target > 0)){
    # Prepare data which has node-specific OTR and has final outcome
    data_obs_target <- data_obs_node[ti <= t - target_tp, c(col_pretreat, "pA", node_list$A, "Y"), with = FALSE]
    blip_predict_task_obs_node_target <- make_sl3_Task(
      data = data_obs_target,
      covariates = c(node_list$W),
      outcome = "Y"
    )
    blip_pred_result_target <- data.frame(predictCATE(blip_fit, blip_predict_task_obs_node_target, b_learner))
    g_OTR <- as.numeric(blip_pred_result_target$fit >= 0)
  }
  return(list(fitted = TRUE, pA_new=pA_new, g_OTR=g_OTR, blip_result=blip_pred_result))
}

