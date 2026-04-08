# Estimation, surrogate selection and adaptive randomization procedure 
est_fun_adapt <- function(simulation, Q_learner,
                          node_selection = "TMLE",
                          b_learner = "hal",
                          b_learner_args = list(NULL),
                          conf_level = 0.95,
                          do_tmle_tps = c(11, 21, 31, 41, 50, 55),
                          treat_func_type = "default",
                          skip_if_not_prespecified = FALSE,
                          bound = NA,
                          ...){
  t <- simulation$step
  data_new <- simulation$data_new
  data_full <- simulation$data_full
  batch_size <- nrow(data_new)
  sim_params <- simulation$params
  node_list <- sim_params$node_list
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
  
  target_outcome <- node_list$Y
  target_tp <- dict_SY_tp[[target_outcome]]
  
  # Post-experiment stage: all outcomes are observed
  if (simulation$step == simulation$n_steps){
    t <- simulation$step + target_tp - 1
  }
  
  sd_noise <- simulation$params$sd
  
  if (length(sd_noise) == 1){
    sd_noise_SY <- hash(SY_node, rep(sd_noise, length(SY_node)))
  } else if (is.vector(sd_noise) & length(sd_noise) == length(SY_node)){
    sd_noise_SY <- hash(SY_node, sd_noise)
  }
  
  # Prepare observable data of outcome nodes
  col_pretreat <- c("ID", "ti", node_list$W)
  data_obs <-
    data_full[, c(col_pretreat, "pA", node_list$A, SY_node), with = FALSE]
  
  # Prepare g_OTR for TMLE step
  g_OTR_list <- list()
  g_OTR_list[["fair_coin"]] <- rep(0.5, sum(data_obs$ti <= t - target_tp))
  
  # Prepare a dataframe for treatment probabilities for new subjects
  pA_new_candidates <- data.frame(matrix(nrow = batch_size, ncol = length(SY_node)))
  colnames(pA_new_candidates) <- SY_node
  
  # Setup empty pA of candidate designs
  pA_candidates <- simulation$pA_candidates
  blip_pred_results <- data_new
  
  if (t %in% do_tmle_tps | simulation$step == simulation$n_steps) {
    do_OTR <- TRUE
  } else{
    do_OTR <- FALSE
  }
  
  # Estimate CATE ("blip") function for each surrogate / primary outcome
  if (node_selection %in% c("TMLE", SY_node, "fair_coin")){
    for (node in c(SY_node)) {
      # Skip fitting CATE for other nodes if node selection is pre-specified
      if (skip_if_not_prespecified){
        if ((node_selection %in% c(SY_node, "fair_coin") & node != node_selection)) next
      }
      fit_result <- fit_CATE_for_node(node, node_list, t, target_tp, dict_SY_tp, dict_ESY1, dict_ESY0,
                                      data_obs, data_new, Q_learner,
                                      col_pretreat, do_OTR = do_OTR,
                                      b_learner = b_learner, b_learner_args = b_learner_args,
                                      conf_level = conf_level, treat_func_type = treat_func_type)
      if (fit_result$fitted) {
        pA_new_candidates[[node]] <- fit_result$pA_new
      } else{
        pA_new_candidates[[node]] <- rep(0.5, batch_size)
      }
      blip_pred_results <- cbind(blip_pred_results, fit_result$blip_result)
      g_OTR_list[[node]] <- fit_result$g_OTR
    }
    pA_new_candidates[["fair_coin"]] <- 0.5
  } else if (node_selection == "naive"){
    # Get the outcome selected for the naive policy
    available_idx <- which((t - SY_tp) >= 1)
    fallback_nodes <- rev(SY_node[available_idx])
    if (all(hash::values(dict_SY_tp)>t-1)){
      candidate_selected_node <- "fair_coin"
      pA_new_candidates[["fair_coin"]] <- 0.5
    } else{
      available_tps <- which((t - SY_tp) >= 1)
      fallback_nodes <- rev(SY_node[available_tps])
      candidate_selected_node <- "fair_coin"
      pA_new <- rep(0.5, batch_size)
      for (node in fallback_nodes) {
        fit_result <- fit_CATE_for_node(node, node_list, t, target_tp, dict_SY_tp, dict_ESY1, dict_ESY0,
                                        do_OTR = do_OTR, data_obs, data_new, Q_learner,
                                        col_pretreat, b_learner = b_learner, b_learner_args = b_learner_args,
                                        conf_level = conf_level, treat_func_type = treat_func_type)
        if (fit_result$fitted) {
          pA_new_candidates[[node]] <- fit_result$pA_new
          blip_pred_results <- cbind(blip_pred_results, fit_result$blip_result)
          g_OTR_list[[node]] <- fit_result$g_OTR
          candidate_selected_node <- node
          break
        }
      }
      pA_new_candidates[["fair_coin"]] <- 0.5
    }
  } else {
    pA_new_candidates[["fair_coin"]] <- 0.5
  }
  pA_new_candidates <- cbind(data_new[, c("ti", "ID"), with = FALSE], pA_new_candidates)

  # Select the outcome to be used in different designs
  node_candidates <- c("fair_coin", SY_node)
  if (t < max(SY_tp) + 1){
    if (node_selection == "TMLE"){
      selected_node <- "fair_coin"
    } else if (node_selection == "naive"){
      selected_node <- candidate_selected_node
    } else {
      selected_node <- node_selection
    }
    print(paste("selected node at t =", t, ":", selected_node))
    tmle_result_all <- NULL
  } else {
    # Run TMLE for each candidate adaptive design
    if (((node_selection == "TMLE")|(t %in% do_tmle_tps) | (simulation$step == simulation$n_steps))){
      target_pA_candidates <- pA_candidates[ti <= t - target_tp,]
      data_obs_target <- data_obs[ti <= t - target_tp, ]
      data_full_target <- data_full[ti <= t - target_tp, ]
      target_EY1 <- data_full_target[[paste0("EY1_",target_tp)]]
      target_EY0 <- data_full_target[[paste0("EY0_",target_tp)]]
      tmle_result_all <- NULL
      for (tmle_type in c("ADSM","OTR","ATE")){
        if(tmle_type %in% c("OTR","ATE") & (!(t %in% do_tmle_tps)) & (simulation$step != simulation$n_steps)){
          next
        }
        tmle_result <- data.frame(t = t,
                                  pA_candidate = node_candidates,
                                  tmle_type = tmle_type,
                                  tmle_est = NA,
                                  se = NA,
                                  lower = NA,
                                  upper = NA,
                                  truth = NA)
        tmle_node_list <- node_list[c("W", "A", "Y")]
        learner_list <- list(A = Lrnr_mean$new(), Y = Q_learner)
        
        for (i in seq_along(node_candidates)){
          node <- node_candidates[i]
          tmle_try <- tryCatch({
            node_tp <- dict_SY_tp[[node]]
            if ((tmle_type == "ADSM" & node != "fair_coin" & all(target_pA_candidates[[node]] == target_pA_candidates[["fair_coin"]]))|
                (tmle_type == "ATE" & node != "fair_coin")){
              # If the current design is equivalent to simple non-adaptive randomization, skip re-estimation
              tmle_result[i, "tmle_est"] <- tmle_result[1, "tmle_est"]
              tmle_result[i, "se"] <- tmle_result[1, "se"]
              tmle_result[i, "lower"] <- tmle_result[1, "lower"]
              tmle_result[i, "upper"]  <- tmle_result[1, "upper"]
              tmle_result[i, "truth"]  <- tmle_result[1, "truth"]
            } else{
              # Build TMLE spec for different estimands, run TMLE fit, and record results under each candidate design
              ## Expected counterfacutal mean under a candidate adaptive design, using ADSM module
              if (tmle_type == "ADSM"){
                tmle_result[i,"truth"] <- mean(target_pA_candidates[[node]] * target_EY1 + (1- target_pA_candidates[[node]])*target_EY0)
                tmle_spec <- tmle3_Spec_ADSM$new(
                  treatment_level = 1,
                  control_level = 0,
                  g_treat = data_obs_target$pA,
                  g_adapt = target_pA_candidates[[node]]
                )
              }
              
              ## Expected mean under estimated optimal treatment rule, using ADSM module
              if (tmle_type == "OTR"){
                tmle_result[i,"truth"] <- mean(g_OTR_list[[node]]*target_EY1 + (1-g_OTR_list[[node]])*target_EY0)
                tmle_spec <- tmle3_Spec_ADSM$new(
                  treatment_level = 1,
                  control_level = 0,
                  g_treat = data_obs_target$pA,
                  g_adapt = g_OTR_list[[node]]
                )
              }
              
              ## Average treatment effect of final outcome, using ATE module
              if (tmle_type == "ATE"){
                tmle_result[i,"truth"] <- mean(target_EY1 - target_EY0)
                tmle_spec <- tmle3_Spec_ADATE$new(
                  treatment_level = 1,
                  control_level = 0,
                  g_treat = data_obs_target$pA,
                )
              }
              
              ## Define tmle task
              tmle_task <- tmle_spec$make_tmle_task(data_obs_target, tmle_node_list)
              
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
              tmle_result[i, "tmle_est"] <- tmle_fit$summary$tmle_est
              tmle_result[i, "se"] <- tmle_fit$summary$se
              tmle_result[i, "lower"] <- tmle_fit$summary$lower
              tmle_result[i, "upper"]  <- tmle_fit$summary$upper
            }}, error = function(msg){
              print(msg)
            }
          )
        }
        tmle_result_all <- rbind(tmle_result_all, tmle_result)
      }
    } else{
      tmle_result_all <- NULL
    }
    
    # For TMLE-based design selection, choose the design with the highest lower bound
    if (node_selection == "TMLE"){
      tmle_selection_df <- tmle_result_all[tmle_result_all$tmle_type == "ADSM",]
      which_outcome_to_select <- which(tmle_selection_df$lower == max(tmle_selection_df$lower, na.rm = TRUE))
      if (length(which_outcome_to_select) == 0){
        selected_node <- "fair_coin"
      } else{
        selected_node <- tmle_selection_df[head(which_outcome_to_select, 1), "pA_candidate"]
      }
    } else if (node_selection == "naive"){
      selected_node <- candidate_selected_node
    } else {
      selected_node <- node_selection
    }
  }

  # Generate new data to be observed
  pA_new <- pA_new_candidates[[selected_node]]
  data_new_obs <- data_new
  data_new_obs$pA <- pA_new
  data_new_obs$A <- sapply(pA_new, rbinom, n = 1, size = 1)
  for (node in SY_node){
    node_SY1 <- dict_SY1[[node]]
    node_SY0 <- dict_SY0[[node]]
    node_ESY1 <- dict_ESY1[[node]]
    node_ESY0 <- dict_ESY0[[node]]
    data_new_obs[[node_SY1]] <-
      sapply(data_new_obs[[node_ESY1]], rnorm, n = 1, sd = sd_noise_SY[[node]])
    data_new_obs[[node_SY0]] <-
      sapply(data_new_obs[[node_ESY0]], rnorm, n = 1, sd = sd_noise_SY[[node]])
    
    if (length(bound)==2){
      print(paste("bound:", paste0(bound,collapse = ",")))
      data_new_obs[[node_SY1]] <- pmax(bound[1],pmin(data_new_obs[[node_SY1]], bound[2]))
      data_new_obs[[node_SY0]] <- pmax(bound[1],pmin(data_new_obs[[node_SY0]], bound[2]))
      print(paste("\n max:",max(c(data_new_obs[[node_SY1]],data_new_obs[[node_SY0]]))))
      print(paste("\n min:",min(c(data_new_obs[[node_SY1]],data_new_obs[[node_SY0]]))))
      if (max(data_new_obs[[node_SY1]]) > bound[2]|min(data_new_obs[[node_SY1]]) < bound[1]|max(data_new_obs[[node_SY0]]) > bound[2]|min(data_new_obs[[node_SY0]]) < bound[1]){
        stop ("Running out of Y bound!")
      }
    }
    
    data_new_obs[[node]] <-
      data_new_obs$A * data_new_obs[[node_SY1]] +
      (1 - data_new_obs$A) * data_new_obs[[node_SY0]]
  }
  simulation$data_full <- rbind(simulation$data_full, data_new_obs)
  simulation$pA_candidates <- rbind(pA_candidates, pA_new_candidates)
  simulation$selected_nodes <- c(simulation$selected_nodes, selected_node)
  return (list(tmle_result = tmle_result_all, blip_pred_results = blip_pred_results))
}
