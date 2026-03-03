se <- function(x) {sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))}

regret_plot <- function(report, regret_name, ti_end = t_end){
  n_blip_learner <- length(unique(report$blip_learner))
  report <- report %>%
    mutate(blip_learner = paste0("blip_learner: ", ifelse(grepl("ols", estimator_name), "ols", "hal"))) %>%
    filter(ti <= ti_end)
  out <- by(data = report, INDICES = report$blip_learner, FUN = function(m) {
    ggplot(m, aes_string(x = "ti", y = regret_name, color = "estimator_name")) +
      geom_line() +
      facet_wrap(~ blip_learner, ncol = 2) +
      ggtitle(paste0("Regret: ", regret_name)) +
      theme(legend.position = "bottom") +
      guides(color = guide_legend(nrow=2, byrow=TRUE, title.position="top"))
  })
  do.call(grid.arrange, c(out, ncol = n_blip_learner))
}


value_plot_multiple_runs <- function(result){
  value_name <- "psi"
  data_full_list <- lapply(1:length(result), FUN = function(i) {
    df <- result[[i]]$data_full
    df$round <- i
    return (df)
  })
  
  data_full <- bind_rows(data_full_list)
  report <- data_full %>%
    filter(ti <= t_end) %>%
    mutate(blip_learner = paste0("blip_learner: ", ifelse(grepl("ols", estimator_name), "ols", "hal")))
  value <- report %>%
    group_by(round, blip_learner, estimator_name) %>%
    arrange(round, blip_learner, estimator_name, ti) %>%
    mutate(psi = cummean(.data[[EYK1_node]]*pA + .data[[EYK0_node]]*(1-pA))) %>%
    group_by(round, blip_learner, estimator_name, ti) %>%
    filter(row_number() == n()) %>%
    group_by(ti, blip_learner, estimator_name) %>%
    summarise_at(value_name, list(mean = mean, se = se)) %>%
    mutate(t = ti + Y_tp)
  
  n_blip_learner <- length(unique(report$blip_learner))
  value$estimator_name = sapply(value$estimator_name,
                                FUN = function(x){values(est_dict, x)})
  
  design_linetype <- design_colors
  design_alpha <- design_colors
  design_linetype[names(design_linetype) == "Adaptive design (TMLE-OSLAD)"] <- "solid"
  design_linetype[names(design_linetype) != "Adaptive design (TMLE-OSLAD)"] <- "dashed"
  design_alpha[names(design_alpha) == "Adaptive design (TMLE-OSLAD)"] <- 1
  design_alpha[names(design_alpha) != "Adaptive design (TMLE-OSLAD)"] <- 0.75
  design_alpha <- as.numeric(design_alpha)
  
  value_adaptive <- value[value$estimator_name == "Adaptive design (TMLE-OSLAD)", ]
  value_non_adaptive <- value[value$estimator_name != "Adaptive design (TMLE-OSLAD)", ]
  
  p <- ggplot() +
    geom_line(data = value_non_adaptive, aes(x = ti, y = mean, color = estimator_name, linetype = estimator_name, alpha = estimator_name), linewidth = 1) +
    geom_line(data = value_adaptive, aes(x = ti, y = mean, color = estimator_name, linetype = estimator_name, alpha = estimator_name), linewidth = 1) +
    xlab("Time t") +
    ylab("Expected Final Outcome") +
    ggtitle("Expected Final Outcome under Different Adaptive Designs") +
    scale_color_manual(values = design_colors, 
                       breaks = names(design_colors), 
                       labels = plot_names_of_designs,
                       drop = FALSE) +
    scale_linetype_manual(values = design_linetype, 
                          labels = plot_names_of_designs,
                          breaks = names(design_linetype), drop = FALSE) +
    scale_alpha_manual(values = design_alpha,
                       labels = plot_names_of_designs,
                       breaks = names(design_alpha), drop = FALSE) +
    guides(color = guide_legend(ncol = 2, byrow=FALSE, title = "Designs", title.position="top")) +
    guides(linetype = guide_legend(ncol = 2, byrow=FALSE, title = "Designs", title.position="top")) +
    guides(alpha = guide_legend(ncol = 2, byrow=FALSE, title = "Designs", title.position="top")) +
    theme(legend.position = "bottom")
  out <- list(plot = p, table = value)
  return (out)
}


regret_plot_multiple_runs <- function(result, regret_name, ti_start = 1, ti_end = t_end){
  report <- Reduce(rbind, lapply(result, `[[`, "report"))
  report <- report %>%
    mutate(blip_learner = paste0("blip_learner: ", ifelse(grepl("ols", estimator_name), "ols", "hal")))
  regret <- report %>%
    group_by(ti, blip_learner, estimator_name) %>%
    summarise_at(regret_name, list(mean = mean, se = se)) %>%
    filter(ti <= ti_end & ti >= ti_start)
  
  n_blip_learner <- length(unique(report$blip_learner))
  regret$estimator_name = sapply(regret$estimator_name,
                                 FUN = function(x){values(est_dict, x)})
  
  
  p <- ggplot(regret, aes_string(x = "ti", y = "mean", fill = "estimator_name")) +
    geom_line(aes(color = estimator_name), size = 1) +
    geom_ribbon(data = regret,
                aes(x = ti,
                    ymin = mean - 1.96*se,
                    ymax = mean + 1.96*se),
                alpha = 0.3) +
    ggtitle(paste0("Regret: ", regret_name)) +
    xlab("Time t") +
    scale_color_manual(values = design_colors, 
                       breaks = names(design_colors), 
                       labels = plot_names_of_designs,
                       drop = FALSE) +
    scale_fill_manual(values = design_colors,
                      labels = plot_names_of_designs,
                      breaks = names(design_colors), drop = FALSE) +
    guides(color = guide_legend(ncol = 2, byrow=FALSE, title = "Designs", title.position="top")) +
    guides(fill = guide_legend(ncol = 2, byrow=FALSE, title = "Designs", title.position="top")) +
    theme(legend.position = "bottom")
  out <- list(plot = p, table = regret)
  return (out)
}

tmle_fit_coverage <- function(result, baselines){
  tmle_all <- Reduce(rbind, lapply(results_subset[est_tmle_idx], `[[`, "tmle_fit"))
  tmle_fit_dt <- tmle_all %>%
    inner_join(baselines, by = "pA_candidate")
  coverage_result <- tmle_fit_dt %>%
    group_by(t, pA_candidate) %>%
    summarize(coverage = mean(lower <= baseline & upper >= baseline))
  return (coverage_result)
}

extract_selected_surrogates <- function(result){
  selected_surrogates <- data.frame(t(result$selected_nodes %>% select(starts_with("time"))))
  colnames(selected_surrogates) <- "selected_surrogate"
  selected_surrogates$ti <- 1:nrow(selected_surrogates)
  selected_surrogates$estimator_name <- unique(result$selected_nodes$estimator_name)
  return (selected_surrogates)
}

selected_surrogates_multiple_runs <- function(result, start_ti, ti_end = t_end) {
  all_adaptive_designs <-
    colnames(result[[1]]$pA_candidates)[grep("Y_|fair_coin", colnames(result[[1]]$pA_candidates))]
  selected_surrogates <-
    Reduce(rbind, lapply(result, extract_selected_surrogates))
  freq_surrogates <- selected_surrogates %>%
    count(ti, selected_surrogate, estimator_name) %>%
    group_by(ti, estimator_name) %>%
    mutate(Freq = prop.table(n)) %>%
    data.frame() %>%
    filter(ti <= ti_end)
  
  freq_surrogates$selected_outcome <-
    factor(sapply(
      freq_surrogates$selected_surrogate,
      FUN = function(x) {
        values(design_dict, x)
      }
    ),
    levels = as.character(values(design_dict)))
  
  p <-
    ggplot(freq_surrogates %>% filter(ti >= start_ti),
           aes(x = ti, y = Freq, fill = selected_outcome)) +
    geom_bar(position = "fill", stat = "identity") +
    scale_fill_manual(values = design_colors,
                      breaks = names(design_colors), 
                      labels = plot_names_of_designs,
                      drop = TRUE) +
    xlab("Time t") +
    ylab("Frequency") +
    theme(legend.position = "bottom") +
    guides(
      fill = guide_legend(
        ncol = 2,
        byrow = FALSE,
        title = "Candidate Designs",
        title.position = "top"
      )
    ) +
    ggtitle("Frequency of adaptive designs selected by TMLE-OSLAD")
  out <- list(plot = p, table = freq_surrogates)
  return (out)
}

add_regret_c_to_end <- function(result, regret_name){
  regret_name_t <- gsub("_c_ti_to_T","", regret_name)
  result$report[[regret_name]] <- rev(cumsum(rev(result$report[[regret_name_t]])))/rev(result$report[["ti"]])
  return (result$report)
}

selected_surrogates_with_different_alphas <- function(result, start_ti, ti_end = t_end){
  TMLE_est_list <- lapply(1:length(result), FUN = function(i) {
    df <- result[[i]]$tmle_fit
    df$round <- i
    return (df)
  })
  
  dat <- bind_rows(TMLE_est_list) %>% 
    filter(t >= start_ti & t <= t_end) %>% 
    filter(tmle_type == "ADSM")

  z_levels <- c(`90` = qnorm(0.95), `95` = qnorm(0.975), `99` = qnorm(0.995))
  dat_ci <- dat %>%
    mutate(
      ci_lower_90 = tmle_est - z_levels["90"] * se,
      ci_lower_95 = tmle_est - z_levels["95"] * se,
      ci_lower_99 = tmle_est - z_levels["99"] * se
    ) %>%
    pivot_longer(
      cols = starts_with("ci_lower_"),
      names_to = "ci_level",
      values_to = "ci_lower_value"
    ) %>%
    mutate(ci_level = sub("ci_lower_", "", ci_level))
  
  dat_best <- dat_ci %>%
    group_by(estimator_name, round, t, ci_level) %>%
    slice_max(ci_lower_value, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  prob_selected <- dat_best %>%
    count(t, ci_level, pA_candidate) %>%
    group_by(t, ci_level) %>%
    mutate(prob = n / sum(n)) %>%
    ungroup() %>% 
    tidyr::complete(
      t = unique(dat_best$t),             
      ci_level = unique(dat_best$ci_level),
      pA_candidate = unique(dat_best$pA_candidate),
      fill = list(prob = 0, n = 0)   
    )
  
  prob_selected$selected_outcome <-
    factor(sapply(
      prob_selected$pA_candidate,
      FUN = function(x) {
        values(design_dict, x)
      }
    ),
    levels = as.character(values(design_dict)))
  
  p <- ggplot(prob_selected, aes(x = t, y = prob, fill = selected_outcome)) +
    geom_bar(stat = "identity", position = "stack", width = 0.9) +
    scale_fill_manual(values = design_colors,
                      breaks = names(design_colors), 
                      labels = plot_names_of_designs,
                      drop = TRUE) +
    guides(
      fill = guide_legend(
        ncol = 2,
        byrow = FALSE,
        title = "Candidate Designs",
        title.position = "top"
      )
    ) +
    facet_wrap(
      ~ ci_level,
      nrow = 1,
      labeller = labeller(ci_level = function(x) paste0("CI Level: ", x, "%"))
    ) +
    labs(x = "Time t", y = "Frequency", fill = "Candidate Designs") +
    theme_bw() +
    theme(legend.position = "bottom")

  out <- list(plot = p, table = prob_selected)
  return (out)
}




