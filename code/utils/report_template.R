## Plot true CATE functions
data_plot <- results_subset[[1]]$data_full %>% dplyr::select(starts_with(c("ID", "W", "EY")))
data_plot <- DF.to.longDT(data_plot) %>% mutate(outcome = paste0("Y", t, ifelse(t == Y_tp, " (Final Outcome)", " (Surrogate Outcome)")))
if (grepl("d1", sim_name)){
  CATE_plot <- ggplot(data_plot, aes(x = W, y = EY1 - EY0, color = outcome)) +
    scale_color_manual(values = ann_colors,
                       labels = plot_outcome_labels,
                       breaks = names(ann_colors)) +
    xlab("Baseline Covariate") +
    ylab("Conditional Average Treatment Effect") +
    guides(color = guide_legend(title = "Outcome")) +
    geom_line(linewidth = 2)

  ## Figure 1
  pdf(file.path(imageDir, "CATE_plot.pdf"))
  print(CATE_plot)
  dev.off()
} else if (grepl("realSim", sim_name)){
  data_sim <- read.csv("data/realSimData/realSim_data.csv")
  data_sim_long = data_sim %>%
    pivot_longer(
      cols = starts_with("EY"),
      names_to = c("Arm", "Outcome"),
      names_pattern = "EY(\\d)_(\\d+)"
    ) %>%
    pivot_wider(
      names_from = Arm,
      values_from = value,
      names_prefix = "EY"
    ) %>%
    mutate(
      CATE = EY1 - EY0,
      Outcome = paste0("Y", Outcome),
      W1=case_when(
        W1 == 1 ~ "SMS",
        W1 == 2 ~ "SOC",
        W1 == 3 ~ "CCT")
    ) %>%
    mutate(W1 = factor(W1, levels = c("SMS", "SOC", "CCT"))) %>%
    select(ID, ti, W1, W2, Outcome, EY1, EY0, CATE)

  ## Figure S10
  CATE_plot <-
    ggplot(data_sim_long,
           aes(x = W2,
               y = CATE,
               color = Outcome)) +
    geom_line(aes(x = W2, y = CATE),
              linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    facet_grid(~W1, scales = "fixed") +
    labs(
      x = "Time to Lapse After Initial Treatment",
      y = "CATE") +
    guides(colour = guide_legend(title = "Outcome")) +
    theme(legend.position = "top") +
    theme_bw() +
    scale_colour_viridis_d(
      direction = -1,
      labels = c(
        expression(Y[1]), expression(Y[2]), expression(Y[3]), expression(Y[4]), expression(Y[5])
      )
    )
  pdf(file.path(imageDir, "CATE_plot.pdf"))
  print(CATE_plot)
  dev.off()
}

# Configurations of design labels and colors
plot_names_of_designs <- c(
  sapply(S_tp, function(i)
    bquote("Adaptive design (based on surrogate outcome " * Y[.(i)] * ")")
  ),
  bquote("Adaptive design (based on final outcome " * Y[.(Y_tp)] * ")"),
  expression("Non-adaptive randomization"),
  expression("Adaptive design (TMLE-OSLAD)"),
  "Adaptive design (based on the naive policy)"
)

ann_colors_Y_k_and_faircoin <- ann_colors_Y_k
names(ann_colors_Y_k_and_faircoin)[length(SY_tp)+1] <- "fair_coin"

est_tmle_idx <- sapply(results_subset, function(result) {return(ifelse(grepl("TMLE", unique(result$report$estimator_name)), TRUE, FALSE))})
M_rep <- length(results_subset[est_tmle_idx])
EYK1_node <- paste0("EY1_",Y_tp)
EYK0_node <- paste0("EY0_",Y_tp)

EYK_proposed_design_list <- rep(NA, M_rep)
summary_value_of_proposed_design_all <- data.frame(NULL)

# Summarize TMLE-OSLAD results and target estimand
oslad_candidate_designs <- c("fair_coin",paste0("Y_", SY_tp))
oslad_plot_names_of_designs <- c(
  expression("Non-adaptive randomization"),
  sapply(S_tp, function(i)
    bquote("Adaptive design (based on surrogate outcome " * Y[.(i)] * ")")
  ),
  bquote("Adaptive design (based on final outcome " * Y[.(Y_tp)] * ")"),
  expression("Adaptive design (TMLE-OSLAD)"),
  "Adaptive design (based on the naive policy)"
)

for (tmleType in c("ADSM", "OTR", "ATE")){
  tmlefit_truePsi_M <- data.frame(NULL)
  for (m in 1:M_rep) {
    if (tmleType == "ADSM"){
      data_full_proposed_design <- results_subset[est_tmle_idx][[m]]$data_full
      max_ti <- max(data_full_proposed_design$ti)
      EYK_proposed_design_list[m] <- mean(data_full_proposed_design$A * data_full_proposed_design[[EYK1_node]] +
                                            (1 -data_full_proposed_design$A) * data_full_proposed_design[[EYK0_node]])

      data_full_proposed_design$value_of_proposed_design <-
        data_full_proposed_design$A * data_full_proposed_design[[EYK1_node]] +
        (1 -data_full_proposed_design$A) * data_full_proposed_design[[EYK0_node]]
      summary_value_of_proposed_design <- data_full_proposed_design %>%
        arrange(ti) %>%
        mutate(cummean_value_of_proposed_design = cummean(value_of_proposed_design)) %>%
        group_by(ti) %>%
        summarize(value_of_proposed_design_from_1_to_ti = cummean_value_of_proposed_design[row_number() == n()]) %>%
        mutate(t = ti + Y_tp)
      summary_value_of_proposed_design_all <- rbind(summary_value_of_proposed_design_all, summary_value_of_proposed_design)
      rm(data_full_proposed_design)
      rm(summary_value_of_proposed_design)
    }
    pA_candidates <- results_subset[est_tmle_idx][[m]]$pA_candidates
    tmlefit <- results_subset[est_tmle_idx][[m]]$tmle_fit %>%
      mutate(ti = t - Y_tp) %>%
      filter(ti <= t_end) %>%
      filter(tmle_type == tmleType)
    selected_outcomes <- as.character(results_subset[est_tmle_idx][[m]]$selected_nodes[grepl("time_",names(results_subset[est_tmle_idx][[m]]$selected_nodes))][1:t_end])
    tmlefit_truePsi <- tmlefit %>%
      mutate(pA_candidate = factor(pA_candidate, levels = oslad_candidate_designs)) %>%
      mutate(cover = (truth <= upper & truth >= lower), M = m) %>%
      mutate(truePsi = truth) %>%
      filter(!is.na(tmle_est))

    tmlefit_truePsi_M <- rbind(tmlefit_truePsi_M, tmlefit_truePsi)

    if (grepl("realSim", sim_name)){
      time_to_plot <- c(11,15,19)
    } else{
      time_to_plot <- c(11,21,31,41,50)
    }
    simulation_name <- unique(tmlefit_truePsi$simulation_name)
    if ((tmleType == "ADSM" & sim_name == "DGD_hetero_d1") & ((simulation_name == "scenario_1" & m == 1) | (tmleType == "ADSM" & simulation_name == "scenario_2" & m == 112))){
      time_to_plot <- c(11,21,31,41,50)
      tmlefit_truePsi_forplot <-
        tmlefit_truePsi %>%
        filter(t %in% time_to_plot & ti <= t_end) %>%
        mutate(position = t+1*(as.numeric(as.factor(pA_candidate))-1)) %>%
        group_by(t) %>%
        mutate(selected = lower == max(lower)) %>%
        ungroup()

      p_tmlefit_all <-
        ggplot(tmlefit_truePsi_forplot, aes(x = t, y = tmle_est, color = pA_candidate)) +
        geom_point(aes(x = position, y = tmle_est)) + # x: estimate
        geom_errorbar(aes(x = position, ymin = lower, ymax = upper),
                      linewidth = 0.9, linetype = 1, alpha = 1) +
        geom_point(
          data = subset(tmlefit_truePsi_forplot, selected),
          aes(x = position, y = tmle_est, color = pA_candidate),
          shape = 21,
          size = 4,
          stroke = 1,
          fill = NA,
          alpha = 1,
          show.legend = FALSE
        ) +
        xlab("Time t") +
        ylab(TeX("$\\hat{\\psi}_{t,k}$")) +
        # ylab(TeX("$\\psi_{t,k}$")) +
        scale_x_continuous(breaks = time_to_plot)+
        ggtitle("TMLE Estimates") +
        theme(legend.position = "bottom") +
        scale_color_manual(values = ann_colors_Y_k_and_faircoin,
                           labels = oslad_plot_names_of_designs) +
        guides(color = guide_legend(nrow=3, title = "Candidate Designs", title.position="top"))

    ## Figure 2c, Figure 2d: Illustrative example of TMLE selection mechanism
    pdf(file.path(imageDir, sprintf("TMLE_output_example-%s.pdf", tmleType)), height = 9, width = 10)
    print(p_tmlefit_all)
    dev.off()
    }
  }

  tmlefit_coverage <- tmlefit_truePsi_M %>%
    group_by(t, ti, pA_candidate) %>%
    summarise(truePsi_sd = sd(truth)/sqrt(M_rep),
              truePsi = mean(truth),
              bias = mean(tmle_est - truePsi),
              variance = sd(tmle_est)^2,
              mse = mean((tmle_est - truePsi)^2),
              estimated_se = mean(se),
              oracle_se = sd(tmle_est),
              coverage = mean(cover),
              coverage_sd = sd(cover)/sqrt(M_rep))

  ## Table 1, Table S2, Table S3 (`tmleType` == "ADSM"); and
  ## Table 2, Table S6 (`tmleType` == "OTR" or "ATE")
  ## (except OTR of Y5 that uses CV TMLE)
  write.csv(tmlefit_coverage %>% mutate_if(is.numeric, ~ signif(.,3)),
            file.path(tableDir, sprintf("TMLE_coverage-%s.csv", tmleType)))
}



# CV-TMLE results for OTR of final outcome Y5
cv_tmle_OTR_files <- dir(OTR_report_Dir, pattern = "CV_TMLE.*.csv", full.names = TRUE, recursive = T)
if (length(cv_tmle_OTR_files) != 0){
  cv_tmle_OTR_files <- cv_tmle_OTR_files[grepl(dir_name,cv_tmle_OTR_files) & grepl(sim_spec_name,cv_tmle_OTR_files)]
  cv_tmle_OTR <- lapply(cv_tmle_OTR_files, read.csv) %>% bind_rows()
  cv_tmle_OTR_coverage <- cv_tmle_OTR %>%
    group_by(estimator) %>%
    mutate(cover = (truth <= upper & truth >= lower)) %>%
    mutate(truePsi = truth) %>%
    group_by(estimator) %>%
    summarise(truePsi_se = sd(truth)/sqrt(M_rep),
              truePsi = mean(truth),
              bias = mean(tmle_est - truePsi),
              variance = sd(tmle_est)^2,
              mse = mean((tmle_est - truePsi)^2),
              estimated_se = mean(se),
              oracle_se = sd(tmle_est),
              coverage = mean(cover),
              coverage_sd = sd(cover)/sqrt(M_rep))
  
  ## Table 2: OTR of Y5 using CV TMLE
  write.csv(cv_tmle_OTR_coverage %>% mutate_if(is.numeric, ~ signif(.,3)),
            file.path(tableDir, "CV_TMLE_coverage-OTR_of_Final_Outcome.csv"))
}

# Figure 3b: Expected final outcome when implementing different designs
pdata <- value_plot_multiple_runs(results_subset)
table <- pdata[["table"]] %>% mutate_if(is.numeric, ~ signif(.,5))
write.csv(table, file.path(tableDir, "Expected_Final_Outcome_of_Different_Designs_from_1_to_ti.csv"))

p <- pdata[["plot"]]
pdf(file.path(imageDir, "Expected_Final_Outcome_of_Different_Designs_from_1_to_ti.pdf"))
print(p)
dev.off()

# Frequency of design selection in TMLE_OSLAD
pdata <- selected_surrogates_multiple_runs(
  results_subset[est_tmle_idx], start_ti = 7, ti_end = t_end
)
table <- pdata[["table"]] %>% mutate_if(is.numeric, ~round(.,2))
write.csv(table, file.path(tableDir,"Frequency_of_outcomes_selected_by_SL.csv"))

## Figure 2a, Figure 2b, Figure 3a, Figure S9
p <- pdata[["plot"]]
if (!grepl("realSim", sim_name)){
  pdf(file.path(imageDir, "Frequency_of_outcomes_selected_by_OSLAD.pdf"), height = 10, width = 10)
} else {
  pdf(file.path(imageDir, "Frequency_of_outcomes_selected_by_OSLAD.pdf"), height = 8, width = 9)
}
print(p)
dev.off()

# Frequency of design selection in TMLE_OSLAD using different TMLE confidence
# interval levels
pdata <- selected_surrogates_with_different_alphas(
  results_subset[est_tmle_idx], start_ti = 7, ti_end = t_end
)
table <- pdata[["table"]] %>% mutate_if(is.numeric, ~round(.,2))
write.csv(table, file.path(tableDir,"Frequency_of_outcomes_selected_by_different_tmle_alphas.csv"))

## Figure S2, Figure S3
p <- pdata[["plot"]]
pdf(file.path(imageDir, "Frequency_of_outcomes_selected_by_different_tmle_alphas_barplots.pdf"))
print(p)
dev.off()


# Probability of not choosing the optimal treatment
## Table S4
pdata <- regret_plot_multiple_runs(results_subset, "hamming_loss")
table <- pdata[["table"]] %>% mutate_if(is.numeric, ~ round(.,3))
write.csv(table, file.path(tableDir, "Prob_Suboptimal_Arm_at_t.csv"))

p <- pdata[["plot"]] +
  ylab("Probability") +
  ggtitle("Probability of not choosing the optimal treatment at time t")
pdf(file.path(imageDir, "Prob_Suboptimal_Arm_at_t.pdf"))
print(p)
dev.off()

# Regret plot for ADAPT-R simulation
## Table S5
pdata <- regret_plot_multiple_runs(results_subset, "regret_avg")
reg_table <- pdata[["table"]] %>% mutate_if(is.numeric, ~ round(.,3))
write.csv(reg_table, file.path(tableDir, "Regret_at_t.csv"))

if (grepl("DGD_realSim", sim_name)){
  regret_nonadaptive <- reg_table %>% filter(ti==t_end,estimator_name=="Non-adaptive randomization") %>% pull(mean)
  design_linetype <- design_colors
  design_alpha <- design_colors
  design_linetype[names(design_linetype) == "Adaptive design (TMLE-OSLAD)"] <- "solid"
  design_linetype[names(design_linetype) != "Adaptive design (TMLE-OSLAD)"] <- "dashed"
  design_alpha[names(design_alpha) != "Adaptive design (TMLE-OSLAD)"] <- 0.8
  design_alpha[names(design_alpha) == "Adaptive design (TMLE-OSLAD)"] <- 1
  design_alpha <- as.numeric(design_alpha)
  names(design_alpha) <- names(design_colors)

  ## Figure S11
  p_reg <- ggplot(reg_table) +
    geom_line(aes(x = ti, y = mean,
                  color    = estimator_name,
                  linetype = estimator_name,
                  alpha    = estimator_name),
              linewidth = 1) +
    xlab("Time t") +
    ylab("Regret") +
    coord_cartesian(ylim = c(0, 0.1), clip = "off") +
    annotate("text",
             x     = 19,
             y     = 0.09,
             label = sprintf("Regret of non-adaptive \n randomization: %s", regret_nonadaptive),
             color = "#A58AFF",
             hjust = 0.2,
             vjust = -0.1,
             size  = 3) +
    scale_color_manual(values = design_colors,
                       breaks = names(design_colors),
                       labels = plot_names_of_designs,
                       drop   = FALSE) +
    scale_linetype_manual(values = design_linetype,
                          breaks = names(design_linetype),
                          labels = plot_names_of_designs,
                          drop   = FALSE) +
    scale_alpha_manual(values = design_alpha,
                       breaks = names(design_alpha),
                       labels = plot_names_of_designs,
                       drop   = FALSE) +
    guides(
      color    = guide_legend(ncol = 2, title = "Designs", title.position="top"),
      linetype = guide_legend(ncol = 2, title = "Designs", title.position="top"),
      alpha    = guide_legend(ncol = 2, title = "Designs", title.position="top")
    ) +
    theme_minimal(base_size = 15) +
    theme(legend.position = "bottom",
          plot.margin = margin(5, 40, 5, 5))
  pdf(file.path(imageDir, "Regret_at_t.pdf"), width = 16, height = 8)
  print(p_reg)
  dev.off()
} else{
  ## Figure S2
  p_reg <- pdata[["plot"]] +
    ylab("Regret") +
    ggtitle("Regret at time t")
  pdf(file.path(imageDir, "Regret_at_t.pdf"))
  print(p_reg)
  dev.off()
}


