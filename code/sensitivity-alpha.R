# Sensitivity analysis of the choice of alpha in CATE CI
library(dplyr)
library(here)
library(hash)
here()
sim_name <- "DGD_hetero_d1"
dir_name <- "t_end_50_batch_size_50_n_runs_500"

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

design_levels <- c(paste0(
  rep("Adaptive design (based on surrogate Y", length(S_tp)), S_tp, ")"),
  "Adaptive design (based on final outcome Y5)",
  "Adaptive design (TMLE-OSLAD)",
  "Non-adaptive randomization")

pdf.options(width = 10, height = 8)

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
oslad_designs <- c(
  "Non-adaptive randomization", 
  paste0(rep("Adaptive design (based on surrogate Y", length(S_tp)), S_tp, ")"),
  "Adaptive design (based on final outcome Y5)")

design_colors <- setNames(design_colors, 
                          c(design_levels[1:length(SY_tp)],
                            "Non-adaptive randomization",
                            "Adaptive design (TMLE-OSLAD)",
                            "Adaptive design (based on the naive policy)"))

plot_names_of_designs <- c(
  sapply(S_tp, function(i) 
    bquote("Adaptive design (based on surrogate outcome " * Y[.(i)] * ")")
  ),
  bquote("Adaptive design (based on final outcome " * Y[.(Y_tp)] * ")"),
  expression("Non-adaptive randomization"),
  expression("Adaptive design (TMLE-OSLAD)"),
  "Adaptive design (based on the naive policy)"
)

# Figure S4, S5: Frequency of outcomes selected by different CATE CI levels
for (sim_spec_name in c("scenario_1","scenario_2")){
  freq_table_all <- data.frame(NULL)
  treat_func_type <- "default"
  folder_names <- c("DGD_hetero_d1", "DGD_hetero_d1-conf-0.9", "DGD_hetero_d1-conf-0.99")
  for (folder_name in folder_names){
    tableDir <- here(file.path("output/Tables", folder_name, treat_func_type, dir_name, sim_spec_name))
    file_name <- list.files(tableDir, pattern = "Frequency_of_outcomes_selected_by_SL", recursive = TRUE)
    print(file_name)
    freq_table <- read.csv(file.path(tableDir, file_name))
    freq_table$treat_func_type <- treat_func_type
    freq_table$conf <- case_when(
      folder_name == "DGD_hetero_d1" ~ "0.95",
      folder_name == "DGD_hetero_d1-conf-0.9" ~ "0.90",
      folder_name == "DGD_hetero_d1-conf-0.99" ~ "0.99",
    )
    freq_table <- freq_table %>% 
      filter(ti >= 7) %>% 
      mutate(Freq = n/500) %>% 
      mutate(selected_outcome = factor(selected_outcome, levels = oslad_designs))
    freq_table_all <- bind_rows(freq_table_all, freq_table)
  }
  
  conf_label_mapping <- c(
    "0.90" = "CI Level: 90%",
    "0.95" = "CI Level: 95%",
    "0.99" = "CI Level: 99%"
  )
  
  p <- ggplot(freq_table_all, aes(x = ti, y = Freq, fill = selected_outcome)) +
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
    facet_wrap(~ conf, 
               labeller = as_labeller(conf_label_mapping),
               nrow = 1) +
    labs(x = "Time t", y = "Frequency", fill = "Candidate Designs") +
    theme_bw() +
    theme(legend.position = "bottom")
  
  imageDir <- file.path("output/Images", sim_name, "cate_alpha_sensitivity" , dir_name, sim_spec_name)
  tableDir <- file.path("output/Tables", sim_name, "cate_alpha_sensitivity" , dir_name, sim_spec_name)
  
  if (!dir.exists(imageDir)) dir.create(imageDir, recursive = T)
  if (!dir.exists(tableDir)) dir.create(tableDir, recursive = T)
  
  write.csv(table, file.path(tableDir,"Frequency_of_outcomes_selected_by_different_cate_alphas.csv"))
  
  pdf(file.path(imageDir, "Frequency_of_outcomes_selected_by_different_cate_alphas_barplots.pdf"), width = 12, height = 6)
  print(p)
  dev.off()

}

