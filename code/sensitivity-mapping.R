# Sensitivity analysis on different mapping functions from CATE to treatment probability
library(dplyr)
library(here)
source(here("code/utils/CATE_helper.R"))
here()
sim_name <- "DGD_hetero_d1"
dir_name <- "t_end_50_batch_size_50_n_runs_500"


# Set follow-up time of different outcomes
S_tp <- c(1,2,3,4)
Y_tp <- 5
SY_tp <- c(S_tp,Y_tp)
n_S <- length(S_tp)
n_SY <- length(SY_tp)
ann_colors <- c(scales::hue_pal()(length(SY_tp)+2), "#808080")
ann_colors <- setNames(ann_colors, c(paste0("Y", S_tp," (Surrogate Outcome)"), "Y5 (Final Outcome)", "fair_coin", "TMLE-OSLAD", "naive"))
ann_colors_Y_k <- scales::hue_pal()(length(SY_tp))
ann_colors_Y_k <- setNames(ann_colors, c(paste0("Y_", SY_tp)))

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


# Figure S7 and Figure S8
for (sim_spec_name in c("scenario_1","scenario_2")){
  freq_table_all <- data.frame(NULL)
  for (treat_func_type in c("default","logistics","mild_inv_S","sharp_inv_S")){
    print(treat_func_type)
    tableDir <- file.path("output/Tables", sim_name, treat_func_type, dir_name, sim_spec_name)
    file_name <- list.files(tableDir, pattern = "Frequency_of_outcomes_selected_by_SL")
    print(file_name)
    freq_table <- read.csv(file.path(tableDir, file_name))
    freq_table$treat_func_type <- treat_func_type
    freq_table <- freq_table %>% 
      filter(ti >= 7) %>% 
      mutate(Freq = n/500) %>% 
      mutate(selected_outcome = factor(selected_outcome, levels = oslad_designs))
    freq_table_all <- bind_rows(freq_table_all, freq_table)
  }
  
  func_label_mapping <- c(
    "default"     = "h[nu]^{(0)}",
    "logistics"   = "h[nu]^{(1)}",
    "mild_inv_S"  = "h[nu]^{(2)}",
    "sharp_inv_S" = "h[nu]^{(3)}"
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
    facet_wrap(~ treat_func_type, 
               labeller = as_labeller(func_label_mapping, label_parsed),
               nrow = 1) +
    labs(x = "Time t", y = "Frequency", fill = "Candidate Designs") +
    theme_bw() +
    theme(legend.position = "bottom")
  
  imageDir <- file.path("output/Images", sim_name, "treat_func_sensitivity" , dir_name, sim_spec_name)
  if (!dir.exists(imageDir)) dir.create(imageDir, recursive = T)
  
  pdf(file.path(imageDir, "Frequency_of_outcomes_selected_by_different_treat_funcs_barplots.pdf"), width = 12, height = 6)
  print(p)
  dev.off()
}


# Plot mapping functions
fit <- seq(-1.5 * 1.96, 1.5 * 1.96, length.out = 100)
se.fit <- rep(1,100)

# Evaluate the four mappings
f0 <- treatByCATE(fit, se.fit, treat_func_type = "default")
f1 <- treatByCATE(fit, se.fit, treat_func_type = "logistics")
f2 <- treatByCATE(fit, se.fit, treat_func_type = "mild_inv_S")
f3 <- treatByCATE(fit, se.fit, treat_func_type = "sharp_inv_S")
df_plot <- data.frame(
  fit = rep(fit, times = 4),
  probability = c(f0, f1, f2, f3),
  Function = factor(rep(
    c("f^{(0)}(z)", "f^{(1)}(z)", "f^{(2)}(z)", "f^{(3)}(z)"),
    each = length(fit)
  ))
)

# Make the plot (Figure S9)
p_mappings <- ggplot(df_plot, aes(x = fit/1.96*se.fit, y = probability, color = Function)) +
  geom_line(linewidth = 1.2) +
  theme_minimal() +
  labs(
    x = "z",
    y = "P(A=1)",
    color = "Function"
  ) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  theme(
    text = element_text(size = 14)
  ) +
  scale_color_discrete(
    labels = c(
      expression(h[nu]^{(0)}),
      expression(h[nu]^{(1)}),
      expression(h[nu]^{(2)}),
      expression(h[nu]^{(3)})
    )
  ) +
  scale_y_continuous(breaks=c(0.10,0.25,0.50,0.75,0.90))

imageDir <- file.path("output/Images", sim_name, "treat_func_sensitivity" , dir_name)
pdf(file.path(imageDir, "Different_treat_funcs.pdf"))
print(p_mappings)
dev.off()

