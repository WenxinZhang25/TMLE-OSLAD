##### Prepare data to simulate ADAPT-R setup
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(here)
library(patchwork)
library(glmnet)
library(ggplot2)
library(lubridate)

## Create data folder
dataDir <- here("data/realSimData")
plotDir <- here("output/Images/realSimPlots")

if (!dir.exists(dataDir)) dir.create(dataDir, recursive = TRUE)
if (!dir.exists(plotDir)) dir.create(plotDir, recursive = TRUE)

## Prepare data
# data <- read_csv(here("../ADAPT-R/data/backbone/backbone.csv"))
# load(here("../ADAPT-R/data/classifying dates/dates_list_extend.RData"))
data <- data %>% filter(at_rerand_txt %in% c("NAVIGATOR", "SMS+VOUCHER"))
  
# Outcome data
outcome_fun = function(x_dates) {
  id_start.at = which(x_dates$START.AT == "START2/RRF")
  id_end.at = which(x_dates$END.AT == "END2/enroll + 2yr")
  if (length(id_start.at) == 0) {
    return (NULL)
  } else {
    x_dates_cut.at = x_dates[id_start.at:id_end.at,]
    rownames(x_dates_cut.at) = NULL
    x_dates_surrogates = x_dates_cut.at %>%
      mutate(tic_time = row_number()) %>%
      mutate(prop_tic = cumsum(classification == "in care")/(tic_time-cumsum(!(classification %in% c("in care","out of care"))))) %>%
      mutate(rerand_date = x_dates[id_start.at, "date"],
             surrogate_date = date,
             final_date = x_dates[id_end.at, "date"],
             S_tp = as.numeric(date - rerand_date)+1,
             Y_tp = as.numeric(final_date - rerand_date)+1) %>%
      select(c("ID", "rerand_date", "surrogate_date",
               "final_date", "S_tp", "Y_tp", "tic_time", "prop_tic", "rerand_date"))
  }
  return(x_dates_surrogates)
}

surrogates = lapply(dates_list_13, outcome_fun)
all_surrogates = do.call("rbind", surrogates)

time_points_of_interest <- seq(50,250,50)
final_outcome_of_interest <- "prop_tic_250"
surrogates_of_interest <- all_surrogates %>%
  filter(tic_time %in% time_points_of_interest)
all_outcomes <- unique(c(paste0("prop_tic_", time_points_of_interest), final_outcome_of_interest))

data_sim <- data %>%
  filter(yr2_1_vl_final != "missing") %>%
  left_join(surrogates_of_interest, by = "ID") %>%
  pivot_wider(names_prefix = "prop_tic_", names_from = tic_time, values_from = prop_tic) %>%
  mutate(ti = as.numeric(rerand_date-min(rerand_date))+1) %>%
  as.data.frame() %>%
  arrange(rerand_date)

# Fit outcome models
outcome_fits <- list()
for (tp in time_points_of_interest) {
  prop_tic_var <- paste0("prop_tic_", tp)
  glm_fit_tic <- glm(paste(prop_tic_var, "~
                     at_rerand_txt + at_rerand_txt * at_rand_txt +
                     at_rerand_txt * Ttilde_lapse.at.START0_END1_13 +
                     at_rerand_txt * at_rand_txt * Ttilde_lapse.at.START0_END1_13"),
                     data = data_sim, family = "quasibinomial")
  summary(glm_fit_tic)
  outcome_fits[[prop_tic_var]] <- glm_fit_tic
}

# Plot CATE functions
summarize_glm_fit <- function(outcome, glm_fits, data){
  glm_fit = glm_fits[[outcome]]
  glm_pred_A <- predict(glm_fit, newdata = data,
                        type = "response")
  glm_pred_1 <- predict(glm_fit, newdata = data %>% mutate(at_rerand_txt = "NAVIGATOR"),
                        type = "response")
  glm_pred_0 <- predict(glm_fit, newdata = data %>% mutate(at_rerand_txt = "SMS+VOUCHER"),
                        type = "response")
  data_with_prediction <- data %>%
    mutate(outcome = outcome) %>%
    mutate(pred_A = glm_pred_A,
           pred_1 = glm_pred_1,
           pred_0 = glm_pred_0) %>%
    mutate(pred_CATE = pred_1 - pred_0)
  return (data_with_prediction)
}

summary_of_all_fits <- lapply(all_outcomes,
                              FUN = summarize_glm_fit,
                              glm_fits = outcome_fits,
                              data = data_sim)

summary_of_all_fits <- Reduce(rbind, summary_of_all_fits) %>%
  mutate(outcome = factor(outcome, levels = all_outcomes))

summary_of_all_fits_Yk <- summary_of_all_fits %>%
  mutate(outcome = case_when(outcome == "prop_tic_50" ~ "Y1",
                             outcome == "prop_tic_100" ~ "Y2",
                             outcome == "prop_tic_150" ~ "Y3",
                             outcome == "prop_tic_200" ~ "Y4",
                             outcome == "prop_tic_250" ~ "Y5")) %>%
  mutate(at_rand_txt = ifelse(as.character(at_rand_txt) == "VOUCHER", "CCT", at_rand_txt)) %>%
  mutate(at_rand_txt = factor(as.character(at_rand_txt), levels = c("SMS", "SOC", "CCT")))

p_glm_pred_CATE_combined_Yk <-
  ggplot(summary_of_all_fits_Yk,
         aes(x = Ttilde_lapse.at.START0_END1_13,
             y = pred_CATE,
             color = outcome)) +
  geom_line(aes(x = Ttilde_lapse.at.START0_END1_13, y = pred_CATE),
            linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_grid(~at_rand_txt, scales = "fixed") +
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

ggsave(file.path(plotDir, "ADAPT-R-Multiple_CATE_comparison_Yk.pdf"),
       p_glm_pred_CATE_combined_Yk, width = 7, height = 5)

# Save data for running simulation
realSim_data <- data_sim %>%
  select(ID, at_rand_txt, Ttilde_lapse.at.START0_END1_13, ti) %>%
  mutate(ti = ti %/% 50 + 1) %>%
  mutate(at_rerand_txt = "NAVIGATOR") %>%
  mutate(EY1_1 = predict(outcome_fits[["prop_tic_50"]], newdata = ., type = "response"),
         EY1_2 = predict(outcome_fits[["prop_tic_100"]], newdata = ., type = "response"),
         EY1_3 = predict(outcome_fits[["prop_tic_150"]], newdata = ., type = "response"),
         EY1_4 = predict(outcome_fits[["prop_tic_200"]], newdata = ., type = "response"),
         EY1_5 = predict(outcome_fits[["prop_tic_250"]], newdata = ., type = "response")) %>%
  mutate(at_rerand_txt = "SMS+VOUCHER") %>%
  mutate(EY0_1 = predict(outcome_fits[["prop_tic_50"]], newdata = ., type = "response"),
         EY0_2 = predict(outcome_fits[["prop_tic_100"]], newdata = ., type = "response"),
         EY0_3 = predict(outcome_fits[["prop_tic_150"]], newdata = ., type = "response"),
         EY0_4 = predict(outcome_fits[["prop_tic_200"]], newdata = ., type = "response"),
         EY0_5 = predict(outcome_fits[["prop_tic_250"]], newdata = ., type = "response")) %>%
  mutate(W1 = as.numeric(factor(at_rand_txt, levels = c("SMS", "SOC", "VOUCHER"))), W2 = Ttilde_lapse.at.START0_END1_13) %>%
  select(c("ID","ti","W1","W2"), starts_with("EY")) %>%
  unique() %>%
  mutate(ID = row_number())

write.csv(realSim_data, file = file.path(dataDir, "realSim_data.csv"))

