
#'
#' SUPPLEMENT for go fish analysis
#'

rm(list=ls())
library(emmeans)
library(lme4)
library(patchwork)
library(tidyverse)
library(viridis)

# Read in and process data from explain/describe trials
read_trial_data = function(filepath) {
  trial_data = read_csv(filepath)
  trial_data = trial_data %>%
    mutate(
      Condition = ifelse(is_control == TRUE, "Describe", "Explain"),
      input_correct =
        (input_prediction_catches_fish == prediction_catches_fish))
  trial_data$input_evidence_response = str_replace_all(
    trial_data$input_evidence_response, "\n" , "[newline]")
  return(trial_data)
}

# Read in and process summary data file
read_summary_data = function(filepath) {
  # Read in data at filepath and add/modify columns as needed
  summary_data = read_csv(filepath)
  summary_data = summary_data %>%
    mutate(condition = ifelse(is_control == TRUE, "Describe", "Explain"),
           experiment_completion_time = (expt_end_ts - expt_start_ts) / 1000)
  return(summary_data)
}

# Read in and process data from rule evaluation task
read_evaluation_data = function(filepath) {
  evaluation_data = read_csv(filepath)
  evaluation_data = evaluation_data %>%
    mutate(condition = ifelse(is_control == TRUE, "Describe", "Explain"))
  return(evaluation_data)
}




# Summarize prediction data for graphing
get_prediction_summary = function(trial_data) {
  prediction_summary = trial_data %>%
    group_by(Condition, trial_index) %>%
    summarize(accuracy_mean = sum(input_correct) / n(),
              accuracy_se = sqrt((accuracy_mean * (1 - accuracy_mean)) / n()))
  return(prediction_summary)
}


# Generic plot theme for use in most graphs
individ_plot_theme = theme(
  # titles
  plot.title = element_text(face = "bold", size = 24),
  axis.title.y = element_text(face = "bold", size = 24),
  axis.title.x = element_text(face = "bold", size = 20),
  legend.title = element_text(face = "bold", size = 16),
  # axis text
  axis.text.y = element_text(size = 16, face = "bold"),
  axis.text.x = element_text(size = 16, face = "bold"),
  # legend text
  legend.text = element_text(size = 18, face = "bold"),
  # backgrounds, lines
  panel.background = element_blank(),
  strip.background = element_blank(),
  panel.grid = element_line(color = "gray"),
  axis.line = element_line(color = "black"),
  # positioning
  legend.position = "bottom",
  legend.key = element_rect(colour = "transparent", fill = "transparent")
)



# ANALYSIS: E1 PREDICTIONS ====


# Experiment 1

# Watch out, global pathway!
setwd("/Users/erikbrockbank/web/elc-lab/go_fish/analysis/")
ROUND1_TRIAL_DATA = "round1/02_go_fish_trials.csv"
ROUND2_TRIAL_DATA = "round2/02_go_fish_trials.csv"


trial_data = bind_rows(read_trial_data(ROUND1_TRIAL_DATA),
                       read_trial_data(ROUND2_TRIAL_DATA))
trial_data$Condition = factor(trial_data$Condition,
                                 levels = c("Explain", "Describe"))

prediction_summary = get_prediction_summary(trial_data)

prediction_subject_summary = trial_data %>%
  group_by(subjID, Condition) %>%
  summarize(subj_accuracy = sum(input_correct) / n())




# Plot subject accuracy by condition
e1_condition_accuracy = prediction_subject_summary %>%
  group_by(Condition) %>%
  summarize(mean_accuracy = mean(subj_accuracy),
            sd_accuracy = sd(subj_accuracy),
            se_accuracy = sd(subj_accuracy) / sqrt(n())) %>%
  ggplot(aes(x = Condition, y = mean_accuracy, color = Condition, fill = Condition)) +
  geom_bar(stat = "identity", alpha = 0.5, width = 0.5) +
  geom_errorbar(aes(ymin = mean_accuracy - se_accuracy,
                    ymax = mean_accuracy + se_accuracy),
                width = 0.25) +
  geom_hline(yintercept = 0.5, linetype = "dashed", size = 1) +
  scale_color_viridis(discrete = T,
                      name = element_blank(),
                      begin = 0.25,
                      end = 0.75) +
  scale_fill_viridis(discrete = T,
                     name = element_blank(),
                     begin = 0.25,
                     end = 0.75) +
  ylim(c(0, 1)) +
  labs(x = "", y = "Mean accuracy") +
  ggtitle("Subject accuracy") +
  individ_plot_theme +
  theme(axis.text.x = element_blank())



# Plot prediction summary
e1_trial_accuracy = prediction_summary %>%
  ggplot(aes(x = trial_index, y = accuracy_mean, color = Condition)) +
  geom_point(size = 4, alpha = 0.75) +
  geom_errorbar(aes(ymin = accuracy_mean - accuracy_se,
                    ymax = accuracy_mean + accuracy_se),
                width = 0.5) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylim(c(0.25, 1)) +
  labs(x = "Trial", y = "Correct response pct") +
  ggtitle("Trial accuracy") +
  scale_color_viridis(discrete = T,
                      name = element_blank(),
                      begin = 0.25,
                      end = 0.75) +
  scale_x_discrete(name = "Trial",
                   limits = c(1, 2, 3, 4, 5, 6, 7, 8)) +
  individ_plot_theme


# Plots
e1_condition_accuracy + e1_trial_accuracy


# Model
m0_pred = with(trial_data,
               glmer(as.numeric(input_correct) ~ (1|trial_index) + (1|subjID),
                     family = binomial()))
m1_pred = with(trial_data,
               glmer(as.numeric(input_correct) ~ Condition + (1|trial_index) + (1|subjID),
                     family = binomial()))

anova(m1_pred, m0_pred, family = 'LRT')
emmeans(m1_pred, pairwise ~ Condition) # NB: these emmeans are logit differences

# TODO can we get an estimated slope on input_correct ~ trial_index? Or trial_index * Condition interaction




# ANALYSIS: E2 PREDICTIONS ====

# NB: the below requires access to the Experiment 2 data which does not live
# in the same repo as this code (and the Experiment 1 data used above).
# Experiment 2 data can be found at: https://github.com/erik-brockbank/go_fish_v2

# Watch out, global pathway!
setwd("/Users/erikbrockbank/web/elc-lab/go_fish_v2/analysis/")
TRIAL_DATA = "02_go_fish_v2_trials.csv"
SUMMARY_DATA = "01_go_fish_v2_meta.csv"
EVAL_DATA = "03_go_fish_v2_evaluation.csv"

CATCH_RULE = "If a lure combination has a red shape on the bottom, it will catch fish."
CATCH_RULE_CUTOFF = 80


trial_data = read_trial_data(TRIAL_DATA)
summary_data = read_summary_data(SUMMARY_DATA)
evaluation_data = read_evaluation_data(EVAL_DATA)

trial_data$Condition = factor(trial_data$Condition,
                              levels = c("Explain", "Describe"))

# Select participants that should be removed
catch_users = evaluation_data %>%
  filter(rule_text == CATCH_RULE & input_rule_rating > CATCH_RULE_CUTOFF)
table(catch_users$condition)
unique(catch_users$subjID)


sd(summary_data$experiment_completion_time)
COMPLETION_TIME_CUTOFF = mean(summary_data$experiment_completion_time) + 5 * sd(summary_data$experiment_completion_time)
# COMPLETION_TIME_CUTOFF = 10000
# COMPLETION_TIME_CUTOFF = 2000

completion_time_users = summary_data %>%
  filter(experiment_completion_time > COMPLETION_TIME_CUTOFF) %>%
  select(subjID)

CATCH_USERS = c(catch_users$subjID, completion_time_users$subjID)

trial_data = trial_data %>%
  filter(!subjID %in% CATCH_USERS)

prediction_summary = get_prediction_summary(trial_data)

prediction_subject_summary = trial_data %>%
  group_by(subjID, Condition) %>%
  summarize(subj_accuracy = sum(input_correct) / n())




# Plots

# Plot subject accuracy by condition
e2_condition_accuracy = prediction_subject_summary %>%
  group_by(Condition) %>%
  summarize(mean_accuracy = mean(subj_accuracy),
            sd_accuracy = sd(subj_accuracy),
            se_accuracy = sd(subj_accuracy) / sqrt(n())) %>%
  ggplot(aes(x = Condition, y = mean_accuracy, color = Condition, fill = Condition)) +
  geom_bar(stat = "identity", alpha = 0.5, width = 0.5) +
  geom_errorbar(aes(ymin = mean_accuracy - se_accuracy,
                    ymax = mean_accuracy + se_accuracy),
                width = 0.25) +
  geom_hline(yintercept = 0.5, linetype = "dashed", size = 1) +
  scale_color_viridis(discrete = T,
                      name = element_blank(),
                      begin = 0.25,
                      end = 0.75) +
  scale_fill_viridis(discrete = T,
                     name = element_blank(),
                     begin = 0.25,
                     end = 0.75) +
  ylim(c(0, 1)) +
  labs(x = "", y = "Mean accuracy") +
  ggtitle("Subject accuracy") +
  individ_plot_theme +
  theme(axis.text.x = element_blank())


# Plot prediction summary
e2_trial_accuracy = prediction_summary %>%
  ggplot(aes(x = trial_index, y = accuracy_mean, color = Condition)) +
  geom_point(size = 4, alpha = 0.75) +
  geom_errorbar(aes(ymin = accuracy_mean - accuracy_se,
                    ymax = accuracy_mean + accuracy_se),
                width = 0.5) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylim(c(0.25, 1)) +
  labs(x = "Trial", y = "Correct response pct") +
  ggtitle("Trial accuracy") +
  scale_color_viridis(discrete = T,
                      name = element_blank(),
                      begin = 0.25,
                      end = 0.75) +
  scale_x_discrete(name = "Trial",
                   limits = c(1, 2, 3, 4, 5, 6, 7, 8)) +
  individ_plot_theme

e2_condition_accuracy + e2_trial_accuracy



# Model
m0_pred = with(trial_data,
               glmer(as.numeric(input_correct) ~ (1|trial_index) + (1|subjID),
                     family = binomial()))
m1_pred = with(trial_data,
               glmer(as.numeric(input_correct) ~ Condition + (1|trial_index) + (1|subjID),
                     family = binomial()))

anova(m1_pred, m0_pred, family = 'LRT')
emmeans(m1_pred, pairwise ~ Condition) # NB: these emmeans are logit differences


