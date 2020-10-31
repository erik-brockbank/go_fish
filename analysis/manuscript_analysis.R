#'
#' Analysis for manuscript submission
#'


rm(list = ls())
setwd("~/web/go_fish/analysis/")

library(tidyverse)
library(viridis)
library(patchwork)
library(pwr)
library(ES)


# GLOBALS ======================================================================
# Data files
ROUND1_SUMMARY_DATA = "round1/01_go_fish_meta.csv"
ROUND1_TRIAL_DATA = "round1/02_go_fish_trials.csv"
ROUND1_GENERATION_RESP_DATA = "round1/03_go_fish_generation_free_resp.csv"
ROUND1_GENERATION_JUDG_DATA = "round1/04_go_fish_generation_judgment.csv"
ROUND1_EVAL_DATA = "round1/05_go_fish_evaluation.csv"
ROUND1_MEMORY_DATA = "round1/06_go_fish_memory.csv"

ROUND2_SUMMARY_DATA = "round2/01_go_fish_meta.csv"
ROUND2_TRIAL_DATA = "round2/02_go_fish_trials.csv"
ROUND2_GENERATION_RESP_DATA = "round2/03_go_fish_generation_free_resp.csv"
ROUND2_GENERATION_JUDG_DATA = "round2/04_go_fish_generation_judgment.csv"
ROUND2_EVAL_DATA = "round2/05_go_fish_evaluation.csv"
ROUND2_MEMORY_DATA = "round2/06_go_fish_memory.csv"

GENERATION_RESP_DATA_CODED = "free_response_combined.csv"
EXPLANATION_DATA_CODED = "explanation_coded.csv"


# Data labels
RULE_EVAL_LABELS = c("TRUE" = "Target rule", "FALSE" = "All other rules")
DISTRACTOR_LABELS = c("TRUE" = "Target rule", "FALSE" = "Distractor rule")

DISTRACTOR_RULE = "If a lure combination has a yellow shape or a diamond on the bottom, it will catch fish."

# the memory probe item with this shape config was erroneously coded as being in experiment when it wasn't
# we correct the `memory_shape_in_expt` column value when reading in the data
MEMORY_MISCUE = "{'top_shape': 'diamond', 'top_color': 'blue', 'top_texture': False, 'bottom_shape': 'circle', 'bottom_color': 'blue', 'bottom_texture': False}"



CODED_MEASURE_FILTER = c("shape_total", "color_total", "purple_dot_total")
coded_measure_width = 10
CODED_MEASURE_LOOKUP = c("mechanism_total" = str_wrap("Mechanism", width = coded_measure_width),
                         # "shape_total" = str_wrap("Shape (total)", width = coded_measure_width),
                         "shape_abstract_total" = str_wrap("Shape (abstract)", width = coded_measure_width),
                         "shape_concrete_total" = str_wrap("Shape (concrete)", width = coded_measure_width),
                         # "color_total" = str_wrap("Color (total)", width = coded_measure_width),
                         "color_abstract_total" = str_wrap("Color (abstract)", width = coded_measure_width),
                         "color_concrete_total" = str_wrap("Color (concrete)", width = coded_measure_width),
                         # "purple_dot_total" = str_wrap("Purple dot (total)", width = coded_measure_width),
                         "purple_dot_abstract_total" = str_wrap("Purple dot (abstract)", width = coded_measure_width),
                         "purple_dot_concrete_total" = str_wrap("Purple dot (concrete)", width = coded_measure_width)
                         )
CODED_MEASURE_LEVELS = c(str_wrap("Mechanism", width = coded_measure_width),
                         # str_wrap("Shape (total)", width = coded_measure_width),
                         str_wrap("Shape (abstract)", width = coded_measure_width), str_wrap("Shape (concrete)", width = coded_measure_width),
                         # str_wrap("Color (total)", width = coded_measure_width),
                         str_wrap("Color (abstract)", width = coded_measure_width), str_wrap("Color (concrete)", width = coded_measure_width),
                         # str_wrap("Purple dot (total)", width = coded_measure_width),
                         str_wrap("Purple dot (abstract)", width = coded_measure_width), str_wrap("Purple dot (concrete)", width = coded_measure_width))



# ANALYSIS FUNCTIONS ===========================================================

# Read in and process summary data file
read_summary_data = function(filepath, is_round1) {
  # Read in data at filepath and add/modify columns as needed
  summary_data = read_csv(filepath)
  summary_data = summary_data %>%
    mutate(Condition = ifelse(is_control == TRUE, "Describe", "Explain"),
           Round1 = is_round1,
           experiment_completion_time = (expt_end_ts - expt_start_ts) / 1000)
  return(summary_data)
}


# Read in and process data from explain/describe trials
read_trial_data = function(filepath, is_round1) {
  trial_data = read_csv(filepath)
  trial_data = trial_data %>%
    mutate(
      Condition = ifelse(is_control == TRUE, "Describe", "Explain"),
      Round1 = is_round1,
      input_correct =
        (input_prediction_catches_fish == prediction_catches_fish))
  trial_data$input_evidence_response = str_replace_all(
    trial_data$input_evidence_response, "\n" , "[newline]")
  return(trial_data)
}


# Read in and process data from generation "judgment" (binary response) task
read_generation_judgment_data = function(filepath, is_round1) {
  generation_judgment_data = read_csv(filepath)
  generation_judgment_data = generation_judgment_data %>%
    mutate(
      Condition = ifelse(is_control == TRUE, "Describe", "Explain"),
      Round1 = is_round1,
      input_correct =
        (input_judgment == judgment_catches_fish))
  return(generation_judgment_data)
}


# Read in and process data from rule evaluation task
read_evaluation_data = function(filepath, is_round1) {
  evaluation_data = read_csv(filepath)
  evaluation_data = evaluation_data %>%
    mutate(Condition = ifelse(is_control == TRUE, "Describe", "Explain"),
           Round1 = is_round1)
  return(evaluation_data)
}


# Read in and process data from memory task at end of experiment
read_memory_data = function(filepath, is_round1) {
  memory_data = read_csv(filepath)
  memory_data = memory_data %>%
    mutate(Condition = ifelse(is_control == TRUE, "Describe", "Explain"),
           Round1 = is_round1,
           memory_correct =
             (memory_shape_in_expt == input_shape_in_expt))

  # Fix mis-coded memory probe
  memory_data %>%
    filter(memory_shape == MEMORY_MISCUE) %>%
    mutate(memory_shape_in_expt = 0)
  return(memory_data)
}


# Read in coded free response data (note this requires writing free response data to csv in initialization code)
read_coded_free_resp_data = function(filename, summary_data) {
  generation_free_resp_coded = read_csv(filename)
  generation_free_resp_coded = generation_free_resp_coded %>%
    select(subjID, free_response_str, Revision) %>%
    inner_join(., summary_data, by = "subjID") %>%
    select(subjID, Condition, free_response_str, Revision)

  return(generation_free_resp_coded)
}


# Read in coded explanation/description data
read_coded_explanation_data = function(filename) {
  explanation_free_resp_coded = read_csv(filename)
  return(explanation_free_resp_coded)
}


# Summarize prediction data for graphing
get_prediction_summary = function(trial_data) {
  prediction_summary = trial_data %>%
    group_by(Condition, trial_index) %>%
    summarize(accuracy = sum(input_correct) / n())
  return(prediction_summary)
}


# Summarize generation judgment data by participant
get_generation_judgment_subj_summary = function(generation_judgment_data) {
  generation_subject_summary = generation_judgment_data %>%
    group_by(Condition, subjID) %>%
    summarize(subj_accuracy = sum(input_correct) / n())
  return(generation_subject_summary)
}


# Summarize generation judgment data across participants
get_generation_judgment_summary = function(generation_judgment_subject_summary) {
  generation_judg_summary = generation_judgment_subject_summary %>%
    group_by(Condition) %>%
    summarize(mean_accuracy = mean(subj_accuracy),
              subjects = n(),
              se_accuracy = sd(subj_accuracy) / sqrt(n()),
              ci_lower = mean_accuracy - se_accuracy,
              ci_upper = mean_accuracy + se_accuracy)
  return(generation_judg_summary)
}


# Summarize generation free response data across participants
get_generation_free_response_summary = function(generation_free_response_coded) {
  generation_free_response_summary = generation_free_response_coded %>%
    group_by(Condition) %>%
    summarize(subjects = n(),
              correct_generation_pct = sum(Revision) / n())
  return(generation_free_response_summary)
}


# Summarize evaluation data across participants and conditions for target and non-target rules
get_evaluation_summary = function(evaluation_data) {
  # Average rating across participants on target rule
  eval_summary_target_rule = evaluation_data %>%
    filter(is_target_rule == TRUE) %>% # for target rule, summarize across participants
    group_by(Condition) %>%
    summarize(ruleset = "Target",
              mean_rating = mean(input_rule_rating),
              subjects = n(),
              se_rating = sd(input_rule_rating) / sqrt(n()),
              ci_lower = mean_rating - se_rating,
              ci_upper = mean_rating + se_rating)

  # Average rating across participants on distractor rule
  eval_summary_distractor_rule = evaluation_data %>%
    filter(rule_text == DISTRACTOR_RULE) %>% # for distractor rule, summarize across participants
    group_by(Condition) %>%
    summarize(ruleset = "Distractor",
              mean_rating = mean(input_rule_rating),
              subjects = n(),
              se_rating = sd(input_rule_rating) / sqrt(n()),
              ci_lower = mean_rating - se_rating,
              ci_upper = mean_rating + se_rating)

  # Average of each participant's average across non-target rules
  eval_summary_other_rules = evaluation_data %>%
    filter(is_target_rule == FALSE &
             rule_text != DISTRACTOR_RULE) %>% # for all other rules, summarize average of participant avgs
    group_by(Condition, subjID) %>%
    summarize(mean_subj_rating = mean(input_rule_rating),
              rules = n()) %>%
    group_by(Condition) %>%
    summarize(ruleset = "All other rules",
              mean_rating = mean(mean_subj_rating),
              subjects = n(),
              se_rating = sd(mean_subj_rating) / sqrt(n()),
              ci_lower = mean_rating - se_rating,
              ci_upper = mean_rating + se_rating)

  eval_summary = rbind(eval_summary_target_rule, eval_summary_distractor_rule, eval_summary_other_rules)
  return(eval_summary)
}


# Summarize experiment completion time data
get_time_summary = function(time_data) {
  time_summary = time_data %>%
    group_by(Condition) %>%
    summarize(mean_task_time = mean(experiment_completion_time),
              subjects = n(),
              se_task_time = sd(experiment_completion_time) / sqrt(subjects),
              ci_lower = mean_task_time - se_task_time,
              ci_upper = mean_task_time + se_task_time)
  return(time_summary)
}


# Summarize average trial completion time by participant
get_trial_time_subj_summary = function(trial_data) {
  trial_subject_summary = trial_data %>%
    mutate(trial_completion_time = (trial_n_end_ts - trial_n_start_ts) / 1000) %>%
    group_by(Round1, Condition, subjID) %>%
    summarize(mean_trial_completion = mean(trial_completion_time),
              trials = n(),
              se_trial_completion = sd(trial_completion_time) / sqrt(trials),
              ci_lower = mean_trial_completion - se_trial_completion,
              ci_upper = mean_trial_completion + se_trial_completion)
  return(trial_subject_summary)
}


# Get summary of time spent on trials in each condition across participants
get_trial_time_summary = function(trial_time_subj_summary) {
  trial_time_summary = trial_time_subj_summary %>%
    # trial time completion not available in round1 data
    filter(Round1 == FALSE) %>%
    group_by(Condition) %>%
    # These column names kept the same as those in `get_time_summary` for easier graphing
    summarize(mean_task_time = mean(mean_trial_completion),
              subjects = n(),
              se_trial_time = sd(mean_trial_completion) / sqrt(subjects),
              ci_lower = mean_task_time - se_trial_time,
              ci_upper = mean_task_time + se_trial_time)
  return(trial_time_summary)
}


# Summarize memory performance data by participant
get_memory_subj_summary = function(memory_data) {
  memory_subj_accuracy = memory_data %>%
    group_by(Condition, subjID) %>%
    summarize(
      subj_correct = sum(memory_correct),
      total = n(),
      subj_accuracy = subj_correct / total)
  return(memory_subj_accuracy)
}


# Summarize memory performance across participants by condition
get_memory_summary = function(memory_subject_summary) {
  memory_summary = memory_subject_summary %>%
    group_by(Condition) %>%
    summarize(mean_memory_accuracy = mean(subj_accuracy),
              subjects = n(),
              se_memory_accuracy = sd(subj_accuracy) / sqrt(n()),
              ci_lower = mean_memory_accuracy - se_memory_accuracy,
              ci_upper = mean_memory_accuracy + se_memory_accuracy)
  return(memory_summary)
}


# Summarize explanation/description coded data
get_explanation_coded_subj_summary = function(explanation_data) {
  explanation_summary = explanation_data %>%
    group_by(Condition, Subject) %>%
    summarize(mechanism_total = sum(`FINAL - total mechanisms`),
              shape_total = sum(`FINAL - total shape references`),
              shape_abstract_total = sum(`FINAL - abstract shape references`),
              shape_concrete_total = sum(`FINAL - concrete shape references`),
              color_total = sum(`FINAL - total color references`),
              color_abstract_total = sum(`FINAL - abstract color references`),
              color_concrete_total = sum(`FINAL - concrete color references`),
              purple_dot_total = sum(`FINAL - total purple dot references`),
              purple_dot_abstract_total = sum(`FINAL - abstract purple dot references`),
              purple_dot_concrete_total = sum(`FINAL - concrete purple dot references`)) %>%
    gather(
      key = "measure",
      value = "subject_total",
      -Condition,
      -Subject
    )
  return(explanation_summary)
}


# Summarize coded explanation/description results across participants by condition
get_explanation_coded_summary = function(explanation_subject_summary) {
  explanation_summary = explanation_subject_summary %>%
    filter(!measure %in% CODED_MEASURE_FILTER) %>%
    mutate(measure = factor(CODED_MEASURE_LOOKUP[measure], levels = CODED_MEASURE_LEVELS)) %>%
    group_by(Condition, measure) %>%
    summarize(subjects = n(),
              measure_mean = mean(subject_total),
              measure_se = sd(subject_total) / sqrt(subjects)
              )
  return(explanation_summary)
}


# Auxiliary function for printing out t test statistics
report_t_summary = function(t_test) {
  paste("Mean 1:", round(t_test$estimate[1], 2), "||",
        "Mean 2:", round(t_test$estimate[2], 2), "||",
        "t (", t_test$parameter, ") =", round(t_test$statistic, 2), ",",
        "p =", round(t_test$p.value, 3),
        sep = " ")
}

# Auxiliary function for printing out chi sq test statistics
report_chisq_summary = function(chisq_test) {
  paste("Count/Prop 1:", round(chisq_test$estimate[1], 3), "||",
        "Count/Prop 2:", round(chisq_test$estimate[2], 3), "||",
        "X^2 (", chisq_test$parameter, ") =", round(chisq_test$statistic, 2), ",",
        "p =", round(chisq_test$p.value, 3),
        sep = " ")
}

# Auxiliary function for printing out wilcoxon signed-rank test statistics
report_wilcox_summary = function(wilcox_test) {
  z_val = qnorm(wilcox_test$p.value / 2)
  paste("z =", round(z_val, 2), ",",
        "p =", round(wilcox_test$p.value, 3),
        sep = " ")
}



# PLOTTING FUNCTIONS ===========================================================

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


# Line plot of prediction accuracy by condition for each trial
# (percent of participants in each condition who were correct in each trial)
# NB: this plot not included in cog sci submission
plot_prediction_summary = function(prediction_summary) {
  prediction_summary %>%
    filter(trial_index > 4) %>%
    ggplot(aes(x = trial_index, y = accuracy, color = Condition)) +
    geom_line() +
    geom_hline(yintercept = 0.5, linetype = "dashed") +
    labs(x = "Trial index", y = "Accuracy") +
    # ggtitle("Prediction accuracy in each round") +
    scale_color_viridis(discrete = T,
                        name = element_blank()) +
    individ_plot_theme
}


# Bar chart of classification accuracy on binary generation judgment task by condition
plot_generation_judgments = function(generation_judgment_summary) {
  generation_judgment_summary %>%
    ggplot(aes(x = Condition, y = mean_accuracy,
               color = Condition, fill = Condition)) +
    geom_bar(stat = "identity", alpha = 0.5, width = 0.5) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.25) +
    geom_hline(yintercept = 0.5, linetype = "dashed", size = 1) +
    # labs(x = "", y = "Mean classification accuracy") +
    labs(x = "", y = "Percent Correct") +
    # ggtitle("Accuracy on generation judgment task") +
    ggtitle("Classification Accuracy") +
    ylim(c(0, 1)) +
    scale_color_viridis(discrete = T,
                        name = element_blank(),
                        # Change defaults to be blue/green instead of yellow/purple
                        begin = 0.25,
                        end = 0.75) +
    scale_fill_viridis(discrete = T,
                       name = element_blank(),
                       # Change defaults to be blue/green instead of yellow/purple
                       begin = 0.25,
                       end = 0.75) +
    individ_plot_theme +
    theme(axis.text.x = element_blank())
}


# Bar chart of rule generation accuracy for coded free response answers
# NB: this chart not included in cog sci submission
plot_generation_free_responses = function(generation_free_resp_summary) {
  generation_free_resp_summary %>%
    ggplot(aes(x = Condition, y = correct_generation_pct,
               color = Condition, fill = Condition)) +
    geom_bar(stat = "identity", width = 0.5, alpha = 0.5) +
    labs(x = "", y = "Percent Correct") +
    ggtitle("Free Response Accuracy") +
    # ggtitle("Rule generation across conditions") +
    ylim(c(0, 1)) +
    scale_color_viridis(discrete = T,
                        name = element_blank(),
                        # Change defaults to be blue/green instead of yellow/purple
                        begin = 0.25,
                        end = 0.75) +
    scale_fill_viridis(discrete = T,
                       name = element_blank(),
                       # Change defaults to be blue/green instead of yellow/purple
                       begin = 0.25,
                       end = 0.75) +
    individ_plot_theme +
    theme(axis.text.x = element_blank())
}


# Bar chart of average evaluation ratings across conditions on rule evaluation task
plot_evaluation_results = function(evaluation_summary, comparison_set) {
  evaluation_summary %>%
    ggplot(aes(x = ruleset, y = mean_rating,
               color = Condition, fill = Condition)) +
    geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.5, alpha = 0.5) +
    geom_errorbar(
      aes(ymin = ci_lower, ymax = ci_upper),
      position = position_dodge(width = 0.5, preserve = "single"),
      width = 0.2) +
    labs(y = "Mean evaluation rating") +
    # ggtitle("Evaluation across conditions") +
    scale_x_discrete(name = element_blank()) +
                     #labels = comparison_set) +
    scale_color_viridis(discrete = T,
                        name = element_blank(),
                        # Change defaults to be blue/green instead of yellow/purple
                        begin = 0.25,
                        end = 0.75) +
    scale_fill_viridis(discrete = T,
                       name = element_blank(),
                       # Change defaults to be blue/green instead of yellow/purple
                       begin = 0.25,
                       end = 0.75) +
    scale_y_continuous(breaks = seq(1, 7)) +
    individ_plot_theme
}


# Bar chart of experiment completion time or avg. trial time
plot_time_data = function(time_summary, ylab, ymax, title) {
  time_summary %>%
    ggplot(aes(x = Condition, y = mean_task_time,
               color = Condition, fill = Condition)) +
    geom_bar(stat = "identity", width = 0.5, alpha = 0.5) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.25) +
    ylim(0, ymax) +
    labs(x = "", y = ylab) +
    ggtitle(title) +
    scale_color_viridis(discrete = T,
                        name = element_blank(),
                        # Change defaults to be blue/green instead of yellow/purple
                        begin = 0.25,
                        end = 0.75) +
    scale_fill_viridis(discrete = T,
                       name = element_blank(),
                       # Change defaults to be blue/green instead of yellow/purple
                       begin = 0.25,
                       end = 0.75) +
    individ_plot_theme +
    theme(axis.text.x = element_blank())
          #plot.title = element_text(size = 32, face = "bold"))
}


# Bar chart of average memory accuracy across conditions
plot_memory_data = function(memory_summary) {
  memory_summary %>%
    ggplot(aes(x = Condition, y = mean_memory_accuracy,
               color = Condition, fill = Condition)) +
    geom_bar(stat = "identity", width = 0.5, alpha = 0.5) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.25) +
    geom_hline(yintercept = 0.5, linetype = "dashed", size = 1) +
    ylim(c(0, 1)) +
    labs(x = "", y = "Accuracy") +
    ggtitle("Mean memory performance") +
    scale_color_viridis(discrete = T,
                        name = element_blank(),
                        # Change defaults to be blue/green instead of yellow/purple
                        begin = 0.25,
                        end = 0.75) +
    scale_fill_viridis(discrete = T,
                       name = element_blank(),
                       # Change defaults to be blue/green instead of yellow/purple
                       begin = 0.25,
                       end = 0.75) +
    individ_plot_theme +
    theme(axis.text.x = element_blank())
}


# Bar chart of counts of feature references in coded explanations/descriptions, by condition
plot_coded_explanation_data = function(explanation_coded_summary) {
  explanation_coded_summary %>%
    ggplot(aes(x = measure, y = measure_mean,
               color = Condition, fill = Condition)) +
    geom_bar(stat = "identity", position = position_dodge(preserve = "single"),
             width = 0.5, alpha = 0.5) +
    geom_errorbar(aes(ymin = measure_mean - measure_se, ymax = measure_mean + measure_se),
                  position = position_dodge(width = 0.5, preserve = "single"),
                  width = 0.25) +
    ggtitle("Explanation and description measures") +
    labs(x = "", y = "Mean number of references") +
    scale_color_viridis(discrete = T,
                        name = element_blank(),
                        # Change defaults to be blue/green instead of yellow/purple
                        begin = 0.25,
                        end = 0.75) +
    scale_fill_viridis(discrete = T,
                       name = element_blank(),
                       # Change defaults to be blue/green instead of yellow/purple
                       begin = 0.25,
                       end = 0.75) +
    individ_plot_theme

}





# DATA INITIALIZATION ==========================================================

# Read in data
summary_data = bind_rows(read_summary_data(ROUND1_SUMMARY_DATA, TRUE),
                         read_summary_data(ROUND2_SUMMARY_DATA, FALSE))
trial_data = bind_rows(read_trial_data(ROUND1_TRIAL_DATA, TRUE),
                       read_trial_data(ROUND2_TRIAL_DATA, FALSE))
generation_free_resp_coded = read_coded_free_resp_data(GENERATION_RESP_DATA_CODED, summary_data)
generation_judgment_data = bind_rows(read_generation_judgment_data(ROUND1_GENERATION_JUDG_DATA, TRUE),
                                     read_generation_judgment_data(ROUND2_GENERATION_JUDG_DATA, FALSE))
evaluation_data = bind_rows(read_evaluation_data(ROUND1_EVAL_DATA, TRUE),
                            read_evaluation_data(ROUND2_EVAL_DATA, FALSE))
memory_data = bind_rows(read_memory_data(ROUND1_MEMORY_DATA, TRUE),
                        read_memory_data(ROUND2_MEMORY_DATA, FALSE))

# Summarize data
prediction_summary = get_prediction_summary(trial_data)
generation_free_resp_summary = get_generation_free_response_summary(generation_free_resp_coded)
generation_judgment_subject_summary = get_generation_judgment_subj_summary(generation_judgment_data)
generation_judgment_summary = get_generation_judgment_summary(generation_judgment_subject_summary)
evaluation_summary = get_evaluation_summary(evaluation_data)

completion_time_summary = get_time_summary(summary_data)
# NB: 17 initial round1 subjects have incomplete trial time data; impacts trial_time_subject_summary and trial_time_summary
trial_time_subject_summary = get_trial_time_subj_summary(trial_data)
trial_time_summary = get_trial_time_summary(trial_time_subject_summary)
memory_subject_summary = get_memory_subj_summary(memory_data)
memory_summary = get_memory_summary(memory_subject_summary)


# Coded explanation / description data
explanation_coded_data = read_coded_explanation_data(EXPLANATION_DATA_CODED)
explanation_coded_summary_subjects = get_explanation_coded_subj_summary(explanation_coded_data)
explanation_coded_summary = get_explanation_coded_summary(explanation_coded_summary_subjects)






# DATA ANALYSIS: GENERATION ====================================================

# 1. Generation free response task
# NB: this plot not included in cog sci submission, but statistics are reported
generation_fr = plot_generation_free_responses(generation_free_resp_summary)

generation_props = generation_free_resp_coded %>%
  group_by(Condition) %>%
  summarize(success = sum(Revision),
            total = n())
chisq_gen = prop.test(c(generation_props$success), c(generation_props$total))
report_chisq_summary(chisq_gen)


# 2. Generation judgment task: overall accuracy across conditions
generation_class = plot_generation_judgments(generation_judgment_summary)

t_gen = t.test(
  generation_judgment_subject_summary$subj_accuracy[generation_judgment_subject_summary$Condition == "Describe"],
  generation_judgment_subject_summary$subj_accuracy[generation_judgment_subject_summary$Condition == "Explain"],
  var.equal = T
)
report_t_summary(t_gen)


# 3. Generation judgment task: proportion of people who got 100% across conditions
# NB: similar Chi-sq test with raw count of people who were *coded as getting rule* is what we report above
# for the free response task so this is effectively a parallel analysis for people who got 100% on judgment task
generation_task_comparison = generation_free_resp_coded %>%
  inner_join(generation_judgment_subject_summary, by = c("subjID", "Condition"))
# NB: we want table to be subj_accuracy == 1 but we do < 1 below to get count of == 1 as the *first* column
count_data = table(generation_task_comparison$Condition, generation_task_comparison$subj_accuracy < 1)
chisq.test(count_data) # NB: this is equivalent to prop.test(count_data)

# Plot results
generation_fr + generation_class



# DATA ANALYSIS: EVALUATION ====================================================

# 1. Evaluation of target rule across conditions
plot_evaluation_results(evaluation_summary, RULE_EVAL_LABELS)

t_eval = t.test(
  evaluation_data$input_rule_rating[evaluation_data$Condition == "Describe" & evaluation_data$is_target_rule == TRUE],
  evaluation_data$input_rule_rating[evaluation_data$Condition == "Explain" & evaluation_data$is_target_rule == TRUE],
  var.equal = T
)
report_t_summary(t_eval)


# 2. Wilcoxon signed-rank test showing that target rule is different from all other rules across both groups
eval_summary_other_rules = evaluation_data %>%
  filter(is_target_rule == FALSE) %>%
  group_by(is_target_rule, Condition, subjID) %>%
  summarize(mean_subj_rating = mean(input_rule_rating))

eval_difference = evaluation_data %>%
  group_by(subjID, Condition) %>%
  filter(is_target_rule == TRUE) %>%
  inner_join(., eval_summary_other_rules, by = "subjID") %>%
  mutate(diff = input_rule_rating - mean_subj_rating) %>%
  select(subjID, Condition.x, diff)

wil_exp = wilcox.test(eval_difference$diff[eval_difference$Condition.x == "Explain"], exact = F)
wil_des = wilcox.test(eval_difference$diff[eval_difference$Condition.x == "Describe"], exact = F)
report_wilcox_summary(wil_exp) # Explainers
report_wilcox_summary(wil_des) # Describers



# 3. Evaluation of *distractor rule*

# 3.1 Evaluation of distractor rule across conditions
t_eval_comp = t.test(
  evaluation_data$input_rule_rating[evaluation_data$Condition == "Describe"
                                    & evaluation_data$rule_text == DISTRACTOR_RULE],
  evaluation_data$input_rule_rating[evaluation_data$Condition == "Explain"
                                    & evaluation_data$rule_text == DISTRACTOR_RULE],
  var.equal = T
)
report_t_summary(t_eval_comp)



# 3.2 Comparison of distractor rule and target rule across conditions
t_eval_target_dist_exp = t.test(
  evaluation_data$input_rule_rating[evaluation_data$Condition == "Explain"
                                    & evaluation_data$rule_text == "If a lure combination has a yellow shape or a diamond on the bottom, it will catch fish."],
  evaluation_data$input_rule_rating[evaluation_data$Condition == "Explain"
                                    & evaluation_data$is_target_rule == TRUE],
  var.equal = T
)

t_eval_target_dist_desc = t.test(
  evaluation_data$input_rule_rating[evaluation_data$Condition == "Describe"
                                    & evaluation_data$rule_text == "If a lure combination has a yellow shape or a diamond on the bottom, it will catch fish."],
  evaluation_data$input_rule_rating[evaluation_data$Condition == "Describe"
                                    & evaluation_data$is_target_rule == TRUE],
  var.equal = T
)

report_t_summary(t_eval_target_dist_exp) # Explainers
report_t_summary(t_eval_target_dist_desc) # Describers
# It appears that ratings of the distractor rule relative to the target rule were
# *more* different among explainers than controls
# (explainers: 6.6 v. 4.12; controls: 6.09 v. 4.74), even though the differences were significant for both groups.

# Test whether this difference is significant with ANOVA
eval_data_distractor_target = evaluation_data %>%
  filter(is_target_rule == T | rule_text == "If a lure combination has a yellow shape or a diamond on the bottom, it will catch fish.")
# sanity check
unique(eval_data_distractor_target$rule_text)
table(eval_data_distractor_target$subjID)
# check for significant interaction between condition and target rule v. distractor
interaction_test = aov(data = eval_data_distractor_target, input_rule_rating ~ Condition*is_target_rule)
summary(interaction_test)
# getting DF for reporting F statistics in Anova above
unique(eval_data_distractor_target$subjID)


# 4. Evaluation among participants who *did not generate correct rule*

# 4.1 Evaluation of target rule across conditions (this matches 1 above)
subset_participants = generation_free_resp_coded %>%
  select(subjID, Condition, Revision) %>%
  filter(Revision == 0) %>%
  # filter(Revision == 1) %>%
  inner_join(., evaluation_data, by = c("subjID", "Condition"))
# Sanity check the join above
table(subset_participants$subjID) # 6 (number of eval rows) for each subj ID
unique(subset_participants$subjID) # 56 for incorrect rule gen, 30 for correct
sum(generation_free_resp_coded$Revision) # 30 for incorrect rule gen: this is equal to 86 (total) - number of unique participants above or equal to number of unique part. above

# Plot data
eval_summary_subset = get_evaluation_summary(subset_participants)
# NB: this plot not included in manuscript
# plot_evaluation_results(eval_summary_subset, RULE_EVAL_LABELS)

# Analysis: compare ratings of target rule
t_subset_target = t.test(
  subset_participants$input_rule_rating[subset_participants$Condition == "Describe"
                                           & subset_participants$is_target_rule == TRUE],
  subset_participants$input_rule_rating[subset_participants$Condition == "Explain"
                                           & subset_participants$is_target_rule == TRUE],
  var.equal = T
)
report_t_summary(t_subset_target)

# 4.2. Wilcoxon signed-rank test showing that target rule is different from all other rules *among participants who didn't get the correct rule*
eval_summary_other_rules_subset = subset_participants %>%
  filter(is_target_rule == FALSE) %>% # for target rule, summarize across participants
  group_by(is_target_rule, Condition, subjID) %>%
  summarize(mean_subj_rating = mean(input_rule_rating))

eval_difference_subset = subset_participants %>%
  group_by(subjID, Condition) %>%
  filter(is_target_rule == TRUE) %>%
  inner_join(., eval_summary_other_rules_subset, by = "subjID") %>%
  mutate(diff = input_rule_rating - mean_subj_rating) %>%
  select(subjID, Condition.x, diff)

wil_exp_subset = wilcox.test(eval_difference_subset$diff[eval_difference_subset$Condition.x == "Explain"], exact = F)
wil_des_subset = wilcox.test(eval_difference_subset$diff[eval_difference_subset$Condition.x == "Describe"], exact = F)
report_wilcox_summary(wil_exp_subset) # Explainers
report_wilcox_summary(wil_des_subset) # Describers


# 4.3 Evaluation of distractor rule across conditions (this matches 3.1 above)
t_subset_dist = t.test(
  subset_participants$input_rule_rating[subset_participants$Condition == "Describe"
                                        & subset_participants$rule_text == DISTRACTOR_RULE],
  subset_participants$input_rule_rating[subset_participants$Condition == "Explain"
                                        & subset_participants$rule_text == DISTRACTOR_RULE],
  var.equal = T
)
report_t_summary(t_subset_dist)

# 4.3 Comparison of distractor rule and target rule across conditions (this matches 3.2 above)
t_subset_target_dist_exp = t.test(
  subset_participants$input_rule_rating[subset_participants$Condition == "Explain"
                                        & subset_participants$rule_text == "If a lure combination has a yellow shape or a diamond on the bottom, it will catch fish."],
  subset_participants$input_rule_rating[subset_participants$Condition == "Explain"
                                        & subset_participants$is_target_rule == TRUE],
  var.equal = T
)

t_subset_target_dist_desc = t.test(
  subset_participants$input_rule_rating[subset_participants$Condition == "Describe"
                                        & subset_participants$rule_text == "If a lure combination has a yellow shape or a diamond on the bottom, it will catch fish."],
  subset_participants$input_rule_rating[subset_participants$Condition == "Describe"
                                        & subset_participants$is_target_rule == TRUE],
  var.equal = T
)

report_t_summary(t_subset_target_dist_exp) # Explainers
report_t_summary(t_subset_target_dist_desc) # Describers
# It appears that among participants who *didn't generate the target rule*,
# ratings of the distractor rule relative to the target rule were *more* different among explainers than controls
# (explainers: 6.24 v. 4.62; controls: 5.91 v. 5.17), even though the differences were significant for both groups.

# Test whether this difference is significant with ANOVA
subset_participants_distractor_target = subset_participants %>%
  filter(is_target_rule == T | rule_text == "If a lure combination has a yellow shape or a diamond on the bottom, it will catch fish.")
# sanity check
unique(subset_participants_distractor_target$rule_text)
table(subset_participants_distractor_target$subjID)

# check for significant interaction between condition and target rule v. distractor
interaction_test = aov(data = subset_participants_distractor_target, input_rule_rating ~ Condition*is_target_rule)
summary(interaction_test)
# getting DF for reporting F statistics in Anova above
unique(subset_participants_distractor_target$subjID)





# COVARIATE ANALYSIS: MEMORY ===================================================

# 1. Memory performance compared to chance
# NB: not doing binomial test here because we are looking at accuracy percentages for N subjects
t_mem_chance_exp = t.test(memory_subject_summary$subj_accuracy[memory_subject_summary$Condition == "Explain"],
                          mu = 0.5,
                          equal.var = T)

t_mem_chance_desc = t.test(memory_subject_summary$subj_accuracy[memory_subject_summary$Condition == "Describe"],
                           mu = 0.5,
                           equal.var = T)

report_t_summary(t_mem_chance_exp)
report_t_summary(t_mem_chance_desc)


# 2. Memory accuracy across conditions
mem = plot_memory_data(memory_summary) # This plot is combined with time on task plots further down

t_mem = t.test(memory_subject_summary$subj_accuracy[memory_subject_summary$Condition == "Describe"],
               memory_subject_summary$subj_accuracy[memory_subject_summary$Condition == "Explain"],
               var.equal = T)

report_t_summary(t_mem)


# 3. Memory performance comparing participants who did and didn't get the correct rule
memory_hypothesis_join = generation_free_resp_coded %>%
  select(subjID, Condition, Revision) %>%
  inner_join(., memory_subject_summary, by = c("subjID"))

t_mem_correct = t.test(memory_hypothesis_join$subj_accuracy[memory_hypothesis_join$Revision == 0],
                       memory_hypothesis_join$subj_accuracy[memory_hypothesis_join$Revision == 1],
                       var.equal = T)

report_t_summary(t_mem_correct)



# COVARIATE ANALYSIS: TIME =====================================================

# 1. Overall time on task across conditions
# This plot is combined with memory and other time on task plots further down
time_on_task = plot_time_data(completion_time_summary, ylab = "Seconds", ymax = 1000, title = "Mean time on experiment")

t_time = t.test(summary_data$experiment_completion_time[summary_data$Condition == "Describe"],
                summary_data$experiment_completion_time[summary_data$Condition == "Explain"],
                var.equal = T)
report_t_summary(t_time) # Means are seconds on task


# 2. Time on evidence trials across conditions

# NB: 17 initial round1 subjects have incomplete trial time data; impacts trial_time_subject_summary and trial_time_summary
unique(trial_time_subject_summary$subjID[is.na(trial_time_subject_summary$mean_trial_completion)])
table(trial_time_subject_summary$Round1[is.na(trial_time_subject_summary$mean_trial_completion)])
table(trial_time_subject_summary$Condition[is.na(trial_time_subject_summary$mean_trial_completion)])

# This plot combined with earlier memory and time on task plots further down
time_on_trials = plot_time_data(trial_time_summary, ylab = "Seconds", ymax = 80, title = "Mean time on trials")

t_trials = t.test(trial_time_subject_summary$mean_trial_completion[trial_time_subject_summary$Condition == "Describe" &
                                                                     trial_time_subject_summary$Round1 == FALSE],
                  trial_time_subject_summary$mean_trial_completion[trial_time_subject_summary$Condition == "Explain" &
                                                                     trial_time_subject_summary$Round1 == FALSE],
                  var.equal = T)
report_t_summary(t_trials) # Means are seconds on trials

# Plot graphs from 1. and 2. above side by side with patchwork
# time_on_task + time_on_trials
mem + time_on_task + time_on_trials +
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 20, face = "bold"))


# 3. Overall time on task comparing participants who did and didn't get the correct rule
task_time_join = generation_free_resp_coded %>%
  select(subjID, Condition, Revision) %>%
  inner_join(., summary_data, by = c("subjID", "Condition")) %>%
  select(subjID, Condition, Revision, experiment_completion_time)

t_task_time_correct = t.test(task_time_join$experiment_completion_time[task_time_join$Revision == 0],
                             task_time_join$experiment_completion_time[task_time_join$Revision == 1],
                             var.equal = T)

report_t_summary(t_task_time_correct) # Means are seconds on task


# 4. Time on evidence trials for participants who did and didn't get the correct rule

# NB: 17 initial round1 subjects have incomplete trial time data; impacts trial_time_subject_summary and trial_time_summary
unique(trial_time_subject_summary$subjID[is.na(trial_time_subject_summary$mean_trial_completion)])
table(trial_time_subject_summary$Round1[is.na(trial_time_subject_summary$mean_trial_completion)])
table(trial_time_subject_summary$Condition[is.na(trial_time_subject_summary$mean_trial_completion)])

trial_time_join = generation_free_resp_coded %>%
  select(subjID, Condition, Revision) %>%
  inner_join(., trial_time_subject_summary, by = c("subjID", "Condition")) %>%
  select(subjID, Condition, Revision, mean_trial_completion)

t_trial_time_correct = t.test(trial_time_join$mean_trial_completion[trial_time_join$Revision == 0],
                              trial_time_join$mean_trial_completion[trial_time_join$Revision == 1],
                              var.equal = T)

report_t_summary(t_trial_time_correct) # Means are seconds on trials




# COVARIATE ANALYSIS: EXPLANATIONS / DESCRIPTIONS ==============================

# sanity checks
glimpse(explanation_coded_data)
glimpse(explanation_coded_summary_subjects)
glimpse(explanation_coded_summary)
unique(explanation_coded_data$Subject)

# Plot results
plot_coded_explanation_data(explanation_coded_summary)

# Check mechanism effect: were explainers more likely to provide a mechanistic account?
t_mech = t.test(explanation_coded_summary_subjects$subject_total[explanation_coded_summary_subjects$measure == "mechanism_total" &
                                                                   explanation_coded_summary_subjects$Condition == "Explain"],
                explanation_coded_summary_subjects$subject_total[explanation_coded_summary_subjects$measure == "mechanism_total" &
                                                                   explanation_coded_summary_subjects$Condition == "Describe"],
                var.equal = T)
report_t_summary(t_mech) # Means are avg. number of references



# Analysis (copied from Williams & Lombrozo, 2010)
# First, do abstract feature references show a main effect of condition?
# i.e. do explainers make more abstract references?
abstract_data = explanation_coded_summary_subjects %>%
  filter(measure %in% c("shape_abstract_total", "color_abstract_total", "purple_dot_abstract_total"))
anova_abstract = aov(data = abstract_data, subject_total ~ Condition + measure)
summary(anova_abstract)
# Do concrete feature references show a main effect of condition?
# i.e. do control pariticipants make more concrete references?
concrete_data = explanation_coded_summary_subjects %>%
  filter(measure %in% c("shape_concrete_total", "color_concrete_total", "purple_dot_concrete_total"))
anova_concrete = aov(data = concrete_data, subject_total ~ Condition + measure)
summary(anova_concrete)



# ANOVAs suggest main effect of condition on number of concrete and abstract features
# Below t-tests confirm direction/significance for each feature individually

# Shape
t_shape_abs = t.test(explanation_coded_summary_subjects$subject_total[explanation_coded_summary_subjects$measure == "shape_abstract_total" &
                                                                   explanation_coded_summary_subjects$Condition == "Explain"],
                explanation_coded_summary_subjects$subject_total[explanation_coded_summary_subjects$measure == "shape_abstract_total" &
                                                                   explanation_coded_summary_subjects$Condition == "Describe"],
                var.equal = T)
report_t_summary(t_shape_abs) # Means are avg. number of references

t_shape_conc = t.test(explanation_coded_summary_subjects$subject_total[explanation_coded_summary_subjects$measure == "shape_concrete_total" &
                                                                        explanation_coded_summary_subjects$Condition == "Explain"],
                     explanation_coded_summary_subjects$subject_total[explanation_coded_summary_subjects$measure == "shape_concrete_total" &
                                                                        explanation_coded_summary_subjects$Condition == "Describe"],
                     var.equal = T)
report_t_summary(t_shape_conc) # Means are avg. number of references

# Color
t_color_abs = t.test(explanation_coded_summary_subjects$subject_total[explanation_coded_summary_subjects$measure == "color_abstract_total" &
                                                                        explanation_coded_summary_subjects$Condition == "Explain"],
                     explanation_coded_summary_subjects$subject_total[explanation_coded_summary_subjects$measure == "color_abstract_total" &
                                                                        explanation_coded_summary_subjects$Condition == "Describe"],
                     var.equal = T)
report_t_summary(t_color_abs) # Means are avg. number of references

t_color_conc = t.test(explanation_coded_summary_subjects$subject_total[explanation_coded_summary_subjects$measure == "color_concrete_total" &
                                                                         explanation_coded_summary_subjects$Condition == "Explain"],
                      explanation_coded_summary_subjects$subject_total[explanation_coded_summary_subjects$measure == "color_concrete_total" &
                                                                         explanation_coded_summary_subjects$Condition == "Describe"],
                      var.equal = T)
report_t_summary(t_color_conc) # Means are avg. number of references


# Purple dot
t_dot_abs = t.test(explanation_coded_summary_subjects$subject_total[explanation_coded_summary_subjects$measure == "purple_dot_abstract_total" &
                                                                        explanation_coded_summary_subjects$Condition == "Explain"],
                     explanation_coded_summary_subjects$subject_total[explanation_coded_summary_subjects$measure == "purple_dot_abstract_total" &
                                                                        explanation_coded_summary_subjects$Condition == "Describe"],
                     var.equal = T)
report_t_summary(t_dot_abs) # Means are avg. number of references

t_dot_conc = t.test(explanation_coded_summary_subjects$subject_total[explanation_coded_summary_subjects$measure == "purple_dot_concrete_total" &
                                                                         explanation_coded_summary_subjects$Condition == "Explain"],
                      explanation_coded_summary_subjects$subject_total[explanation_coded_summary_subjects$measure == "purple_dot_concrete_total" &
                                                                         explanation_coded_summary_subjects$Condition == "Describe"],
                      var.equal = T)
report_t_summary(t_dot_conc) # Means are avg. number of references





# APPENDIX ANALYSIS: POWER =====================================================

# Effect size and power for generation free response task effect
p1 = generation_props$success[generation_props$Condition == "Explain"] / generation_props$total[generation_props$Condition == "Explain"]
p2 = generation_props$success[generation_props$Condition == "Describe"] / generation_props$total[generation_props$Condition == "Describe"]

ES.h(p1, p2) # Effect size

power.prop.test(n = generation_props$total[generation_props$Condition == "Explain"], # could use either condition here
                p1 = p1,
                p2 = p2,
                # alternative = "one.sided",
                sig.level = 0.05) # Power to detect our effect size

power.prop.test(p1 = p1,
                p2 = p2,
                power = 0.8,
                # alternative = "one.sided",
                sig.level = 0.05) # Observations needed to detect our effect size with 80% power


# Effect size and power for generation judgment task effect
p1 = generation_judgment_summary$mean_accuracy[generation_judgment_summary$Condition == "Explain"]
p2 = generation_judgment_summary$mean_accuracy[generation_judgment_summary$Condition == "Describe"]

ES.h(p1, p2) # Effect size

power.prop.test(n = generation_props$total[generation_props$Condition == "Explain"], # could use either condition here
                p1 = p1,
                p2 = p2,
                # alternative = "one.sided",
                sig.level = 0.05) # Power to detect our effect size

power.prop.test(p1 = p1,
                p2 = p2,
                power = 0.8,
                # alternative = "one.sided",
                sig.level = 0.05) # Observations needed to detect our effect size with 80% power


# Effect size and power for evaluation effect (different rating of target across conditions)
power_data = evaluation_data %>%
  filter(is_target_rule == TRUE) %>%
  group_by(Condition) %>%
  summarize(mean_rating = mean(input_rule_rating),
            sd_rating = sd(input_rule_rating),
            var_rating = var(input_rule_rating),
            n = n())

# NB SDs are pretty different for two conditions so equal variance may be a stretch?
sd_pooled = sqrt(((power_data$sd_rating[power_data$Condition == "Explain"]^2) +
                    (power_data$sd_rating[power_data$Condition == "Describe"]^2)) / 2)
sd_pooled = ((power_data$var_rating[power_data$Condition == "Explain"] * (power_data$n[power_data$Condition == "Explain"] - 1)) +
  (power_data$var_rating[power_data$Condition == "Describe"] * (power_data$n[power_data$Condition == "Describe"] - 1))) /
  ((power_data$n[power_data$Condition == "Explain"] - 1) + (power_data$n[power_data$Condition == "Describe"] - 1))


mean_diff = power_data$mean_rating[power_data$Condition == "Explain"] -
  power_data$mean_rating[power_data$Condition == "Describe"]

power.t.test(n = power_data$n[power_data$Condition == "Explain"], # can use either condition here
             delta = mean_diff,
             sd = sd_pooled,
             alternative = "one.sided",
             sig.level = 0.05) # Power to detect our effect size

power.t.test(delta = mean_diff,
             sd = sd_pooled,
             sig.level = 0.05,
             alternative = "one.sided",
             power = 0.8) # Number of subjects needed to detect an effect with 80% power



# APPENDIX ANALYSIS: PREDICTIONS ===============================================

plot_prediction_summary(prediction_summary) # TODO get error bars on this

# Fit regressions to data just to ensure no signal
# TODO if including this, revise models to only look at trials > 4
# (or change graph above to look at all trials)
mod_exp = lm(data = prediction_summary[prediction_summary$Condition == "Explain",], accuracy ~ trial_index)
mod_des = lm(data = prediction_summary[prediction_summary$Condition == "Describe",], accuracy ~ trial_index)

summary(mod_exp)
summary(mod_des)



# APPENDIX ANALYSIS: Coded generation versus classification ====================

# See how our coding compared to accuracy in the classification task
generation_task_comparison = generation_free_resp_coded %>%
  inner_join(generation_judgment_subject_summary, by = c("subjID", "Condition"))

# How many people did we code as getting the rule who were < 100% on the classification?
generation_task_comparison %>%
  filter(Revision == 1 & subj_accuracy < 1)

# How many people got 100% on the classification but were coded as not getting the rule?
generation_task_comparison %>%
  filter(Revision == 0 & subj_accuracy == 1)

# Plot relationship between coded accuracy and classification accuracy
generation_task_comparison %>%
  ggplot(aes(x = subj_accuracy, y = Revision, color = Condition)) +
  geom_jitter(width = 0.05, height = 0.05, alpha = 0.75) +
  scale_color_viridis(discrete = T) +
  labs(x = "Classification accuracy", y = "Coded free resp. correct") +
  individ_plot_theme

# Correlation between the two
cor.test(generation_task_comparison$Revision, generation_task_comparison$subj_accuracy)




# APPENDIX ANALYSIS: Exploring evaluation distributions ========================

plot_evaluation_histogram = function(eval_data) {
  eval_data %>%
    ggplot(aes(x = input_rule_rating)) +
    geom_histogram(breaks = c(1, 2, 3, 4, 5, 6, 7),
                   position = "identity",
                   color = "lightblue", fill = "lightblue", alpha = 0.75) +
    labs(x = "Evaluation rating (1-7)") +
    individ_plot_theme
}

# ALL participants
# Explain condition, target rule
evaluation_data %>%
  filter(Condition == "Explain" & is_target_rule == T) %>%
  plot_evaluation_histogram()

# Describe condition, target rule
evaluation_data %>%
  filter(Condition == "Describe" & is_target_rule == T) %>%
  plot_evaluation_histogram()

# ONLY participants who did not generate correct target rule
# Explain condition, target rule
subset_participants %>%
  filter(Condition == "Explain" & is_target_rule == T) %>%
  plot_evaluation_histogram()

# Describe condition, target rule
subset_participants %>%
  filter(Condition == "Describe" & is_target_rule == T) %>%
  plot_evaluation_histogram()





# APPENDIX ANALYSIS: Evaluations broken out by generation success ==============

# Plot eval ratings of target rule among participants who did and didn't get correct rule in each condition
breakout_participants = generation_free_resp_coded %>%
  select(subjID, Condition, Revision) %>%
  inner_join(., evaluation_data, by = c("subjID", "Condition"))

subset_summary = breakout_participants %>%
  filter(is_target_rule == T) %>%
  # filter(rule_text == DISTRACTOR_RULE) %>%
  group_by(Condition, as.factor(Revision)) %>%
  summarize(mean_rating = mean(input_rule_rating),
            n = n(),
            se = sd(input_rule_rating) / sqrt(n),
            ci_lower = mean_rating - se,
            ci_upper = mean_rating + se) %>%
  rename(Revision = `as.factor(Revision)`)

breakout_participants %>%
  filter(is_target_rule == T) %>%
  # filter(rule_text == DISTRACTOR_RULE) %>%
  ggplot(aes(x = Condition, y = input_rule_rating, color = as.factor(Revision))) +
  geom_jitter(width = 0.25, alpha = 0.5, height = 0.25, size = 4) +
  geom_point(data = subset_summary,
             aes(x = Condition, y = mean_rating, color = Revision),
             size = 6) +
  geom_errorbar(data = subset_summary,
                aes(x = Condition, y = mean_rating, color = Revision, ymin = ci_lower, ymax = ci_upper),
                width = 0.25,
                size = 1) +
  scale_color_viridis(discrete = T,
                      labels = c("0" = "Incorrect hypothesis", "1" = "Correct hypothesis"),
                      name = element_blank(),
                      # Change defaults to be blue/green instead of yellow/purple
                      begin = 0.25,
                      end = 0.75) +
  scale_y_discrete(breaks = c(2, 3, 4, 5, 6, 7),
                   labels = c("2" = "2", "3" = "3", "4" = "4", "5" = "5", "6" = "6", "7" = "7"),
                   limits = c(2, 3, 4, 5, 6, 7)) +
  labs(y = "Rating (1-7)") +
  ggtitle("Target rule evaluation") +
  individ_plot_theme




