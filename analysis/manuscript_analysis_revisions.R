#'
#' REVISED analysis for manuscript submission
#'


rm(list = ls())
setwd("/Users/erikbrockbank/web/elc-lab/go_fish/analysis/")

library(tidyverse)
library(viridis)
library(patchwork)
library(pwr)
library(ES)
library(lme4)
library(boot)
library(emmeans)


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
        (input_judgment == judgment_catches_fish),
      accuracy_weighted = ifelse(
        input_correct,
        input_judgment_conf,
        -input_judgment_conf
      )
    )
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
  generation_judg_summary$Condition = factor(generation_judg_summary$Condition,
                                             levels = c("Explain", "Describe"))
  return(generation_judg_summary)
}


# Summarize generation free response data across participants
get_generation_free_response_summary = function(generation_free_response_coded) {
  generation_free_response_summary = generation_free_response_coded %>%
    group_by(Condition) %>%
    summarize(subjects = n(),
              correct_generation_pct = sum(Revision) / n(),
              correct_generation_sem = sqrt((correct_generation_pct * (1 - correct_generation_pct)) / subjects))
  generation_free_response_summary$Condition = factor(generation_free_response_summary$Condition,
                                                      levels = c("Explain", "Describe"))
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
  eval_summary$ruleset = factor(eval_summary$ruleset, levels = c("Target", "Distractor", "All other rules"))
  eval_summary$Condition = factor(eval_summary$Condition, levels = c("Explain", "Describe"))
  return(eval_summary)
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




# Bar chart of classification accuracy on binary generation judgment task by condition
plot_generation_judgments = function(generation_judgment_summary) {
  generation_judgment_summary %>%
    ggplot(aes(x = Condition, y = mean_accuracy,
               color = Condition, fill = Condition)) +
    geom_bar(stat = "identity", alpha = 0.5, width = 0.5) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.25) +
    geom_hline(yintercept = 0.5, linetype = "dashed", size = 1) +
    # labs(x = "", y = "Mean classification accuracy") +
    labs(x = "", y = "") + # NB: no y label here because we combine plots
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
plot_generation_free_responses = function(generation_free_resp_summary) {
  generation_free_resp_summary %>%
    ggplot(aes(x = Condition, y = correct_generation_pct,
               color = Condition, fill = Condition)) +
    geom_bar(stat = "identity", width = 0.5, alpha = 0.5) +
    geom_errorbar(aes(ymin = correct_generation_pct - correct_generation_sem,
                      ymax = correct_generation_pct + correct_generation_sem),
                  width = 0.25) +
    labs(x = "", y = "Percent Correct") +
    ggtitle("Free Response Accuracy") +
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
generation_free_resp_summary = get_generation_free_response_summary(generation_free_resp_coded)
generation_judgment_subject_summary = get_generation_judgment_subj_summary(generation_judgment_data)
generation_judgment_summary = get_generation_judgment_summary(generation_judgment_subject_summary)
evaluation_summary = get_evaluation_summary(evaluation_data)
memory_subject_summary = get_memory_subj_summary(memory_data)
memory_summary = get_memory_summary(memory_subject_summary)

# Coded explanation / description data
explanation_coded_data = read_coded_explanation_data(EXPLANATION_DATA_CODED)
explanation_coded_summary_subjects = get_explanation_coded_subj_summary(explanation_coded_data)
explanation_coded_summary = get_explanation_coded_summary(explanation_coded_summary_subjects)




# ANALYSIS: POWER ====
# NB: This is revised from power analysis in original manuscript_analysis
# TODO may want to split this out into separate sub-sections for better readability

# Power for E1
# How big of an effect size were we able to detect in E1 with 80% power?
pwr.2p.test(n = 43,
            power = 0.8,
            sig.level = 0.05)




# Bonawitz & Griffiths (2010) E1 (E2/E3 not relevant for our approach)
# Reproducing their stats:
m = as.table(rbind(c(13, 7), c(7, 13))) # This is based on 65%, 35% correct rule identification
dimnames(m) = list(condition = c("Strong prime", "Neutral prime"), target = c("Correct", "Incorrect"))
chisq.test(m)
# NB: this differs from chi sq. result reported, unclear why
# NB: also, they report df = 40 but also mention excluding 2 participants

# What was their power?
power.prop.test(
  p1 = 0.65,
  p2 = 0.35,
  n = 20, # NB: 40 participants across 2 conditions
  sig.level = .05
) # Looks like they were a little under-powered...

# How many subjects do we need for similar proportions?
power.prop.test(
  p1 = 0.65,
  p2 = 0.35,
  sig.level = .05,
  power = 0.8
) # If we want their effect with 80% power, we need 42.4 participants per group

# Run 2p test for comparison to prop test above
ES.h(0.65, 0.35)
pwr.2p.test(
  h = ES.h(p1 = 0.65, p2 = 0.35),
  sig.level = .05,
  power = 0.8
)

# Finally, is there a way to do the same calculations using R2's Cramer's V
# rather than the raw proportions above?

# Cramer's V := sqrt(chi_sq / N(k-1))
# B&G report chi-sq = 3.6, df(40, 1)
v = sqrt((3.6/(40*1)))

# These calculations follow instructions at r pwr vignette for pwr.chisq.test
prob = matrix(c(0.325,0.175,0.175,0.325), ncol=2,
              dimnames = list(c("Explain","Describe"),c("Correct","Incorrect")))
ES.w2(prob) # NB: this produces same value as Cramer's V

# What was their power?
pwr.chisq.test(w = ES.w2(prob),
               N = 40, # Now we use total N
               df = 1,
               sig.level = 0.05
) # NB: this is almost identical to prop.test above
# How many subjects do we need?
pwr.chisq.test(w = ES.w2(prob),
               df = 1,
               power = 0.8,
               sig.level = 0.05
) # NB: this is almost identical to the prop.test results as well


# Williams & Lombrozo (2010) -> closer comparison to our task
# E1 free response target rule production (Table 2)
# Explain: 26 out of 75; Describe: 6 out of 75
power.prop.test(
  p1 = (26/75),
  p2 = (6/75),
  n = 75, # NB: toggling this to 43 shows we have 87% power
  sig.level = 0.05
) # 98% power
power.prop.test(
  p1 = (26/75),
  p2 = (6/75),
  sig.level = 0.05,
  power = 0.8
) # We need 35 people per group

# These calculations follow instructions at r pwr vignette for pwr.chisq.test
prob = matrix(c(26/150,49/150,6/150,69/150), ncol=2,
              dimnames = list(c("Explain","Describe"),c("Correct","Incorrect")))
ES.w2(prob)

# What was their power?
pwr.chisq.test(w = ES.w2(prob),
               N = 150, # NB: toggling this to 86 reveals we have 86% power
               df = 1,
               sig.level = 0.05
) # NB: this is almost identical to prop.test above
# How many subjects do we need?
pwr.chisq.test(w = ES.w2(prob),
               df = 1,
               power = 0.8,
               # N = 86,
               sig.level = 0.05
) # We need 75 people total


# Williams/Lombrozo 2010 E1 test accuracy
# NB: this is eye-balled from Fig. 3
power.prop.test(
  p1 = 0.65,
  p2 = 0.4,
  sig.level = 0.05,
  n = 75 # NB: toggling this to 43 shows we're under-powered in 2AFC generation task
) # They had 87% power

power.prop.test(
  p1 = 0.65,
  p2 = 0.4,
  sig.level = 0.05,
  power = 0.8
)
# For 80% power we would need 62 people per condition
# But note this is 2AFC classification accuracy, not free response


# What power did we have to detect our E1 results (if they are accurate)?
generation_props = generation_free_resp_coded %>%
  group_by(Condition) %>%
  summarize(success = sum(Revision),
            total = n())

# Effect size and power for generation free response task effect
p1 = generation_props$success[generation_props$Condition == "Explain"] / generation_props$total[generation_props$Condition == "Explain"]
p2 = generation_props$success[generation_props$Condition == "Describe"] / generation_props$total[generation_props$Condition == "Describe"]

ES.h(p1, p2) # Effect size


# What is our power with given Ns?
pwr.2p.test(h = ES.h(p1, p2),
            n = generation_props$total[generation_props$Condition == "Explain"], # could use either condition here
            sig.level = 0.05)

# Similar analysis with cohen's w
exp_corr = generation_props$success[generation_props$Condition == "Explain"]
exp_incorr = generation_props$total[generation_props$Condition == "Explain"] - exp_corr
des_corr = generation_props$success[generation_props$Condition == "Describe"]
des_incorr = generation_props$total[generation_props$Condition == "Describe"] - des_corr
total = sum(generation_props$total)
prob = matrix(c(exp_corr/total, exp_incorr/total, des_corr/total, des_incorr/total),
              ncol=2,
              dimnames = list(c("Explain","Describe"),c("Correct","Incorrect")))
ES.w2(prob)

pwr.chisq.test(w = ES.w2(prob),
               N = 86,
               df = 1,
               sig.level = 0.05
) # NB: this is almost identical to prop.test above


# Double-checking E2 power

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

# NB: one.sided seems justified here since we're testing claim that explain eval > describe eval
power.t.test(delta = mean_diff,
             sd = sd_pooled,
             sig.level = 0.05,
             alternative = "one.sided",
             power = 0.8) # Number of subjects needed to detect an effect with 80% power




# ANALYSIS: GENERATION ====

# Figures
generation_fr = plot_generation_free_responses(generation_free_resp_summary)
generation_class = plot_generation_judgments(generation_judgment_summary)
generation_fr + generation_class


# Analysis

# Free response task
generation_props = generation_free_resp_coded %>%
  group_by(Condition) %>%
  summarize(success = sum(Revision),
            total = n())
prop.test(c(generation_props$success), c(generation_props$total))

# Classification task
# Similar Chi-sq test with raw count of people who were *coded as getting rule* is what we report above
# for the free response task so this is effectively a parallel analysis for people who got 100% on judgment task
generation_task_comparison = generation_free_resp_coded %>%
  inner_join(generation_judgment_subject_summary, by = c("subjID", "Condition"))
# NB: we want table to be subj_accuracy == 1 but we do < 1 below to get count of == 1 as the *first* column
count_data = table(generation_task_comparison$Condition, generation_task_comparison$subj_accuracy < (8/8))
chisq.test(count_data) # NB: this is equivalent to prop.test(count_data)
# Same test for 7/8 correct and 6/8 correct
count_data = table(generation_task_comparison$Condition, generation_task_comparison$subj_accuracy < (7/8))
chisq.test(count_data)
count_data = table(generation_task_comparison$Condition, generation_task_comparison$subj_accuracy < (6/8))
chisq.test(count_data)


# Mixed effects analysis
# NB: judgment_index (trial number) is identical factor as shape ID because order was fixed
# table(generation_judgment_data$judgment_index, generation_judgment_data$judgment_shape)


generation_judgment_data$Condition = factor(generation_judgment_data$Condition,
                                               levels = c("Explain", "Describe"))

# Compare item-level accuracy
m0 = with(generation_judgment_data,
          glmer(as.numeric(input_correct) ~ (1|subjID) + (1|judgment_index),
                family = "binomial"))

m1 = with(generation_judgment_data,
          glmer(as.numeric(input_correct) ~ Condition + (1|subjID) + (1|judgment_index),
                family = "binomial"))

# summary(m1)
# m1 %>% coef()
anova(m1, m0, test = 'LRT')



# Compare confidence-weighted accuracy
m0 = with(generation_judgment_data,
          # random intercept for subject and trial
          lmer(accuracy_weighted ~ (1|subjID) + (1|judgment_index),
               REML = F))

m1 = with(generation_judgment_data,
          lmer(accuracy_weighted ~ Condition + (1|subjID) + (1|judgment_index),
               REML = F))

# summary(m1)
# m1 %>% coef()
anova(m1, m0, test = 'LRT')




# ANALYSIS: EVALUATION ====

# Figures
plot_evaluation_results(evaluation_summary, RULE_EVAL_LABELS)


# Analysis

# glimpse(evaluation_data)
# unique(evaluation_data$rule_text)
evaluation_rule_abbrev = c(
  "If a lure combination has a red shape or a blue shape, it will catch fish." = "misc1",
  "If a lure combination has a diamond, it will catch fish." = "misc2",
  "If a lure combination has a pointy shape on the bottom, it will catch fish." = "target",
  "There is no pattern to which lure combinations catch fish: the results are random, but there are approximately equal numbers that catch fish and don’t." = "random",
  "If a lure combination has a yellow shape or a diamond on the bottom, it will catch fish." = "distractor",
  "If a lure combination has a purple dot on at least one of the lures, it will catch fish." = "misc3"
)

evaluation_rule_abbrev_coarse = c(
  "If a lure combination has a red shape or a blue shape, it will catch fish." = "other",
  "If a lure combination has a diamond, it will catch fish." = "other",
  "If a lure combination has a pointy shape on the bottom, it will catch fish." = "target",
  "There is no pattern to which lure combinations catch fish: the results are random, but there are approximately equal numbers that catch fish and don’t." = "other",
  "If a lure combination has a yellow shape or a diamond on the bottom, it will catch fish." = "distractor",
  "If a lure combination has a purple dot on at least one of the lures, it will catch fish." = "other"
)

evaluation_data = evaluation_data %>%
  mutate(rule_text_abbrev = as.factor(evaluation_rule_abbrev[rule_text]),
         rule_text_abbrev_coarse = as.factor(evaluation_rule_abbrev_coarse[rule_text]))
# glimpse(evaluation_data)

# Basic model comparison: does condition matter across all evals?
m0 = lmer(formula = input_rule_rating ~ rule_text_abbrev + (1|subjID),
          data = evaluation_data,
          REML = F)
m1.main = lmer(formula = input_rule_rating ~ rule_text_abbrev + Condition + (1|subjID),
               data = evaluation_data,
               REML = F)
m1.int = lmer(formula = input_rule_rating ~ rule_text_abbrev * Condition + (1|subjID),
              data = evaluation_data,
              REML = F)

anova(m0, m1.main, m1.int, test = 'LRT')
emmeans(m1.int, specs = pairwise ~ rule_text_abbrev * Condition)
# No effects of condition for target or distractor
# contrast                                 estimate    SE  df t.ratio p.value
# target Describe - target Explain          -0.5116 0.363 467  -1.410 0.9612
# distractor Describe - distractor Explain   0.6279 0.363 467   1.731 0.8531

# Bayes factors?
# library(bayestestR)
# bayesfactor_models(m1.int, denominator = m1.main)



# Standard tests

# target rule ratings across conditions
t_eval = t.test(
  evaluation_data$input_rule_rating[evaluation_data$Condition == "Describe" & evaluation_data$rule_text_abbrev == "target"],
  evaluation_data$input_rule_rating[evaluation_data$Condition == "Explain" & evaluation_data$rule_text_abbrev == "target"],
  var.equal = T
)
t_eval

# distractor rule ratings across conditions
t_distractor = t.test(
  evaluation_data$input_rule_rating[evaluation_data$Condition == "Describe" & evaluation_data$rule_text_abbrev == "distractor"],
  evaluation_data$input_rule_rating[evaluation_data$Condition == "Explain" & evaluation_data$rule_text_abbrev == "distractor"],
  var.equal = T
)
t_distractor


# difference between target and distractor ratings across conditions
t_eval_target_dist_exp = t.test(
  evaluation_data$input_rule_rating[evaluation_data$Condition == "Explain"
                                    & evaluation_data$rule_text_abbrev == "distractor"],
  evaluation_data$input_rule_rating[evaluation_data$Condition == "Explain"
                                    & evaluation_data$rule_text_abbrev == "target"],
  paired = T,
  var.equal = T
)
t_eval_target_dist_exp

t_eval_target_dist_desc = t.test(
  evaluation_data$input_rule_rating[evaluation_data$Condition == "Describe"
                                    & evaluation_data$rule_text_abbrev == "distractor"],
  evaluation_data$input_rule_rating[evaluation_data$Condition == "Describe"
                                    & evaluation_data$rule_text_abbrev == "target"],
  paired = T,
  var.equal = T
)
t_eval_target_dist_desc


# anova interaction test
# check for significant interaction between condition and target/distractor evals
eval_data_distractor_target = evaluation_data %>%
  filter(rule_text_abbrev == "target" | rule_text_abbrev == "distractor")
interaction_test = aov(data = eval_data_distractor_target, input_rule_rating ~ Condition*is_target_rule)
summary(interaction_test)



# ANALYSIS: MEMORY ====

# Figures
memory_summary$Condition = factor(memory_summary$Condition, levels = c("Explain", "Describe"))
mem_plot = plot_memory_data(memory_summary) # This plot is combined with time on task plots further down
mem_plot


# Analysis

# Mixed effects: effect of condition?
# unique(memory_data$memory_shape)
shape_lookup = c(
  "{'top_shape': 'teardrop', 'top_color': 'yellow', 'top_texture': False, 'bottom_shape': 'diamond', 'bottom_color': 'green', 'bottom_texture': False}" = "1",
  "{'top_shape': 'triangle', 'top_color': 'yellow', 'top_texture': False, 'bottom_shape': 'circle', 'bottom_color': 'green', 'bottom_texture': True}" = "2",
  "{'top_shape': 'oval', 'top_color': 'green', 'top_texture': False, 'bottom_shape': 'diamond', 'bottom_color': 'red', 'bottom_texture': True}" = "3",
  "{'top_shape': 'teardrop', 'top_color': 'red', 'top_texture': True, 'bottom_shape': 'triangle', 'bottom_color': 'yellow', 'bottom_texture': False}" = "4",
  "{'top_shape': 'diamond', 'top_color': 'blue', 'top_texture': False, 'bottom_shape': 'circle', 'bottom_color': 'blue', 'bottom_texture': False}" = "5",
  "{'top_shape': 'star', 'top_color': 'blue', 'top_texture': False, 'bottom_shape': 'circle', 'bottom_color': 'red', 'bottom_texture': False}" = "6",
  "{'top_shape': 'circle', 'top_color': 'yellow', 'top_texture': False, 'bottom_shape': 'oval', 'bottom_color': 'yellow', 'bottom_texture': False}" = "7",
  "{'top_shape': 'star', 'top_color': 'blue', 'top_texture': False, 'bottom_shape': 'star', 'bottom_color': 'yellow', 'bottom_texture': True}" = "8"
)

memory_data = memory_data %>%
  mutate(shape_index = as.factor(shape_lookup[memory_shape]))
# glimpse(memory_data)

m0 = with(memory_data,
          glmer(as.numeric(memory_correct) ~ (1|subjID) + (1|shape_index),
                family = "binomial"))

m1 = with(memory_data,
          glmer(as.numeric(memory_correct) ~ Condition + (1|subjID) + (1|shape_index),
                family = "binomial"))

anova(m1, m0, test = 'LRT')
# summary(m1)
# What are estimated accuracy percents in each condition?
emmeans(m1, pairwise ~ Condition)$emmeans
inv.logit(0.695)
inv.logit(0.838)



# Role of memory in hypothesis generation

memory_hypothesis_join = generation_free_resp_coded %>%
  select(subjID, Condition, Revision) %>%
  rename("generation_accuracy" = Revision) %>%
  inner_join(generation_judgment_subject_summary, by = c("subjID", "Condition")) %>%
  rename("classification_accuracy" = subj_accuracy) %>%
  inner_join(memory_subject_summary, by = c("subjID", "Condition")) %>%
  rename("memory_accuracy" = subj_accuracy) %>%
  subset(., select = -c(subj_correct, total)) %>%
  mutate(Condition = as.factor(Condition))

# free response accuracy (0/1) ~ memory accuracy + Condition
mem1 = with(memory_hypothesis_join,
                  glm(generation_accuracy ~ memory_accuracy + Condition,
                      family = 'binomial'))
summary(mem1)


# Classification accuracy (item level) ~ memory accuracy + Condition
generation_judgment_data = generation_judgment_data %>%
  left_join(memory_subject_summary, by = c("subjID", "Condition")) %>%
  rename("memory_accuracy" = subj_accuracy) %>%
  subset(., select = -c(subj_correct, total))
glimpse(generation_judgment_data)


m0 = with(generation_judgment_data,
          glmer(as.numeric(input_correct) ~ (1|subjID) + (1|judgment_index),
                family = "binomial"))

m1.mem = with(generation_judgment_data,
              glmer(as.numeric(input_correct) ~ memory_accuracy + (1|subjID) + (1|judgment_index),
                    family = "binomial"))

m1.cond = with(generation_judgment_data,
          glmer(as.numeric(input_correct) ~ memory_accuracy + Condition + (1|subjID) + (1|judgment_index),
                family = "binomial"))

# summary(m1)
# m1 %>% coef()
anova(m0, m1.mem, m1.cond, test = 'LRT')




# ANALYSIS: EXPLANATION / DESCRIPTION CONTENT ====

# Figures
explanation_coded_summary$Condition = factor(explanation_coded_summary$Condition,
                                             levels = c("Explain", "Describe"))
plot_coded_explanation_data(explanation_coded_summary)


# Analysis

# Do explainers reference *more* features?
explanation_coded_data = explanation_coded_data %>%
  rowwise() %>%
  mutate(total_refs = sum(`FINAL - total shape references`, `FINAL - total color references`, `FINAL - total purple dot references`),
         total_abstract = sum(`FINAL - abstract shape references`, `FINAL - abstract color references`, `FINAL - abstract purple dot references`),
         total_concrete = sum(`FINAL - concrete shape references`, `FINAL - concrete color references`, `FINAL - concrete purple dot references`),
         total_mechanisms = `FINAL - total mechanisms`)
glimpse(explanation_coded_data)


m0.total = glmer(formula = total_refs ~ (1 | Subject) + (1 | `Trial index`),
                 data = explanation_coded_data,
                 family = "poisson")
m1.total = glmer(formula = total_refs ~ Condition + (1 | Subject) + (1 | `Trial index`),
                 data = explanation_coded_data,
                 family = "poisson")
anova(m1.total, m0.total, test = 'LRT')

# What are estimated mean total counts?
emmeans(m1.total, pairwise ~ Condition)
exp(1.343)
exp(0.089)
hist(explanation_coded_data$total_refs[explanation_coded_data$Condition == "Explain"])
hist(explanation_coded_data$total_refs[explanation_coded_data$Condition == "Describe"])


# convert to long form
explanation_coded_long = explanation_coded_data %>%
  select(Subject, Condition, `Trial index`, total_abstract, total_concrete, total_mechanisms) %>%
  pivot_longer(.,
               cols = total_abstract:total_mechanisms,
               names_to = "ref_type",
               names_prefix = "total_",
               values_to = "Count")
# sanity check the above
hist(explanation_coded_long$Count[explanation_coded_long$ref_type == "mechanisms"])
hist(explanation_coded_long$Count[explanation_coded_long$ref_type == "abstract"])
hist(explanation_coded_long$Count[explanation_coded_long$ref_type == "concrete"])


m0 = glmer(formula = Count ~ (1 | Subject) + (1 | `Trial index`),
           data = explanation_coded_long,
           family = "poisson")
m1.cond = glmer(formula = Count ~ Condition + (1 | Subject) + (1 | `Trial index`),
                data = explanation_coded_long,
                family = "poisson")
m1.main = glmer(formula = Count ~ Condition + ref_type + (1 | Subject) + (1 | `Trial index`),
                data = explanation_coded_long,
                family = "poisson")
m1.int = glmer(formula = Count ~ Condition * ref_type + (1 | Subject) + (1 | `Trial index`),
               data = explanation_coded_long,
               family = "poisson")

anova(m0, m1.cond, m1.main, m1.int, test = 'LRT')
emmeans(m1.int, pairwise ~ Condition * ref_type)

# $emmeans
# Condition ref_type   emmean    SE  df asymp.LCL asymp.UCL
# Describe  abstract   -2.434 0.208 Inf    -2.841    -2.027
# Explain   abstract   -0.659 0.109 Inf    -0.873    -0.445
# Describe  concrete    1.324 0.078 Inf     1.171     1.477
# Explain   concrete   -0.577 0.107 Inf    -0.786    -0.367
# Describe  mechanisms -2.802 0.245 Inf    -3.282    -2.321
# Explain   mechanisms -0.734 0.112 Inf    -0.953    -0.515

# $contrasts
# contrast                                 estimate    SE  df z.ratio p.value
# Describe abstract - Explain abstract      -1.7751 0.229 Inf  -7.755 <.0001
# Describe concrete - Explain concrete       1.9004 0.122 Inf  15.632 <.0001
# Describe mechanisms - Explain mechanisms  -2.0677 0.264 Inf  -7.827 <.0001




# Do explainers succeed only when they have *good* explanations?

response_generation_success = explanation_coded_data %>%
  rename("subjID" = Subject) %>%
  inner_join(generation_free_resp_coded, by = c("subjID", "Condition")) %>%
  rename("free_resp_accuracy" = Revision)
# glimpse(response_generation_success)


# Get total concrete, abstract, and mechanisms
response_agg = response_generation_success %>%
  group_by(subjID, Condition) %>%
  summarize(mech_total = sum(`FINAL - total mechanisms`),
            abs_total = sum(total_abstract_refs),
            concrete_total = sum(total_concrete_refs),
            refs_total = sum(abs_total, concrete_total),
            abs_prop = abs_total / sum(abs_total, concrete_total),
            mech_prop = mech_total / refs_total,
            free_resp_accuracy = unique(free_resp_accuracy)) %>%
  ungroup()

explanation_agg = response_agg %>%
  filter(Condition == "Explain")
# NB: this drops us to 34 participants that we have coded explanation data for

# Correlation between these variables
explanation_agg %>%
  select(mech_total, abs_total, concrete_total) %>%
  cor(.)
# mech / abs
cor.test(explanation_agg$mech_total, explanation_agg$abs_total)
plot(explanation_agg$mech_total, explanation_agg$abs_total)
# mech / concrete
cor.test(explanation_agg$mech_total, explanation_agg$concrete_total)
plot(explanation_agg$mech_total, explanation_agg$concrete_total)
# abs / concrete
cor.test(explanation_agg$abs_total, explanation_agg$concrete_total)
plot(explanation_agg$abs_total, explanation_agg$concrete_total)


free_resp_model = glm(free_resp_accuracy ~ refs_total + mech_total + abs_total,
                      data = explanation_agg,
                      family = "binomial")
summary(free_resp_model)


# Relationship between classification accuracy and references

response_class = response_agg %>%
  left_join(generation_judgment_data, by = c("subjID", "Condition"))

response_class = response_class %>%
  filter(Condition == "Explain")
# glimpse(response_class)

m0 = glmer(formula = as.numeric(input_correct) ~ (1|subjID) + (1|judgment_index),
           data = response_class,
           family = "binomial")

m1 = glmer(formula = as.numeric(input_correct) ~ refs_total + (1|subjID) + (1|judgment_index),
           data = response_class,
           family = "binomial")

m2 = glmer(formula = as.numeric(input_correct) ~ refs_total + mech_total + (1|subjID) + (1|judgment_index),
           data = response_class,
           family = "binomial")

m3 = glmer(formula = as.numeric(input_correct) ~ refs_total + mech_total + abs_total + (1|subjID) + (1|judgment_index),
           data = response_class,
           family = "binomial")

anova(m0, m1, m2, m3, test = 'LRT')
emmeans(m3, ~ refs_total + mech_total + abs_total)


# Can we compare to people who didn't use mechanism / abstract references?
# Not really...
explanation_agg %>%
  filter(mech_total == 0)
explanation_agg %>%
  filter(abs_total <= 3)




# APPENDIX ANALYSIS: Coded generation versus classification ====

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





# APPENDIX ANALYSIS: Evaluations broken out by generation success ====

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




