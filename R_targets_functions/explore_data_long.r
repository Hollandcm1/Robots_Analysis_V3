# explore_data_long


# Figures of interest
# 1. Time by trial per participant
# 2. Time by trial per participant and condition
# 3. Force x histogram
# 4. Force y histogram
# 5. Force magnitude histogram
# 6. Average velocity by trial per participant
# 7. Average velocity by trial per participant and condition
# 8. Path length
# 9. Proximity

# library(targets)
# library(here)
# library(tidyverse)
# library(ggplot2)
# library(ggbeeswarm)

# data <- tar_read(data_long_calculated)

explore_data_long <- function(data) {

  # save path
  save_path <- here("output", "exploreitory")

  # aggragate data
  trial_summary <- data %>%
    group_by(participant, condition_names, trial) %>%
    summarise(
      time = mean(time),
      force_x = mean(force_x),
      force_y = mean(force_y),
      force_magnitude = mean(force_magnitude),
      velocity = mean(average_velocity),
      path_length = sum(path_length),
      proximity = mean(average_proximity)
    ) 


  ########################################
  ### Number of trials per participant ###
  ########################################

  # plot
  p <- ggplot(trial_summary, aes(x = participant, fill = condition_names)) +
    geom_bar() +
    labs(
      title = "Number of trials per participant",
      x = "Participant",
      y = "Number of trials"
    ) +
    theme_minimal() +
    geom_hline(yintercept = 24, linetype = "dashed", color = "red") 

  p

  # save
  ggsave(here(save_path, "number_of_trials_per_participant.png"), p, width = 8, height = 6, units = "in")


  ###################################################
  ### Time by trial per participant and condition ###
  ###################################################

  # plot
  dodge_width <- 0.75

  p <- ggplot(trial_summary, aes(x = participant, y = time, fill = condition_names)) +
    geom_violin(position = position_dodge(width = dodge_width), alpha =0.5) +
    geom_boxplot(width = 0.2, position = position_dodge(width = dodge_width)) +
    geom_point(position = position_dodge(width = dodge_width)) +
    labs(
      title = "Time by trial per participant and condition",
      x = "Trial",
      y = "Time (s)"
    ) +
    theme_minimal()

  p

  # save
  ggsave(here(save_path, "time_by_trial_per_participant_and_condition.png"), p, width = 8, height = 6, units = "in")
    

  #########################################
  ### Force x histogram per participant ###
  #########################################

  # plot
  p <- ggplot(trial_summary, aes(x = force_x, fill = condition_names)) +
    geom_histogram(bins = 7, position = "dodge") +
    labs(
      title = "Force x histogram",
      x = "Force x",
      y = "Frequency"
    ) +
    theme_minimal()

  p

  # save
  ggsave(here(save_path, "force_x_histogram.png"), p, width = 8, height = 6, units = "in")


  ###################################
  ### Force x box and violin plot ###
  ###################################

  # plot
  p <- ggplot(trial_summary, aes(x = participant, y = force_x)) +
    geom_violin(alpha = 0.5) +
    geom_boxplot(width = 0.2) +
    geom_beeswarm() +
    labs(
      title = "Force x by participant",
      x = "Participant",
      y = "Force x"
    ) +
    theme_minimal()

  p

  # save
  ggsave(here(save_path, "force_x_by_participant.png"), p, width = 8, height = 6, units = "in")


  #########################################
  ### Force y histogram per participant ###
  #########################################

  # plot
  p <- ggplot(trial_summary, aes(x = force_y, fill = condition_names)) +
    geom_histogram(bins = 7, position = "dodge") +
    labs(
      title = "Force y histogram",
      x = "Force y",
      y = "Frequency"
    ) +
    theme_minimal()

  p

  # save
  ggsave(here(save_path, "force_y_histogram.png"), p, width = 8, height = 6, units = "in")


  ###################################
  ### Force y box and violin plot ###
  ###################################

  # plot
  p <- ggplot(trial_summary, aes(x = participant, y = force_y)) +
    geom_violin(alpha = 0.5) +
    geom_boxplot(width = 0.2) +
    geom_beeswarm() +
    labs(
      title = "Force y by participant",
      x = "Participant",
      y = "Force y"
    ) +
    theme_minimal()

  p

  # save
  ggsave(here(save_path, "force_y_by_participant.png"), p, width = 8, height = 6, units = "in")


  #################################################
  ### Force magnitude histogram per participant ###
  #################################################

  # plot
  p <- ggplot(trial_summary, aes(x = force_magnitude, fill = condition_names)) +
    geom_histogram(bins = 7, position = "dodge") +
    labs(
      title = "Force magnitude histogram",
      x = "Force magnitude",
      y = "Frequency"
    ) +
    theme_minimal()

  p

  # save
  ggsave(here(save_path, "force_magnitude_histogram.png"), p, width = 8, height = 6, units = "in")


  ######################################
  ### Force magnitude box and violin ###
  ######################################

  # plot
  p <- ggplot(trial_summary, aes(x = participant, y = force_magnitude)) +
    geom_violin(alpha = 0.5) +
    geom_boxplot(width = 0.2) +
    geom_beeswarm() +
    labs(
      title = "Force magnitude by participant",
      x = "Participant",
      y = "Force magnitude"
    ) +
    theme_minimal()

  p

  # save
  ggsave(here(save_path, "force_magnitude_by_participant.png"), p, width = 8, height = 6, units = "in")


  ##################################################
  ### Average velocity by trial per participant ###
  ##################################################

  # plot
  p <- ggplot(trial_summary, aes(x = participant, y = velocity, fill = condition_names)) +
    geom_violin(position = position_dodge(width = dodge_width), alpha =0.5) +
    geom_boxplot(width = 0.2, position = position_dodge(width = dodge_width)) +
    geom_point(position = position_dodge(width = dodge_width)) +
    labs(
      title = "Average velocity by trial per participant",
      x = "Participant",
      y = "Average velocity"
    ) +
    theme_minimal()

  p

  # save
  ggsave(here(save_path, "average_velocity_by_trial_per_participant.png"), p, width = 8, height = 6, units = "in")


  ##########################################################
  ### Path length by trial per participant and condition ###
  ##########################################################

  # plot
  p <- ggplot(trial_summary, aes(x = participant, y = path_length, fill = condition_names)) +
    geom_violin(position = position_dodge(width = dodge_width), alpha =0.5) +
    geom_boxplot(width = 0.2, position = position_dodge(width = dodge_width)) +
    geom_point(position = position_dodge(width = dodge_width)) +
    labs(
      title = "Path length by trial per participant",
      x = "Participant",
      y = "Path length"
    ) +
    theme_minimal()

  p

  # save
  ggsave(here(save_path, "path_length_by_trial_per_participant.png"), p, width = 8, height = 6, units = "in")


  ########################################################
  ### Proximity by trial per participant and condition ###
  ########################################################

  # plot
  p <- ggplot(trial_summary, aes(x = participant, y = proximity, fill = condition_names)) +
    geom_violin(position = position_dodge(width = dodge_width), alpha =0.5) +
    geom_boxplot(width = 0.2, position = position_dodge(width = dodge_width)) +
    geom_point(position = position_dodge(width = dodge_width)) +
    labs(
      title = "Proximity by trial per participant",
      x = "Participant",
      y = "Proximity"
    ) +
    theme_minimal()

  p

  # save
  ggsave(here(save_path, "proximity_by_trial_per_participant.png"), p, width = 8, height = 6, units = "in")


  ###############################################
  ### histogram of raw data - force_magnitude ###
  ###############################################

  # plot
  p <- ggplot(data, aes(x = force_magnitude)) +
    geom_histogram(bins = 50) +
    labs(
      title = "Force magnitude histogram",
      x = "Force magnitude",
      y = "Frequency"
    ) +
    theme_minimal()

  p

  # save
  ggsave(here(save_path, "force_magnitude_histogram_raw.png"), p, width = 8, height = 6, units = "in")


  ##################################################
  ### scatter plot of raw data - force_magnitude ###
  ##################################################

  # plot
  p <- ggplot(data, aes(x = force_x, y = force_y, color = force_magnitude)) +
    geom_point(size = 0.5, alpha = 0.3) +
    labs(
      title = "Force x vs Force y",
      x = "Force x",
      y = "Force y"
    ) +
    theme_minimal()

  p

  # save
  ggsave(here(save_path, "force_x_vs_force_y_raw.png"), p, width = 6, height = 6, units = "in")

  
  ###############################################
  ### histogram of raw data - linear_velocity ###
  ###############################################

  # plot
  p <- ggplot(data, aes(x = linear_velocity)) +
    geom_histogram(bins = 50) +
    labs(
      title = "Linear velocity histogram",
      x = "Linear velocity",
      y = "Frequency"
    ) +
    theme_minimal()

  p

  # save
  ggsave(here(save_path, "linear_velocity_histogram_raw.png"), p, width = 8, height = 6, units = "in")
}
