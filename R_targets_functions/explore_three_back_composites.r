# explore_three_back_composites

# library(targets)
# library(ggplot2)
# library(here)


# data <- tar_read(three_back_data_with_composites)

explore_three_back_composites <- function(data) {

  # save path 
  save_path <- here("output", "three_back_exploration")
  proportion_correct_path <- here(save_path, "proportion_correct")
  proportion_error_path <- here(save_path, "proportion_error")
  proportion_pause_path <- here(save_path, "proportion_pause")
  proportion_error_and_pause_path <- here(save_path, "proportion_error_and_pause")

  # make directories if they don't exist
  if (!dir.exists(proportion_correct_path)) {
    dir.create(proportion_correct_path)
  }
  if (!dir.exists(proportion_error_path)) {
    dir.create(proportion_error_path)
  }
  if (!dir.exists(proportion_pause_path)) {
    dir.create(proportion_pause_path)
  }
  if (!dir.exists(proportion_error_and_pause_path)) {
    dir.create(proportion_error_and_pause_path)
  }

  # make factors
  data$participant <- as.factor(data$participant)
  data$trial <- as.factor(data$trial)
  data$haptic <- as.factor(data$haptic)
  data$visual <- as.factor(data$visual)

  ##########################
  ### Proportion Correct ###
  ##########################

  ### Individual ###

  # plot barchart of proportion correct for each participant
  for (participant in unique(data$participant)) {
    participant_data <- data[data$participant == participant,]
    p <- ggplot(participant_data, aes(x = trial, y = proportion_correct)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Participant", participant),
          x = "Block",
          y = "Proportion Correct") +
      theme_minimal()

    ggsave(here(proportion_correct_path, paste("participant_", participant, "proportion_correct.png")), p)
  }

  ### Combined ###

  # boxplot of proportion correct for all participants
  p <- ggplot(data, aes(x = participant, y = proportion_correct, fill = participant, group = participant)) +
    geom_violin(alpha = 0.5, width = 5) +
    geom_boxplot(width = 0.3) +
    geom_point() + 
    labs(title = "Proportion Correct",
        x = "Participant",
        y = "Proportion Correct") +
    theme_minimal()

  ggsave(here(proportion_correct_path, "all_participants_proportion_correct.png"), p)

  ### Histogram ###

  p <- ggplot(data, aes(x = proportion_correct)) +
    geom_histogram(binwidth = 0.01) +
    labs(title = "Proportion Correct",
        x = "Proportion Correct",
        y = "Frequency") +
    theme_minimal()

  ggsave(here(proportion_correct_path, "proportion_correct_histogram.png"), p)


  #######################
  ### Propotion Error ###
  #######################

  ### Individual ###

  # plot barchart of proportion error for each participant
  for (participant in unique(data$participant)) {
    participant_data <- data[data$participant == participant,]
    p <- ggplot(participant_data, aes(x = trial, y = proportion_error)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Participant", participant),
          x = "Block",
          y = "Proportion Error") +
      theme_minimal()

    ggsave(here(proportion_error_path, paste("participant_", participant, "proportion_error.png")), p)
  }

  ### Combined ###

  # boxplot of proportion error for all participants
  p <- ggplot(data, aes(x = participant, y = proportion_error, fill = participant, group = participant)) +
    geom_violin(alpha = 0.5, width = 5) +
    geom_boxplot(width = 0.3) +
    geom_point() + 
    labs(title = "Proportion Error",
        x = "Participant",
        y = "Proportion Error") +
    theme_minimal()

  ggsave(here(proportion_error_path, "all_participants_proportion_error.png"), p)

  ### Histogram ###

  p <- ggplot(data, aes(x = proportion_error)) +
    geom_histogram(binwidth = 0.01) +
    labs(title = "Proportion Error",
        x = "Proportion Error",
        y = "Frequency") +
    theme_minimal()

  ggsave(here(proportion_error_path, "proportion_error_histogram.png"), p)



  ########################
  ### Proportion Pause ###
  ########################

  ### Individual ###

  # plot barchart of proportion pause for each participant
  for (participant in unique(data$participant)) {
    participant_data <- data[data$participant == participant,]
    p <- ggplot(participant_data, aes(x = trial, y = proportion_pause)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Participant", participant),
          x = "Block",
          y = "Proportion Pause") +
      theme_minimal()

    ggsave(here(proportion_pause_path, paste("participant_", participant, "proportion_pause.png")), p)
  }

  ### Combined ###

  # boxplot of proportion pause for all participants
  p <- ggplot(data, aes(x = participant, y = proportion_pause, fill = participant, group = participant)) +
    geom_violin(alpha = 0.5, width = 5) +
    geom_boxplot(width = 0.3) +
    geom_point() + 
    labs(title = "Proportion Pause",
        x = "Participant",
        y = "Proportion Pause") +
    theme_minimal()

  ggsave(here(proportion_pause_path, "all_participants_proportion_pause.png"), p)

  ### Histogram ###

  p <- ggplot(data, aes(x = proportion_pause)) +
    geom_histogram(binwidth = 0.01) +
    labs(title = "Proportion Pause",
        x = "Proportion Pause",
        y = "Frequency") +
    theme_minimal()

  ggsave(here(proportion_pause_path, "proportion_pause_histogram.png"), p)



  ##################################
  ### Proportion Error and Pause ###
  ##################################

  ### Individual ###

  # plot barchart of proportion error and pause for each participant
  for (participant in unique(data$participant)) {
    participant_data <- data[data$participant == participant,]
    p <- ggplot(participant_data, aes(x = trial, y = proportion_error_and_pause)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Participant", participant),
          x = "Block",
          y = "Proportion Error and Pause") +
      theme_minimal()

    ggsave(here(proportion_error_and_pause_path, paste("participant_", participant, "proportion_error_and_pause.png")), p)
  }

  ### Combined ###

  # boxplot of proportion error and pause for all participants
  p <- ggplot(data, aes(x = participant, y = proportion_error_and_pause, fill = participant, group = participant)) +
    geom_violin(alpha = 0.5, width = 5) +
    geom_boxplot(width = 0.3) +
    geom_point() + 
    labs(title = "Proportion Error and Pause",
        x = "Participant",
        y = "Proportion Error and Pause") +
    theme_minimal()

  ggsave(here(proportion_error_and_pause_path, "all_participants_proportion_error_and_pause.png"), p)

  ### Histogram ###

  p <- ggplot(data, aes(x = proportion_error_and_pause)) +
    geom_histogram(binwidth = 0.01) +
    labs(title = "Proportion Error and Pause",
        x = "Proportion Error and Pause",
        y = "Frequency") +
    theme_minimal()

  ggsave(here(proportion_error_and_pause_path, "proportion_error_and_pause_histogram.png"), p)

}