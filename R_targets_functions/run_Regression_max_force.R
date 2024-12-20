# run_Regression_max_force

# run_Regression_average_force

run_Regression_max_force <- function(data) {
  
  # isolate for within maze
  # data <- data %>%
  #   filter(within_maze == 1)
  
  #################
  ### Aggragate ###
  #################
  average_force_by_trial <- data %>%
    group_by(participant, trial, condition_nums, time_through_maze, max_force, path_length, haptic, visual) %>%
    summarise(average_force = mean(force_magnitude, na.rm = TRUE))
  
  # average_force_by_trial <- data %>%
  #   group_by(participant, trial, condition_names) %>%
  #   summarise(average_force = mean(force_magnitude, na.rm = TRUE))
  # 
  # average_force_by_participant <- average_force_by_trial %>%
  #   group_by(participant, condition_names) %>%
  #   summarise(average_force = mean(average_force, na.rm = TRUE))
  # 
  # average_force_by_trial_hv <- data %>%
  #   group_by(participant, trial, haptic, visual) %>%
  #   summarise(average_force = mean(force_magnitude, na.rm = TRUE))
  # 
  # average_force_by_participant_hv <- average_force_by_trial_hv %>%
  #   group_by(participant, haptic, visual) %>%
  #   summarise(average_force = mean(average_force, na.rm = TRUE))
  
  #############
  ### Model ###
  #############
  model1 <- lmer(time_through_maze ~ max_force + (1|participant) + (1|trial), data = data)
  summary(model1)
  tab_model(model1)
  capture.output(model1, file = here('output', 'Regression_max_force','0_model1.txt'))
  
  
  #################
  ### Visualize ###
  #################
  g1 <- ggplot(data = average_force_by_trial, aes(x = max_force, y = time_through_maze)) +
    geom_point() +
    geom_smooth(method = "lm") +
    theme_minimal() +
    labs(title = "Time Through Maze by Max Force", x = "Max Force", y = "Time Through Maze")
  print(g1)
  ggsave(here('output', 'Regression_max_force','0_max_force_vs_time_through_maze.png'), plot = g1, width = 8, height = 6)
  
}