# run_ANOVA_velocity

# data <- data_long

run_ANOVA_velocity <- function(data) {
  
  # isolate for within maze
  # data <- data %>%
  #   filter(within_maze == 1)
  # check that the filter worked
  # unique(data$within_maze)
  
  #################
  ### Aggregate ###
  #################
  average_velocity_by_trial <- data %>%
    group_by(participant, trial, condition_names) %>%
    summarise(average_velocity = mean(linear_velocity, na.rm = TRUE))
  
  average_velocity_by_participant <- average_velocity_by_trial %>%
    group_by(participant, condition_names) %>%
    summarise(average_velocity = mean(average_velocity, na.rm = TRUE))
  
  #######################
  ### Check Normality ###
  #######################
  png(file = here('output','ANOVA_velocity','0_Histogram_of_Average_Velocity.png'), width = 800, height = 600)
  hist(average_velocity_by_trial$average_velocity, breaks = 20, col = "skyblue", border = "black", xlab = "Average Velocity", main = "Histogram of Average Velocity")
  dev.off()
  
  ##############
  ### ANOVAs ###
  ##############
  
  # anova_results <- aov(average_velocity ~ condition + Error(participant), data = average_velocity)
  anova_results <- aov(average_velocity ~ condition_names, data = average_velocity_by_trial)
  summary(anova_results)
  
  # Post-hoc
  posthoc_results <- TukeyHSD(anova_results)
  posthoc_results
  
  #################
  ### Visaulize ###
  #################
  # posthoc result
  png(file = here('output','ANOVA_velocity','0_post_hoc_analysis.png'), width = 800, height = 600)
  plot(posthoc_results)
  dev.off()
  
  # anova result
  # plot(anova_results)
  
  # plot the data
  g1 <- ggplot(average_velocity_by_trial, aes(x = condition_names, y = average_velocity, color=condition_names)) +
    geom_boxplot() +
    geom_jitter(width = 0.2) +
    theme_minimal() +
    labs(title = "Average Velocity by Condition", x = "Condition", y = "Average Velocity") 
  print(g1)
  ggsave(g1, file = here('output','ANOVA_velocity','1_Average_velocity_by_condition_trial_level.png'))
  
  g2 <- ggplot(average_velocity_by_trial, aes(x=participant, y=average_velocity, group=participant, colour=participant)) +
    geom_boxplot() +
    geom_jitter(width=0.2, height=0) +
    theme_minimal() +
    labs(title = "Average Velocity by Participant", x = "Participant", y = "Average Velocity")
  print(g2)
  ggsave(g2, file = here('output','ANOVA_velocity','2_Average_velocity_by_participant_trial_level.png'))
  
  g3 <- ggplot(average_velocity_by_participant, aes(x = condition_names, y = average_velocity, color=condition_names)) +
    geom_violin() +
    geom_boxplot(width=0.2) +
    geom_jitter(width = 0.2, height=0) +
    theme_minimal() +
    labs(title = "Average Participant Velocity by Condition", x = "Condition", y = "Average Velocity") 
  print(g3)
  ggsave(g3, file = here('output','ANOVA_velocity','3_Average_velocity_by_condition_participant_level.png'))
  
  # bar chart
  g4 <- ggplot(average_velocity_by_participant, aes(x = condition_names, y = average_velocity, fill=condition_names)) +
    stat_summary(fun = "mean", geom = "col") +
    geom_jitter(width = 0.2, height=0) +
    theme_minimal() +
    labs(title = "Average Participant Velocity by Condition", x = "Condition", y = "Average Velocity")
  print(g4)
  ggsave(g4, file = here('output','ANOVA_velocity','4_Average_velocity_by_condition_participant_level.png'))

}