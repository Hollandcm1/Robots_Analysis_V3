# run_ANOVA_max_force

# data <- data_long

run_ANOVA_max_force <- function(data) {

  # isolate for within maze
  # data <- data %>%
  #   filter(within_maze == 1)
  # check that the filter worked
  # unique(data$within_maze)
  
  #################
  ### Aggregate ###
  #################
  max_force_by_trial <- data %>%
    group_by(participant, trial, condition_names) %>%
    summarise(max_force = max(force_magnitude, na.rm = TRUE))
  
  max_force_by_participant <- max_force_by_trial %>%
    group_by(participant, condition_names) %>%
    summarise(max_force = max(max_force, na.rm = TRUE))
  
  max_force_by_trial_hv <- data %>%
    group_by(participant, trial, haptic, visual) %>%
    summarise(max_force = max(force_magnitude, na.rm = TRUE))
  
  max_force_by_participant_hv <- max_force_by_trial_hv %>%
    group_by(participant, haptic, visual) %>%
    summarise(max_force = max(max_force, na.rm = TRUE))
  
  #########################
  ### Check Assumptions ###
  #########################
  
  # check for normality
  normality_test_results <- shapiro.test(max_force_by_trial_hv$max_force)
  print(normality_test_results)
  capture.output(normality_test_results, file = here('output','ANOVA_max_force','0_Normality_test_results_for_max_force_using_shapiro.txt'))
  
  png(file = here('output','ANOVA_max_force','0_Histogram_of_Max_Force.png'), width = 800, height = 600)
  h1 <- hist(max_force_by_trial_hv$max_force, breaks = 20, col = "skyblue", border = "black", xlab = "Max Force", main = "Histogram of Max Force")
  dev.off()
  
  ##############
  ### ANOVAs ###
  ##############
  
  # ANOVA 1
  anova_results <- aov(max_force ~ condition_names, data = max_force_by_participant)
  summary(anova_results)
  capture.output(anova_results, file = here('output','ANOVA_max_force','1_Max_froce_by_condition_participant_level.txt'))
  
  anova_results <- aov(max_force ~ condition_names, data = max_force_by_trial)
  summary(anova_results)
  capture.output(anova_results, file = here('output','ANOVA_max_force','2_Max_froce_by_condition_trial_level.txt'))
  
  
  # ANOVA 2
  anova_results_hv <- aov(max_force ~ haptic * visual, data = max_force_by_participant_hv)
  summary(anova_results_hv)
  capture.output(anova_results_hv, file = here('output','ANOVA_max_force','3_Max_froce_by_haptic_visual_participant_level.txt'))
  
  anova_results_hv <- aov(max_force ~ haptic * visual, data = max_force_by_trial_hv)
  summary(anova_results_hv)
  capture.output(anova_results_hv, file = here('output','ANOVA_max_force','4_Max_froce_by_haptic_visual_trial_level.txt'))
  
  
  
  #################
  ### Visualize ###
  #################
  g1 <- ggplot(max_force_by_participant, aes(x = condition_names, y = max_force, color=condition_names)) +
    geom_boxplot() +
    geom_jitter(width = 0.2) +
    theme_minimal() +
    labs(title = "max Force by Condition", x = "Condition", y = "max Force") 
  print(g1)
  ggsave(g1, file = here('output','ANOVA_max_force','5_max_force_by_condition_participant_level.png'))
  
  g2 <- ggplot(max_force_by_participant_hv, aes(x = haptic, y = max_force, color=visual, group=haptic)) +
    geom_boxplot() +
    geom_jitter(width = 0.2) +
    theme_classic() +
    labs(title = "max Force by Haptic and Visual", x = "Haptic", y = "max Force") +
    facet_wrap(~visual)
  print(g2)
  ggsave(g2, file = here('output','ANOVA_max_force','6_max_force_by_haptic_visual_participant_level.png'))

  g3 <- ggplot(max_force_by_trial, aes(x = condition_names, y = max_force, color=condition_names)) +
    geom_boxplot() +
    geom_jitter(width = 0.2) +
    theme_classic() +
    labs(title = "max Force by Condition", x = "Condition", y = "max Force")
  print(g3)
  ggsave(g3, file = here('output','ANOVA_max_force','7_max_force_by_condition_trial_level.png'))
  
  g4 <- ggplot(max_force_by_trial_hv, aes(x = haptic, y = max_force, color=visual, group=haptic)) +
    geom_boxplot() +
    geom_jitter(width = 0.2) +
    theme_classic() +
    labs(title = "max Force by Haptic and Visual", x = "Haptic", y = "max Force") +
    facet_wrap(~visual)
  print(g4)
  ggsave(g4, file = here('output','ANOVA_max_force','8_max_force_by_haptic_visual_trial_level.png'))
}

