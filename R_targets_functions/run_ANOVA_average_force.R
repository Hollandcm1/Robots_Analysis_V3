# run_ANOVA_force

# data <- data_long

run_ANOVA_average_force <- function(data) {
  
  # isolate for within maze
  # data <- data %>%
  #   filter(within_maze == 1)
  # check that the filter worked
  # unique(data$within_maze)
  
  #################
  ### Aggregate ###
  #################
  average_force_by_trial <- data %>%
    group_by(participant, trial, condition_names) %>%
    summarise(average_force = mean(force_magnitude, na.rm = TRUE))
  
  average_force_by_participant <- average_force_by_trial %>%
    group_by(participant, condition_names) %>%
    summarise(average_force = mean(average_force, na.rm = TRUE))
  
  average_force_by_trial_hv <- data %>%
    group_by(participant, trial, haptic, visual) %>%
    summarise(average_force = mean(force_magnitude, na.rm = TRUE))
  
  average_force_by_participant_hv <- average_force_by_trial_hv %>%
    group_by(participant, haptic, visual) %>%
    summarise(average_force = mean(average_force, na.rm = TRUE))
  
  #########################
  ### Check Assumptions ###
  #########################
  # check for normality
  normality_test_results <- shapiro.test(average_force_by_trial_hv$average_force)
  print(normality_test_results)
  capture.output(normality_test_results, file = here('output','ANOVA_average_force','0_Normality_test_results_for_average_force_using_shapiro.txt'))
  
  png(file = here('output','ANOVA_average_force','0_Histogram_of_Average_Force.png'), width = 800, height = 600)
  h1 <- hist(average_force_by_trial_hv$average_force, breaks = 20, col = "skyblue", border = "black", xlab = "Average Force", main = "Histogram of Average Force")
  dev.off()
  # save the histogram
  #ggsave(h1, file = here('output','ANOVA_average_force','0_Histogram_of_Average_Force.png'))
  #library(stats)
  #mauchly_result <- mauchly(anova_results_hv)
  #print(mauchly_result)
  
  ##############
  ### ANOVAs ###
  ##############
  
  # ANOVA 1
  anova_results <- aov(average_force ~ condition_names, data = average_force_by_participant)
  summary(anova_results)
  capture.output(anova_results, file = here('output','ANOVA_average_force','1_Average_froce_by_condition_participant_level.txt'))
  
  anova_results <- aov(average_force ~ condition_names, data = average_force_by_trial)
  summary(anova_results)
  capture.output(anova_results, file = here('output','ANOVA_average_force','2_Average_froce_by_condition_trial_level.txt'))
  
  # ANOVA 2
  anova_results_hv <- aov(average_force ~ haptic * visual, data = average_force_by_participant_hv)
  summary(anova_results_hv)
  capture.output(anova_results_hv, file = here('output','ANOVA_average_force','3_Average_froce_by_haptic_visual_participant_level.txt'))
  
  anova_results_hv <- aov(average_force ~ haptic * visual + Error(participant/trial), data = average_force_by_trial_hv)
  summary(anova_results_hv)
  capture.output(anova_results_hv, file = here('output','ANOVA_average_force','4_Average_froce_by_haptic_visual_trial_level.txt'))
  
  # GGe correction anova 
  warning('GG Corrections not yet working')
  # library(ez)
  # library(afex)
  # library(sjstats)
  # results <- aov_ez(id = "participant", 
  #                   dv = "average_force", 
  #                   data = average_force_by_trial_hv,
  #                   within = c("haptic", "visual"),
  #                   #between = "Condition",
  #                   detailed = TRUE)
  # 
  # print(results)
  # # summary(results)
  # 
  # effect_sizes <- effectsize::eta_squared(results, partial = TRUE, ci.lvl = NA)  # Set ci.lvl=NA to exclude confidence intervals for faster computation
  # print(effect_sizes)
  # 
  # 
  # results <- ezANOVA(wid = "participant", 
  #                   dv = "average_force", 
  #                   data = average_force_by_trial_hv,
  #                   within = c("trial", "haptic", "visual"),
  #                   #between = "Condition",
  #                   detailed = TRUE)
  # 
  # print(results)
  
  
  #################
  ### Visualize ###
  #################
  
  g1 <- ggplot(average_force_by_participant, aes(x = condition_names, y = average_force, color=condition_names)) +
    geom_boxplot() +
    geom_jitter(width = 0.2) +
    theme_minimal() +
    labs(title = "Average Force by Condition", x = "Condition", y = "Average Force") 
  print(g1)
  ggsave(g1, file = here('output','ANOVA_average_force','5_Average_froce_by_condition_participant_level.png'))
  
  g2 <- ggplot(average_force_by_participant_hv, aes(x = haptic, y = average_force, color=visual, group=haptic)) +
    geom_boxplot() +
    geom_jitter(width = 0.2) +
    theme_classic() +
    labs(title = "Average Force by Haptic and Visual", x = "Haptic", y = "Average Force") +
    facet_wrap(~visual)
  print(g2)
  ggsave(g2, file = here('output','ANOVA_average_force','6_Average_froce_by_haptic_visual_participant_level.png'))
  
  g3 <- ggplot(average_force_by_trial_hv, aes(x = haptic, y = average_force, color=visual, group=haptic)) +
    geom_boxplot() +
    geom_jitter(width = 0.2) +
    theme_classic() +
    labs(title = "Average Force by Haptic and Visual", x = "Haptic", y = "Average Force") +
    facet_wrap(~visual)
  print(g3)
  ggsave(g3, file = here('output','ANOVA_average_force','7_Average_froce_by_haptic_visual_trial_level.png'))
  
  g4 <- ggplot(average_force_by_trial_hv, aes(x = visual, y = average_force, colour=visual)) +
    geom_boxplot() +
    geom_jitter(width = 0.2) +
    theme_minimal() +
    labs(title = "Average Force", x = "Vision", y = "Average Force") 
  print(g4)
  ggsave(g4, file = here('output','ANOVA_average_force','8_Average_froce_by_visual_trial_level.png'))
  
  g5 <- ggplot(average_force_by_trial_hv, aes(x = visual, y = average_force, group=participant)) +
    geom_line() +
    geom_jitter(width = 0.2) +
    theme_minimal() +
    labs(title = "Average Force", x = "Vision", y = "Average Force") 
  print(g5)
  
  g5 <- ggplot(average_force_by_participant_hv, aes(x = visual, y = average_force, group=participant)) +
    geom_line() +
    geom_jitter(width = 0.2) +
    theme_minimal() +
    labs(title = "Average Force", x = "Vision", y = "Average Force") 
  print(g5)
  
  
  return(Sys.time())
  
}


