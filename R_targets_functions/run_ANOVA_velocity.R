# run_ANOVA_velocity

# library(targets)
# library(tidyverse)
# library(here)
# library(lme4)
# library(ggplot2)
# library(sjPlot)
# library(ggbeeswarm)
# library(flexplot)

# data <- tar_read(data_long_calculated)

run_ANOVA_velocity <- function(data) {

  save_path <- here('output','ANOVA_velocity')
  # make directory if it doesn't already exist
  if (!dir.exists(save_path)) {
    dir.create(save_path)
  }
  
  # isolate for within maze
  # data <- data %>%
  #   filter(within_maze == 1)
  # check that the filter worked
  # unique(data$within_maze)
  
  #################
  ### Aggregate ###
  #################
  average_velocity_by_trial <- data %>%
    group_by(participant, trial) %>%
    summarise(
      average_velocity = mean(linear_velocity, na.rm = TRUE),
      haptic = unique(haptic),
      visual = unique(visual),
      arithmatic = unique(arithmatic)
      )
  
  # average_velocity_by_participant <- average_velocity_by_trial %>%
  #   group_by(participant, condition_names) %>%
  #   summarise(average_velocity = mean(average_velocity, na.rm = TRUE))
  
  #######################
  ### Check Normality ###
  #######################
  png(file = here(save_path,'0_Histogram_of_Average_Velocity.png'), width = 800, height = 600)
  hist(average_velocity_by_trial$average_velocity, breaks = 20, col = "skyblue", border = "black", xlab = "Average Velocity", main = "Histogram of Average Velocity")
  dev.off()
  
  ##############
  ### ANOVAs ###
  ##############
  
  # anova_results <- aov(average_velocity ~ condition + Error(participant), data = average_velocity)
  anova_results <- aov(average_velocity ~ haptic * visual * arithmatic, data = average_velocity_by_trial)
  summary_anova_result <- capture.output(summary(anova_results))
  write(summary_anova_result, file = here(save_path,'ANOVA_Results.txt'))

  # model <- lmer(average_velocity ~ haptic * visual * arithmatic + (1|participant), data = average_velocity_by_trial)
  # tab_model(model)

  #################
  ### Visaulize ###
  #################
  
  # plot average_velocity by visual
  p <- ggplot(average_velocity_by_trial, aes(x = visual, y = average_velocity, fill = visual)) +
    geom_violin(alpha = 0.5) + 
    geom_boxplot(width = 0.3) +
    geom_beeswarm() +
    labs(title = "Average Velocity by Visual Condition", x = "Visual Condition", y = "Average Velocity") +
    theme_classic()

  p

  # save
  ggsave(here(save_path,'average_velocity_by_visual.png'), p)

  # plot average_velocity by haptic, visual, and arithmatic
  p <- flexplot(
    data = average_velocity_by_trial,
    formula = average_velocity ~ haptic + visual | arithmatic,
  )

  p

  # save
  ggsave(here(save_path,'average_velocity_by_haptic_visual_arithmatic.png'), p)

}