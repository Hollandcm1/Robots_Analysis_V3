# run_ANOVA_time

# library(targets)
# library(here) 
# library(tidyverse)
# library(ggplot2)
# library(lme4)
# library(ggbeeswarm)
# library(flexplot)

# data <- tar_read(data_long_calculated)

run_ANOVA_time <- function(data) {

  save_path <- here('output','ANOVA_time')
  # make directory if it doesn't already exist
  if (!dir.exists(save_path)) {
    dir.create(save_path)
  }

  #################
  ### Aggregate ###
  #################
  trial_summary <- data %>%
    group_by(participant, trial) %>%
    summarise(
      time = mean(time_through_maze),
      haptic = unique(haptic),
      visual = unique(visual),
      arithmatic = unique(arithmatic)
    )

  #######################
  ### Check Normality ###
  #######################
  png(file = here(save_path,'0_Histogram_of_Time.png'), width = 800, height = 600)
  hist(trial_summary$time, breaks = 20, col = "skyblue", border = "black", xlab = "Time", main = "Histogram of Time")
  dev.off()

  ##############
  ### ANOVAs ###
  ##############

  anova_results <- aov(time ~ haptic * visual * arithmatic, data = trial_summary)
  summary_anova_result <- capture.output(summary(anova_results))
  write(summary_anova_result, file = here(save_path,'ANOVA_Results.txt'))

  #################
  ### Visualize ###
  #################

  # plot time by visual
  p <- ggplot(trial_summary, aes(x = visual, y = time)) +
    geom_boxplot() +
    geom_beeswarm() +
    labs(title = "Time by Visual Condition", x = "Visual Condition", y = "Time") +
    theme_classic()

  p

  # save
  ggsave(here(save_path,'Time_by_Visual.png'), plot = p, width = 8, height = 6, units = "in", dpi = 300)


  # plot time by haptic, visual, and arithmatic
  p <- flexplot(
    data = trial_summary,
    formula = time ~ haptic + visual | arithmatic,
  )

  p

  # save
  ggsave(here(save_path,'Time_by_Haptic_Visual_Arithmatic.png'), plot = p, width = 8, height = 6, units = "in", dpi = 300)

}