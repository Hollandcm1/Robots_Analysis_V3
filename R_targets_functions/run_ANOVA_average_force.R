# run_ANOVA_average_force

# library(targets)
# library(here) 
# library(tidyverse)
# library(ggplot2)
# library(lme4)
# library(ggbeeswarm)
# library(flexplot)

# data <- tar_read(data_long_calculated)

run_ANOVA_average_force <- function(data) {

  save_path <- here('output','ANOVA_average_force')
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
      average_force = mean(average_force),
      haptic = unique(haptic),
      visual = unique(visual),
      arithmatic = unique(arithmatic)
    )

  #######################
  ### Check Normality ###
  #######################
  png(file = here(save_path,'0_Histogram_of_Average_Force.png'), width = 800, height = 600)
  hist(trial_summary$average_force, breaks = 20, col = "skyblue", border = "black", xlab = "Force", main = "Histogram of Average Force")
  dev.off()

  ##############
  ### ANOVAs ###
  ##############

  anova_results <- aov(average_force ~ haptic * visual * arithmatic, data = trial_summary)
  summary_anova_result <- capture.output(summary(anova_results))
  write(summary_anova_result, file = here(save_path,'ANOVA_Results.txt'))

  #################
  ### Visualize ###
  #################

  # plot max force by haptic, visual, and arithmatic
  p <- flexplot(
    data = trial_summary,
    formula = average_force ~ haptic + visual | arithmatic,
  )

  p

  # save
  ggsave(here(save_path,'Average_Force_by_Haptic_Visual_Arithmatic.png'), plot = p, width = 8, height = 6, units = "in", dpi = 300)

}