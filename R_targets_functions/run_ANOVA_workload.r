# run_ANOVA_workload

# library(targets)
# library(here) 
# library(tidyverse)
# library(ggplot2)
# library(lme4)
# library(ggbeeswarm)
# library(flexplot)

# data <- tar_read(workload_data)

run_ANOVA_workload <- function(data) {

  save_path <- here("output", "ANOVA_workload")
  # makedirectory if it doesn't already exist
  if (!dir.exists(save_path)) {
    dir.create(save_path)
  }

  ##############
  ### ANOVAs ###
  ##############

  anova_results <- aov(Workload ~ haptic * visual * arithmatic, data = data)
  summary_anova_result <- capture.output(summary(anova_results))
  write(summary_anova_result, file = here(save_path,'ANOVA_Results.txt'))

  #################
  ### Visualize ###
  #################

  # plot workload by haptic, visual, and arithmatic
  p <- flexplot(
    data = data,
    formula = Workload ~ haptic + visual | arithmatic,
  )

  p

  #save
  ggsave(here(save_path, "Workload_by_haptic_visual_arithmatic.png"), p, width = 10, height = 6, units = "in", dpi = 300)

}