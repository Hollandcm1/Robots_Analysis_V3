# run_LME_time

library(targets)
library(here) 
library(tidyverse)
library(gglot2)
library(lme4)
library(ggbeeswarm)
library(flexplot)
library(sjPlot)

data <- tar_read(data_long_calculated)

save_path <- here("output", "LME_time")
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

###########
### LME ###
###########

model1 <- lmer(time ~ haptic * visual * arithmatic + (1|participant), data = trial_summary)
tab_model(model1)

