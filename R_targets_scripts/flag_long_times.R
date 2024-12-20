# flag long times

library(tidyverse)
library(here)

data <- tar_read(data_long_calculated_exp2)

#################
### Aggregate ###
#################
average_by_trial <- data %>%
  group_by(participant, trial, condition_nums, time_through_maze, max_force, path_length, haptic, visual, map) %>%
  summarise(average_force = mean(force_magnitude, na.rm = TRUE))

# flag trials with long times
average_by_trial$long_time <- ifelse(average_by_trial$time_through_maze > 100, 1, 0)

# count how many long trials there were
long_trials <- average_by_trial %>%
  filter(long_time == 1) %>%
  count() %>%
  print()

# export to csv in the output folder
write_csv(long_trials, here("output", "long_trials.csv"))
