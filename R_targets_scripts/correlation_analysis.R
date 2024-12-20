# correlation analysis 

library(tidyverse)
library(corrplot)

data <- tar_read(strategic_data_appended)

#################
### Aggregate ###
#################
average_by_trial <- data %>%
  group_by(participant, trial, condition_nums, time_through_maze, max_force, path_length, haptic, visual, map, strategic_both, strategic_either, collaborative) %>%
  summarise(average_force = mean(force_magnitude, na.rm = TRUE))

# correlation analysis - path and time
correlation <- cor(average_by_trial$time_through_maze, average_by_trial$path_length)

correlation_test <- cor.test(average_by_trial$time_through_maze, average_by_trial$path_length)

print(correlation)
print(correlation_test)

# correlation analysis - forces

correlation <- cor(average_by_trial$max_force, average_by_trial$average_force)

correlation_test <- cor.test(average_by_trial$max_force, average_by_trial$average_force)

print(correlation)
print(correlation_test)

