# rotation_analysis

library(tidyverse)
library(here)
library(lme4)
library(sjPlot)
library(flexplot)

data_exp1 <- tar_read(strategic_data_appended)
data_exp2 <- tar_read(data_long_calculated_exp2)

#################
### Aggregate ###
#################
data <- data_exp1
average_by_trial <- data %>%
  group_by(participant, condition_nums, time_through_maze, max_force, path_length, haptic, visual, map, average_velocity, average_proximity, average_force, rotation) %>%
  summarise(average_force = mean(force_magnitude, na.rm = TRUE))
# add experiment column
average_by_trial$experiment <- 1
tmp_exp1 <- average_by_trial

data <- data_exp2
average_by_trial <- data %>%
  group_by(participant, condition_nums, time_through_maze, max_force, path_length, haptic, visual, map, average_velocity, average_proximity, average_force, rotation) %>%
  summarise(average_force = mean(force_magnitude, na.rm = TRUE))
# add experiment column
average_by_trial$experiment <- 2
# change participant column to number
average_by_trial$participant <- as.numeric(average_by_trial$participant)
tmp_exp2 <- average_by_trial

# combine
all_data <- rbind(tmp_exp1, tmp_exp2)
average_by_trial <- all_data

# convert to factor as needed
average_by_trial$experiment <- as.factor(average_by_trial$experiment)
# average_by_trial$map <- as.factor(average_by_trial$map)
average_by_trial$haptic <- as.factor(average_by_trial$haptic)
average_by_trial$visual <- as.factor(average_by_trial$visual)
# average_by_trial$rotation <- as.factor(average_by_trial$rotation)




# model for time
model <- lmer(time_through_maze ~ rotation + (1|participant), data = average_by_trial)
tab_model(model)
model <- lmer(time_through_maze ~ rotation*haptic*visual*experiment + (1|participant), data = average_by_trial)
tab_model(model)

# model for path length
model <- lmer(path_length ~ rotation + (1|participant), data = average_by_trial)
tab_model(model)
model <- lmer(path_length ~ rotation*haptic*visual*experiment + (1|participant), data = average_by_trial)
tab_model(model)
flexplot(path_length ~ rotation, data = average_by_trial)
flexplot(path_length ~ rotation + visual, data = average_by_trial)
flexplot(path_length ~ rotation + experiment, data = average_by_trial)
flexplot(path_length ~ rotation + visual | experiment, data = average_by_trial)

# model for velocity
model <- lmer(average_velocity ~ rotation + (1|participant), data = average_by_trial)
tab_model(model)
flexplot(average_velocity ~ rotation, data = average_by_trial)
model <- lmer(average_velocity ~ rotation*haptic*visual*experiment + (1|participant), data = average_by_trial)
tab_model(model)

# model for proximity
model <- lmer(average_proximity ~ rotation + (1|participant), data = average_by_trial)
tab_model(model)
model <- lmer(average_proximity ~ rotation*haptic*visual*experiment + (1|participant), data = average_by_trial)
tab_model(model)
