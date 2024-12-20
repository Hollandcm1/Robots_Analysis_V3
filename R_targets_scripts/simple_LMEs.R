
library(tidyr)
library(dplyr)
library(lme4)
library(here)
library(sjPlot)
library(flexplot)

data_exp1 <- tar_read(strategic_data_appended)
data_exp2 <- tar_read(data_long_calculated_exp2)

  
#################
### Aggregate ###
#################
data <- data_exp1
average_by_trial <- data %>%
  group_by(participant, condition_nums, time_through_maze, max_force, path_length, haptic, visual, map, average_velocity, average_proximity, average_force) %>%
  summarise(average_force = mean(force_magnitude, na.rm = TRUE))
# add experiment column
average_by_trial$experiment <- 1

# Convert vars to factors
average_by_trial$visual <- as.factor(average_by_trial$visual)
average_by_trial$haptic <- as.factor(average_by_trial$haptic)
average_by_trial$participant <- as.factor(average_by_trial$participant)

tmp_exp1_trial <- average_by_trial

# average by condition
average_by_condition <- average_by_trial %>%
  group_by(participant, haptic, visual, condition_nums, experiment) %>%
  summarise(average_time_through_maze = mean(time_through_maze, na.rm = TRUE),
            average_path_length = mean(path_length, na.rm = TRUE),
            average_velocity = mean(average_velocity, na.rm = TRUE),
            average_proximity = mean(average_proximity, na.rm = TRUE),
            average_force = mean(average_force, na.rm = TRUE))

# store for later
tmp_exp1 <- average_by_condition
#tmp_exp1 <- average_by_trial


data <- data_exp2
average_by_trial <- data %>%
  group_by(participant, condition_nums, time_through_maze, max_force, path_length, haptic, visual, map, average_velocity, average_proximity, average_force) %>%
  summarise(average_force = mean(force_magnitude, na.rm = TRUE))
# add experiment column
average_by_trial$experiment <- 2
# change participant column to number
average_by_trial$participant <- as.numeric(average_by_trial$participant)

# Convert vars to factors
average_by_trial$visual <- as.factor(average_by_trial$visual)
average_by_trial$haptic <- as.factor(average_by_trial$haptic)
average_by_trial$participant <- as.factor(average_by_trial$participant)

tmp_exp2_trial <- average_by_trial

# # Average by condition
average_by_condition <- average_by_trial %>%
  group_by(participant, haptic, visual, condition_nums, experiment) %>%
  summarise(average_time_through_maze = mean(time_through_maze, na.rm = TRUE),
            average_path_length = mean(path_length, na.rm = TRUE),
            average_velocity = mean(average_velocity, na.rm = TRUE),
            average_proximity = mean(average_proximity, na.rm = TRUE),
            average_force = mean(average_force, na.rm = TRUE))

# store for later
tmp_exp2 <- average_by_condition
#tmp_exp2 <- average_by_trial

# combine
all_data <- rbind(tmp_exp1, tmp_exp2)
# average_by_trial <- all_data
average_by_condition <- all_data

################################################################################
################################## Pre Averaged ################################
################################################################################

################################
### Experiment 1 Simple LMEs ###
################################

# model <- lmer(time_through_maze ~ visual*haptic + (1|participant), data = average_by_trial) # this is just for testing
model <- lmer(average_time_through_maze ~ visual*haptic + (1|participant), data = tmp_exp1)
summary(model)
tab_model(model)
flexplot(data = tmp_exp1, average_time_through_maze ~ visual + haptic)

model <- lmer(average_path_length ~ visual*haptic + (1|participant), data = tmp_exp1)
summary(model)
tab_model(model)
flexplot(data = tmp_exp1, average_path_length ~ visual + haptic)

model <- lmer(average_velocity ~ visual*haptic + (1|participant), data = tmp_exp1)
summary(model)
tab_model(model)
flexplot(data = tmp_exp1, average_velocity ~ visual + haptic)

model <- lmer(average_proximity ~ visual*haptic + (1|participant), data = tmp_exp1)
summary(model)
tab_model(model)
flexplot(data = tmp_exp1, average_proximity ~ visual + haptic)

model <- lmer(average_force ~ visual*haptic + (1|participant), data = tmp_exp1)
summary(model)
tab_model(model)
flexplot(data = tmp_exp1, average_force ~ visual + haptic)


################################
### Experiment 2 Simple LMEs ###
################################

model <- lmer(average_time_through_maze ~ visual*haptic + (1|participant), data = tmp_exp2)
summary(model)
tab_model(model)
flexplot(data = tmp_exp2, average_time_through_maze ~ visual + haptic)

model <- lmer(average_path_length ~ visual*haptic + (1|participant), data = tmp_exp2)
summary(model)
tab_model(model)
flexplot(data = tmp_exp2, average_path_length ~ visual + haptic)

model <- lmer(average_velocity ~ visual*haptic + (1|participant), data = tmp_exp2)
summary(model)
tab_model(model)
flexplot(data = tmp_exp2, average_velocity ~ visual + haptic)

model <- lmer(average_proximity ~ visual*haptic + (1|participant), data = tmp_exp2)
summary(model)
tab_model(model)
flexplot(data = tmp_exp2, average_proximity ~ visual + haptic)

model <- lmer(average_force ~ visual*haptic + (1|participant), data = tmp_exp2)
summary(model)
tab_model(model)
flexplot(data = tmp_exp2, average_force ~ visual + haptic)



################################################################################
################################ By Trial Version ##############################
################################################################################

################################
### Experiment 1 Simple LMEs ###
################################

model <- lmer(time_through_maze ~ visual*haptic + (1|participant), data = tmp_exp1_trial)
summary(model)
tab_model(model, 
          show.est = TRUE,         # Show estimates
          show.se = TRUE,          # Show standard errors
          show.stat = TRUE,        # We need to identify what t-value corresponds to
          show.p = TRUE,           # Show p-values
          show.ci = FALSE)           
flexplot(data = tmp_exp1_trial, time_through_maze ~ visual + haptic)

model <- lmer(path_length ~ visual*haptic + (1|participant), data = tmp_exp1_trial)
summary(model)
tab_model(model, 
          show.est = TRUE,         # Show estimates
          show.se = TRUE,          # Show standard errors
          show.stat = TRUE,        # We need to identify what t-value corresponds to
          show.p = TRUE,           # Show p-values
          show.ci = FALSE)  
flexplot(data = tmp_exp1_trial, path_length ~ visual + haptic)

model <- lmer(average_velocity ~ visual*haptic + (1|participant), data = tmp_exp1_trial)
summary(model)
tab_model(model, 
          show.est = TRUE,         # Show estimates
          show.se = TRUE,          # Show standard errors
          show.stat = TRUE,        # We need to identify what t-value corresponds to
          show.p = TRUE,           # Show p-values
          show.ci = FALSE)  
flexplot(data = tmp_exp1_trial, average_velocity ~ visual + haptic)

model <- lmer(average_proximity ~ visual*haptic + (1|participant), data = tmp_exp1_trial)
summary(model)
tab_model(model, 
          show.est = TRUE,         # Show estimates
          show.se = TRUE,          # Show standard errors
          show.stat = TRUE,        # We need to identify what t-value corresponds to
          show.p = TRUE,           # Show p-values
          show.ci = FALSE)  
flexplot(data = tmp_exp1_trial, average_proximity ~ visual + haptic)

model <- lmer(average_force ~ visual*haptic + (1|participant), data = tmp_exp1_trial)
summary(model)
tab_model(model, 
          show.est = TRUE,         # Show estimates
          show.se = TRUE,          # Show standard errors
          show.stat = TRUE,        # We need to identify what t-value corresponds to
          show.p = TRUE,           # Show p-values
          show.ci = FALSE)  
flexplot(data = tmp_exp1_trial, average_force ~ visual + haptic)


################################
### Experiment 2 Simple LMEs ###
################################

model <- lmer(time_through_maze ~ visual*haptic + (1|participant), data = tmp_exp2_trial)
summary(model)
tab_model(model, 
          show.est = TRUE,         # Show estimates
          show.se = TRUE,          # Show standard errors
          show.stat = TRUE,        # We need to identify what t-value corresponds to
          show.p = TRUE,           # Show p-values
          show.ci = FALSE)  
flexplot(data = tmp_exp2_trial, time_through_maze ~ visual + haptic)

model <- lmer(path_length ~ visual*haptic + (1|participant), data = tmp_exp2_trial)
summary(model)
tab_model(model, 
          show.est = TRUE,         # Show estimates
          show.se = TRUE,          # Show standard errors
          show.stat = TRUE,        # We need to identify what t-value corresponds to
          show.p = TRUE,           # Show p-values
          show.ci = FALSE)  
flexplot(data = tmp_exp2_trial, path_length ~ visual + haptic)

model <- lmer(average_velocity ~ visual*haptic + (1|participant), data = tmp_exp2_trial)
summary(model)
tab_model(model, 
          show.est = TRUE,         # Show estimates
          show.se = TRUE,          # Show standard errors
          show.stat = TRUE,        # We need to identify what t-value corresponds to
          show.p = TRUE,           # Show p-values
          show.ci = FALSE)  
flexplot(data = tmp_exp2_trial, average_velocity ~ visual + haptic)

model <- lmer(average_proximity ~ visual*haptic + (1|participant), data = tmp_exp2_trial)
summary(model)
tab_model(model, 
          show.est = TRUE,         # Show estimates
          show.se = TRUE,          # Show standard errors
          show.stat = TRUE,        # We need to identify what t-value corresponds to
          show.p = TRUE,           # Show p-values
          show.ci = FALSE)  
flexplot(data = tmp_exp2_trial, average_proximity ~ visual + haptic)

model <- lmer(average_force ~ visual*haptic + (1|participant), data = tmp_exp2_trial)
summary(model)
tab_model(model, 
          show.est = TRUE,         # Show estimates
          show.se = TRUE,          # Show standard errors
          show.stat = TRUE,        # We need to identify what t-value corresponds to
          show.p = TRUE,           # Show p-values
          show.ci = FALSE)  
flexplot(data = tmp_exp2_trial, average_force ~ visual + haptic)







