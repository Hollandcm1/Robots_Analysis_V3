# LME_Analysis_exp2 

# library(lme4)
# library(sjPlot)
# library(flexplot)
# library(here)
# library(dplyr)
# library(ggplot2)
# data<- tar_read(data_long_calculated_exp2)

LME_analysis_exp2 <- function(data){

  #################
  ### Aggregate ###
  #################
  average_by_trial <- data %>%
    group_by(participant, trial, condition_nums, time_through_maze, max_force, path_length, haptic, visual, map, average_velocity, average_proximity) %>%
    summarise(average_force = mean(force_magnitude, na.rm = TRUE))
  
  # export to csv
  # average by conditions for each participant
  average_by_conditions <- average_by_trial %>%
    group_by(participant, haptic, visual, condition_nums) %>%
    summarise(average_time_through_maze = mean(time_through_maze, na.rm = TRUE),
              average_path_length = mean(path_length, na.rm = TRUE),
              average_velocity = mean(average_velocity, na.rm = TRUE),
              average_proximity = mean(average_proximity, na.rm = TRUE))
  write.csv(average_by_conditions, here('output', "average_by_conditions.csv"))
  write.csv(average_by_trial, here('output', "average_by_trial.csv"))
  # flexplot(data = average_by_trial, time_through_maze ~ average_velocity + path_length, method = 'lm')
  
  # convert 
  average_by_trial$map_factor <- as.factor(average_by_trial$map)
  
  ##############
  ### Models ###
  ##############
  
  # Model 1
  model1 <- lmer(time_through_maze ~ haptic * visual * path_length * map_factor + (1|participant), data = average_by_trial)
  summary(model1)
  tab_model(model1)
  
  model2 <- lmer(time_through_maze ~ haptic * visual * path_length + (1|participant), data = average_by_trial)
  summary(model2)
  tab_model(model2)
  flexplot(time_through_maze ~ haptic + path_length, data = average_by_trial)
  flexplot(time_through_maze ~ path_length + haptic, data = average_by_trial, method = 'lm')
  
  #convert haptic and visual to numeric
  average_by_trial$haptic_num <- as.numeric(average_by_trial$haptic)
  average_by_trial$visual_num <- as.numeric(average_by_trial$visual)
  #cor(average_by_trial[, c("haptic_num", "visual_num", "path_length", "map", "collaborative")])
  
  # Model Buidling
  model_0 <- lmer(time_through_maze ~ 1 + (1|participant), data = average_by_trial)
  model_1 <- lmer(time_through_maze ~ haptic + (1|participant), data = average_by_trial) # YES
  tab_model(model_1)
  flexplot(time_through_maze ~ haptic, data = average_by_trial)
  # ggplot version
  ggplot(average_by_trial, aes(x = haptic, y = time_through_maze)) +
    geom_boxplot() +
    geom_jitter(width = 0.2) +
    theme_minimal()
  model_2 <- lmer(time_through_maze ~ visual + (1|participant), data = average_by_trial) # YES
  tab_model(model_2)
  flexplot(time_through_maze ~ visual, data = average_by_trial)
  model_3 <- lmer(time_through_maze ~ haptic * visual + (1|participant), data = average_by_trial) # YES, haptic*visual
  tab_model(model_3)
  flexplot(time_through_maze ~ visual + haptic, data = average_by_trial)
  
  model_6 <- lmer(time_through_maze ~ haptic * visual * path_length + (1|participant), data = average_by_trial) # YES
  tab_model(model_6)
  flexplot(time_through_maze ~ haptic + visual | path_length, data = average_by_trial)
  flexplot(time_through_maze ~ path_length, data = average_by_trial, method='lm')
  flexplot(time_through_maze ~ path_length + visual, data = average_by_trial, method='lm')
  flexplot(time_through_maze ~ path_length + haptic, data = average_by_trial, method='lm')

}



