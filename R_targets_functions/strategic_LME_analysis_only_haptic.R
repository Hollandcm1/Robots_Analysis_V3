# Model

# library(lme4)
# library(sjPlot)
# library(flexplot)
# library(here)
# library(dplyr)
# library(ggplot2)
# data <- tar_read(strategic_data_appended)

# data <- strategic_data_appended

strategic_LME_analysis_only_haptic <- function(data) {
  
  #################
  ### Aggregate ###
  #################
  average_by_trial <- data %>%
    group_by(participant, trial, condition_nums, time_through_maze, max_force, path_length, haptic, visual, map, strategic_both, strategic_either, collaborative) %>%
    summarise(average_force = mean(force_magnitude, na.rm = TRUE))
  
  # subset to only haptic
  average_by_trial <- average_by_trial %>% filter(haptic == 1)
  
  # convert 
  average_by_trial$map_factor <- as.factor(average_by_trial$map)
  
  ##############
  ### Models ###
  ##############
  
  # Model 1
  model1 <- lmer(time_through_maze ~ visual * path_length * map_factor * collaborative + (1|participant), data = average_by_trial)
  summary(model1)
  tab_model(model1)
  #capture.output(model1, file = here('output', 'Strategic_Models', 'model1.txt'))
  
  
  
  #convert haptic and visual to numeric
  average_by_trial$haptic_num <- as.numeric(average_by_trial$haptic)
  average_by_trial$visual_num <- as.numeric(average_by_trial$visual)
  average_by_trial$collaborative <- as.numeric(average_by_trial$collaborative)
  cor(average_by_trial[, c("haptic_num", "visual_num", "path_length", "map", "collaborative")])
  
  model2 <- lmer(time_through_maze ~ visual * path_length * collaborative + (1|participant), data = average_by_trial)
  summary(model2)
  tab_model(model2)
  
  
  # Model Buidling
  
  ### Time Through Maze ###
  
  model_0 <- lmer(time_through_maze ~ 1 + (1|participant), data = average_by_trial)
  model_2 <- lmer(time_through_maze ~ visual + (1|participant), data = average_by_trial) # YES
  tab_model(model_2)
  flexplot(time_through_maze ~ visual, data = average_by_trial)
  
  model_4 <- lmer(time_through_maze ~ collaborative + (1|participant), data = average_by_trial) # YES
  tab_model(model_4)
  flexplot(time_through_maze ~ collaborative, data = average_by_trial)
  model_5 <- lmer(time_through_maze ~ visual * collaborative + (1|participant), data = average_by_trial) # NO
  tab_model(model_5)
  flexplot(time_through_maze ~ collaborative | visual, data = average_by_trial)
  flexplot(time_through_maze ~ visual | collaborative, data = average_by_trial)
  
  model_6 <- lmer(time_through_maze ~ visual * path_length + (1|participant), data = average_by_trial) # YES
  tab_model(model_6)
  flexplot(time_through_maze ~ visual | path_length, data = average_by_trial)
  flexplot(time_through_maze ~ path_length, data = average_by_trial, method='lm')
  flexplot(time_through_maze ~ path_length + visual, data = average_by_trial, method='lm')
  
  model_7 <- lmer(time_through_maze ~ map_factor + (1|participant), data = average_by_trial) # YES
  tab_model(model_7)
  flexplot(time_through_maze ~ map_factor, data = average_by_trial)
  model_8 <- lmer(time_through_maze ~ visual * map_factor + (1|participant), data = average_by_trial) # YES
  tab_model(model_8)
  
  
  ### Path Length ### 
  
  model_12 <- lmer(path_length ~ visual * map_factor * collaborative + (1|participant), data = average_by_trial) # YES)
  tab_model(model_12)
  
  model_13 <- lmer(path_length ~ collaborative + (1|participant), data = average_by_trial) # YES
  tab_model(model_13)
  flexplot(path_length ~ collaborative, data = average_by_trial)
  
  model_14 <- lmer(path_length ~ visual * collaborative + (1|participant), data = average_by_trial) # YES
  tab_model(model_14)
  flexplot(path_length ~ collaborative | visual, data = average_by_trial)
  
  
  
  model_15 <- lmer(time_through_maze ~ collaborative + visual + (1|participant), data = average_by_trial) # YES
  tab_model(model_15)
  flexplot(time_through_maze ~ visual | collaborative, data = average_by_trial)
  
  # combine all models into a single variable
  models <- list(model1, model_0, model_2, model_4, model_5, model_6, model_7, model_8, model_12, model_13, model_14)
  
  # save the models
  save(models, file = here('output', 'Strategic_Models', 'no_haptic_models.RData'))
  
  # save the data
  save(average_by_trial, file = here('output', 'Strategic_Models', 'no_haptic_average_by_trial.RData'))
  
  return(models)
  
}


# Testing
#ggplot(average_by_trial, aes(x=time_through_maze, y=participant)) +
#  geom_point()

