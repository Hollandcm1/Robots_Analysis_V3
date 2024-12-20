# Model

# library(lme4)
# library(sjPlot)
# library(flexplot)
# library(here)
# library(dplyr)
# library(ggplot2)
# data <- tar_read(strategic_data_appended)

# data <- strategic_data_appended

strategic_LME_analysis <- function(data) {

  #################
  ### Aggregate ###
  #################
  average_by_trial <- data %>%
    group_by(participant, trial, condition_nums, time_through_maze, max_force, path_length, haptic, visual, map, strategic_both, strategic_either, collaborative) %>%
    summarise(average_force = mean(force_magnitude, na.rm = TRUE))
  
  # convert 
  average_by_trial$map_factor <- as.factor(average_by_trial$map)
  
  ##############
  ### Models ###
  ##############
  
  # Model 1
  model1 <- lmer(time_through_maze ~ haptic * visual * path_length * map_factor * collaborative + (1|participant), data = average_by_trial)
  summary(model1)
  tab_model(model1)
  capture.output(model1, file = here('output', 'Strategic_Models', 'model1.txt'))
  
  #convert haptic and visual to numeric
  average_by_trial$haptic_num <- as.numeric(average_by_trial$haptic)
  average_by_trial$visual_num <- as.numeric(average_by_trial$visual)
  average_by_trial$collaborative <- as.numeric(average_by_trial$collaborative)
  #cor(average_by_trial[, c("haptic_num", "visual_num", "path_length", "map", "collaborative")])
  
  model2 <- lmer(time_through_maze ~ haptic * visual * path_length * collaborative + (1|participant), data = average_by_trial)
  summary(model2)
  tab_model(model2)
  flexplot(time_through_maze ~ visual + path_length, data = average_by_trial)
  flexplot(time_through_maze ~path_length + visual, data = average_by_trial, method ='lm')
  flexplot(time_through_maze ~ path_length, data = average_by_trial, method ='lm')
  
  # Model Buidling
  model_0 <- lmer(time_through_maze ~ 1 + (1|participant), data = average_by_trial)
  model_1 <- lmer(time_through_maze ~ haptic + (1|participant), data = average_by_trial) # YES
  tab_model(model_1)
  # haptic to facotor, ordered 0 1
  average_by_trial$haptic <- factor(average_by_trial$haptic, levels = c(0, 1))
  flexplot(time_through_maze ~ haptic, data = average_by_trial)
  # ggplot version 
  ggplot(average_by_trial, aes(x=haptic, y=time_through_maze)) +
    geom_boxplot() +
    geom_jitter(width = 0.2) + 
    theme_minimal()
    
  model_2 <- lmer(time_through_maze ~ visual + (1|participant), data = average_by_trial) # YES
  tab_model(model_2)
  flexplot(time_through_maze ~ visual, data = average_by_trial)
  model_3 <- lmer(time_through_maze ~ haptic * visual + (1|participant), data = average_by_trial) # YES, haptic*visual
  tab_model(model_3)
  flexplot(time_through_maze ~ visual + haptic, data = average_by_trial)
  
  model_4 <- lmer(time_through_maze ~ collaborative + (1|participant), data = average_by_trial) # YES
  tab_model(model_4)
  flexplot(time_through_maze ~ collaborative, data = average_by_trial)
  model_5 <- lmer(time_through_maze ~ haptic * visual * collaborative + (1|participant), data = average_by_trial) # NO
  tab_model(model_5)
  flexplot(time_through_maze ~ collaborative | haptic + visual, data = average_by_trial)
  flexplot(time_through_maze ~  haptic + visual | collaborative, data = average_by_trial)
  
  model_6 <- lmer(time_through_maze ~ haptic * visual * path_length + (1|participant), data = average_by_trial) # YES
  tab_model(model_6)
  flexplot(time_through_maze ~ haptic + visual | path_length, data = average_by_trial)
  flexplot(time_through_maze ~ path_length, data = average_by_trial, method='lm')
  flexplot(time_through_maze ~ path_length + visual, data = average_by_trial, method='lm')
  flexplot(time_through_maze ~ path_length + haptic, data = average_by_trial, method='lm')
  
  model_7 <- lmer(time_through_maze ~ map_factor + (1|participant), data = average_by_trial) # YES
  tab_model(model_7)
  flexplot(time_through_maze ~ map_factor, data = average_by_trial)
  model_8 <- lmer(time_through_maze ~ haptic * visual * map_factor + (1|participant), data = average_by_trial) # YES
  tab_model(model_8)
  flexplot(time_through_maze ~ map_factor + haptic + visual, data = average_by_trial)
  flexplot(time_through_maze ~ visual + map_factor, data = average_by_trial)
  flexplot(time_through_maze ~ visual | map_factor, data = average_by_trial)
  
  model_9 <- lmer(time_through_maze ~ path_length * map_factor + (1|participant), data = average_by_trial) # YES
  tab_model(model_9)
  flexplot(time_through_maze ~ path_length + map_factor, data = average_by_trial, method='lm')
  model_10 <- lmer(time_through_maze ~ haptic * path_length + (1|participant), data = average_by_trial) # NO
  tab_model(model_10)
  flexplot(time_through_maze ~ path_length + haptic, data = average_by_trial, method='lm')
  
  model_11 <- lmer(time_through_maze ~ haptic * visual * path_length + (1|participant), data = average_by_trial) # NO
  tab_model(model_11)
  flexplot(time_through_maze ~ visual + collaborative | path_length, data = average_by_trial)
  
  model_12 <- lmer(path_length ~ haptic * visual * map_factor + (1|participant), data = average_by_trial) # YES)
  tab_model(model_12)
  
  
  # put all models in single variable
  models <- list(model1, model_0, model_1, model_2, model_3, model_4, model_5, model_6, model_7, model_8, model_9, model_10, model_11, model_12)
  
  # save the models
  save(models, file = here('output', 'Strategic_Models', 'all_models.RData'))
  
  # save the data
  save(average_by_trial, file = here('output', 'Strategic_Models', 'all_average_by_trial.RData'))
  
  return(models)

}


# Testing
#ggplot(average_by_trial, aes(x=time_through_maze, y=participant)) +
#  geom_point()

