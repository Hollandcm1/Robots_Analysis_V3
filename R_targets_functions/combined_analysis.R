

# library(targets)
# library(lme4)
# library(sjPlot)
# library(flexplot)
# library(here)
# library(dplyr)
# library(ggplot2)
# data_exp1 <- tar_read(strategic_data_appended)
# data_exp2 <- tar_read(data_long_calculated_exp2)


combined_analysis <- function(data_exp1, data_exp2){
    
    #################
    ### Aggregate ###
    #################
    data <- data_exp1
    average_by_trial <- data %>%
      group_by(participant, condition_nums, time_through_maze, max_force, path_length, haptic, visual, map, average_velocity, average_proximity, average_force) %>%
      summarise(average_force = mean(force_magnitude, na.rm = TRUE))
    # add experiment column
    average_by_trial$experiment <- 1
    tmp_exp1 <- average_by_trial
    
    data <- data_exp2
    average_by_trial <- data %>%
      group_by(participant, condition_nums, time_through_maze, max_force, path_length, haptic, visual, map, average_velocity, average_proximity, average_force) %>%
      summarise(average_force = mean(force_magnitude, na.rm = TRUE))
    # add experiment column
    average_by_trial$experiment <- 2
    # change participant column to number
    average_by_trial$participant <- as.numeric(average_by_trial$participant)
    tmp_exp2 <- average_by_trial
    
    # relabel participants values as 26+
    tmp_exp2$participant <- tmp_exp2$participant + 25
    
    # combine
    all_data <- rbind(tmp_exp1, tmp_exp2)
    average_by_trial <- all_data
    
    # convert experiment to factor
    average_by_trial$experiment <- as.factor(average_by_trial$experiment)
    
    # convert haptic and visual to factor
    average_by_trial$haptic <- as.factor(average_by_trial$haptic)
    average_by_trial$visual <- as.factor(average_by_trial$visual)
    
    # export to csv (for standard figures)
    average_by_conditions <- average_by_trial %>%
      group_by(participant, haptic, visual, condition_nums, experiment) %>%
      summarise(average_time_through_maze = mean(time_through_maze, na.rm = TRUE),
                average_path_length = mean(path_length, na.rm = TRUE),
                average_velocity = mean(average_velocity, na.rm = TRUE),
                average_proximity = mean(average_proximity, na.rm = TRUE),
                average_force = mean(average_force, na.rm = TRUE))
    # write.csv(average_by_conditions, here('output', "average_by_conditions_all_data.csv"))
    
    # Large model
    model1 <- lmer(time_through_maze ~ haptic * visual * path_length * experiment + (1|participant), data = average_by_trial)
    summary(model1)
    tab_model(model1)
    
    flexplot(time_through_maze ~ haptic + path_length | experiment, data = average_by_trial)
    flexplot(time_through_maze ~ haptic | experiment + path_length, data = average_by_trial)
    flexplot(time_through_maze ~ path_length + haptic | experiment, data = average_by_trial, method = 'lm')
    
    
    # Model Buidling
    model_0 <- lmer(time_through_maze ~ 1 + (1|participant), data = average_by_trial)
    model_1 <- lmer(time_through_maze ~ haptic + (1|participant), data = average_by_trial) # YES
    tab_model(model_1)
    flexplot(time_through_maze ~ haptic, data = average_by_trial)
    model_2 <- lmer(time_through_maze ~ visual + (1|participant), data = average_by_trial) # YES
    tab_model(model_2)
    flexplot(time_through_maze ~ visual, data = average_by_trial)
    model_3 <- lmer(time_through_maze ~ haptic * visual + (1|participant), data = average_by_trial) # YES, haptic*visual
    tab_model(model_3)
    flexplot(time_through_maze ~ visual + haptic, data = average_by_trial)
    
    model_4 <- lmer(time_through_maze ~ experiment + (1|participant), data = average_by_trial) # YES, haptic*visual*path_length
    tab_model(model_4)
    flexplot(time_through_maze ~ experiment, data = average_by_trial)
    
    model_5 <- lmer(time_through_maze ~ haptic * visual * experiment + (1|participant), data = average_by_trial) #
    tab_model(model_5)
    flexplot(time_through_maze ~ haptic + visual | experiment, data = average_by_trial)
    
    
    
    
    # time model 
    model <- lmer(time_through_maze ~ haptic * visual * experiment + (1 + haptic * visual|participant), data = average_by_trial)
    tab_model(model, show.se = TRUE, show.stat = TRUE)
    flexplot(time_through_maze ~ haptic + visual | experiment, data = average_by_trial)
    flexplot(time_through_maze ~ visual + haptic | experiment, data = average_by_trial)
    
    # velocity model
    model <- lmer(average_velocity ~ haptic * visual * experiment + (1 + haptic * visual|participant), data = average_by_trial)
    tab_model(model, show.se = TRUE, show.stat = TRUE)
    flexplot(average_velocity ~ haptic + visual | experiment, data = average_by_trial)
    flexplot(average_velocity ~ visual + haptic | experiment, data = average_by_trial)
    flexplot(average_velocity ~ experiment, data = average_by_trial)
    
    # path length model
    model <- lmer(path_length ~ haptic * visual * experiment + (1 + haptic * visual|participant), data = average_by_trial)
    tab_model(model, show.se = TRUE, show.stat = TRUE)
    flexplot(path_length ~ haptic + visual | experiment, data = average_by_trial)
    flexplot(path_length ~ haptic + experiment, data = average_by_trial)
    
    # proximity model 
    model <- lmer(average_proximity ~ haptic * visual * experiment + (1 + haptic * visual|participant), data = average_by_trial)
    tab_model(model, show.se = TRUE, show.stat = TRUE)
    flexplot(average_proximity ~ haptic + visual | experiment, data = average_by_trial)
    flexplot(average_proximity ~ visual + haptic | experiment, data = average_by_trial)
    
    return(average_by_conditions)
    
}
