# complex_model_1

library(lme4)
library(sjPlot)
library(flexplot)

# add maps as a factor
data$map <- as.factor(data$map)

#################
### Aggregate ###
#################
average_by_trial <- data %>%
  group_by(participant, trial, condition_nums, time_through_maze, max_force, path_length, haptic, visual, map, strategic_both, strategic_either) %>%
  summarise(average_force = mean(force_magnitude, na.rm = TRUE))

##############
### Models ###
##############

# Model 1
model1 <- lmer(time_through_maze ~ haptic * visual * average_force * path_length * map * strategic_both + (1|participant) + (1|trial), data = average_by_trial)
summary(model1)
tab_model(model1)
capture.output(model1, file = here('output', 'Model_time_through_maze', 'model1.txt'))

# Model 1
model1 <- lmer(time_through_maze ~ haptic * visual * average_force * path_length * max_force * strategic_both + (1|participant) + (1|trial), data = average_by_trial)
summary(model1)
tab_model(model1)
capture.output(model1, file = here('output', 'Model_time_through_maze', 'model1.txt'))

# Model 1
model1 <- lmer(time_through_maze ~ strategic_both + (1|participant) + (1|trial), data = average_by_trial)
summary(model1)
tab_model(model1)
capture.output(model1, file = here('output', 'Model_time_through_maze', 'model1.txt'))

flexplot(data = average_by_trial, time_through_maze ~ strategic_both, method = 'lm')
flexplot(data = average_by_trial, time_through_maze ~ strategic_both | haptic + visual, method = 'lm')


# Rerun this after doign strategic again
model1 <- lmer(time_through_maze ~ haptic * visual * strategic_either + (1|participant), data = average_by_trial)
summary(model1)
tab_model(model1)


# Model 1
model1 <- lmer(time_through_maze ~ strategic_either + (1|participant) + (1|trial), data = average_by_trial)
summary(model1)
tab_model(model1)
capture.output(model1, file = here('output', 'Model_time_through_maze', 'model1.txt'))

flexplot(data = average_by_trial, time_through_maze ~ strategic_either, method = 'lm')
flexplot(data = average_by_trial, time_through_maze ~ strategic_either | haptic + visual, method = 'lm')


#####################
### Visualisation ###
#####################

average_by_trial$map <- as.factor(average_by_trial$map)
flexplot(data = average_by_trial, time_through_maze ~ average_force + path_length | haptic + visual + map, method = 'lm')


flexplot(data = average_by_trial, time_through_maze ~ max_force + path_length | haptic + map, method = 'lm')

# interesting! 
flexplot(data = average_by_trial, time_through_maze ~ max_force + path_length | visual, method = 'lm')


