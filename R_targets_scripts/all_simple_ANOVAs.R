# simple ANOVAs

# libraries
library(effectsize)

# load data
data <- read.csv(here('output', 'average_by_conditions_all_data.csv'))

data_trials_exp2 <- read.csv(here('output', 'average_by_trial.csv'))

# subset od data where experiment is 1
data_exp1 <- subset(data, experiment == 1)

# subset of data where experiment is 2
data_exp2 <- subset(data, experiment == 2)

#################
### Averaging ###
#################

# average by participant and condition
#data_exp1 <- aggregate(data_exp1, by=list(data_exp1$participant, data_exp1$condition_nums), FUN=mean)
#data_exp2 <- aggregate(data_exp2, by=list(data_exp2$participant, data_exp2$condition_nums), FUN=mean)

###########################
### Experiment 1 ANOVAs ###
###########################

model_time<-aov(average_time_through_maze ~ haptic*visual + (participant/condition_nums), data=data_exp1)
# model_time<-aov(average_time_through_maze ~ haptic*visual, data=data_exp1)

summary(model_time)
eta_squared(model_time)

model_velocity<-aov(average_velocity ~ haptic*visual + (participant/condition_nums), data=data_exp1)
summary(model_velocity)
eta_squared(model_velocity)

model_pathlength<-aov(average_path_length ~ haptic*visual + (participant/condition_nums), data=data_exp1)
summary(model_pathlength)
eta_squared(model_pathlength)

model_proximity<-aov(average_proximity ~ haptic*visual + (participant/condition_nums), data=data_exp1)
summary(model_proximity)
eta_squared(model_proximity)

model_force<-aov(average_force ~ haptic*visual + (participant/condition_nums), data=data_exp1)
summary(model_force)
eta_squared(model_force)

###########################
### Experiment 2 ANOVAs ###
###########################

model_time<-aov(average_time_through_maze ~ haptic*visual + (participant/condition_nums), data=data_exp2)
summary(model_time)
eta_squared(model_time)

#model_time<-aov(average_time_through_maze ~ haptic*visual, data=data_exp2)
#summary(model_time)
# model_time<-aov(average_time_through_maze ~ haptic*visual, data=data_exp2)

model_time<-aov(time_through_maze ~ haptic*visual + (participant/condition_nums), data=data_trials_exp2) 
summary(model_time)
eta_squared(model_time)


model_time<-aov(time_through_maze ~ haptic*visual+ Error(participant/(haptic*visual)), data=data_trials_exp2)
summary(model_time)
eta_squared(model_time)


model_velocity<-aov(average_velocity ~ haptic*visual + (participant/condition_nums), data=data_exp2)
summary(model_velocity)
eta_squared(model_velocity)

model_pathlength<-aov(average_path_length ~ haptic*visual + (participant/condition_nums), data=data_exp2)
summary(model_pathlength)
eta_squared(model_pathlength)

model_proximity<-aov(average_proximity ~ haptic*visual + (participant/condition_nums), data=data_exp2)
summary(model_proximity)
eta_squared(model_proximity)

model_force<-aov(average_force ~ haptic*visual + (participant/condition_nums), data=data_exp2)
summary(model_force)
eta_squared(model_force)





