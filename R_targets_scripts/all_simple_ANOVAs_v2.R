# all_simple_ANOVAs_v2

# libraries
library(effectsize)
library(lme4)
library(sjPlot)
library(ez)
library(ggplot2)
library(effects)


# load data
data <- read.csv(here('output', 'average_by_conditions_all_data.csv'))

data_trials_exp2 <- read.csv(here('output', 'average_by_trial.csv'))
# convert to factor as needed
data_trials_exp2$haptic <- as.factor(data_trials_exp2$haptic)
data_trials_exp2$visual <- as.factor(data_trials_exp2$visual)
data_trials_exp2$participant <- as.factor(data_trials_exp2$participant)


# subset od data where experiment is 1
data_exp1 <- subset(data, experiment == 1)
# convert to factor as needed
data_exp1$haptic <- as.factor(data_exp1$haptic)
data_exp1$visual <- as.factor(data_exp1$visual)
data_exp1$participant <- as.factor(data_exp1$participant)

# subset of data where experiment is 2
data_exp2 <- subset(data, experiment == 2)
# convert to factor as needed
data_exp2$haptic <- as.factor(data_exp2$haptic)
data_exp2$visual <- as.factor(data_exp2$visual)
data_exp2$participant <- as.factor(data_exp2$participant)







###########################
### Experiment 2 ANOVAs ###
###########################

# aov version 
model_time_aov <- aov(time_through_maze ~ haptic*visual + Error(participant/(haptic*visual)), data=data_trials_exp2)
#model_time_aov <- aov(time_through_maze ~ haptic*visual, data=data_trials_exp2)
summary(model_time_aov)
eta_squared(model_time_aov, anova = TRUE)

# plot the model 
# Plot the effects of the model
#plot(allEffects(model_time_aov))

# scatterplot 
library(beeswarm)
p <- ggplot(data_trials_exp2, aes(x=haptic, y=time_through_maze, fill=visual, color=visual)) + 
  geom_quasirandom(alpha=0.3) + 
  theme_minimal() + 
  labs(title="Time through maze by haptic and visual conditions", x="Haptic", y="Time through maze") + 
  theme(legend.position="top") +
  facet_wrap(~visual)
p


p <- ggplot(data_trials_exp2, aes(x=haptic, y=time_through_maze, fill=visual)) + 
  geom_boxplot() + 
  theme_minimal() + 
  labs(title="Time through maze by haptic and visual conditions", x="Haptic", y="Time through maze") + 
  theme(legend.position="top")
p


model_time_aov <- aov(average_time_through_maze ~ haptic*visual + Error(participant/(haptic*visual)), data=data_exp2)
summary(model_time_aov)

# ez version
ezANOVA(data=data_trials_exp2, 
        dv=time_through_maze, 
        wid=participant, 
        within=.(haptic, visual), type=3)

# LME version
model_time<-lmer(time_through_maze ~ haptic*visual + (1|participant), data=data_trials_exp2)
summary(model_time)
tab_model(model_time)





