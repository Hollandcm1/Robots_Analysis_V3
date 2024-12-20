library(lme4)
library(sjPlot)

data

# get subset of data where experiment == 1
data1 <- subset(data, experiment == 1)

model <- lmer(data=data1, average_time_through_maze ~ visual * haptic + (1|participant))

summary(model)

tab_model(model)

# do an aov version
model_aov <- aov(data=data1, average_time_through_maze ~ visual * haptic + Error(participant))

summary(model_aov)

tab_model(model_aov)
