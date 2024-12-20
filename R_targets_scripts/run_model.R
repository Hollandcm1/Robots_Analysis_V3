# run_Model

library(lme4)
library(sjPlot)
library(ggbeeswarm)
library(dplyr)
library(ggplot2)
library(ggbeeswarm)
library(lme4)


data <- data_long

# change type
data$condition_num <- as.factor(data$condition_num)
data$participant <- as.factor(data$participant)
data$haptic <- as.factor(data$haptic)
data$visual <- as.factor(data$visual)

# Aggregate
average_force_by_trial <- data %>%
  group_by(participant, trial, condition_nums, time_through_maze, max_force, path_length, haptic, visual, map) %>%
  summarise(average_force = mean(force_magnitude, na.rm = TRUE))

##############
### Models ###
##############

# Model 1
model1 <- lmer(time_through_maze ~ average_force * haptic * visual * max_force * path_length * map + (1|participant) + (1|trial), data = average_force_by_trial)
summary(model1)
tab_model(model1)

# # Model 2
# model2 <- lmer(time_through_maze ~ average_force + (1|participant), data = data)
# summary(model2)
# tab_model(model2)
# 
# # Model 3
# model3 <- lmer(time_through_maze ~ average_force + (1|participant) + (1|trial), data = data)
# summary(model3)
# tab_model(model3)
# 
# # Model 4
# model4 <- lmer(path_length ~ average_force + (1|participant) + (1|trial), data = data)
# summary(model4)
# tab_model(model4)
# 
# # Model 5
# model5 <- lmer(average_force ~ haptic * visual + (1|participant) + (1|trial), data = data)
# summary(model5)
# tab_model(model5)


#################
### Visualize ###
#################

ggplot(data = average_force_by_trial, aes(x = average_force, y = time_through_maze)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "Time Through Maze by Average Force", x = "Average Force", y = "Time Through Maze")

ggplot(data = average_force_by_trial, aes(x = max_force, y = time_through_maze, color = haptic, group = haptic)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "Time Through Maze by Max Force", x = "Max Force", y = "Time Through Maze") +
  facet_wrap(~visual)

ggplot(data = average_force_by_trial, aes(x = path_length, y = average_force)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "Average Force by Path Length", x = "Path Length", y = "Average Force")

ggplot(data = average_force_by_trial, aes(x = haptic, y = average_force, group=visual, colour=visual)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, height = 0) +
  theme_minimal() +
  labs(title = "Average Force by Condition", x = "Haptic", y = "Average Force") +
  facet_wrap(~visual)

# violin version 
ggplot(data = average_force_by_trial, aes(x = haptic, y = average_force, group=condition_num, colour=visual)) +
  geom_violin() +
  geom_boxplot(width=0.2) +
  #geom_jitter(width = 0.2, height=0) +
  geom_quasirandom(data = average_force_by_trial, width = 0.2, height = 0) +
  theme_minimal() +
  labs(title = "Average Force by Condition", x = "Condition", y = "Average Force") +
  facet_wrap(~visual)

ggplot(average_force_by_trial, aes(x = average_force, fill = visual)) +
  geom_histogram(binwidth = 0.1) + # Adjust binwidth as needed
  facet_wrap(~haptic) +
  labs(title = "Distribution of Average Force by Condition", x = "Average Force", y = "Count") +
  theme_minimal()

ggplot(average_force_by_trial, aes(x = average_force, fill = haptic)) +
  geom_histogram(alpha = 0.5, position = "identity", binwidth = 0.1) + # Adjust binwidth as needed
  scale_fill_brewer(palette = "Set1") +  # Optional: Use a color palette for better visibility
  labs(title = "Overlapping Distribution of Average Force by Haptic Condition",
       x = "Vision",
       y = "Count") +
  theme_minimal() +
  facet_wrap(~visual, scales = "free_y") # Use "free_y" if each facet needs to have its own y scale

# ggplot(average_force_by_trial, aes(x = haptic, y=max_force)) +
#   geom_histogram(alpha = 0.5, position = "identity", binwidth = 0.1) + # Adjust binwidth as needed
#   scale_fill_brewer(palette = "Set1") +  # Optional: Use a color palette for better visibility
#   labs(title = "Overlapping Distribution of Average Force by Visual Condition",
#        x = "Haptic",
#        y = "Count") +
#   theme_minimal() +
#   facet_wrap(~haptic, scales = "free_y") # Use "free_y" if each facet needs to have its own y scale)

flexplot(data = average_force_by_trial, time_through_maze ~ max_force + path_length | haptic, method='lm')
flexplot(data = average_force_by_trial, time_through_maze ~ average_force + path_length | haptic, method='lm')

flexplot(data = average_force_by_trial, time_through_maze ~ haptic + path_length | max_force, method='lm')

flexplot(data = average_force_by_trial, time_through_maze ~ path_length + max_force | haptic, method='lm')



