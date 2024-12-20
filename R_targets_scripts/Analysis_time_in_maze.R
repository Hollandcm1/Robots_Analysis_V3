# time within maze


################################################################################
# Anova analysis for predicting time within maze by average force

# aggregate
average_force_by_participant <- data %>%
  group_by(participant, condition, trial, time_through_maze) %>%
  summarise(average_force = mean(force_magnitude, na.rm = TRUE))

average_force_by_participant$force_group <- cut(average_force_by_participant$average_force, breaks = 2)
summary(average_force_by_participant$force_group)

# change type
average_force_by_participant$force_group <- as.factor(average_force_by_participant$force_group)

# average_force_by_participant <- average_force_by_participant %>%
#   mutate(force_group_2 = ntile(average_force, breaks = 2))

# check normality
shapiro.test(average_force_by_participant$time_through_maze)
hist(average_force_by_participant$time_through_maze, breaks = 20, col = "skyblue", border = "black", xlab = "Time Through Maze", main = "Histogram of Time Through Maze")
hist(average_force_by_participant$average_force, breaks = 20, col = "skyblue", border = "black", xlab = "Average Force", main = "Histogram of Average Force")

# check homogeneity of variance
bartlett.test(time_through_maze ~ force_group, data = average_force_by_participant)

ggplot(data = average_force_by_participant, aes(x = average_force, y = time_through_maze)) +
  geom_point() +
  geom_smooth() +
  theme_minimal() +
  labs(title = "Time Through Maze by Average Force", x = "Average Force", y = "Time Through Maze")


# ANOVA 1
anova_results <- aov(time_through_maze ~ force_group, data = average_force_by_participant)
summary(anova_results)
tab_model(anova_results)

# visualize
ggplot(data = average_force_by_participant, aes(x = force_group, y = time_through_maze)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "Time Through Maze by Average Force", x = "Average Force", y = "Time Through Maze")

ggplot(data = average_force_by_participant, aes(x = force_group, y = time_through_maze)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "Time Through Maze by Average Force", x = "Average Force", y = "Time Through Maze") 

ggplot(data = average_force_by_participant, aes(x = force_group, y = time_through_maze)) +
  geom_point() +  # Plot individual data points
  stat_summary(fun = mean, geom = "point", size = 3, color = "red") +  # Plot means as points
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") +  # Connect means with line
  theme_minimal() +
  labs(title = "Time Through Maze by Average Force", x = "Average Force", y = "Time Through Maze")

ggplot(data = average_force_by_participant, aes(x = force_group, y = time_through_maze, color = condition, group = condition)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "Time Through Maze by Average Force", x = "Average Force", y = "Time Through Maze") +
  facet_wrap(~condition)
