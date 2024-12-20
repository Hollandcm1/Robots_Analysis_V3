# explortory

# aggragate
average_by_trial <- data %>%
  group_by(participant, trial, condition_nums, time_through_maze, max_force, path_length, haptic, visual, map) %>%
  summarise(average_force = mean(force_magnitude, na.rm = TRUE))

ggplot(average_by_trial, aes(x = condition_nums, y = average_force, color = condition_nums)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~participant)

ggplot(average_by_trial, aes(x = condition_nums, y = time_through_maze, color = condition_nums)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~participant) +
  theme_minimal()

summary(average_by_trial$average_force)

# histogram of average force
ggplot(average_by_trial, aes(x = average_force, fill = condition_nums)) +
  geom_histogram(binwidth = 0.1) +
  facet_wrap(~participant) +
  theme_minimal()

# filter data for a single participant
participant_1 <- data %>%
  filter(participant == 1)

ggplot(participant_1, aes(x=force_magnitude, fill = condition_nums)) +
  geom_histogram(binwidth = 0.1) +
  theme_minimal()

ggplot(data, aes(x=force_magnitude, fill = condition_nums)) +
  geom_histogram(binwidth = 0.1) +
  theme_minimal()

ggplot(data, aes(x=force_magnitude, fill = condition_nums)) +
  geom_histogram(binwidth = 0.1) +
  theme_minimal() +
  facet_wrap(~participant)


ggplot(data, aes(x=force_magnitude, y=distance_since_prev_frame)) +
  geom_point() +
  theme_minimal() +
  facet_wrap(~participant) +
  ylim(0, 0.3)


