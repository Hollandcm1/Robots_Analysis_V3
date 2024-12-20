# simple visuals

library(ggplot2)
library(dplyr)


# load data
data_exp1 <- tar_read(strategic_data_appended)
data_exp2 <- tar_read(data_long_calculated_exp2)

all_vars <- names(data_exp1)

# remove all vars except force_x, force_y, and frame
data_exp1 <- data_exp1 %>%
  select(force_x, force_y, frame, force_magnitude, linear_velocity, closest_object_distance)
data_exp2 <- data_exp2 %>%
  select(force_x, force_y, frame, force_magnitude, linear_velocity, closest_object_distance)

# combine data
data_exp1 <- data_exp1 %>%
  mutate(exp = 1)
data_exp2 <- data_exp2 %>%
  mutate(exp = 2)
data <- rbind(data_exp1, data_exp2)



###############################################################################
################################### By Frame ##################################
###############################################################################

# overlapping histogram of force_x by frame, colour by experiment with transparency
ggplot(data, aes(x = force_x, fill = factor(exp))) +
  geom_histogram(aes(y = ..density..), alpha = 0.5, position = "identity", bins = 100) +
  labs(title = "Histogram of Force_x by Frame",
       x = "Force_x",
       y = "Density") +
  theme_minimal()

# overlapping histogram of force_y by frame, colour by experiment with transparency
ggplot(data, aes(x = force_y, fill = factor(exp))) +
  geom_histogram(aes(y = ..density..), alpha = 0.5, position = "identity", bins = 100) +
  labs(title = "Histogram of Force_y by Frame",
       x = "Force_y",
       y = "Density") +
  theme_minimal()

# scatter plot of force_x and force_y by frame, colour by experiment
# ggplot(data, aes(x = force_x, y = force_y, colour = factor(exp))) +
#   geom_point() +
#   labs(title = "Scatter Plot of Force_x and Force_y by Frame",
#        x = "Force_x",
#        y = "Force_y") +
#   theme_minimal()

# overlapping histogram of force_magnitude by frame, colour by experiment with transparency
ggplot(data, aes(x = force_magnitude, fill = factor(exp))) +
  geom_histogram(aes(y = ..density..), alpha = 0.5, position = "identity", bins = 100) +
  labs(title = "Histogram of Force Magnitude by Frame",
       x = "Force Magnitude",
       y = "Density") +
  theme_minimal()



# remove velocities less than -5
data <- data %>%
  filter(linear_velocity > -5)
# overlapping histogram of linear_velocity by frame, colour by experiment with transparency
ggplot(data, aes(x = linear_velocity, fill = factor(exp))) +
  geom_histogram(aes(y = ..density..), alpha = 0.5, position = "identity", bins = 100) +
  labs(title = "Histogram of Linear Velocity by Frame",
       x = "Linear Velocity",
       y = "Density") +
  theme_minimal()

# remove distances greater than 10
data <- data %>%
  filter(closest_object_distance < 10)
# overlapping histogram of closest_object_distance by frame, colour by experiment with transparency
ggplot(data, aes(x = closest_object_distance, fill = factor(exp))) +
  geom_histogram(aes(y = ..density..), alpha = 0.5, position = "identity", bins = 100) +
  labs(title = "Histogram of Closest Object Distance by Frame",
       x = "Closest Object Distance",
       y = "Density") +
  theme_minimal()
