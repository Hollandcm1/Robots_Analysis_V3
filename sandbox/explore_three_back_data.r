# explore_three_back_data

library(targets)
library(here)
library(ggplot2)
library(dplyr)
library(flexplot)

data <- tar_read(three_back_data_with_codes)

# save path
save_path <- here("output", "three_back_exploration")

# convert to factors
data$participant <- as.factor(data$participant)
data$condition <- as.factor(data$condition)
data$haptic <- as.factor(data$haptic)
data$visual <- as.factor(data$visual)


########################
### x final position ###
########################


### Individual ###

# plot barchart of trial my x_final_position for each participant
for (participant in unique(data$participant)) {

  p_data <- data[data$participant == participant,]

  p <- ggplot(p_data, aes(x = trial, y = x_final_position)) +
    geom_bar(stat = "identity") +
    labs(title = paste("Participant", participant, "x_count"))

  final_path <- here(save_path, "x_count")
  # make the dir if it doesn't exists
  if (!dir.exists(final_path)) {
    dir.create(final_path)
  }
  ggsave(here(final_path, paste("participant_", participant, "_x_count.png")), p)

}


### Combined ###

# boxplots of x_final_position for all participants
p <- ggplot(data, aes(x = participant, y = x_final_position, group = participant, fill = participant)) +
  geom_violin(alpha = 0.5) +
  geom_boxplot(width = 0.3) +
  geom_point() +
  labs(title = "All Participants x_count") +
  # remove legend
  theme_minimal() +
  theme(legend.position = "none") 

final_path <- here(save_path, "x_count")
ggsave(here(final_path, "all_participants_x_count.png"), p)


### Histogram ###

p <- ggplot(data, aes(x = x_final_position)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Histogram of x_final_position") +
  theme_minimal()

final_path <- here(save_path, "x_count")
ggsave(here(final_path, "histogram_x_count.png"), p)



###############
### p count ###
###############


### Individual ###

# plot barchart of trial my p_count for each participant
for (participant in unique(data$participant)) {

  p_data <- data[data$participant == participant,]

  p <- ggplot(p_data, aes(x = trial, y = p_count)) +
    geom_bar(stat = "identity") +
    labs(title = paste("Participant", participant, "p_count"))

  final_path <- here(save_path, "p_count")
  # make the dir if it doesn't exists
  if (!dir.exists(final_path)) {
    dir.create(final_path)
  }
  ggsave(here(final_path, paste("participant_", participant, "_p_count.png")), p)

}


### Combined ###

# boxplots of p_count for all participants
p <- ggplot(data, aes(x = participant, y = p_count, group = participant, fill = participant)) +
  geom_violin(alpha = 0.5) +
  geom_boxplot(width = 0.3) +
  geom_point() +
  labs(title = "All Participants p_count") +
  # remove legend
  theme_minimal() +
  theme(legend.position = "none")

final_path <- here(save_path, "p_count")
ggsave(here(final_path, "all_participants_p_count.png"), p)


### Histogram ###

p <- ggplot(data, aes(x = p_count)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Histogram of p_count") +
  theme_minimal()

final_path <- here(save_path, "p_count")
ggsave(here(final_path, "histogram_p_count.png"), p)


###############
### e count ###
###############


### Individual ###

# plot barchart of trial my e_count for each participant
for (participant in unique(data$participant)) {

  p_data <- data[data$participant == participant,]

  p <- ggplot(p_data, aes(x = trial, y = e_count)) +
    geom_bar(stat = "identity") +
    labs(title = paste("Participant", participant, "e_count"))

  final_path <- here(save_path, "e_count")
  # make the dir if it doesn't exists
  if (!dir.exists(final_path)) {
    dir.create(final_path)
  }
  ggsave(here(final_path, paste("participant_", participant, "_e_count.png")), p)

}


### Combined ###

# boxplots of e_count for all participants
p <- ggplot(data, aes(x = participant, y = e_count, group = participant, fill = participant)) +
  geom_violin(alpha = 0.5) +
  geom_boxplot(width = 0.3) +
  geom_point() +
  labs(title = "All Participants e_count") +
  # remove legend
  theme_minimal() +
  theme(legend.position = "none")

final_path <- here(save_path, "e_count")
ggsave(here(final_path, "all_participants_e_count.png"), p)


### Histogram ###

p <- ggplot(data, aes(x = e_count)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Histogram of e_count") +
  theme_minimal()

final_path <- here(save_path, "e_count")
ggsave(here(final_path, "histogram_e_count.png"), p)



################################
### by condition exploration ###
################################

condition_summary <- data %>%
  group_by(participant, condition) %>%
  summarise(
    mean_x_final_position = mean(x_final_position, na.rm = TRUE),
    mean_p_count = mean(p_count, na.rm = TRUE),
    mean_e_count = mean(e_count, na.rm = TRUE),
    haptic = unique(haptic),
    visual = unique(visual),
  )


# plot x_final position
p <- flexplot(
  data = condition_summary,
  formula = mean_x_final_position ~ haptic + visual,
)

final_path <- here(save_path, "condition_summary")
# make the dir if it doesn't exists
if (!dir.exists(final_path)) {
  dir.create(final_path)
}
ggsave(here(final_path, "x_final_position_condition_summary.png"), p)


# plot p_count
p <- flexplot(
  data = condition_summary,
  formula = mean_p_count ~ haptic + visual,
)

final_path <- here(save_path, "condition_summary")
ggsave(here(final_path, "p_count_condition_summary.png"), p)


# plot e_count
p <- flexplot(
  data = condition_summary,
  formula = mean_e_count ~ haptic + visual,
)

final_path <- here(save_path, "condition_summary")
ggsave(here(final_path, "e_count_condition_summary.png"), p)
