# make_final_figures

library(targets)
library(here)
library(dplyr)
library(ggplot2)
library(ggbeeswarm)


workload_data <- tar_read(workload_data)
three_back_data_with_composites <- tar_read(three_back_data_with_composites)
data_long_calculated <- tar_read(data_long_calculated)


# save_paht
save_path <- here("Output", "Manuscript_Figures")
workload_path <- file.path(save_path, "workload_plot")
three_back_path <- file.path(save_path, "three_back_plot")
general_path <- file.path(save_path, "general_plot")
# make dirs if they do not exist
if (!dir.exists(save_path)) {
  dir.create(save_path, recursive = TRUE)
}
if (!dir.exists(workload_path)) {
  dir.create(workload_path, recursive = TRUE)
}
if (!dir.exists(three_back_path)) {
  dir.create(three_back_path, recursive = TRUE)
}
if (!dir.exists(general_path)) {
  dir.create(general_path, recursive = TRUE)
}

################
### Workload ###
################

# Summarize participant-level averages
workload_participant_summary <- workload_data %>%
  group_by(Participant, arithmatic) %>%
  summarise(mean_workload = mean(WorkloadSum, na.rm = TRUE), .groups = "drop")

# Summarize condition-level averages and standard error using base R
workload_condition_summary <- aggregate(mean_workload ~ arithmatic, data = workload_participant_summary, FUN = mean)
se_workload <- aggregate(mean_workload ~ arithmatic, data = workload_participant_summary, FUN = function(x) sd(x) / sqrt(length(x)))
workload_condition_summary$se_workload <- se_workload$mean_workload


# --- Plot workload by arithmatic ---
pw1 <- ggplot(workload_condition_summary, aes(x = arithmatic, y = mean_workload, fill = arithmatic)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = mean_workload - se_workload, ymax = mean_workload + se_workload), width = 0.2) +
  geom_point(data = workload_participant_summary, aes(x = arithmatic, y = mean_workload), 
             position = position_jitter(width = 0.15), shape = 21, fill = "black", color = "black", size = 2) +
  labs(
    title = "Mean Workload by Arithmatic",
    x = "Arithmatic",
    y = "Mean Workload"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  ylim(0, 60) 

pw1

# Save the plot
ggsave(filename = file.path(workload_path, "workload_simple_bar_plot.png"), plot = pw1, width = 8, height = 6, dpi = 300)



# --- plot workload by arithmatic with lines for each participant ---
pw2 <- ggplot(workload_participant_summary, aes(x = arithmatic, y = mean_workload, group = Participant)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Workload by Arithmatic with Participant Lines",
    x = "Arithmatic",
    y = "Mean Workload"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  ylim(0, 60)

pw2

# Save the plot
ggsave(filename = file.path(workload_path, "workload_participant_lines_plot.png"), plot = pw2, width = 8, height = 6, dpi = 300)


# --- combine above to types of plots ---
pw_combined <- ggplot(workload_condition_summary, aes(x = arithmatic, y = mean_workload, fill = arithmatic)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = mean_workload - se_workload, ymax = mean_workload + se_workload), width = 0.2) +
  geom_point(data = workload_participant_summary, aes(x = arithmatic, y = mean_workload), 
             shape = 21, fill = "black", color = "black", size = 2) +
  labs(
    title = "Mean Workload by Arithmatic with Participant Points",
    x = "Arithmatic",
    y = "Mean Workload"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  ylim(0, 60) +
  geom_line(data = workload_participant_summary, aes(group = Participant), color = "#5b5b5b", alpha = 0.5)

pw_combined

# Save the combined plot
ggsave(filename = file.path(workload_path, "workload_combined_plot.png"), plot = pw_combined, width = 8, height = 6, dpi = 300)


###############
### General ###
###############

# summarize data 
data_long_calculated_summary <- data_long_calculated %>%
  group_by(participant, haptic, visual, arithmatic) %>%
  summarise(
    mean_velocity = mean(average_velocity, na.rm = TRUE),
    mean_time = mean(time_through_maze, na.rm = TRUE),
    mean_force = mean(average_force, na.rm = TRUE),
    mean_proximity = mean(average_proximity, na.rm = TRUE),
    mean_path_length = mean(path_length, na.rm = TRUE),
    .groups = "drop"
  )

# Summarize condition-level velocity
velocity_condition_summary <- data_long_calculated_summary %>%
  group_by(haptic, visual, arithmatic) %>%
  summarise(
    mean_velocity = mean(mean_velocity, na.rm = TRUE),
    se_velocity = sd(mean_velocity, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# --- Velocity --- 

pv1 <- ggplot() +
  geom_col(
    data = velocity_condition_summary,
    aes(x = visual, y = mean_velocity, fill = haptic),
    position = position_dodge(width = 0.9),
    width = 0.7
  ) +
  geom_errorbar(
    data = velocity_condition_summary,
    aes(x = visual, y = mean_velocity, ymin = mean_velocity - se_velocity, ymax = mean_velocity + se_velocity, fill = haptic),
    position = position_dodge(width = 0.9),
    width = 0.2
  ) +
  geom_point(
    data = data_long_calculated_summary,
    aes(x = visual, y = mean_velocity, fill = haptic),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9),
    size = 2, alpha = 0.7, shape = 21,
    color = "black"
  ) +
  facet_wrap(~arithmatic) +
  labs(
    title = "Mean Velocity by Visual and Haptic",
    x = "Visual Condition",
    y = "Mean Velocity (cm/s)"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

pv1


# --- Velocity v2 ---


# --- 3-Way Interaction: Velocity by Task, Haptic, and Visual ---

# Rename for clarity
data_threeway_summary <- data_long_calculated_summary %>%
  rename(
    task = arithmatic,
    vision = visual
  )

# Summarize condition-level velocity
threeway_velocity_summary <- data_threeway_summary %>%
  group_by(task, haptic, vision) %>%
  summarise(
    mean_velocity = mean(mean_velocity, na.rm = TRUE),
    se_velocity = sd(mean_velocity, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Faceted line plot with participant jitter
pv3 <- ggplot() +
  geom_line(
    data = threeway_velocity_summary,
    aes(x = task, y = mean_velocity, color = vision, group = vision),
    position = position_dodge(width = 0.3),
    linewidth = 1
  ) +
  geom_errorbar(
    data = threeway_velocity_summary,
    aes(x = task, ymin = mean_velocity - se_velocity, ymax = mean_velocity + se_velocity, color = vision, group = vision),
    width = 0.15,
    position = position_dodge(width = 0.3)
  ) +
  geom_point(
    data = data_threeway_summary,
    aes(x = task, y = mean_velocity, color = vision),
    size = 1.8,
    alpha = 0.5,
    position = position_jitter(width = 0.2)
  ) +
  facet_wrap(~ haptic) +
  labs(
    title = "3-Way Interaction: Velocity by Task, Haptic, and Visual",
    x = "Task Condition",
    y = "Mean Velocity (cm/s)",
    color = "Visual"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

pv3

# Save the 3-way interaction plot
ggsave(filename = file.path(general_path, "velocity_3way_interaction_plot.png"), plot = pv3, width = 10, height = 6, dpi = 300)


# --- Violin-Box Plot: Velocity by Task, Visual, and Haptic ---

pv4 <- ggplot(data_threeway_summary, aes(x = task, y = mean_velocity, fill = vision)) +
  geom_violin(position = position_dodge(width = 0.8), alpha = 0.6, width = 0.5, trim = FALSE) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.02, outlier.shape = NA, color = "black") +
  facet_wrap(~ haptic) +
  labs(
    title = "Violin-Box Plot: Velocity by Task, Visual, and Haptic",
    x = "Task",
    y = "Mean Velocity (cm/s)",
    fill = "Visual"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

pv4

ggsave(filename = file.path(general_path, "velocity_violin_box_plot.png"), plot = pv4, width = 10, height = 6, dpi = 300)


# --- Bar Plot with Error Bars: Velocity by Task, Visual, and Haptic ---

pv5 <- ggplot(threeway_velocity_summary, aes(x = task, y = mean_velocity, fill = vision)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.6) +
  geom_errorbar(
    aes(ymin = mean_velocity - se_velocity, ymax = mean_velocity + se_velocity),
    width = 0.2,
    position = position_dodge(width = 0.8)
  ) +
  facet_wrap(~ haptic) +
  labs(
    title = "Bar Plot: Velocity by Task, Visual, and Haptic",
    x = "Task",
    y = "Mean Velocity (cm/s)",
    fill = "Visual"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

pv5

ggsave(filename = file.path(general_path, "velocity_bar_plot.png"), plot = pv5, width = 10, height = 6, dpi = 300)



# --- 

library(ggdist)

pv6 <- ggplot(data_threeway_summary, aes(x = task, y = mean_velocity, fill = vision)) +
  stat_halfeye(
    adjust = 0.5, width = 0.6, .width = 0, justification = -0.2,
    point_colour = NA, alpha = 0.6
  ) +
  geom_boxplot(
    width = 0.12, outlier.shape = NA, alpha = 0.8,
    position = position_nudge(x = 0.2)
  ) +
  geom_jitter(
    aes(color = vision),
    width = 0.1, size = 1.5, alpha = 0.5,
    position = position_nudge(x = 0.25)
  ) +
  facet_wrap(~ haptic) +
  labs(
    title = "Raincloud Plot: Velocity by Task, Visual, and Haptic",
    x = "Task",
    y = "Mean Velocity (cm/s)",
    fill = "Visual"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

pv6

ggsave(filename = file.path(general_path, "velocity_raincloud_plot.png"), plot = pv6, width = 10, height = 6, dpi = 300)


# ---

library(viridis)

pv7 <- ggplot(threeway_velocity_summary, aes(x = task, y = vision, fill = mean_velocity)) +
  geom_tile() +
  geom_text(aes(label = round(mean_velocity, 1)), color = "white") +
  facet_wrap(~ haptic) +
  scale_fill_viridis(option = "D", direction = -1) +
  labs(
    title = "Heatmap of Mean Velocity by Task, Vision, and Haptic",
    x = "Task",
    y = "Vision",
    fill = "Velocity"
  ) +
  theme_minimal()

pv7

ggsave(filename = file.path(general_path, "velocity_heatmap.png"), plot = pv7, width = 8, height = 5, dpi = 300)


# --- Participant Slopes Plot: Velocity by Task, Visual, and Haptic ---

pv8 <- ggplot(data_threeway_summary, aes(x = task, y = mean_velocity, group = participant, color = vision)) +
  geom_line(alpha = 0.4) +
  geom_point(size = 1.5) +
  facet_wrap(~ haptic) +
  labs(
    title = "Participant Slopes: Velocity by Task, Visual, and Haptic",
    x = "Task",
    y = "Mean Velocity (cm/s)",
    color = "Visual"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

pv8

ggsave(filename = file.path(general_path, "velocity_participant_slopes.png"), plot = pv8, width = 10, height = 6, dpi = 300)



# --- LME double check ---

library(lme4)
library(lmerTest)
library(emmeans)
library(sjPlot)

# Convert to factors (if not already)
data_threeway_summary <- data_threeway_summary %>%
  mutate(
    task = factor(task),
    haptic = factor(haptic),
    vision = factor(vision),
    participant = factor(participant)
  )

# Fit the model
lme_model <- lmer(mean_velocity ~ task * haptic * vision + (1 | participant), data = data_threeway_summary)

# Show ANOVA table with Type III tests
anova_output <- anova(lme_model, type = 3)
print(anova_output)

# Optional: Estimated marginal means and post-hoc
emms <- emmeans(lme_model, ~ task * haptic * vision)
pairs_output <- contrast(emms, "pairwise")
print(pairs_output)

tab_model(lme_model)





lme_model <- lmer(linear_velocity ~ arithmatic * haptic * visual + (1 | participant), data = data_long_calculated)

tab_model(lme_model)





library(dplyr)
library(lme4)
library(lmerTest)

# Aggregate to trial level
trial_level_data <- data_long_calculated %>%
  group_by(participant, trial, arithmatic, haptic, visual) %>%
  summarise(
    mean_velocity = mean(linear_velocity, na.rm = TRUE),
    .groups = "drop"
  )

# Convert predictors to factors
trial_level_data <- trial_level_data %>%
  mutate(
    participant = factor(participant),
    arithmatic = factor(arithmatic),
    haptic = factor(haptic),
    visual = factor(visual)
  )

# Fit the model at the trial level
lme_model_trial <- lmer(mean_velocity ~ arithmatic * haptic * visual + (1 | participant), data = trial_level_data)

tab_model(lme_model_trial)

# View ANOVA
anova(lme_model_trial, type = 3)

# Optional: check estimates
summary(lme_model_trial)




#############################
# Participant Slopes Plot (Trial-Level): Velocity by Task, Visual, and Haptic
#############################

# Prepare trial-level data for plotting
trial_level_data <- trial_level_data %>%
  rename(task = arithmatic, vision = visual)

pv9 <- ggplot(trial_level_data, aes(x = task, y = mean_velocity, group = participant, color = vision)) +
  geom_line(alpha = 0.3) +
  geom_point(size = 1.2) +
  facet_wrap(~ haptic) +
  labs(
    title = "Participant Slopes (Trial-Level): Velocity by Task, Visual, and Haptic",
    x = "Task",
    y = "Mean Velocity (cm/s)",
    color = "Visual"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

pv9

ggsave(filename = file.path(general_path, "velocity_participant_slopes_trial_level.png"), plot = pv9, width = 10, height = 6, dpi = 300)


# --- same as above but with boxplots as well

pv10 <- ggplot(trial_level_data, aes(x = task, y = mean_velocity)) +
  geom_boxplot(aes(fill = vision), outlier.size = 0.5, position = position_dodge(width = 0.75), width = 0.6, alpha = 0.5) +
  geom_line(aes(group = participant, color = vision), alpha = 0.3, position = position_dodge(width = 0.75)) +
  geom_point(aes(color = vision), size = 1.2, position = position_dodge(width = 0.75)) +
  facet_wrap(~ haptic) +
  labs(
    title = "Participant Boxplots (Trial-Level): Velocity by Task, Visual, and Haptic",
    x = "Task",
    y = "Mean Velocity (cm/s)",
    fill = "Visual",
    color = "Visual"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

pv10

ggsave(filename = file.path(general_path, "velocity_participant_boxplots_trial_level.png"), plot = pv10, width = 10, height = 6, dpi = 300)





# Rename for clarity
# trial_level_data_for_plot <- trial_level_data %>%
#   rename(task = arithmatic, vision = visual)

trial_level_data_for_plot <- trial_level_data

# Summarize condition-level velocity
threeway_velocity_summary <- trial_level_data_for_plot %>%
  group_by(task, haptic, vision) %>%
  summarise(
    mean_velocity = mean(mean_velocity, na.rm = TRUE),
    se_velocity = sd(mean_velocity, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Plot: 3-way interaction (trial-level)
pv3_v2 <- ggplot() +
  geom_line(
    data = threeway_velocity_summary,
    aes(x = task, y = mean_velocity, color = vision, group = vision),
    linewidth = 1,
    position = position_dodge(width = 0.3)
  ) +
  geom_errorbar(
    data = threeway_velocity_summary,
    aes(x = task, ymin = mean_velocity - se_velocity, ymax = mean_velocity + se_velocity, color = vision, group = vision),
    width = 0.15,
    position = position_dodge(width = 0.3)
  ) +
  geom_point(
    data = trial_level_data_for_plot,
    aes(x = task, y = mean_velocity, color = vision),
    alpha = 0.5,
    size = 1.8,
    position = position_jitter(width = 0.2)
  ) +
  facet_wrap(~ haptic) +
  labs(
    title = "3-Way Interaction (Trial-Level): Velocity by Task, Haptic, and Visual",
    x = "Task",
    y = "Mean Velocity (cm/s)",
    color = "Visual"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# Show or save the plot
pv3_v2

ggsave(filename = file.path(general_path, "velocity_3way_interaction_plot_v2.png"), plot = pv3, width = 10, height = 6, dpi = 300)



pv3_v3 <- ggplot() +
  geom_boxplot(
    data = trial_level_data_for_plot,
    aes(x = task, y = mean_velocity, fill = vision),
    alpha = 0.25,
    width = 0.4,
    position = position_dodge(width = 0.6),
    outlier.shape = NA
  ) +
  geom_line(
    data = threeway_velocity_summary,
    aes(x = task, y = mean_velocity, color = vision, group = vision),
    linewidth = 1,
    position = position_dodge(width = 0.3)
  ) +
  geom_errorbar(
    data = threeway_velocity_summary,
    aes(x = task, ymin = mean_velocity - se_velocity, ymax = mean_velocity + se_velocity, color = vision, group = vision),
    width = 0.15,
    position = position_dodge(width = 0.3)
  ) +
  geom_point(
    data = trial_level_data_for_plot,
    aes(x = task, y = mean_velocity, color = vision),
    alpha = 0.5,
    size = 1.8,
    position = position_jitter(width = 0.2)
  ) +
  facet_wrap(~ haptic) +
  labs(
    title = "3-Way Interaction (Trial-Level): Velocity by Task, Haptic, and Visual",
    x = "Task",
    y = "Mean Velocity",
    fill = "Visual",
    color = "Visual"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# Show or save the plot
pv3_v3

ggsave(filename = file.path(general_path, "velocity_3way_interaction_boxplot_plot_v4.png"), plot = pv3_v3, width = 10, height = 6, dpi = 300)





pv3_v4 <- ggplot() +
  geom_col(
    data = threeway_velocity_summary,
    aes(x = task, y = mean_velocity, fill = vision),
    position = position_dodge(width = 0.6),
    width = 0.5,
    alpha = 0.4
  ) +
  geom_errorbar(
    data = threeway_velocity_summary,
    aes(x = task, ymin = mean_velocity - se_velocity, ymax = mean_velocity + se_velocity, color = vision),
    position = position_dodge(width = 0.6),
    width = 0.2
  ) +
  geom_point(
    data = trial_level_data_for_plot,
    aes(x = task, y = mean_velocity, color = vision),
    size = 1.5,
    alpha = 0.5,
    position = position_jitter(width = 0.2)
  ) +
  facet_wrap(~ haptic) +
  labs(
    title = "3-Way Interaction: Velocity with Bars and Raw Data",
    x = "Task",
    y = "Mean Velocity (cm/s)",
    fill = "Visual",
    color = "Visual"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# Show or save the plot
pv3_v4

ggsave(filename = file.path(general_path, "velocity_3way_interaction_bar_plot_v2.png"), plot = pv3_v4, width = 10, height = 6, dpi = 300)





pv3_v4 <- ggplot() +
  geom_col(
    data = threeway_velocity_summary,
    aes(x = task, y = mean_velocity, fill = vision),
    position = position_dodge(width = 0.6),
    width = 0.5,
    alpha = 0.4
  ) +
  geom_errorbar(
    data = threeway_velocity_summary,
    aes(x = task, ymin = mean_velocity - se_velocity, ymax = mean_velocity + se_velocity, color = vision),
    position = position_dodge(width = 0.6),
    width = 0.2
  ) +
  geom_point(
    data = trial_level_data_for_plot,
    aes(x = task, y = mean_velocity, color = vision),
    size = 1.5,
    alpha = 0.5,
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.6)
  ) +
  facet_wrap(~ haptic) +
  labs(
    title = "3-Way Interaction: Velocity with Bars and Raw Data",
    x = "Task",
    y = "Mean Velocity (cm/s)",
    fill = "Visual",
    color = "Visual"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# Show or save the plot
pv3_v4

ggsave(filename = file.path(general_path, "velocity_3way_interaction_bar_plot_v3.png"), plot = pv3_v4, width = 10, height = 6, dpi = 300)










pv3_v3 <- ggplot() +
  geom_boxplot(
    data = trial_level_data_for_plot,
    aes(x = task, y = mean_velocity, fill = vision),
    alpha = 0.25,
    width = 0.4,
    position = position_dodge(width = 0.6),
    outlier.shape = NA
  ) +
  geom_line(
    data = threeway_velocity_summary,
    aes(x = task, y = mean_velocity, color = vision, group = vision),
    linewidth = 1,
    position = position_dodge(width = 0.3)
  ) +
  geom_errorbar(
    data = threeway_velocity_summary,
    aes(x = task, ymin = mean_velocity - se_velocity, ymax = mean_velocity + se_velocity, color = vision, group = vision),
    width = 0.15,
    position = position_dodge(width = 0.3)
  ) +
  geom_point(
    data = trial_level_data_for_plot,
    aes(x = task, y = mean_velocity, color = vision),
    size = 1.5,
    alpha = 0.5,
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.6)
  ) +
  facet_wrap(~ haptic) +
  labs(
    title = "3-Way Interaction (Trial-Level): Velocity by Task, Haptic, and Visual",
    x = "Task",
    y = "Mean Velocity",
    fill = "Visual",
    color = "Visual"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# Show or save the plot
pv3_v3

ggsave(filename = file.path(general_path, "velocity_3way_interaction_boxplot_plot_v4.png"), plot = pv3_v3, width = 10, height = 6, dpi = 300)




pv3_v3_task_facet <- ggplot() +
  geom_boxplot(
    data = trial_level_data_for_plot,
    aes(x = vision, y = mean_velocity, fill = vision),
    alpha = 0.25,
    width = 0.4,
    position = position_dodge(width = 0.6),
    outlier.shape = NA
  ) +
  geom_line(
    data = threeway_velocity_summary,
    aes(x = vision, y = mean_velocity, color = vision, group = vision),
    linewidth = 1,
    position = position_dodge(width = 0.3)
  ) +
  geom_errorbar(
    data = threeway_velocity_summary,
    aes(x = vision, ymin = mean_velocity - se_velocity, ymax = mean_velocity + se_velocity, color = vision, group = vision),
    width = 0.15,
    position = position_dodge(width = 0.3)
  ) +
  geom_point(
    data = trial_level_data_for_plot,
    aes(x = vision, y = mean_velocity, color = vision),
    size = 1.5,
    alpha = 0.5,
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.6)
  ) +
  facet_wrap(~ task) +
  labs(
    title = "3-Way Interaction (Trial-Level): Velocity by Vision, Haptic, and Task (Faceted by Task)",
    x = "Vision",
    y = "Mean Velocity",
    fill = "Visual",
    color = "Visual"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# Show or save the plot
pv3_v3_task_facet

ggsave(
  filename = file.path(general_path, "velocity_3way_interaction_boxplot_plot_task_facet.png"),
  plot = pv3_v3_task_facet,
  width = 10, height = 6, dpi = 300
)







library(dplyr)
library(tidyr)
library(ggplot2)

# Step 1: Recode and create grouping variable
trial_level_data_for_plot <- trial_level_data_for_plot %>%
  mutate(
    vision_label = ifelse(vision == 1, "present", "absent"),
    group = interaction(vision_label, haptic, sep = "_")
  )

threeway_velocity_summary <- threeway_velocity_summary %>%
  mutate(
    vision_label = ifelse(vision == 1, "present", "absent"),
    group = interaction(vision_label, haptic, sep = "_")
  )

# Step 2: Plot
## Add numeric x mapping for vision × haptic
group_levels <- sort(unique(threeway_velocity_summary$group))
group_mapping <- setNames(seq_along(group_levels), group_levels)

threeway_velocity_summary <- threeway_velocity_summary %>%
  mutate(
    group_numeric = group_mapping[group]
  )

segment_data <- threeway_velocity_summary %>%
  group_by(task) %>%
  arrange(group_numeric, .by_group = TRUE) %>%
  summarise(
    x_start = first(group_numeric),
    x_end = last(group_numeric),
    y_start = first(mean_velocity),
    y_end = last(mean_velocity),
    .groups = "drop"
  )

pv3_v3_task_facet <- ggplot() +
  geom_boxplot(
    data = trial_level_data_for_plot,
    aes(x = group, y = mean_velocity, fill = vision_label),
    alpha = 0.25,
    width = 0.5,
    outlier.shape = NA
  ) +
  # Add manual line segments connecting means per group within each facet
  geom_segment(
    data = segment_data,
    aes(x = x_start, xend = x_end, y = y_start, yend = y_end),
    inherit.aes = FALSE,
    color = "black",
    linewidth = 1
  ) +
  geom_errorbar(
    data = threeway_velocity_summary,
    aes(x = group, ymin = mean_velocity - se_velocity, ymax = mean_velocity + se_velocity, color = vision_label),
    width = 0.15
  ) +
  geom_point(
    data = trial_level_data_for_plot,
    aes(x = group, y = mean_velocity, color = vision_label),
    size = 1.5,
    alpha = 0.5,
    position = position_jitter(width = 0.15)
  ) +
  facet_wrap(~ task) +
  labs(
    title = "3-Way Interaction (Trial-Level): Velocity by Vision and Haptic (Faceted by Task)",
    x = "Vision × Haptic",
    y = "Mean Velocity (cm/s)",
    fill = "Visual",
    color = "Visual"
  ) +
  scale_x_discrete(limits = group_levels) +
  theme_minimal() +
  theme(legend.position = "top")

# Show or save the plot
pv3_v3_task_facet

ggsave(
  filename = file.path(general_path, "velocity_3way_interaction_boxplot_plot_task_facet.png"),
  plot = pv3_v3_task_facet,
  width = 10, height = 6, dpi = 300
)












trial_level_data_for_plot <- trial_level_data

# Summarize condition-level velocity
threeway_velocity_summary <- trial_level_data_for_plot %>%
  group_by(task, haptic, vision) %>%
  summarise(
    mean_velocity = mean(mean_velocity, na.rm = TRUE),
    se_velocity = sd(mean_velocity, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# relabel for clarity
threeway_velocity_summary <- threeway_velocity_summary %>%
  mutate(
    vision = ifelse(vision == 1, "degraded", "fair"),
    haptic = ifelse(haptic == 1, "present", "absent"),
    task = ifelse(task == 1, "dual", "single")
  )

trial_level_data_for_plot <- trial_level_data_for_plot %>%
  mutate(
    vision = ifelse(vision == 1, "degraded", "fair"),
    haptic = ifelse(haptic == 1, "present", "absent"),
    task = ifelse(task == 1, "dual", "single")
  )

# order vision as fair then degraded
trial_level_data_for_plot$vision <- factor(trial_level_data_for_plot$vision, levels = c("fair", "degraded"))
threeway_velocity_summary$vision <- factor(threeway_velocity_summary$vision, levels = c("fair", "degraded"))

pv3_v3 <- ggplot() +
  geom_boxplot(
    data = trial_level_data_for_plot,
    aes(x = vision, y = mean_velocity, fill = haptic),
    alpha = 0.25,
    width = 0.4,
    position = position_dodge(width = 0.6),
    outlier.shape = NA
  ) +
  geom_line(
    data = threeway_velocity_summary,
    aes(x = vision, y = mean_velocity, color = haptic, group = haptic),
    linewidth = 1,
    position = position_dodge(width = 0.6)
  ) +
  geom_errorbar(
    data = threeway_velocity_summary,
    aes(x = vision, ymin = mean_velocity - se_velocity, ymax = mean_velocity + se_velocity, color = haptic, group = haptic),
    width = 0.15,
    position = position_dodge(width = 0.3)
  ) +
  geom_point(
    data = trial_level_data_for_plot,
    aes(x = vision, y = mean_velocity, color = haptic),
    alpha = 0.5,
    size = 1.8,
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.6)
  ) +
  facet_wrap(~ task) +
  labs(
    title = "3-Way Interaction (Trial-Level): Velocity by Task, Haptic, and Visual",
    x = "Vision",
    y = "Mean Velocity",
    fill = "Haptic",
    color = "Haptic"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# Show or save the plot
pv3_v3

ggsave(filename = file.path(general_path, "_velocity_3way_interaction_boxplot_plot_v4.png"), plot = pv3_v3, width = 10, height = 6, dpi = 300)







# Create a new version with facet by haptic
pv3_v3_facet_by_haptic <- ggplot() +
  geom_boxplot(
    data = trial_level_data_for_plot,
    aes(x = task, y = mean_velocity, fill = vision),
    alpha = 0.25,
    width = 0.4,
    position = position_dodge(width = 0.6),
    outlier.shape = NA
  ) +
  geom_line(
    data = threeway_velocity_summary,
    aes(x = task, y = mean_velocity, color = vision, group = vision),
    linewidth = 1,
    position = position_dodge(width = 0.6)
  ) +
  geom_errorbar(
    data = threeway_velocity_summary,
    aes(x = task, ymin = mean_velocity - se_velocity, ymax = mean_velocity + se_velocity, color = vision, group = vision),
    width = 0.15,
    position = position_dodge(width = 0.3)
  ) +
  geom_point(
    data = trial_level_data_for_plot,
    aes(x = task, y = mean_velocity, color = vision),
    alpha = 0.5,
    size = 1.8,
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.6)
  ) +
  facet_wrap(~ haptic) +
  labs(
    title = "3-Way Interaction (Trial-Level): Velocity by Haptic, Task, and Visual",
    x = "Task",
    y = "Mean Velocity",
    fill = "Vision",
    color = "Vision"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# Show or save the plot
pv3_v3_facet_by_haptic

ggsave(
  filename = file.path(general_path, "_velocity_3way_interaction_boxplot_plot_facet_by_haptic.png"),
  plot = pv3_v3_facet_by_haptic,
  width = 10, height = 6, dpi = 300
)








# Create a new version with facet by haptic
pv3_v3_facet_by_haptic <- ggplot() +
  geom_boxplot(
    data = trial_level_data_for_plot,
    aes(x = haptic, y = mean_velocity, fill = vision),
    alpha = 0.25,
    width = 0.4,
    position = position_dodge(width = 0.6),
    outlier.shape = NA
  ) +
  geom_line(
    data = threeway_velocity_summary,
    aes(x = haptic, y = mean_velocity, color = vision, group = vision),
    linewidth = 1,
    position = position_dodge(width = 0.6)
  ) +
  geom_errorbar(
    data = threeway_velocity_summary,
    aes(x = haptic, ymin = mean_velocity - se_velocity, ymax = mean_velocity + se_velocity, color = vision, group = vision),
    width = 0.15,
    position = position_dodge(width = 0.3)
  ) +
  geom_point(
    data = trial_level_data_for_plot,
    aes(x = haptic, y = mean_velocity, color = vision),
    alpha = 0.5,
    size = 1.8,
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.6)
  ) +
  facet_wrap(~ task) +
  labs(
    title = "3-Way Interaction (Trial-Level): Velocity by Haptic, Task, and Visual",
    x = "Task",
    y = "Mean Velocity (cm/s)",
    fill = "Vision",
    color = "Vision"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# Show or save the plot
pv3_v3_facet_by_haptic

ggsave(
  filename = file.path(general_path, "_velocity_3way_interaction_boxplot_plot_facet_by_task_2.png"),
  plot = pv3_v3_facet_by_haptic,
  width = 10, height = 6, dpi = 300
)
