library(targets)
library(here)
library(dplyr)
library(ggplot2)
library(ggbeeswarm)
library(ggpubr)
library(patchwork)
library(stats)


# Global label variables for consistency across plots
label_tasktype <- "Task Type"
label_workload <- "Mean Workload"
label_velocity <- "Average Velocity"
label_haptic <- "Haptic"
label_visual <- "Visual Condition"
label_time <- "Time Through Maze"
label_force <- "Average Force"
label_proximity <- "Average Proximity"
label_pathlen <- "Path Length"
label_threeback_e_count <- "3-back E Count"
label_threeback_prop    <- "3-back Proportion Correct"

# ===== Plot control variables =====
# Text and theme
base_font_size        <- 14
title_face            <- "bold"
title_hjust           <- 0.5
title_size            <- 14
axis_title_size       <- 12
axis_text_size        <- 11
legend_title_size     <- 12
legend_text_size      <- 11
legend_pos_panel      <- "right"   # used inside single/facet panels
legend_pos_combined   <- "bottom"  # used after guides='collect'

# Geometries
dodge_width           <- 0.7
bar_width             <- 0.6
errorbar_width        <- 0.4
errorbar_linewidth    <- 0.8
point_size            <- 2
point_alpha           <- 0.3
jitter_width          <- 0.2

# Other layout tweaks
expand_limits_mult    <- 1.05  # multiplier for y expand on sig bars etc.

# Save/export
dpi_out               <- 300
# Per-figure defaults (override as needed)
width_workload        <- 6
height_workload       <- 4
width_combined        <- 8
height_combined       <- 8

# Level labels
tasktype_labels <- c("Single Task", "Dual Task")
visual_labels <- c("Fair", "Degraded")
haptic_labels <- c("Absent", "Present")


workload_data <- tar_read(workload_data)
three_back_data_with_composites <- tar_read(three_back_data_with_composites)
data_long_calculated <- tar_read(data_long_calculated)


date_folder <- format(Sys.Date(), "%Y-%m-%d")
save_root   <- here("Output", "Manuscript_Figures")
save_path   <- file.path(save_root, date_folder)
if (!dir.exists(save_path)) {
  dir.create(save_path, recursive = TRUE)
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
se_workload <- aggregate(mean_workload ~ arithmatic, data = workload_participant_summary,
                         FUN = function(x) stats::sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))))

workload_condition_summary$se_workload <- se_workload$mean_workload

# y-position for significance annotation (just above the highest mean+SE)
y_max <- max(workload_condition_summary$mean_workload + workload_condition_summary$se_workload, na.rm = TRUE)
sig_y <- y_max + 0.05 * y_max


# Create bar plot for workload by task type
workload_condition_summary$TaskType <- factor(workload_condition_summary$arithmatic,
                                              levels = c(0, 1),
                                              labels = tasktype_labels)

workload_plot <- ggplot(workload_condition_summary, aes(x = TaskType, y = mean_workload)) +
  geom_bar(stat = "identity", fill = "steelblue", width = bar_width) +
  geom_errorbar(aes(ymin = mean_workload - se_workload,
                    ymax = mean_workload + se_workload),
                width = errorbar_width,
                na.rm = TRUE) +
  # ggpubr::stat_compare_means(
  #   data = dplyr::mutate(workload_participant_summary,
  #                        TaskType = factor(arithmatic, levels = c(0, 1),
  #                                          labels = tasktype_labels)),
  #   aes(x = TaskType, y = mean_workload),
  #   method = "t.test",
  #   label = "p.signif",
  #   comparisons = list(c("Single Task", "Dual Task")),
  #   label.y = sig_y,
  #   inherit.aes = FALSE,
  #   size = 5,
  #   bracket.size = 0.7
  # ) +
  geom_point(
    data = workload_participant_summary,
    aes(x = factor(arithmatic, levels = c(0, 1), labels = tasktype_labels),
        y = mean_workload),
    position = position_jitter(width = jitter_width), size = point_size, alpha = point_alpha, color = "black"
  ) +
  labs(x = label_tasktype, y = label_workload) +
  theme_classic(base_size = base_font_size) +
  theme(
    axis.title  = element_text(size = axis_title_size),
    axis.text   = element_text(size = axis_text_size),
    legend.title= element_text(size = legend_title_size),
    legend.text = element_text(size = legend_text_size)
  ) +
  expand_limits(y = sig_y * expand_limits_mult)

# Save the plot
ggsave(filename = file.path(save_path, "workload_bar_plot.png"),
       plot = workload_plot, width = width_workload, height = height_workload, dpi = dpi_out)



########################
### Average Velocity ###
########################


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

# Condition-level summary (mean +/- SE) for average velocity
velocity_condition_summary <- data_long_calculated_summary %>%
  group_by(visual, haptic, arithmatic) %>%
  summarise(
    average_velocity = mean(mean_velocity, na.rm = TRUE),
    se_velocity = sd(mean_velocity, na.rm = TRUE) / sqrt(sum(!is.na(mean_velocity))),
    .groups = "drop"
  )

# relabel is as mean_velocity for clarity
velocity_condition_summary <- velocity_condition_summary %>%
  rename(mean_velocity = average_velocity)

# Factors for pretty labels and facets
velocity_condition_summary$TaskType <- factor(velocity_condition_summary$arithmatic,
                                              levels = c(0, 1),
                                              labels = tasktype_labels)
velocity_condition_summary$Visual <- factor(velocity_condition_summary$visual,
                                            levels = c(0, 1),
                                            labels = visual_labels)
velocity_condition_summary$Haptic <- factor(velocity_condition_summary$haptic,
                                            levels = c(0, 1),
                                            labels = haptic_labels)

data_long_calculated_summary$TaskType <- factor(data_long_calculated_summary$arithmatic,
                                                levels = c(0, 1),
                                                labels = tasktype_labels)
data_long_calculated_summary$Visual <- factor(data_long_calculated_summary$visual,
                                              levels = c(0, 1),
                                              labels = visual_labels)
data_long_calculated_summary$Haptic <- factor(data_long_calculated_summary$haptic,
                                              levels = c(0, 1),
                                              labels = haptic_labels)

# Two plots split by Visual, then combined
fair_summary <- velocity_condition_summary %>% filter(Visual == "Fair")
degraded_summary <- velocity_condition_summary %>% filter(Visual == "Degraded")

fair_points <- data_long_calculated_summary %>% filter(Visual == "Fair")
degraded_points <- data_long_calculated_summary %>% filter(Visual == "Degraded")

# Plot structure: x = TaskType, fill = Haptic
fair_plot <- ggplot(fair_summary, aes(x = TaskType, y = mean_velocity, fill = Haptic)) +
  geom_bar(stat = "identity", position = position_dodge(width = dodge_width), width = bar_width) +
  geom_errorbar(aes(ymin = mean_velocity - se_velocity,
                    ymax = mean_velocity + se_velocity,
                    group = Haptic),
                position = position_dodge(width = dodge_width),
                width = errorbar_width,
                linewidth = errorbar_linewidth,
                na.rm = TRUE) +
  geom_point(
    data = fair_points,
    aes(x = TaskType, y = mean_velocity, shape = Haptic),
    position = position_jitterdodge(jitter.width = jitter_width, dodge.width = dodge_width),
    size = point_size, alpha = point_alpha, color = "black", inherit.aes = FALSE
  ) +
  labs(title = "Visual: Fair", x = label_tasktype, y = label_velocity, fill = label_haptic, shape = label_haptic) +
  theme_classic(base_size = base_font_size) +
  theme(
    legend.position = legend_pos_panel,
    plot.title = element_text(face = title_face, hjust = title_hjust, size = title_size),
    axis.title  = element_text(size = axis_title_size),
    axis.text   = element_text(size = axis_text_size),
    legend.title= element_text(size = legend_title_size),
    legend.text = element_text(size = legend_text_size)
  )


degraded_plot <- ggplot(degraded_summary, aes(x = TaskType, y = mean_velocity, fill = Haptic)) +
  geom_bar(stat = "identity", position = position_dodge(width = dodge_width), width = bar_width) +
  geom_errorbar(aes(ymin = mean_velocity - se_velocity,
                    ymax = mean_velocity + se_velocity,
                    group = Haptic),
                position = position_dodge(width = dodge_width),
                width = errorbar_width,
                linewidth = errorbar_linewidth,
                na.rm = TRUE) +
  geom_point(
    data = degraded_points,
    aes(x = TaskType, y = mean_velocity, shape = Haptic),
    position = position_jitterdodge(jitter.width = jitter_width, dodge.width = dodge_width),
    size = point_size, alpha = point_alpha, color = "black", inherit.aes = FALSE
  ) +
  labs(title = "Visual: Degraded", x = label_tasktype, fill = label_haptic, shape = label_haptic, y = label_velocity) +
  theme_classic(base_size = base_font_size) +
  theme(
    legend.position = legend_pos_panel,
    plot.title = element_text(face = title_face, hjust = title_hjust, size = title_size),
    axis.title  = element_text(size = axis_title_size),
    axis.text   = element_text(size = axis_text_size),
    legend.title= element_text(size = legend_title_size),
    legend.text = element_text(size = legend_text_size)
  )

# Combine with shared legend
combined_velocity_plot <- fair_plot + degraded_plot +
  plot_layout(ncol = 2, guides = "collect") & theme(
    legend.position = legend_pos_combined,
    legend.title = element_text(size = legend_title_size),
    legend.text  = element_text(size = legend_text_size)
  )

# Save combined figure
ggsave(filename = file.path(save_path, "average_velocity_by_visual_combined.png"),
       plot = combined_velocity_plot, width = width_combined, height = height_combined, dpi = dpi_out)


######################
### Time (Maze)   ####
######################
# Condition-level summary (mean +/- SE) for time
time_condition_summary <- data_long_calculated_summary %>%
  group_by(visual, haptic, arithmatic) %>%
  summarise(
    average_time = mean(mean_time, na.rm = TRUE),
    se_time = stats::sd(mean_time, na.rm = TRUE) / sqrt(sum(!is.na(mean_time))),
    .groups = "drop"
  )
# relabel as mean_time for clarity
time_condition_summary <- time_condition_summary %>%
  rename(mean_time = average_time)

# Factors
time_condition_summary$TaskType <- factor(time_condition_summary$arithmatic, levels = c(0,1), labels = tasktype_labels)
time_condition_summary$Visual <- factor(time_condition_summary$visual, levels = c(0,1), labels = visual_labels)
time_condition_summary$Haptic <- factor(time_condition_summary$haptic, levels = c(0,1), labels = haptic_labels)

# Split summaries & points
fair_time_summary <- time_condition_summary %>% filter(Visual == "Fair")
degraded_time_summary <- time_condition_summary %>% filter(Visual == "Degraded")
fair_time_points <- data_long_calculated_summary %>% filter(Visual == "Fair")
degraded_time_points <- data_long_calculated_summary %>% filter(Visual == "Degraded")

# Plots
fair_time_plot <- ggplot(fair_time_summary, aes(x = TaskType, y = mean_time, fill = Haptic)) +
  geom_bar(stat = "identity", position = position_dodge(width = dodge_width), width = bar_width) +
  geom_errorbar(aes(ymin = mean_time - se_time,
                    ymax = mean_time + se_time,
                    group = Haptic),
                position = position_dodge(width = dodge_width),
                width = errorbar_width,
                linewidth = errorbar_linewidth,
                na.rm = TRUE) +
  geom_point(
    data = fair_time_points, aes(x = TaskType, y = mean_time, shape = Haptic),
    position = position_jitterdodge(jitter.width = jitter_width, dodge.width = dodge_width),
    size = point_size, alpha = point_alpha, color = "black", inherit.aes = FALSE
  ) +
  labs(title = "Visual: Fair", x = label_tasktype, y = label_time, fill = label_haptic, shape = label_haptic) +
  theme_classic(base_size = base_font_size) +
  theme(
    legend.position = legend_pos_panel,
    plot.title = element_text(face = title_face, hjust = title_hjust, size = title_size),
    axis.title  = element_text(size = axis_title_size),
    axis.text   = element_text(size = axis_text_size),
    legend.title= element_text(size = legend_title_size),
    legend.text = element_text(size = legend_text_size)
  )

degraded_time_plot <- ggplot(degraded_time_summary, aes(x = TaskType, y = mean_time, fill = Haptic)) +
  geom_bar(stat = "identity", position = position_dodge(width = dodge_width), width = bar_width) +
  geom_errorbar(aes(ymin = mean_time - se_time,
                    ymax = mean_time + se_time,
                    group = Haptic),
                position = position_dodge(width = dodge_width),
                width = errorbar_width,
                linewidth = errorbar_linewidth,
                na.rm = TRUE) +
  geom_point(
    data = degraded_time_points, aes(x = TaskType, y = mean_time, shape = Haptic),
    position = position_jitterdodge(jitter.width = jitter_width, dodge.width = dodge_width),
    size = point_size, alpha = point_alpha, color = "black", inherit.aes = FALSE
  ) +
  labs(title = "Visual: Degraded", x = label_tasktype, y = label_time, fill = label_haptic, shape = label_haptic) +
  theme_classic(base_size = base_font_size) +
  theme(
    legend.position = legend_pos_panel,
    plot.title = element_text(face = title_face, hjust = title_hjust, size = title_size),
    axis.title  = element_text(size = axis_title_size),
    axis.text   = element_text(size = axis_text_size),
    legend.title= element_text(size = legend_title_size),
    legend.text = element_text(size = legend_text_size)
  )

combined_time_plot <- fair_time_plot + degraded_time_plot + plot_layout(ncol = 2, guides = "collect") & theme(
  legend.position = legend_pos_combined,
  legend.title = element_text(size = legend_title_size),
  legend.text  = element_text(size = legend_text_size)
)

ggsave(filename = file.path(save_path, "time_by_visual_combined.png"), plot = combined_time_plot, width = width_combined, height = height_combined, dpi = dpi_out)


######################
### Force          ####
######################
force_condition_summary <- data_long_calculated_summary %>%
  group_by(visual, haptic, arithmatic) %>%
  summarise(
    average_force = mean(mean_force, na.rm = TRUE),
    se_force = stats::sd(mean_force, na.rm = TRUE) / sqrt(sum(!is.na(mean_force))),
    .groups = "drop"
  )
# relabel as mean_force for clarity
force_condition_summary <- force_condition_summary %>%
  rename(mean_force = average_force) 

force_condition_summary$TaskType <- factor(force_condition_summary$arithmatic, levels = c(0,1), labels = tasktype_labels)
force_condition_summary$Visual <- factor(force_condition_summary$visual, levels = c(0,1), labels = visual_labels)
force_condition_summary$Haptic <- factor(force_condition_summary$haptic, levels = c(0,1), labels = haptic_labels)

fair_force_summary <- force_condition_summary %>% filter(Visual == "Fair")
degraded_force_summary <- force_condition_summary %>% filter(Visual == "Degraded")
fair_force_points <- data_long_calculated_summary %>% filter(Visual == "Fair")
degraded_force_points <- data_long_calculated_summary %>% filter(Visual == "Degraded")

fair_force_plot <- ggplot(fair_force_summary, aes(x = TaskType, y = mean_force, fill = Haptic)) +
  geom_bar(stat = "identity", position = position_dodge(width = dodge_width), width = bar_width) +
  geom_errorbar(aes(ymin = mean_force - se_force,
                    ymax = mean_force + se_force,
                    group = Haptic),
                position = position_dodge(width = dodge_width),
                width = errorbar_width,
                linewidth = errorbar_linewidth,
                na.rm = TRUE) +
  geom_point(
    data = fair_force_points, aes(x = TaskType, y = mean_force, shape = Haptic),
    position = position_jitterdodge(jitter.width = jitter_width, dodge.width = dodge_width),
    size = point_size, alpha = point_alpha, color = "black", inherit.aes = FALSE
  ) +
  labs(title = "Visual: Fair", x = label_tasktype, y = label_force, fill = label_haptic, shape = label_haptic) +
  theme_classic(base_size = base_font_size) +
  theme(
    legend.position = legend_pos_panel,
    plot.title = element_text(face = title_face, hjust = title_hjust, size = title_size),
    axis.title  = element_text(size = axis_title_size),
    axis.text   = element_text(size = axis_text_size),
    legend.title= element_text(size = legend_title_size),
    legend.text = element_text(size = legend_text_size)
  )

degraded_force_plot <- ggplot(degraded_force_summary, aes(x = TaskType, y = mean_force, fill = Haptic)) +
  geom_bar(stat = "identity", position = position_dodge(width = dodge_width), width = bar_width) +
  geom_errorbar(aes(ymin = mean_force - se_force,
                    ymax = mean_force + se_force,
                    group = Haptic),
                position = position_dodge(width = dodge_width),
                width = errorbar_width,
                linewidth = errorbar_linewidth,
                na.rm = TRUE) +
  geom_point(
    data = degraded_force_points, aes(x = TaskType, y = mean_force, shape = Haptic),
    position = position_jitterdodge(jitter.width = jitter_width, dodge.width = dodge_width),
    size = point_size, alpha = point_alpha, color = "black", inherit.aes = FALSE
  ) +
  labs(title = "Visual: Degraded", x = label_tasktype, y = label_force, fill = label_haptic, shape = label_haptic) +
  theme_classic(base_size = base_font_size) +
  theme(
    legend.position = legend_pos_panel,
    plot.title = element_text(face = title_face, hjust = title_hjust, size = title_size),
    axis.title  = element_text(size = axis_title_size),
    axis.text   = element_text(size = axis_text_size),
    legend.title= element_text(size = legend_title_size),
    legend.text = element_text(size = legend_text_size)
  )

combined_force_plot <- fair_force_plot + degraded_force_plot + plot_layout(ncol = 2, guides = "collect") & theme(
  legend.position = legend_pos_combined,
  legend.title = element_text(size = legend_title_size),
  legend.text  = element_text(size = legend_text_size)
)

ggsave(filename = file.path(save_path, "force_by_visual_combined.png"), plot = combined_force_plot, width = width_combined, height = height_combined, dpi = dpi_out)


######################
### Proximity      ####
######################
proximity_condition_summary <- data_long_calculated_summary %>%
  group_by(visual, haptic, arithmatic) %>%
  summarise(
    average_proximity = mean(mean_proximity, na.rm = TRUE),
    se_proximity = stats::sd(mean_proximity, na.rm = TRUE) / sqrt(sum(!is.na(mean_proximity))),
    .groups = "drop"
  )
# relabel as mean_proximity for clarity
proximity_condition_summary <- proximity_condition_summary %>%
  rename(mean_proximity = average_proximity)

proximity_condition_summary$TaskType <- factor(proximity_condition_summary$arithmatic, levels = c(0,1), labels = tasktype_labels)
proximity_condition_summary$Visual <- factor(proximity_condition_summary$visual, levels = c(0,1), labels = visual_labels)
proximity_condition_summary$Haptic <- factor(proximity_condition_summary$haptic, levels = c(0,1), labels = haptic_labels)

fair_prox_summary <- proximity_condition_summary %>% filter(Visual == "Fair")
degraded_prox_summary <- proximity_condition_summary %>% filter(Visual == "Degraded")
fair_prox_points <- data_long_calculated_summary %>% filter(Visual == "Fair")
degraded_prox_points <- data_long_calculated_summary %>% filter(Visual == "Degraded")

fair_prox_plot <- ggplot(fair_prox_summary, aes(x = TaskType, y = mean_proximity, fill = Haptic)) +
  geom_bar(stat = "identity", position = position_dodge(width = dodge_width), width = bar_width) +
  geom_errorbar(aes(ymin = mean_proximity - se_proximity,
                    ymax = mean_proximity + se_proximity,
                    group = Haptic),
                position = position_dodge(width = dodge_width),
                width = errorbar_width,
                linewidth = errorbar_linewidth,
                na.rm = TRUE) +
  geom_point(
    data = fair_prox_points, aes(x = TaskType, y = mean_proximity, shape = Haptic),
    position = position_jitterdodge(jitter.width = jitter_width, dodge.width = dodge_width),
    size = point_size, alpha = point_alpha, color = "black", inherit.aes = FALSE
  ) +
  labs(title = "Visual: Fair", x = label_tasktype, y = label_proximity, fill = label_haptic, shape = label_haptic) +
  theme_classic(base_size = base_font_size) +
  theme(
    legend.position = legend_pos_panel,
    plot.title = element_text(face = title_face, hjust = title_hjust, size = title_size),
    axis.title  = element_text(size = axis_title_size),
    axis.text   = element_text(size = axis_text_size),
    legend.title= element_text(size = legend_title_size),
    legend.text = element_text(size = legend_text_size)
  )

degraded_prox_plot <- ggplot(degraded_prox_summary, aes(x = TaskType, y = mean_proximity, fill = Haptic)) +
  geom_bar(stat = "identity", position = position_dodge(width = dodge_width), width = bar_width) +
  geom_errorbar(aes(ymin = mean_proximity - se_proximity,
                    ymax = mean_proximity + se_proximity,
                    group = Haptic),
                position = position_dodge(width = dodge_width),
                width = errorbar_width,
                linewidth = errorbar_linewidth,
                na.rm = TRUE) +
  geom_point(
    data = degraded_prox_points, aes(x = TaskType, y = mean_proximity, shape = Haptic),
    position = position_jitterdodge(jitter.width = jitter_width, dodge.width = dodge_width),
    size = point_size, alpha = point_alpha, color = "black", inherit.aes = FALSE
  ) +
  labs(title = "Visual: Degraded", x = label_tasktype, y = label_proximity, fill = label_haptic, shape = label_haptic) +
  theme_classic(base_size = base_font_size) +
  theme(
    legend.position = legend_pos_panel,
    plot.title = element_text(face = title_face, hjust = title_hjust, size = title_size),
    axis.title  = element_text(size = axis_title_size),
    axis.text   = element_text(size = axis_text_size),
    legend.title= element_text(size = legend_title_size),
    legend.text = element_text(size = legend_text_size)
  )

combined_prox_plot <- fair_prox_plot + degraded_prox_plot + plot_layout(ncol = 2, guides = "collect") & theme(
  legend.position = legend_pos_combined,
  legend.title = element_text(size = legend_title_size),
  legend.text  = element_text(size = legend_text_size)
)

ggsave(filename = file.path(save_path, "proximity_by_visual_combined.png"), plot = combined_prox_plot, width = width_combined, height = height_combined, dpi = dpi_out)


######################
### Path Length    ####
######################
pathlen_condition_summary <- data_long_calculated_summary %>%
  group_by(visual, haptic, arithmatic) %>%
  summarise(
    average_path_length = mean(mean_path_length, na.rm = TRUE),
    se_path_length = stats::sd(mean_path_length, na.rm = TRUE) / sqrt(sum(!is.na(mean_path_length))),
    .groups = "drop"
  )
# relabel as mean_path_length for clarity
pathlen_condition_summary <- pathlen_condition_summary %>%
  rename(mean_path_length = average_path_length)

pathlen_condition_summary$TaskType <- factor(pathlen_condition_summary$arithmatic, levels = c(0,1), labels = tasktype_labels)
pathlen_condition_summary$Visual <- factor(pathlen_condition_summary$visual, levels = c(0,1), labels = visual_labels)
pathlen_condition_summary$Haptic <- factor(pathlen_condition_summary$haptic, levels = c(0,1), labels = haptic_labels)

fair_pathlen_summary <- pathlen_condition_summary %>% filter(Visual == "Fair")
degraded_pathlen_summary <- pathlen_condition_summary %>% filter(Visual == "Degraded")
fair_pathlen_points <- data_long_calculated_summary %>% filter(Visual == "Fair")
degraded_pathlen_points <- data_long_calculated_summary %>% filter(Visual == "Degraded")

fair_pathlen_plot <- ggplot(fair_pathlen_summary, aes(x = TaskType, y = mean_path_length, fill = Haptic)) +
  geom_bar(stat = "identity", position = position_dodge(width = dodge_width), width = bar_width) +
  geom_errorbar(aes(ymin = mean_path_length - se_path_length,
                    ymax = mean_path_length + se_path_length,
                    group = Haptic),
                position = position_dodge(width = dodge_width),
                width = errorbar_width,
                linewidth = errorbar_linewidth,
                na.rm = TRUE) +
  geom_point(
    data = fair_pathlen_points, aes(x = TaskType, y = mean_path_length, shape = Haptic),
    position = position_jitterdodge(jitter.width = jitter_width, dodge.width = dodge_width),
    size = point_size, alpha = point_alpha, color = "black", inherit.aes = FALSE
  ) +
  labs(title = "Visual: Fair", x = label_tasktype, y = label_pathlen, fill = label_haptic, shape = label_haptic) +
  theme_classic(base_size = base_font_size) +
  theme(
    legend.position = legend_pos_panel,
    plot.title = element_text(face = title_face, hjust = title_hjust, size = title_size),
    axis.title  = element_text(size = axis_title_size),
    axis.text   = element_text(size = axis_text_size),
    legend.title= element_text(size = legend_title_size),
    legend.text = element_text(size = legend_text_size)
  )

degraded_pathlen_plot <- ggplot(degraded_pathlen_summary, aes(x = TaskType, y = mean_path_length, fill = Haptic)) +
  geom_bar(stat = "identity", position = position_dodge(width = dodge_width), width = bar_width) +
  geom_errorbar(aes(ymin = mean_path_length - se_path_length,
                    ymax = mean_path_length + se_path_length,
                    group = Haptic),
                position = position_dodge(width = dodge_width),
                width = errorbar_width,
                linewidth = errorbar_linewidth,
                na.rm = TRUE) +
  geom_point(
    data = degraded_pathlen_points, aes(x = TaskType, y = mean_path_length, shape = Haptic),
    position = position_jitterdodge(jitter.width = jitter_width, dodge.width = dodge_width),
    size = point_size, alpha = point_alpha, color = "black", inherit.aes = FALSE
  ) +
  labs(title = "Visual: Degraded", x = label_tasktype, y = label_pathlen, fill = label_haptic, shape = label_haptic) +
  theme_classic(base_size = base_font_size) +
  theme(
    legend.position = legend_pos_panel,
    plot.title = element_text(face = title_face, hjust = title_hjust, size = title_size),
    axis.title  = element_text(size = axis_title_size),
    axis.text   = element_text(size = axis_text_size),
    legend.title= element_text(size = legend_title_size),
    legend.text = element_text(size = legend_text_size)
  )

combined_pathlen_plot <- fair_pathlen_plot + degraded_pathlen_plot + plot_layout(ncol = 2, guides = "collect") & theme(
  legend.position = legend_pos_combined,
  legend.title = element_text(size = legend_title_size),
  legend.text  = element_text(size = legend_text_size)
)


ggsave(filename = file.path(save_path, "path_length_by_visual_combined.png"), plot = combined_pathlen_plot, width = width_combined, height = height_combined, dpi = dpi_out)


#############################
### 3-back (Dual-task)    ###
#############################
# Filter to dual-task only (arithmatic == 1)
threeback_dual <- three_back_data_with_composites %>%
  dplyr::filter(arithmatic == 1)

# Resolve the proportion-correct column name robustly
prop_col <- if ("proportion_correct" %in% names(threeback_dual)) {
  "proportion_correct"
} else if ("proportion correct" %in% names(threeback_dual)) {
  "proportion correct"
} else {
  NA_character_
}

# Participant-level summary (per Visual x Haptic)
threeback_participant_summary <- threeback_dual %>%
  dplyr::group_by(participant, haptic, visual) %>%
  dplyr::summarise(
    mean_e_count       = mean(e_count, na.rm = TRUE),
    mean_prop_correct  = if (!is.na(prop_col)) mean(.data[[prop_col]], na.rm = TRUE) else NA_real_,
    .groups = "drop"
  )

# Condition-level summaries (mean +/- SE)
e_count_condition_summary <- threeback_participant_summary %>%
  dplyr::group_by(visual, haptic) %>%
  dplyr::summarise(
    average_e_count = mean(mean_e_count, na.rm = TRUE),
    se_e_count   = stats::sd(mean_e_count, na.rm = TRUE) / sqrt(sum(!is.na(mean_e_count))),
    .groups = "drop"
  )
# relabel as mean_e_count for clarity
e_count_condition_summary <- e_count_condition_summary %>%
  dplyr::rename(mean_e_count = average_e_count)

prop_condition_summary <- threeback_participant_summary %>%
  dplyr::group_by(visual, haptic) %>%
  dplyr::summarise(
    average_prop_correct = mean(mean_prop_correct, na.rm = TRUE),
    se_prop_correct   = stats::sd(mean_prop_correct, na.rm = TRUE) / sqrt(sum(!is.na(mean_prop_correct))),
    .groups = "drop"
  )
# relabel as mean_prop_correct for clarity
prop_condition_summary <- prop_condition_summary %>%
  dplyr::rename(mean_prop_correct = average_prop_correct)

# Factor labels for pretty facets
e_count_condition_summary$Visual <- factor(e_count_condition_summary$visual, levels = c(0,1), labels = visual_labels)
e_count_condition_summary$Haptic <- factor(e_count_condition_summary$haptic, levels = c(0,1), labels = haptic_labels)
prop_condition_summary$Visual    <- factor(prop_condition_summary$visual,    levels = c(0,1), labels = visual_labels)
prop_condition_summary$Haptic    <- factor(prop_condition_summary$haptic,    levels = c(0,1), labels = haptic_labels)

threeback_participant_summary$Visual <- factor(threeback_participant_summary$visual, levels = c(0,1), labels = visual_labels)
threeback_participant_summary$Haptic <- factor(threeback_participant_summary$haptic, levels = c(0,1), labels = haptic_labels)

# ===== E COUNT PLOTS =====
fair_ec_summary     <- e_count_condition_summary %>% dplyr::filter(Visual == "Fair")
degraded_ec_summary <- e_count_condition_summary %>% dplyr::filter(Visual == "Degraded")

fair_ec_points      <- threeback_participant_summary %>% dplyr::filter(Visual == "Fair")
degraded_ec_points  <- threeback_participant_summary %>% dplyr::filter(Visual == "Degraded")

fair_ec_plot <- ggplot(fair_ec_summary, aes(x = Haptic, y = mean_e_count)) +
  geom_bar(stat = "identity", width = bar_width, fill = "steelblue") +
  geom_errorbar(aes(ymin = mean_e_count - se_e_count,
                    ymax = mean_e_count + se_e_count),
                width = errorbar_width,
                linewidth = errorbar_linewidth,
                na.rm = TRUE) +
  geom_point(
    data = fair_ec_points,
    aes(x = Haptic, y = mean_e_count),
    position = position_jitter(width = jitter_width),
    size = point_size, alpha = point_alpha, color = "black",
    inherit.aes = FALSE
  ) +
  labs(title = "Visual: Fair", x = label_haptic, y = label_threeback_e_count) +
  theme_classic(base_size = base_font_size) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = title_face, hjust = title_hjust, size = title_size),
    axis.title  = element_text(size = axis_title_size),
    axis.text   = element_text(size = axis_text_size)
  )


degraded_ec_plot <- ggplot(degraded_ec_summary, aes(x = Haptic, y = mean_e_count)) +
  geom_bar(stat = "identity", width = bar_width, fill = "steelblue") +
  geom_errorbar(aes(ymin = mean_e_count - se_e_count,
                    ymax = mean_e_count + se_e_count),
                width = errorbar_width,
                linewidth = errorbar_linewidth,
                na.rm = TRUE) +
  geom_point(
    data = degraded_ec_points,
    aes(x = Haptic, y = mean_e_count),
    position = position_jitter(width = jitter_width),
    size = point_size, alpha = point_alpha, color = "black",
    inherit.aes = FALSE
  ) +
  labs(title = "Visual: Degraded", x = label_haptic, y = label_threeback_e_count) +
  theme_classic(base_size = base_font_size) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = title_face, hjust = title_hjust, size = title_size),
    axis.title  = element_text(size = axis_title_size),
    axis.text   = element_text(size = axis_text_size)
  )

combined_ec_plot <- fair_ec_plot + degraded_ec_plot +
  plot_layout(ncol = 2)

ggsave(filename = file.path(save_path, "threeback_e_count_by_visual.png"),
       plot = combined_ec_plot, width = width_combined, height = height_combined, dpi = dpi_out)

# ===== PROPORTION CORRECT PLOTS =====
# If the proportion-correct column was not found, skip plotting with a warning
if (!is.na(prop_col)) {
  fair_pc_summary     <- prop_condition_summary %>% dplyr::filter(Visual == "Fair")
  degraded_pc_summary <- prop_condition_summary %>% dplyr::filter(Visual == "Degraded")

  fair_pc_points     <- threeback_participant_summary %>% dplyr::filter(Visual == "Fair")
  degraded_pc_points <- threeback_participant_summary %>% dplyr::filter(Visual == "Degraded")

  fair_pc_plot <- ggplot(fair_pc_summary, aes(x = Haptic, y = mean_prop_correct)) +
    geom_bar(stat = "identity", width = bar_width, fill = "steelblue") +
    geom_errorbar(aes(ymin = mean_prop_correct - se_prop_correct,
                      ymax = mean_prop_correct + se_prop_correct),
                  width = errorbar_width,
                  linewidth = errorbar_linewidth,
                  na.rm = TRUE) +
    geom_point(
      data = fair_pc_points,
      aes(x = Haptic, y = mean_prop_correct),
      position = position_jitter(width = jitter_width),
      size = point_size, alpha = point_alpha, color = "black",
      inherit.aes = FALSE
    ) +
    labs(title = "Visual: Fair", x = label_haptic, y = label_threeback_prop) +
    theme_classic(base_size = base_font_size) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = title_face, hjust = title_hjust, size = title_size),
      axis.title  = element_text(size = axis_title_size),
      axis.text   = element_text(size = axis_text_size)
    )

  degraded_pc_plot <- ggplot(degraded_pc_summary, aes(x = Haptic, y = mean_prop_correct)) +
    geom_bar(stat = "identity", width = bar_width, fill = "steelblue") +
    geom_errorbar(aes(ymin = mean_prop_correct - se_prop_correct,
                      ymax = mean_prop_correct + se_prop_correct),
                  width = errorbar_width,
                  linewidth = errorbar_linewidth,
                  na.rm = TRUE) +
    geom_point(
      data = degraded_pc_points,
      aes(x = Haptic, y = mean_prop_correct),
      position = position_jitter(width = jitter_width),
      size = point_size, alpha = point_alpha, color = "black",
      inherit.aes = FALSE
    ) +
    labs(title = "Visual: Degraded", x = label_haptic, y = label_threeback_prop) +
    theme_classic(base_size = base_font_size) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = title_face, hjust = title_hjust, size = title_size),
      axis.title  = element_text(size = axis_title_size),
      axis.text   = element_text(size = axis_text_size)
    )

  combined_pc_plot <- fair_pc_plot + degraded_pc_plot +
    plot_layout(ncol = 2)

  ggsave(filename = file.path(save_path, "threeback_prop_correct_by_visual.png"),
         plot = combined_pc_plot, width = width_combined, height = height_combined, dpi = dpi_out)
} else {
  warning("Skipping 3-back proportion-correct plots: no 'proportion_correct' or 'proportion correct' column found.")
}
