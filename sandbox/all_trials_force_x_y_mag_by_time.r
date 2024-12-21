# all_trials_force_x_y_mag_by_time

data <- tar_read(possible_fighting_flagged_data)

# save path
save_path <- here("sandbox", "all_trials_force_x_y_mag_by_time")

for (p_num in unique(data$participant)) {
  print(paste("Plotting participant", p_num))
  p_data <- data %>% filter(participant == p_num)

  for (t_num in unique(p_data$trial)) {
    print(paste("Plotting trial", t_num))
    t_data <- p_data %>% filter(trial == t_num)

    # plot the force x, y, and magnitude by time
    g <- ggplot(t_data, aes(x = time, y = force_x, color = "force_x")) +
      geom_line() +
      geom_line(aes(x = time, y = force_y, color = "force_y")) +
      geom_line(aes(x = time, y = force_magnitude, color = "force_mag")) +
      labs(title = paste("Participant", p_num, "Trial", t_num, "Force x, y, and magnitude by time"),
           x = "Time (s)", y = "Force (N)", color = "Force component") +
      scale_color_manual(values = c("force_x" = "red", "force_y" = "blue", "force_mag" = "black")) +
      theme_minimal() +
      theme(legend.position = "top")
      
    # save the plo
    ggsave(filename = here(save_path, paste("_P", p_num, "_T", t_num, ".png")), plot = g)

  } 
}
