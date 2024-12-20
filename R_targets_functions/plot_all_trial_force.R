# plot_all_trial_force

#data <- data

plot_all_trial_force <- function (data, maps) {

  for (p_num in unique(data$participant)) {
    
    print(paste("Plotting participant", p_num))
    participant_data <- data[data$participant == p_num,]
    
    for (t_num in unique(participant_data$trial)) {
      
      print(paste("Plotting trial", t_num))
      trial_data <- participant_data[participant_data$trial == t_num,]
      
      # filter for fighting
      fighting_data <- trial_data %>%
        filter(fighting == 1)
      
      base_plot <- ggplot() +
        ggtitle(paste("Participant", p_num, "Trial", t_num)) +
        theme_classic() +
        xlim(-10, 10) +
        ylim(-10, 10)
      
      # fighting layer
      g0 <- geom_point(data = fighting_data, aes(x = position_x_robot, y = position_y_robot), size = 5, colour = "black")
      
      # all data layer
      g1 <- geom_point(data = trial_data, aes(x = position_x_robot, y = position_y_robot, colour = force_magnitude))
      
      # map data layer
      map_num <- unique(trial_data$map)
      map <- maps[[paste0('env', map_num)]]
      rotation <- unique(trial_data$rotation)
      # apply rotation around 0, 0
      map <- map %>%
        mutate(
          rotation_radians = rotation * pi / 180,  # Convert degrees to radians
          V1_new = V1 * cos(rotation_radians) - V2 * sin(rotation_radians),  # New x-coordinate
          V2_new = V1 * sin(rotation_radians) + V2 * cos(rotation_radians)  # New y-coordinate
        )
      g2 <- geom_point(data = map, aes(x = V1_new, y = V2_new))
      
      # start location layer
      default <- data.frame(x = 0, y = 9)
      # apply rotation
      default <- default %>%
        mutate(
          rotation_radians = rotation * pi / 180,  # Convert degrees to radians
          x_new = x * cos(rotation_radians) - y * sin(rotation_radians),  # New x-coordinate
          y_new = x * sin(rotation_radians) + y * cos(rotation_radians)  # New y-coordinate
        )
      g3 <- geom_point(data = default, aes(x = x_new, y = y_new), size = 15, colour = "green", fill = 'green', shape = 22)
      
      # # add haptic and visual label
      # haptic <- unique(trial_data$haptic)
      # visual <- unique(trial_data$visual)
      # g4 <- annotate("text", x = -7, y = 10, label = paste("Haptic:", haptic, "Visual:", visual), size = 5)
      # # add it as legend
      # g4 <- geom_text(aes(x = -7, y = 10, label = paste("Haptic:", haptic, "Visual:", visual)), size = 5)
      # 
      # g4 <- grid.text(label = paste("Haptic:", haptic, "Visual:", visual), x = 0.9, y = 0.1, just = c("right", "bottom"))
      # 
      
      
      final_plot <- base_plot + g0 + g1 + g2 + g3 +
        scale_colour_gradient(low = "blue", high = "red", limits = c(0, 2.83))
      
      # add haptic and visual label
      haptic <- unique(trial_data$haptic)
      visual <- unique(trial_data$visual)
      final_plot <- ggdraw(final_plot) +
        draw_label(paste("Haptic:", haptic, "Visual:", visual), x = 0.9, y=0.9, hjust = 1)
      
      print(final_plot)
      
      # g0 <- ggplot(fighting_data, aes(x = position_x_robot, y = position_y_robot)) +
      #   geom_point(size = 2) +
      #   ggtitle(paste("Participant", p_num, "Trial", t_num)) +
      #   theme_classic() +
      #   xlim(-10, 10) +
      #   ylim(-10, 10)
      # print(g0)
      # 
      # g1 <- ggplot(trial_data, aes(x = position_x_robot, y = position_y_robot, colour = force_magnitude)) +
      #   geom_point() +
      #   scale_colour_gradient(low = "blue", high = "red", limits = c(0, 2.5)) +
      #   ggtitle(paste("Participant", p_num, "Trial", t_num)) +
      #   theme_classic() +
      #   xlim(-10, 10) +
      #   ylim(-10, 10)
      # print(g1)
      # 
      # g3 <- g0 + g1
      # print(g3)
      
      ggsave(here('output', 'figures_force', paste("participant", p_num), paste("_trial", t_num, ".png")), plot = final_plot, device = "png")
      
    }
    
  }

}