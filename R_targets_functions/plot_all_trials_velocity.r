# plot_all_trials_velocity

# library(tidyverse)
# library(here)
# library(targets)

# data <- tar_read(possible_fighting_flagged_data)
# maps <- tar_read(maps)

plot_all_trials_velocity <- function(data, maps) {

  for (p_num in unique(data$participant)) {
    print(paste("Plotting participant", p_num))
    p_data <- data %>% filter(participant == p_num)

    for (t_num in unique(p_data$trial)) {
      print(paste("Plotting trial", t_num))
      t_data <- p_data %>% filter(trial == t_num)

      base_plot <- ggplot() +
        ggtitle(paste("Participant", p_num, "Trial", t_num)) +
        theme_classic() +
        xlim(-10, 10) +
        ylim(-10, 10)
        

      # map data layer
      map_num <- unique(t_data$map)
      map <- maps[[paste0('env', map_num)]]
      rotation <- unique(t_data$rotation)
      # apply rotation around 0, 0
      map <- map %>%
        mutate(
          rotation_radians = rotation * pi / 180,  # Convert degrees to radians
          V1_new = V1 * cos(rotation_radians) - V2 * sin(rotation_radians),  # New x-coordinate
          V2_new = V1 * sin(rotation_radians) + V2 * cos(rotation_radians)  # New y-coordinate
        )
      g2 <- geom_point(data = map, aes(x = V1_new, y = V2_new))

      # plot 
      g <- geom_point(data = t_data, aes(x = position_x_robot, y = position_y_robot, colour = linear_velocity)) 
        # scale_colour_gradient(low = "blue", high = "red", limits = c(0, 2)) +
        #labs(title = paste0('Participant ', p_num, ' Trial ', t_num)) +
        #theme_classic() +
        # geom_point(data = t_data %>% select(position_objects), aes(x = x, y = y), colour = 'black', size = 1) +
        #xlim(-10, 10) +
        #ylim(-10, 10)

      default <- data.frame(x = 0, y = 9)
      # apply rotation
      default <- default %>%
        mutate(
          rotation_radians = rotation * pi / 180,  # Convert degrees to radians
          x_new = x * cos(rotation_radians) - y * sin(rotation_radians),  # New x-coordinate
          y_new = x * sin(rotation_radians) + y * cos(rotation_radians)  # New y-coordinate
        )
      g3 <- geom_point(data = default, aes(x = x_new, y = y_new), size = 15, colour = "green", fill = 'green', shape = 22)

      final_plot <- base_plot +  g + g2 + g3 +
          scale_colour_gradient(low = "blue", high = "red", limits = c(0, 5))
        
      # add haptic and visual label
      haptic <- unique(t_data$haptic)
      visual <- unique(t_data$visual)
      final_plot <- ggdraw(final_plot) +
        draw_label(paste("Haptic:", haptic, "Visual:", visual), x = 0.9, y=0.9, hjust = 1)

      # save 
      ggsave(here("output", "figures_velocity_v2", paste("participant", p_num), paste0("P", p_num, "_T", t_num, "_velocity.png")), final_plot)
        
        
    }

    
  }

}