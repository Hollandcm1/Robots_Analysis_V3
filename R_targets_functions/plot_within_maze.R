

# library(ggplot2)
# library(here)

plot_within_maze <- function(data) {
  
  # go through all participants
  for (p_num in names(data)) {
    participant_data <- data[[p_num]]
    list_names <- names(participant_data)
    
    print(paste0('Working on Participant ', p_num))
    
    # go through all trials
    for (name in list_names) {
      
      # strip the name to just pull the trial number 
      trial_number <- as.numeric(gsub(".*_(\\d+)", "\\1", name))
      
      participant_number = p_num
      trial_number = trial_number
      #return_figure = TRUE
      
      # get the data for the participant
      participant_data <- data[[participant_number]]
      
      # get the data for the trial
      trial_number_long = paste0('P', participant_number, '_', trial_number)
      trial_data <- participant_data[[trial_number_long]]
      
      # get within maze data
      within_maze_data <- as.data.frame(trial_data$within_maze)
      names(within_maze_data) <- c('within_maze')
      
      # get position data
      position_data <- as.data.frame(trial_data$position_robot)
      names(position_data) <- c('pos_x', 'pos_y', 'NA2')
      
      # combine the data
      combined_data <- cbind(within_maze_data, position_data)
      
      # get map data
      map_data <- as.data.frame(trial_data$position_objects)
      names(map_data) <- c('x', 'y', 'NA3')
      
      # plot the position of the robot with colour as within_maze
      g <- ggplot(combined_data, aes(x = pos_x, y = pos_y, color = within_maze)) +
        geom_point() +
        geom_point(data = map_data, aes(x = x, y = y), color = 'black') +
        ggtitle(paste0('Participant ', participant_number, ' Trial ', trial_number)) +
        theme_minimal() +
        theme(legend.position = 'none') + 
        xlim(-10, 10) +
        ylim(-10, 10)
      
      # save figure
      output_dir <- here('output', 'figures_within_maze', paste0('participant_', p_num))
      if (!dir.exists(output_dir)) {
        dir.create(output_dir)
      }
      ggsave(filename = paste0(output_dir, '/', paste0('trial_', trial_number, '.png')), plot = g, width = 10, height = 10)
      
    }
    
    print('All trials plotted')
    #return(Sys.time())
    
  }
  
}