#' @title Plot trial by velocity
#' @description This function plots the velocity of the participant's trials
#' @param data A list of lists, where each list contains the data for a single participant.
#' @export 
#' @examples
#' plot_trial_by_velocity(data, participant_number = 1, trial_number = 1)

plot_trial_by_velocity <- function(data, participant_number = 1, trial_number = 1, return_figure=FALSE){
  
  # get the data for the participant
  participant_data <- data[[participant_number]]
  
  # get the data for the trial
  trial_number_long = paste0('P', participant_number, '_', trial_number)
  trial_data <- participant_data[[trial_number_long]]
  
  # get the velocity data
  #velocity_data <- as.data.frame(trial_data$velocity_robot)
  velocity_data <- as.data.frame(trial_data$linear_and_angular_velocity)
  names(velocity_data) <- c('velocity', 'NA1')
  
  # get position data
  position_data <- as.data.frame(trial_data$position_robot)
  names(position_data) <- c('pos_x', 'pos_y', 'NA2')
  
  # combine the data
  combined_data <- cbind(velocity_data, position_data)
  
  # get map data
  map_data <- as.data.frame(trial_data$position_objects)
  names(map_data) <- c('x', 'y', 'NA3')
  
  # plot the position of the robot with colour as velocity
  g <- ggplot(combined_data, aes(x = pos_x, y = pos_y, colour = velocity)) + 
    geom_point() +
    scale_colour_gradient(low = "blue", high = "red", limits = c(0, 2)) +
    labs(title = paste0('Participant ', participant_number, ' Trial ', trial_number)) +
    theme_classic() +
    geom_point(data = map_data, aes(x = x, y = y), colour = 'black', size = 1) +
    xlim(-10, 10) +
    ylim(-10, 10)
  
  if (return_figure == TRUE) {
    return(g)
  } else {
    print(g)
  }
  
}
