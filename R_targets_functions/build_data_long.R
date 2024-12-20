# build_data_long_force

# peak force?
# time by force matrix?

build_data_long <- function(data, codes) {
  
  print("Building data_long_force")
  data_long <- data.frame()
  
  # loop through all participants
  participants <- names(data)
  for (p_num in 1:length(participants)) {
    
    print(paste("Participant", participants[p_num]))
    
    # loop through all trials
    trials <- names(data[[participants[p_num]]])
    for (trial_num in 1:length(trials)) {
      
      start_time <- Sys.time()
      
      print(paste("---Trial", trials[trial_num]))
      
      trial_data <- data[[participants[p_num]]][[trials[trial_num]]]
      
      force <- as.data.frame(trial_data['force_input'])
      
      if (nrow(force) == 0) {
        warning(paste("Participant", participants[p_num], "Trial", trials[trial_num], "has no force data"))
        next
      }
      
      # get magnitude of x and y force
      force$force_mag <- sqrt(force[,1]^2 + force[,2]^2)
      
      # plot force magnitude by time
      # ggplot(force, aes(x = 1:nrow(force), y = force_mag)) +
      #   geom_line() +
      #   labs(title = paste("Participant", participants[p_num], "Trial", trials[trial_num])) +
      #   theme_classic()
      
      # Initialize data frame (using force for length)
      dat <- data.frame(participant = numeric(nrow(force)))
      
      # add basic participant information
      dat$participant <- p_num
      dat$trial <- trial_num
      dat$frame <- 1:nrow(force)
      
      # Add force
      dat$force_x <- force[,1]
      dat$force_y <- force[,2]
      dat$force_magnitude <- force$force_mag
      
      # within maze data
      within_maze <- as.data.frame(trial_data['within_maze'])
      dat$within_maze <- within_maze$within_maze
      
      # time data
      time <- as.data.frame(trial_data['time'])
      dat$time <- time$time
      
      # linear velocity
      linear_velocity <- as.data.frame(trial_data['linear_and_angular_velocity_robot'])
      dat$linear_velocity <- linear_velocity$linear_and_angular_velocity_robot.1
      
      # position robot
      position <- as.data.frame(trial_data['position_robot'])
      dat$position_x_robot <- position$position_robot.1
      dat$position_y_robot <- position$position_robot.2
      
      # position leader
      position <- as.data.frame(trial_data['position_leader'])
      dat$position_x_leader <- position$position_leader.1
      dat$position_y_leader <- position$position_leader.2
      
      # haptic data 
      condition <- trial_data[['condition']]
      haptic_row <- which(codes$Factor == 'Haptic')
      visual_row <- which(codes$Factor == 'Visual')
      column_name <- paste('Condition', condition)
      haptic <- codes[haptic_row, column_name]
      visual <- codes[visual_row, column_name]
      haptic <- rep(haptic, nrow(position))
      visual <- rep(visual, nrow(position))
      dat$haptic <- haptic
      dat$visual <- visual
      
      # condition as well
      condition <- trial_data[['condition']]
      dat$condition <- condition
      
      # map
      map <- trial_data[['environment']]
      dat$map <- map
      
      # rotation
      rotation <- trial_data[['rotation']]
      dat$rotation <- rotation
      
      # save dat to data_long
      data_long <- rbind(data_long, dat)
      
      # time reports for code optimization
      current_time <- Sys.time() - start_time
      print(current_time)
      
    }
    
  }
  
  return(data_long)
  
}