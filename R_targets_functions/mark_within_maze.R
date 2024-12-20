# mark_within_maze

mark_within_maze <- function(data) {
  # loop through all participants
  participants <- names(data)
  for (p_num in 1:length(participants)) {
    
    print(paste("Participant", participants[p_num]))
    df_participant <- data[[paste(p_num)]]
    
    # loop through all trials
    trials <- names(data[[participants[p_num]]])
    for (trial_num in 1:length(trials)) {
      
      print(paste("---Trial", trials[trial_num]))
      df_trial <- df_participant[[trial_num]]
      
      position_data <- df_trial[['position_robot']]
      rotation <- df_trial[['rotation']]
      
      # skip if no position data
      if (nrow(position_data) == 0) {
        warning(paste("Participant", participants[p_num], "Trial", trials[trial_num], "has no position data"))
        next
      }
      
      # This will determine which column to use for how to cut the data. When traveling
      # vertically we want to cut the data based on a y criteria, when traveling horizontally
      # we want to cut the data based on an x criteria.
      if (rotation == 0 | rotation == 180) {
        column_decider <- 2
      } else if (rotation == 90 | rotation == 270) {
        column_decider <- 1
      }
      
      
      # The logic of this next bit kinda sucks to follow, but the idea is that it will
      # iterate through every frame of a trial, first looking for when the robot
      # has moved into the maze. Once it has, it will jump forward 500 frames to prevent
      # an accidental exit (i.e. they immediately leave). Then it will continue to mark
      # frames as being inside the maze until the robot leaves the maze. Once the robot
      # leaves the maze, it will stop marking frames. within = 1, outside = 0.
      jumps_left <- 0
      start_found <- 0
      end_found <- 0
      inside_maze <- rep(0, nrow(position_data))
      for (counter_frame in 1:nrow(position_data)) {
        
        # jump 50 when start is found - this is to prevent issues with people entering
        # and accidentally leaving breifly. The data for those 500 frames is still recorded
        if (jumps_left > 0) {
          jumps_left <- jumps_left - 1
          # mark inside maze
          inside_maze[counter_frame] <- 1
          next
        }
        
        # do nothing if outside the maze / (find when they leave the maze?)
        if (position_data[counter_frame, column_decider] < -5 | 
            position_data[counter_frame, column_decider] > 5 | 
            end_found == 1) {
          
          # mark that the end has been found, forcing the rest of of the trials to be skipped
          if (start_found == 1) {
            end_found <- 1
          }
          
          next
          
        } else { # mark frames that they are inside the maze
          
          inside_maze[counter_frame] <- 1
          
          # mark that the start has been found and provide jumps 
          if (sum(inside_maze == 0)) {
            start_found <- 1
            jumps_left <- 0
          }
          
        }
      }
      
      # add inside_maze to the data
      df_trial$within_maze <- inside_maze
      
      # add the data back to the original data
      data[[paste(p_num)]][[trial_num]] <- df_trial
      
    }
  }
  
  return(data)
  
}





