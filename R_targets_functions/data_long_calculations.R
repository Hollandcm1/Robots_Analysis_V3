# data_long_calculations 

# time to get through maze
# average velocity
# path length
# near object (eventually)

#data <- data_long

data_long_calculations <- function(data, maps) {

  ########################
  ### Average Velocity ###
  ########################
  
  print("Calculating Average Velocity")
  
  # calculate average velocity by participant and trial
  average_velocity_by_trial <- data %>%
    group_by(participant, trial) %>%
    # filter(within_maze == 1) %>%
    summarise(average_velocity = mean(linear_velocity, na.rm = TRUE))
  
  # merge back to original data
  data <- merge(data, average_velocity_by_trial, by = c("participant", "trial"))
  
  
  #########################
  ### Time Through Maze ###
  #########################
  
  print("Calculating Time Through Maze")
  
  # calculate time through maze by participant and trial
  time_through_maze_by_trial <- data %>%
    group_by(participant, trial) %>%
    # filter(within_maze == 1) %>%
    summarise(time_through_maze = max(time) - min(time))
  
  # merge back to original data
  data <- merge(data, time_through_maze_by_trial, by = c("participant", "trial"))
  
  
  ###################
  ### Path Length ###
  ###################
  
  print("Calculating Path Length")
  
  # calculate path length by participant and trial
  data <- data %>%
    group_by(participant, trial) %>%
    arrange(participant, trial, frame) %>%
    mutate(prev_x = lag(position_x_robot, order_by = frame),
           prev_y = lag(position_y_robot, order_by = frame),
           distance_since_prev_frame = sqrt((position_x_robot - prev_x)^2 + (position_y_robot - prev_y)^2)
           ) %>%
    ungroup()
  
  # fix NA values
  data$distance_since_prev_frame[is.na(data$distance_since_prev_frame)] <- 0
  
  # calculate path length by participant and trial
  path_length_by_trial <- data %>%
    group_by(participant, trial) %>%
    # filter(within_maze == 1) %>%
    summarise(path_length = sum(distance_since_prev_frame))
  
  # merge back to original data
  data <- merge(data, path_length_by_trial, by = c("participant", "trial"))
  
  
  #####################
  ### Average Force ###
  #####################
  
  print("Calculating Average Force")
  
  # calculate average force by participant and trial
  average_force_by_trial <- data %>%
    group_by(participant, trial) %>%
    # filter(within_maze == 1) %>%
    summarise(average_force = mean(force_magnitude, na.rm = TRUE))
  
  # merge back to original data
  data <- merge(data, average_force_by_trial, by = c("participant", "trial"))
  
  
  #################
  ### Max Force ###
  #################
  
  print("Calculating Max Force")
  
  # calculate max force by participant and trial
  max_force_by_trial <- data %>%
    group_by(participant, trial) %>%
    # filter(within_maze == 1) %>%
    summarise(max_force = max(force_magnitude, na.rm = TRUE))
  
  # merge back to original data
  data <- merge(data, max_force_by_trial, by = c("participant", "trial"))
  
  #################
  ### Condition ###
  #################
  
  print("Condition Name Correcting")
  
  data$condition_names <- factor(data$condition, levels = c(1, 2, 3, 4), labels = c("v1_h1", "v0_h1", "v1_h0", "v0_h0"))
  data$condition_nums <- data$condition
  
  # remove old condition column
  data <- data %>%
    select(-condition)
  
  ###################################
  ### Proximity to Nearest Object ###
  ###################################
  
  data <- proximity_calculation(data, maps)
  
  #########################
  ### Average Proximity ###
  #########################
  
  print("Calculating Average Proximity")
  
  # calculate average proximity by participant and trial
  average_proximity_by_trial <- data %>%
    group_by(participant, trial) %>%
    # filter(within_maze == 1) %>%
    summarise(average_proximity = mean(closest_object_distance, na.rm = TRUE))
  
  # merge back to original data
  data <- merge(data, average_proximity_by_trial, by = c("participant", "trial"))
  
  
  #############################
  ### Data Type Corrections ###
  #############################
  data$condition_nums <- as.factor(data$condition_nums)
  data$condition_names <- as.factor(data$condition_names)
  data$participant <- as.factor(data$participant)
  data$haptic <- as.factor(data$haptic)
  data$visual <- as.factor(data$visual)

  return(data)
}



proximity_calculation <- function(data, maps) {

  print("Calculating Proximity to Nearest Objects")
  
  data$closest_object_distance <- NA
  
  for (p_num in unique(data$participant)) {
    
    print(paste("Participant", p_num))
    participant_data <- data %>%
      filter(participant == p_num)
    
    for (t_num in unique(participant_data$trial)) {
      
      print(paste("Trial", t_num))
      trial_data <- participant_data %>%
        filter(trial == t_num)
      
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
      
      # loop through all frames
      for (f_num in unique(trial_data$frame)) {
        
        #print(paste("Participant", p_num, "Trial", t_num, "Frame", f_num))
        frame_data <- trial_data %>%
          filter(frame == f_num)
        
        # get the robot's position
        # robot_x <- frame_data$position_x_robot
        # robot_y <- frame_data$position_y_robot
        
        # calculate distance to all objects
        # map <- map %>%
        #   mutate(
        #     distance = sqrt((robot_x - V1_new)^2 + (robot_y - V2_new)^2)
        #   )
        map <- map %>%
          mutate(
            distance = sqrt((frame_data$position_x_robot - V1_new)^2 + (frame_data$position_y_robot - V2_new)^2)
          )
        
        
        # get the closest object
        closest_object <- map %>%
          filter(distance == min(distance))
        
        # store the closest object
        #data$closest_object[which(data$participant == p_num & data$trial == t_num & data$frame == f_num)] <- closest_object$object
        # data$closest_object_distance[which(data$participant == p_num & data$trial == t_num & data$frame == f_num)] <- closest_object$distance
        
        # store with trial data
        trial_data$closest_object[which(trial_data$frame == f_num)] <- closest_object$distance
        
      }
      
      # add trial data back to data
      data$closest_object_distance[which(data$participant == p_num & data$trial == t_num)] <- trial_data$closest_object
      
    }
  }
  
  return(data)

}


# 
# library(data.table)
# library(dplyr)
# 
# proximity_calculation_dt <- function(data, maps) {
#   setDT(data) # Convert to data.table
#   
#   # Calculate radians once for each map
#   maps <- lapply(maps, function(map) {
#     map[, rotation_radians := unique(data$rotation) * pi / 180]
#     map[, V1_new := V1 * cos(rotation_radians) - V2 * sin(rotation_radians)]
#     map[, V2_new := V1 * sin(rotation_radians) + V2 * cos(rotation_radians)]
#     map
#   })
#   
#   # Add robot positions and calculate distances
#   data[, `:=` (closest_object_distance = Inf), by = .(participant, trial, frame)]
#   data[, `:=` (robot_x = unique(position_x_robot), robot_y = unique(position_y_robot)), by = .(participant, trial, frame)]
#   
#   # Calculate distances and update the closest distance in a more vectorized manner
#   data[, closest_object_distance := {
#     map <- maps[[paste0('env', unique(map))]]
#     sapply(seq_len(.N), function(i) {
#       min(sqrt((robot_x[i] - map$V1_new)^2 + (robot_y[i] - map$V2_new)^2))
#     })
#   }, by = .(participant, trial, frame)]
#   
#   return(data)
# }
# 
# 
# 
# 
# 
# 
# library(dplyr)
# library(purrr)
# 
# proximity_calculation_v3 <- function(data, maps) {
#   print("Calculating Proximity to Nearest Objects")
#   
#   # Compute unique participants and trials outside the loop
#   participants <- unique(data$participant)
#   
#   results <- map_df(participants, function(p_num) {
#     print(paste("Participant", p_num))
#     participant_data <- data %>%
#       filter(participant == p_num)
#     
#     trials <- unique(participant_data$trial)
#     map_df(trials, function(t_num) {
#       print(paste("Trial", t_num))
#       
#       trial_data <- participant_data %>%
#         filter(trial == t_num)
#       map_num <- unique(trial_data$map)
#       rotation <- unique(trial_data$rotation) * pi / 180
#       map_env <- maps[[paste0('env', map_num)]]
#       
#       # Rotate map coordinates
#       map_env <- map_env %>%
#         mutate(
#           V1_new = V1 * cos(rotation) - V2 * sin(rotation),
#           V2_new = V1 * sin(rotation) + V2 * cos(rotation)
#         )
#       
#       frames <- unique(trial_data$frame)
#       map_df(frames, function(f_num) {
#         print(paste("Frame", f_num))
#         
#         frame_data <- trial_data %>%
#           filter(frame == f_num)
#         robot_x <- unique(frame_data$position_x_robot)
#         robot_y <- unique(frame_data$position_y_robot)
#         
#         # Calculate distances for all map objects
#         distances <- sqrt((robot_x - map_env$V1_new)^2 + (robot_y - map_env$V2_new)^2)
#         closest_distance <- min(distances)
#         
#         # Update closest object distance for the specific frame
#         data$closest_object_distance[data$participant == p_num & data$trial == t_num & data$frame == f_num] <- closest_distance
#         return(data.frame(closest_object_distance = closest_distance))
#       })
#     })
#   })
#   
#   return(data)
# }
# 
# 


# try visualizing it to make sure it makes sense
# p_num <- 1
# t_num <- 1
# participant_data <- data %>%
#   filter(participant == p_num)
# trial_data <- participant_data %>%
#   filter(trial == t_num)
# ggplot(data=trial_data, aes(x=position_x_robot, y=position_y_robot, colour=closest_object_distance)) +
#   geom_point()




# 
# participants <- unique(data$participant)
# for (p_num in participants) {
#   
#   print(paste("Participant", p_num))
#   
#   trials <- unique(data$trial)
#   for (trial in trials) {
#     
#     # get the data for the participant
#     trial_data <- data %>%
#       filter(participant == p_num & trial == trial)
#     
#     
#   }
# }
# 
# 
# average_force_by_trial <- data %>%
#   group_by(participant, trial, condition) %>%