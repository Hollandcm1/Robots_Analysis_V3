#' @title condense_data
#' @description This function takes the raw data from the experiment and condenses it into a single file for each participant.
#' @param codes_participant_conditions A data frame containing the participant numbers and the conditions they were in.
#' @param codes_conditions A data frame containing the conditions and the environments they were in.
#' @return A list of lists, where each list contains the data for a single participant.
#' @export
#' @examples
#' condense_data(codes.participant_conditions)
#' @importFrom here here
#' @importFrom readmat readMat
#' @importFrom dplyr select
#' @importFrom base basename
#' 

# library(R.matlab)

condense_data <- function(codes_participant_conditions){
  
  all_data <- list()
  #all_data
  #p_data <- list()
  
  #print(codes_participant_conditions)
  
  #browser()
  
  # Loop through all participants
  for (p_num in 1:length(codes_participant_conditions$Participant)) {
    
    print(paste("Compiling participant ", p_num))
    # create paths
    participant <- paste0('P', p_num)
    participant_filepath <- here('data', 'raw', participant)
    filenames <- list.files(participant_filepath, full.names = TRUE)
    
    # go through each file found and append to a single file
    for (f in filenames) {

      # if filename contains "discarded", continue
      if (grepl("discarded", f)) {
        next
      }
      
      base <- basename(f)
      # check env
      if (grepl("env0", base)) {
        next # this is practice data that we don't need
      }
      # if filetype is xlsx, skip
      if (grepl("xlsx", base)) {
        next
      }
      # pull data
      tmp_data <- readMat(f)
      
      # rename element names as needed
      tmp_data <- rename_list_element(tmp_data, "Po", "position_objects")
      tmp_data <- rename_list_element(tmp_data, "Pg", "position_goal")
      tmp_data <- rename_list_element(tmp_data, "X1", "position_robot")
      tmp_data <- rename_list_element(tmp_data, "X.L", "position_leader")
      tmp_data <- rename_list_element(tmp_data, "adaptive.gains", "adaptive_gains")
      tmp_data <- rename_list_element(tmp_data, "dX1", "velocity_robot")
      tmp_data <- rename_list_element(tmp_data, "force.input", "force_input")
      tmp_data <- rename_list_element(tmp_data, "order", "map_and_condition_order")
      tmp_data <- rename_list_element(tmp_data, "r", "position_and_velocity_joystick")
      tmp_data <- rename_list_element(tmp_data, "rg", "size_goal")
      tmp_data <- rename_list_element(tmp_data, "t", "time")
      tmp_data <- rename_list_element(tmp_data, "vw1", "linear_and_angular_velocity_robot")
      
      # Check if the participant's list exists; if not, initialize it
      if (is.null(all_data[[as.character(p_num)]])) {
        all_data[[as.character(p_num)]] <- list()
      }
      
      all_data[[as.character(p_num)]][[base]] <- tmp_data
      
    }
  }
  
  # save all_data
  compiled_data <- all_data
  save(compiled_data, file = here("data", "processed", "compiled", "compiled_data.RData"))
  return(compiled_data)
}

# 
# # load requirements
# source(here("scripts", "import_codebook.R"))
# source(here("scripts", "define_environments_and_conditions.R"))
# source(here("scripts", "rename_list_element_function.R"))
# 
# all_data <- list()
# #p_data <- list()
# 
# # Loop through all participants
# for (p_num in 1:length(codes.participant_conditions$Participant)) {
#   
#   print(paste("Compiling participant ", p_num))
#   # create paths
#   participant <- paste0('P', p_num)
#   participant_filepath <- here('data', 'raw', participant)
#   filenames <- list.files(participant_filepath, full.names = TRUE)
#   
#   # go through each file found and append to a single file
#   for (f in filenames) {
#     
#     base <- basename(f)
#     # check env
#     if (grepl("env0", base)) {
#       next # this is practice data that we don't need
#     }
#     # pull data
#     tmp_data <- readMat(f)
#     
#     # rename element names as needed
#     tmp_data <- rename_list_element(tmp_data, "Po", "position_objects")
#     tmp_data <- rename_list_element(tmp_data, "Pg", "position_goal")
#     tmp_data <- rename_list_element(tmp_data, "X1", "position_robot")
#     tmp_data <- rename_list_element(tmp_data, "X.L", "position_leader")
#     tmp_data <- rename_list_element(tmp_data, "adaptive.gains", "adaptive_gains")
#     tmp_data <- rename_list_element(tmp_data, "dX1", "velocity_robot")
#     tmp_data <- rename_list_element(tmp_data, "force.input", "force_input")
#     tmp_data <- rename_list_element(tmp_data, "order", "map_and_condition_order")
#     tmp_data <- rename_list_element(tmp_data, "r", "position_and_velocity_joystick")
#     tmp_data <- rename_list_element(tmp_data, "rg", "size_goal")
#     tmp_data <- rename_list_element(tmp_data, "t", "time")
#     tmp_data <- rename_list_element(tmp_data, "vw1", "linear_and_angular_velocity_robot")
#     
#     # Check if the participant's list exists; if not, initialize it
#     if (is.null(all_data[[as.character(p_num)]])) {
#       all_data[[as.character(p_num)]] <- list()
#     }
#     
#     all_data[[as.character(p_num)]][[base]] <- tmp_data
#     
#   }
# }
# 
# # save all_data
# compiled_data <- all_data
# save(compiled_data, file = here("data", "processed", "compiled", "compiled_data.RData"))
