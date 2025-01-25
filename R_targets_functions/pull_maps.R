#' @title Pull Maps
#' @description This script pulls maps from the raw data files and saves them as csv files in the processed data folder.
#' @param codes_participant_conditions A data frame containing the participant conditions.
#' @param codes_conditions A data frame containing the conditions.
#' @param environments A vector containing the environment numbers.
#' @return NULL
#' @examples
#' pull_maps(codes_participant_conditions, codes_conditions, environments)
#' @importFrom stringr str_extract
#' @importFrom here here
#' @importFrom Rmatlab readMat R.matlab
#' @export

library(stringr)

pull_maps <- function(codes_participant_conditions, codes_conditions, environments) {
  
  # create environment list we can manipulate
  environments_left <- environments
  maps <- list()
  
  # loop through all participants
  for (p_num in 1:length(codes_participant_conditions$Participant)) {
    #print(p_num)
    # create path string
    participant <- paste0('P', p_num)
    participant_filepath <- here('data', 'raw', participant)
    filenames <- list.files(participant_filepath, full.names = TRUE)
    
    # go through all files for that participant
    for (f in filenames) {
      
      base <- basename(f) # pull just the file name
      
      # Create conditional for checking remaining environments
      environments_to_check <- environments_left %>%
        sapply(function(x) paste0("env", x)) %>%
        paste(collapse = "|")
      
      # check if environments_left is empty
      if (length(environments_left) == 0) {
        break # stop if we have found all environments
      }
      
      # check if the data has a rotation of interest (just 0 rotations)
      if (!grepl("rot0", base)) {
        next # skip if not rot0
      }
      
      # check if the data has a map of interest
      if (!grepl(environments_to_check, base)) {
        next # skip if not env3, env5, env6, env7, env8, env9, env10
      }
      
      # pull data so we can pull map
      tmp_data <- readMat(f)
      # pull map
      map <- as.data.frame(tmp_data[['Po']])
      # pull environment number (for saving)
      env_num <- str_extract(base, "env\\d+")
      # store map data
      write.csv(map, here('data', 'processed', 'maps', paste0(env_num, ".csv")), row.names = FALSE)
      # remove the environment we just found from environments_left
      environments_left <- environments_left[environments_left != as.numeric(str_extract(env_num, "\\d+"))]
      # send update to console
      print(paste0("Found ", env_num))
      # store in maps variable
      maps[[env_num]] <- map
      # clear temp_data and map
      rm(tmp_data, map, base, environments_to_check, env_num)
    }
    
  }

  # check if maps has env25
  if (!("env25" %in% names(maps))) {
    # warning
    print("env25 not found, loading from csv")
    # read csv for map env25
    map <- read.csv(here('data', 'processed', 'maps', 'env25.csv'))
    maps$env25 <- map
  }

  return(maps)
  
}
