#' @title Participant corrections
#' @description This function goes through each participant to make sure their data is correct and that the naming if correct
#' @param compiled_data A list of lists, where each list contains the data for a single participant.
#' @return A list of lists, where each list contains the data for a single participant.
#' @export
#' @examples
#' participant_error_correction(compiled_data)
#' 
participant_error_correction_exp2 <- function(compiled_data, codes_participant_conditions){
  
  FALSE
  # initialize corrected data
  corrected_data <- list()
  
  # go through all participants to correct their namings and other data issues
  for (p_num in names(compiled_data)) {
    print(paste("Correcting participant ", p_num))
    tmp <- compiled_data[[as.character(p_num)]]
    list_names <- names(tmp)
    
    participant_warned.names <- FALSE # track if name warning already presented
    participant_warned.size <- FALSE # track if participant warning already presented
    
    # correct for too much data
    if (p_num == 25) {
      tmp[["Trial_Parameters.mat"]] <- NULL
      list_names <- names(tmp)
    }

    if (p_num == 33) {
      tmp[["participant33_env6_rot270_cond1.mat"]] <- NULL
      tmp[["participant33_env9_rot270_cond1.mat"]] <- NULL
      tmp[["participant33_env10_rot0_cond1.mat"]] <- NULL
      tmp[["participant33_env11_rot270_cond1.mat"]] <- NULL
      #list_names <- names(tmp)
    }
    
    if (p_num == 34) {
      tmp[["participant34_env9_rot180_cond2.mat"]] <- NULL
    }

    if (p_num == 35) {
      tmp[["participant35_env9_rot180_cond2.mat"]] <- NULL
    }
    
    if (p_num == 43) {
      tmp[["participant43_env3_rot90_cond1.mat"]] <- NULL
      tmp[["participant43_env6_rot270_cond1.mat"]] <- NULL
      tmp[["participant43_env8_rot270_cond1.mat"]] <- NULL
      tmp[["participant43_env9_rot270_cond1.mat"]] <- NULL
      tmp[["participant43_env10_rot0_cond1.mat"]] <- NULL
      tmp[["participant43_env11_rot270_cond1.mat"]] <- NULL
    }
    
    for (name in list_names) {
      
      
      # if anything needs to be removed 
      if (!grepl("participant", name)) {
        tmp[[name]] <- NULL # remove element
        next
      }
      
      if (grepl("evn0", name)) {
        tmp[[name]] <- NULL # remove element
        next
      }
      
      participant_full_number <- sub(".*participant(\\d+).*", "\\1", name)
      participant_number <- substr(participant_full_number, 2, nchar(participant_full_number))
      
      if (as.numeric(participant_number) == 33 & as.numeric(participant_full_number) > 100) {
        # participant101 cond1 becomes participant1 cond3
        # participant101 cond2 becomes participant1 cond4
        # build new name from original name
        add_to_cond = 0 # how much to correct this participant
        new_name <- sub("participant\\d*(\\d{2})", "participant\\1", name)
        cond_number <- as.numeric(gsub(".*cond(\\d+).*", "\\1", new_name))
        new_name <- sub(paste0("cond", cond_number), paste0("cond", cond_number + add_to_cond), new_name)
        tmp <- rename_list_element(tmp, name, new_name)
        # reasign for testing measures later
        participant_full_number <- sub(".*participant(\\d+).*", "\\1", new_name)
        participant_number <- participant_full_number
        #participant_number <- substr(participant_full_number, nchar(participant_full_number), nchar(participant_full_number))

      }
      
      # # Debugging
      # if (as.numeric(participant_number) == 3) {
      #   print(participant_full_number)
      #   browser()
      # }
      
      if (as.numeric(participant_number) == 34 & as.numeric(participant_full_number) > 100) {
        # participant101 cond1 becomes participant1 cond3
        # participant101 cond2 becomes participant1 cond4
        # build new name from original name
        add_to_cond = 1 # how much to correct this participant
        new_name <- sub("participant\\d*(\\d{2})", "participant\\1", name)
        cond_number <- as.numeric(gsub(".*cond(\\d+).*", "\\1", new_name))
        new_name <- sub(paste0("cond", cond_number), paste0("cond", cond_number + add_to_cond), new_name)
        tmp <- rename_list_element(tmp, name, new_name)
        # reasign for testing measures later
        participant_full_number <- sub(".*participant(\\d+).*", "\\1", new_name)
        participant_number <- participant_full_number
      }
      
      if (as.numeric(participant_number) == 35 & as.numeric(participant_full_number) > 100) {
        # participant101 cond1 becomes participant1 cond3
        # participant101 cond2 becomes participant1 cond4
        # build new name from original name
        add_to_cond = 1 # how much to correct this participant
        new_name <- sub("participant\\d*(\\d{2})", "participant\\1", name)
        cond_number <- as.numeric(gsub(".*cond(\\d+).*", "\\1", new_name))
        new_name <- sub(paste0("cond", cond_number), paste0("cond", cond_number + add_to_cond), new_name)
        tmp <- rename_list_element(tmp, name, new_name)
        # reasign for testing measures later
        participant_full_number <- sub(".*participant(\\d+).*", "\\1", new_name)
        participant_number <- participant_full_number
      }
      
      if (as.numeric(participant_number) == 43 & as.numeric(participant_full_number) > 100) {
        # participant101 cond1 becomes participant1 cond3
        # participant101 cond2 becomes participant1 cond4
        # build new name from original name
        add_to_cond = 0 # how much to correct this participant
        new_name <- sub("participant\\d*(\\d{2})", "participant\\1", name)
        cond_number <- as.numeric(gsub(".*cond(\\d+).*", "\\1", new_name))
        new_name <- sub(paste0("cond", cond_number), paste0("cond", cond_number + add_to_cond), new_name)
        tmp <- rename_list_element(tmp, name, new_name)
        # reasign for testing measures later
        participant_full_number <- sub(".*participant(\\d+).*", "\\1", new_name)
        participant_number <- participant_full_number
      }
      
      
      
      # check if they need condition corrections
      correction_needed <- as.numeric(participant_full_number) > 100
      if (correction_needed & !participant_warned.names) {
        warning(paste('participant', participant_full_number, 'needs correction'))
        participant_warned.names <- TRUE
      }
      
      # check if anyone has too much data
      if (length(tmp) > 32 & !participant_warned.size) {
        warning(paste('participant', participant_full_number, 'has too much data'))
        participant_warned.size <- TRUE
      } else if (length(tmp) < 32 & !participant_warned.size) {
        warning(paste('participant', participant_full_number, 'has too little data'))
        participant_warned.size <- TRUE
      } 
      
      # Debugging
      # if (p_num == 4) {
      #   browser()
      # }
      
    }
    
    # return tmp to original list
    corrected_data[[p_num]] <- tmp # add corrected data
    
    # # Debugging
    # if (length(corrected_data) >= 4) {
    #   if (length(corrected_data[[4]]) > 32) {
    #     browser()
    #   }
    # }
    
    # clear tmp
    tmp <- NULL
    
  }
  
  
  
  # go through all participants and move name info into 'dataframe' info
  for (p_num in names(corrected_data)) {
    tmp <- corrected_data[[p_num]]
    list_names <- names(tmp)
    
    # # Debugging
    # if (p_num == 3) {
    #   browser()
    # }
    
    for (name in list_names) {
      # pull condition number
      cond_number <- as.numeric(gsub(".*cond(\\d+).*", "\\1", name))
      # pull environment number
      env_number <- as.numeric(gsub(".*env(\\d+).*", "\\1", name))
      # pull rotation number
      rot_number <- as.numeric(gsub(".*rot(\\d+).*", "\\1", name))
      
      # Correction based on participant counterbalancing
      #cond_number <- codes_participant_conditions[[cond_number+1]][as.numeric(p_num)]
      cond_number <- codes_participant_conditions[[cond_number + 1]][which(codes_participant_conditions$Participant == as.numeric(p_num))]
      
      # add to list
      tmp[[name]]$condition <- cond_number
      tmp[[name]]$environment <- env_number
      tmp[[name]]$rotation <- rot_number
      
      # create new name without environment and rotation and condition
      new_name <- sub("cond\\d+", "", name)
      new_name <- sub("env\\d+", "", new_name)
      new_name <- sub("rot\\d+", "", new_name)
      new_name <- sub("___", "", new_name)
      new_name <- sub("participant", "P", new_name)
      new_name <- sub(".mat", "", new_name)
      # add a counter to the end that increases with each list_names loop
      new_name <- paste0(new_name, "_", as.character(which(list_names == name)))
      #print(new_name)
      
      if (length(new_name)>1) {
        browser()
      }
      
      # rename list element
      tmp <- rename_list_element(tmp, name, new_name)
      
    }
    
    # replace old list with new list in corrected_data
    corrected_data[[p_num]] <- tmp
    
  }
  
  return(corrected_data)
  
}
