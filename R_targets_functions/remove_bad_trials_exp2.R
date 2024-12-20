# Remove Bad trials exp2


# remove_bad_trials


remove_bad_trials_exp2 <- function(data) {
  
  # remove bad trials
  # loop through all participants
  total_removed <- 0
  participants <- names(data)
  for (p_num in participants) {
    
    print(paste("Participant", p_num))
    
    # loop through all trials
    trials <- names(data[[p_num]])
    for (trial_num in trials) {
      
      # print(paste("---Trial", trials[trial_num]))
      
      if (
        trial_num == "P26_20" |
        trial_num == "P26_4" |
        
        trial_num == "P28_17" |
        
        trial_num == "P29_9" |
        
        trial_num == "P30_1" |
        trial_num == "P30_5" |
        trial_num == "P30_31" |
        
        trial_num == "P32_2" |
        trial_num == "P32_5" |
        trial_num == "P32_11" |
        trial_num == "P32_15" |
        trial_num == "P32_18" |
        trial_num == "P32_26" |
        trial_num == "P32_27" |
        trial_num == "P32_31" |
        
        trial_num == "P33_2" |
        trial_num == "P33_5" |
        trial_num == "P33_9" |
        trial_num == "P33_14" |
        trial_num == "P33_32" |
        
        trial_num == "P38_24" |
        
        trial_num == "P39_1" |
        trial_num == "P39_23" |
        
        trial_num == "P41_2" |
        trial_num == "P41_3" |
        
        trial_num == "P42_1" |
        trial_num == "P42_5" |
        trial_num == "P42_8" |
        trial_num == "P42_11" |
        trial_num == "P42_12" |
        trial_num == "P42_13" |
        trial_num == "P42_20" |
        trial_num == "P42_21" |
        trial_num == "P42_24" |
        trial_num == "P42_28" |
        trial_num == "P42_32" |
        
        trial_num == "P43_3" |
        trial_num == "P43_4" |
        trial_num == "P43_5" |
        trial_num == "P43_8" |
        trial_num == "P43_9" |
        trial_num == "P43_11" |
        trial_num == "P43_14" |
        trial_num == "P43_17" |
        trial_num == "P43_21" |
        trial_num == "P43_24" |
        trial_num == "P43_27" |
        trial_num == "P43_29" |
        
        trial_num == "P44_7" |
        
        trial_num == "P47_3" |
        trial_num == "P47_8" |
        
        trial_num == "P48_8" |
        trial_num == "P48_16" |
        trial_num == "P48_19" |
        trial_num == "P48_25" |
        trial_num == "P48_30"
        
      ){
        # remove trial data
        data[[p_num]][[trial_num]] <- NULL
        warning(paste("Removed", p_num, trial_num))
        total_removed <- total_removed + 1
      }
    }
  }
  
  print(paste("Total removed:", total_removed))
  
  return(data)
}

# double check it's the right trial by visualizing it
# plot_trial_by_velocity(data, 19, 12)
