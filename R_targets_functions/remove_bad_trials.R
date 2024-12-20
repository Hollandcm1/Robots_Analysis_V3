# remove_bad_trials


remove_bad_trials <- function(data) {

  # remove bad trials
  # loop through all participants
  total_removed <- 0
  participants <- names(data)
  for (p_num in 1:length(participants)) {
  
    print(paste("Participant", participants[p_num]))
  
    # loop through all trials
    trials <- names(data[[participants[p_num]]])
    for (trial_num in 1:length(trials)) {
  
      # print(paste("---Trial", trials[trial_num]))
  
      # if (trials[trial_num] == "P2_9" |
      #     trials[trial_num] == "P2_13" |
      #     trials[trial_num] == "P3_2" |
      #     trials[trial_num] == "P3_4" |
      #     trials[trial_num] == "P3_7" |
      #     trials[trial_num] == "P3_10" |
      #     trials[trial_num] == "P3_14" |
      #     trials[trial_num] == "P3_16" |
      #     trials[trial_num] == "P3_18" |
      #     trials[trial_num] == "P3_20" |
      #     trials[trial_num] == "P3_23" |
      #     trials[trial_num] == "P5_1" |
      #     trials[trial_num] == "P5_5" |
      #     trials[trial_num] == "P5_9" |
      #     trials[trial_num] == "P5_13" |
      #     trials[trial_num] == "P5_19" |
      #     trials[trial_num] == "P5_23" |
      #     trials[trial_num] == "P5_25" |
      #     trials[trial_num] == "P5_27" |
      #     trials[trial_num] == "P5_31" |
      #     trials[trial_num] == "P6_2" |
      #     # trials[trial_num] == "P6_18" | # weird jump
      #     trials[trial_num] == "P6_28" |
      #     trials[trial_num] == "P7_2" |
      #     # trials[trial_num] == "P7_7" | # this one is close, but they don't actually enter the obeject
      #     trials[trial_num] == "P7_10" |
      #     trials[trial_num] == "P7_20" |
      #     trials[trial_num] == "P7_25" |
      #     trials[trial_num] == "P7_26" |
      #     trials[trial_num] == "P7_30" |
      #     # trials[trial_num] == "P7_18" | # somehow this has a split second on negative velocity?
      #     trials[trial_num] == "P9_22" |
      #     trials[trial_num] == "P10_7" |
      #     # trials[trial_num] == "P11_17" | # this one is close, but they don't actually enter the obeject
      #     trials[trial_num] == "P12_2" |
      #     trials[trial_num] == "P12_6" |
      #     trials[trial_num] == "P12_10" |
      #     trials[trial_num] == "P12_13" |
      #     trials[trial_num] == "P12_22" |
      #     trials[trial_num] == "P12_25" |
      #     trials[trial_num] == "P12_30" |
      #     trials[trial_num] == "P14_4" |
      #     trials[trial_num] == "P14_5" |
      #     trials[trial_num] == "P14_17" |
      #     trials[trial_num] == "P14_27" |
      #     trials[trial_num] == "P14_32" |
      #     # trials[trial_num] == "P15_14" | # this one is close, but they don't actually enter the obeject
      #     trials[trial_num] == "P15_24" |
      #     trials[trial_num] == "P16_1" |
      #     trials[trial_num] == "P16_14" |
      #     trials[trial_num] == "P16_20" |
      #     # trials[trial_num] == "P16_24" | # this one is close, but they don't actually enter the obeject
      #     trials[trial_num] == "P16_32" |
      #     trials[trial_num] == "P17_24" |
      #     trials[trial_num] == "P18_3" |
      #     trials[trial_num] == "P18_16" |
      #     #trials[trial_num] == "P19_1" | # weird jump
      #     trials[trial_num] == "P19_5" |
      #     trials[trial_num] == "P19_18" |
      #     trials[trial_num] == "P19_21" |
      #     trials[trial_num] == "P19_26" |
      #     trials[trial_num] == "P19_27" |
      #     trials[trial_num] == "P19_31" |
      #     trials[trial_num] == "P20_2" |
      #     trials[trial_num] == "P20_6" |
      #     trials[trial_num] == "P20_10" |
      #     trials[trial_num] == "P20_20" |
      #     trials[trial_num] == "P20_22" |
      #     trials[trial_num] == "P20_25" |
      #     trials[trial_num] == "P20_29" |
      #     trials[trial_num] == "P21_4" |
      #     trials[trial_num] == "P23_11" |
      #     trials[trial_num] == "P24_23" |
      #     trials[trial_num] == "P24_27" |
      #     trials[trial_num] == "P25_2" |
      #     trials[trial_num] == "P25_6" |
      #     trials[trial_num] == "P25_10" |
      #     trials[trial_num] == "P25_25" |
      #     trials[trial_num] == "P25_30" 
      #     ){
      #   # remove trial data
      #   data[[participants[p_num]]][[trials[trial_num]]] <- NULL
      #   warning(paste("Removed", participants[p_num], trials[trial_num]))
      #   total_removed <- total_removed + 1
      # }

      # Warn that no trials are being removed
      warning("No trials are being removed. Uncomment the code to remove trials.")
    }
  }
  
  print(paste("Total removed:", total_removed))

  return(data)
}

# double check it's the right trial by visualizing it
# plot_trial_by_velocity(data, 19, 12)
