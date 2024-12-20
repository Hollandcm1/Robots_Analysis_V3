

# data <- data_long_calculated
# strategic_data <- combined_data

append_strategic_behaviour <- function(data, strategic_data) {
  
  # Convert the participant and trial columns to numeric
  data$participant <- as.numeric(data$participant)
  data$trial <- as.numeric(data$trial)
  
  # Select only the required columns from strategic_data
  strategic_data_selected <- strategic_data %>%
    select(participant, trial, strategic_agreement, strategic_both, strategic_either, collaborative)
  
  # Perform the merge
  merged_data <- data %>%
    left_join(strategic_data_selected, by = c("participant", "trial"))
  
  return(merged_data)
}


