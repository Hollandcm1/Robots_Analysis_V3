# append_codes_to_three_back


# data <- tar_read(summary_three_back_data)
# codes_participant_conditions <- tar_read(codes_participant_conditions)
# codes_conditions <- tar_read(codes_conditions)

append_codes_to_three_back <- function(data, codes_participant_conditions, codes_conditions) {

  # create empty columns called haptic, visual, and arithmatic
  data <- data.frame(data, haptic = NA, visual = NA, arithmatic = NA, condition = NA)


  # condition correction
  for (i in 1:nrow(data)) {

    p_num <- data$participant[i]
    trial_num <- data$trial[i]

    if (trial_num >= 1 & trial_num <= 3) {
      cond_number <- 1
    } else if (trial_num >= 4 & trial_num <= 6) {
      cond_number <- 2
    } else if (trial_num >= 7 & trial_num <= 9) {
      cond_number <- 3
    } else if (trial_num >= 10 & trial_num <= 12) {
      cond_number <- 4
    } else {
      error("trial number out of range")
    }

    true_cond_number <- codes_participant_conditions[[cond_number + 1]][which(codes_participant_conditions$Participant == as.numeric(p_num))]

    haptic_row_num <- which(codes_conditions$Factor == "Haptic")
    frequency_row_num <- which(codes_conditions$Factor == "Frequency")

    data$haptic[i] <- codes_conditions[[true_cond_number + 1]][haptic_row_num]
    data$visual[i] <- codes_conditions[[true_cond_number + 1]][frequency_row_num]
    data$arithmatic[i] <- 1
    data$condition[i] <- true_cond_number

  }

  return(data)

} 