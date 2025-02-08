# format_three_back

# library(targets)
# library(here)
# library(stringr)

# data <- tar_read(three_back_data)

format_three_back <- function(data) {

  print("Formattting 3-back data")

  # get all unique participants
  participants <- unique(data$participant)

  trials <- c(1:12)

  # initialize storage data frame
  summary_three_back_data <- data.frame()

  # loop through each participant
  for (participant in participants) {

    if (participant == 1){ 
      print("SKIPPING P1")
      next
    }
    if (participant == 2){ 
      print("SKIPPING P2")
      next
    }

    # filter data for the current participant
    participant_data <- data[data$participant == participant, ]

    # loop through each trial
    for (trial in trials) {

      # Get the index of the trial column
      trial_col <- which(colnames(participant_data) == trial)

      # pull column to right of trial column
      trial_data <- participant_data[, (trial_col + 1)]

      # Replace NA with "_" and collapse into a single string
      trial_string <- paste(ifelse(is.na(trial_data), "_", trial_data), collapse = "")

      # count the number of "e"s and "p"s and "x"s
      e_count <- str_count(trial_string, "e")
      p_count <- str_count(trial_string, "p")
      x_count <- str_count(trial_string, "x")

      ################
      ### Warnings ###
      ################

      # check if there is more than one x
      if (x_count > 1) {
        print(paste0("Participant ", participant, " Trial ", trial, " has more than one 'x'"))
      }

      # check if e or p show up after an x
      x_position <- regexpr("x", trial_string)[1]
      substring_after_x <- substr(trial_string, x_position + 1, nchar(trial_string))
      any_letter_after_x <- any(grepl("[a-zA-Z]", substring_after_x))
      if (any_letter_after_x) {
        print(paste0("Participant ", participant, " Trial ", trial, " has a letter after 'x'"))
      }

      # check if there is no x
      if (x_count == 0) {
        print(paste0("Participant ", participant, " Trial ", trial, " has no 'x'"))
      }


      # number of trials they made it to
      true_x_position <- grep("x", trial_data, fixed = TRUE)
      if (length(true_x_position) == 0) {
        true_x_position <- NA
      } else {
        true_x_position <- true_x_position[1]
      }


      # store summary data
      summary_three_back_data <- rbind(summary_three_back_data, data.frame(participant = participant, trial = trial, e_count = e_count, p_count = p_count, x_count = x_count, x_final_position = true_x_position))

    }
    
  }

  return(summary_three_back_data)

} 