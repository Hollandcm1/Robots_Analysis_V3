# import_three_back

# library(here)
# library(openxlsx)


import_three_back <- function() {

  print("Importing 3-back data")

  # initialize storage data frame
  all_three_back_data <- data.frame()

  for (i in 1:100) {

    padded_number <- sprintf("%03d", i)
    
    successful_read <- FALSE
    
    tryCatch({
      padded_string <- paste0("Participant ", padded_number)
      # assuming read.xlsx() is from openxlsx and here() is the correct path
      three_back_data <- read.xlsx(here("data", "3-back", "3-Back Data.xlsx"), sheet = padded_string, skipEmptyCols = FALSE)
      three_back_data$participant <- i
      # remove the first column
      three_back_data <- three_back_data[, -1]
      # store
      all_three_back_data <- rbind(all_three_back_data, three_back_data)
      successful_read <- TRUE
      print(paste0("Participant ", padded_number, " found"))
    }, error = function(e) {
      # moving on to the next case with lowercase
    })
    
    if (!successful_read) {
      tryCatch({
        padded_string <- paste0("participant ", padded_number)
        three_back_data <- read.xlsx(here("data", "3-back", "3-Back Data.xlsx"), sheet = padded_string, skipEmptyCols = FALSE)
        three_back_data$participant <- i
        # remove the first column
        three_back_data <- three_back_data[, -1]
        # store
        all_three_back_data <- rbind(all_three_back_data, three_back_data)
        successful_read <- TRUE
        print(paste0("participant ", padded_number, " found"))
      }, error = function(e) {
        # Participant not found in any form
      })
    }
    
    if (!successful_read) {
      # print(paste0("Participant ", padded_number, " not found in any form"))
    }
  }

  return(all_three_back_data)

}


