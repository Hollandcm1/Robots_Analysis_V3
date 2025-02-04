# pull_workload

# library(here)

# codes_participant_conditions <- tar_read(codes_participant_conditions)
# codes_conditions <- tar_read(codes_conditions)

pull_workload <- function(codes_participant_conditions, codes_conditions) {

  data <- read.csv(here("data", "NASA-TLX", "Summaryreport.csv"))

  # relabel column Q1 as Participant
  data <- data.frame(data, Participant = data$Q1)
  data$Q1 <- NULL

  # relabel column Q2 as Condition
  data <- data.frame(data, Condition = data$Q2)
  data$Q2 <- NULL

  # create empty columns called haptic, visual, and arithmatic
  data <- data.frame(data, haptic = NA, visual = NA, arithmatic = NA)

  # condition corrections 
  for (i in 1:nrow(data)) {

    p_num <- data$Participant[i]
    cond_string <- data$Condition[i]
    cond_number_num <- as.numeric(gsub("\\D", "", cond_string))
    cond_number_letter <- sub("^\\d+", "", cond_string)
    true_cond_number <- codes_participant_conditions[[cond_number_num + 1]][which(codes_participant_conditions$Participant == as.numeric(p_num))]

    haptic_row_num <- which(codes_conditions$Factor == "Haptic")
    frequency_row_num <- which(codes_conditions$Factor == "Frequency")

    data$haptic[i] <- codes_conditions[[true_cond_number + 1]][haptic_row_num]
    data$visual[i] <- codes_conditions[[true_cond_number + 1]][frequency_row_num]

    if (cond_number_letter == ".A") {
      data$arithmatic[i] <- 1
    } else {
      data$arithmatic[i] <- 0
    }

  }

  # remove Condition column 
  data$Condition <- NULL

  # convert columns to factors
  data$Participant <- as.factor(data$Participant)
  data$haptic <- as.factor(data$haptic)
  data$visual <- as.factor(data$visual)
  data$arithmatic <- as.factor(data$arithmatic)

  # In columns Q3 through Q8, relabel vales: if value contains 1 somewhere in it, relabel as 1. If a value contains 10 somewhere in it, relabel as 10
  data$Q3 <- ifelse(grepl(0, data$Q3), '10', ifelse(grepl(1, data$Q3), '1', data$Q3))
  data$Q4 <- ifelse(grepl(0, data$Q4), '10', ifelse(grepl(1, data$Q4), '1', data$Q4))
  data$Q5 <- ifelse(grepl(0, data$Q5), '10', ifelse(grepl(1, data$Q5), '1', data$Q5))
  data$Q6 <- ifelse(grepl(0, data$Q6), '10', ifelse(grepl(1, data$Q6), '1', data$Q6))
  data$Q7 <- ifelse(grepl(0, data$Q7), '10', ifelse(grepl(1, data$Q7), '1', data$Q7))
  data$Q8 <- ifelse(grepl(0, data$Q8), '10', ifelse(grepl(1, data$Q8), '1', data$Q8))

  # convert columns Q3 through Q8 to numeric
  data$Q3 <- as.numeric(data$Q3)
  data$Q4 <- as.numeric(data$Q4)
  data$Q5 <- as.numeric(data$Q5)
  data$Q6 <- as.numeric(data$Q6)
  data$Q7 <- as.numeric(data$Q7)
  data$Q8 <- as.numeric(data$Q8)

  # calculate the average workload
  data$Workload <- rowMeans(data[, c("Q3", "Q4", "Q5", "Q6", "Q7", "Q8")])

  # create new column that has workload out of 100
  data$Workload100 <- data$Workload * 10

  # create new column for sum of workload
  data$WorkloadSum <- rowSums(data[, c("Q3", "Q4", "Q5", "Q6", "Q7", "Q8")])

  return(data)
  
} 