
# library(readxl)
# library(here)

import_strategic_data <- function(){

  # load Excel data
  data_1 <- read_excel(here("data", "strategic behaviours","Strategic Behaviour 1.xlsx"))
  data_2 <- read_excel(here("data", "strategic behaviours","Strategic Behaviour 2.xlsx"))
  data_collaborative <- read_excel(here("data", "strategic behaviours","Agreed Scores v3.xlsx"))
  
  # combine
  TRUE
  combined_data <- data_collaborative[1:4]
  combined_data$S1 <- data_1$`haptic strategy`
  combined_data$S2 <- data_2$`haptic strategy`
  combined_data$collaborative <- data_collaborative$`Agreed`
  
  # calculate agreement of S1 and S2
  combined_data$agreement <- ifelse(combined_data$S1 == combined_data$S2, 1, 0)
  # calculate average agreement
  mean(combined_data$agreement)
  
  # calculate number of trials identical in S1 and S2
  sum(combined_data$S1)
  sum(combined_data$S2)
  
  # calculate propotions for each strategy
  mean(combined_data$S1)
  mean(combined_data$S2)
  
  # create column for when both S1 and S2 are 1
  combined_data$both <- ifelse(combined_data$S1 == 1 & combined_data$S2 == 1, 1, 0)
  
  # rename agreement as strategic_agreement
  combined_data <- combined_data %>% rename(strategic_agreement = agreement)
  
  #remane both as strategic_both
  combined_data <- combined_data %>% rename(strategic_both = both)
  
  
  # create column for when either S1 or S2 are 1
  combined_data$either <- ifelse(combined_data$S1 == 1 | combined_data$S2 == 1, 1, 0)
  
  # rename either as strategic_either
  combined_data <- combined_data %>% rename(strategic_either = either)
  
  
  return(combined_data)

}