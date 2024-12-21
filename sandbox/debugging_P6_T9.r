# debugging_P6_T9

library(targets)
library(dplyr)

data <- tar_read(corrected_data)

# pull data for participant 6
data_P6 <- data %>%
  filter(participant == 6)

