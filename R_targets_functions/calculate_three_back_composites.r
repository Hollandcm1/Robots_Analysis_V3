# calculate_three_back_composites

# library(targets)

# data <- tar_read(three_back_data_with_codes)


calculate_three_back_composites <- function(data) {
  ### proportion_correct = (x_final_position - e_count) / x_final_position
  data$proportion_correct <- (data$x_final_position - data$e_count) / data$x_final_position
  
  ### proportion_error = e_count / x_final_position
  data$proportion_error <- data$e_count / data$x_final_position
  
  ### proportion_pause = p_count / x_final_position
  data$proportion_pause <- data$p_count / data$x_final_position
  
  ### proportion_error_and_pause = (e_count + p_count) / x_final_position
  data$proportion_error_and_pause <- (data$e_count + data$p_count) / data$x_final_position
  
  return(data)

}
