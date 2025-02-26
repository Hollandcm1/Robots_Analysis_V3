# save_data_for_Ryan

# library(targets)
# library(here)

# workload <- tar_read(workload_data)
# three_back <- tar_read(three_back_data_with_composites)


save_data_for_Ryan <- function(workload, three_back) {
  
  # save_path
  save_path <- here("output", "Ryans_dataframes") 
  # create directory if it doesn't exist
  if (!dir.exists(save_path)) {
    dir.create(save_path)
  }

  # save each dataframe as seperate csv
  write.csv(workload, file.path(save_path, "workload.csv"))
  write.csv(three_back, file.path(save_path, "three_back.csv"))

  }