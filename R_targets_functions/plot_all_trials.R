#' 

# library(ggplot2)
# library(here)
# data <- tar_read(corrected_data)

plot_all_trials <- function(data) {
  
  # go through all participants
  for (p_num in names(data)) {
    participant_data <- data[[p_num]]
    list_names <- names(participant_data)
    
    print(paste0('Working on Participant ', p_num))
    
    # go through all trials
    for (name in list_names) {
      
      # strip the name to just pull the trial number 
      trial_number <- as.numeric(gsub(".*_(\\d+)", "\\1", name))
      
      # get figure 
      g <- plot_trial_by_velocity(data, participant_number = p_num, trial_number = trial_number, return_figure = TRUE)
      
      # save figure
      output_dir <- here('output', 'figures_velocity', paste0('participant_', p_num))
      if (!dir.exists(output_dir)) {
        dir.create(output_dir)
      }
      ggsave(filename = paste0(output_dir, '/', paste0('trial_', trial_number, '.png')), plot = g, width = 10, height = 10)
    }
  }
  
  print('All trials plotted')
  return(Sys.time())

}