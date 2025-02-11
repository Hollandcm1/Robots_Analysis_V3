# explore_workload_data

# library(targets)
# library(here)
# library(ggplot2)

# data <- tar_read(workload_data)

explore_workload_data <- function(data) {

  #save_path
  save_path <- here("output", "workload_exploration") 
  workloadSum_path <- here(save_path, "WorkloadSum")
  if (!dir.exists(save_path)) {
    dir.create(save_path)
  }
  if (!dir.exists(workloadSum_path)) {
    dir.create(workloadSum_path)
  }


  ### Individual ### 
  for (participant in unique(data$Participant)) {

    participant_data <- data[data$Participant == participant,]
    
    p <- ggplot(participant_data, aes(x = 1:nrow(participant_data), y = WorkloadSum)) +
      geom_bar(stat = "identity") +
      labs(title = participant) 

    ggsave(here(workloadSum_path, paste0("participant_", participant, "_WorkloadSum.png")), p)
  }


  ### Combined ###
  p <- ggplot(data, aes(x = Participant, y = WorkloadSum, fill = Participant, group = Participant)) +
    geom_violin(alpha = 0.5, width = 3) +
    geom_boxplot(width = 0.3) +
    geom_point() + 
    labs(title = "WorkloadSum by Participant",
        x = "Participant",
        y = "WorkloadSum") + 
    theme_minimal()

  ggsave(here(workloadSum_path, "WorkloadSum_by_Participant.png"), p)


  ### Histogram ###
  p <- ggplot(data, aes(x = WorkloadSum)) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black") +
    labs(title = "WorkloadSum Histogram",
        x = "WorkloadSum",
        y = "Count") + 
    theme_minimal()

  ggsave(here(workloadSum_path, "WorkloadSum_Histogram.png"), p)

}