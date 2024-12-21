# check_forces_in_practice_trial

library(here)
library(R.matlab)
library(targets)
library(ggplot2)

# load data
p_num <- "9"
participant <- paste0('P', p_num)
participant_filepath <- here('data', 'raw_exp2', participant)
filenames <- list.files(participant_filepath, full.names = TRUE)

all_data <- list()
corrected_data <- list()

codes_participant_conditions <- tar_read(codes_participant_conditions)

# run file "R_targets_functions/rename_list_element.R"
source(here("R_targets_functions", "rename_list_element.R"))

for (f in filenames) {
      
    base <- basename(f)
    # check env
    if (grepl("env0", base)) {
      next # this is practice data that we don't need
    }
    # pull data
    tmp_data <- readMat(f)
    
    # rename element names as needed
    tmp_data <- rename_list_element(tmp_data, "Po", "position_objects")
    tmp_data <- rename_list_element(tmp_data, "Pg", "position_goal")
    tmp_data <- rename_list_element(tmp_data, "X1", "position_robot")
    tmp_data <- rename_list_element(tmp_data, "X.L", "position_leader")
    tmp_data <- rename_list_element(tmp_data, "adaptive.gains", "adaptive_gains")
    tmp_data <- rename_list_element(tmp_data, "dX1", "velocity_robot")
    tmp_data <- rename_list_element(tmp_data, "force.input", "force_input")
    tmp_data <- rename_list_element(tmp_data, "order", "map_and_condition_order")
    tmp_data <- rename_list_element(tmp_data, "r", "position_and_velocity_joystick")
    tmp_data <- rename_list_element(tmp_data, "rg", "size_goal")
    tmp_data <- rename_list_element(tmp_data, "t", "time")
    tmp_data <- rename_list_element(tmp_data, "vw1", "linear_and_angular_velocity_robot")
    
    # Check if the participant's list exists; if not, initialize it
    if (is.null(all_data[[as.character(p_num)]])) {
      all_data[[as.character(p_num)]] <- list()
    }
    
    all_data[[as.character(p_num)]][[base]] <- tmp_data
    
  }



corrected_data[[p_num]] <- all_data[[p_num]]

for (p_num in names(corrected_data)) {
  tmp <- corrected_data[[p_num]]
  list_names <- names(tmp)

  for (name in list_names) {
    # pull condition number
    cond_number <- as.numeric(gsub(".*cond(\\d+).*", "\\1", name))
    # pull environment number
    env_number <- as.numeric(gsub(".*env(\\d+).*", "\\1", name))
    # pull rotation number
    rot_number <- as.numeric(gsub(".*rot(\\d+).*", "\\1", name))
    
    # if cond_number is NA, skip
    if (is.na(cond_number)) {
      # remove from data
      tmp[[name]] <- NULL
      next
    }
    
    # Correction based on participant counterbalancing
    cond_number <- codes_participant_conditions[[cond_number+1]][as.numeric(p_num)]
    
    # add to list
    tmp[[name]]$condition <- cond_number
    tmp[[name]]$environment <- env_number
    tmp[[name]]$rotation <- rot_number
    
    # create new name without environment and rotation and condition
    new_name <- sub("cond\\d+", "", name)
    new_name <- sub("env\\d+", "", new_name)
    new_name <- sub("rot\\d+", "", new_name)
    new_name <- sub("___", "", new_name)
    new_name <- sub("participant", "P", new_name)
    new_name <- sub(".mat", "", new_name)
    # add a counter to the end that increases with each list_names loop
    new_name <- paste0(new_name, "_", as.character(which(list_names == name)))
    #print(new_name)
    
    if (length(new_name)>1) {
      browser()
    }
    
    # rename list element
    tmp <- rename_list_element(tmp, name, new_name)
    
  }
  
  # replace old list with new list in corrected_data
  corrected_data[[p_num]] <- tmp
  
}


data <- corrected_data


participant_data <- data[[p_num]]
list_names <- names(participant_data)

data <- participant_data[[list_names[1]]]

data_position <- as.data.frame(data$position_robot)
data_force <- as.data.frame(data$force_input)

# calculate force magnitude (from column V1 and V2)
data_force$force_magnitude <- sqrt(data_force$V1^2 + data_force$V2^2)

# plot V1 by V2 and color by force_magnitude
g <- ggplot(data_force, aes(x = V1, y = V2, color = force_magnitude)) +
  geom_point(alpha = 0.3) +
  scale_color_gradient(low = "blue", high = "red") +
  theme_classic() +
  ggtitle("Force X by Y colored by force magnitude") +
  labs(x = "Force X", y = "Force Y")

# save
ggsave(here("sandbox", "force_v1_v2.png"), g, width = 10, height = 10, units = "in", dpi = 300)
