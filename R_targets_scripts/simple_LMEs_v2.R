library(tidyr)
library(dplyr)
library(lme4)
library(here)
library(sjPlot)
library(flexplot)
library(broom.mixed) # for `tidy` function on mixed models

# Assuming `tar_read` loads your data sets
data_exp1 <- tar_read(strategic_data_appended)
data_exp2 <- tar_read(data_long_calculated_exp2)

# Aggregate data as you have
# ...

### Aggregate ###
data <- data_exp1
average_by_trial <- data %>%
  group_by(participant, condition_nums, time_through_maze, max_force, path_length, haptic, visual, map, average_velocity, average_proximity, average_force) %>%
  summarise(average_force = mean(force_magnitude, na.rm = TRUE))

average_by_trial$experiment <- 1
average_by_trial$visual <- as.factor(average_by_trial$visual)
average_by_trial$haptic <- as.factor(average_by_trial$haptic)
average_by_trial$participant <- as.factor(average_by_trial$participant)
tmp_exp1_trial <- average_by_trial

average_by_condition <- average_by_trial %>%
  group_by(participant, haptic, visual, condition_nums, experiment) %>%
  summarise(average_time_through_maze = mean(time_through_maze, na.rm = TRUE),
            average_path_length = mean(path_length, na.rm = TRUE),
            average_velocity = mean(average_velocity, na.rm = TRUE),
            average_proximity = mean(average_proximity, na.rm = TRUE),
            average_force = mean(average_force, na.rm = TRUE))

tmp_exp1 <- average_by_condition

data <- data_exp2
average_by_trial <- data %>%
  group_by(participant, condition_nums, time_through_maze, max_force, path_length, haptic, visual, map, average_velocity, average_proximity, average_force) %>%
  summarise(average_force = mean(force_magnitude, na.rm = TRUE))
average_by_trial$experiment <- 2
average_by_trial$participant <- as.numeric(average_by_trial$participant)
average_by_trial$visual <- as.factor(average_by_trial$visual)
average_by_trial$haptic <- as.factor(average_by_trial$haptic)
average_by_trial$participant <- as.factor(average_by_trial$participant)
tmp_exp2_trial <- average_by_trial

average_by_condition <- average_by_trial %>%
  group_by(participant, haptic, visual, condition_nums, experiment) %>%
  summarise(average_time_through_maze = mean(time_through_maze, na.rm = TRUE),
            average_path_length = mean(path_length, na.rm = TRUE),
            average_velocity = mean(average_velocity, na.rm = TRUE),
            average_proximity = mean(average_proximity, na.rm = TRUE),
            average_force = mean(average_force, na.rm = TRUE))

tmp_exp2 <- average_by_condition

# Combine
all_data <- rbind(tmp_exp1, tmp_exp2)
average_by_condition <- all_data

# Helper function to fit model and extract desired results
fit_model_and_extract <- function(formula, data) {
  model <- lmer(formula, data = data)
  summary_model <- summary(model)
  tidy_model <- tidy(model)
  
  # Extract F values and p-values
  anova_model <- anova(model)
  
  # Combine results in a single data frame
  results <- tidy_model %>%
    select(term, estimate, std.error, p.value) %>%
    left_join(
      data.frame(row = rownames(anova_model), F.value = anova_model$`F value`, Pr = anova_model$`Pr(>F)`),
      by = c("term" = "row")
    ) %>%
    rename(beta = estimate, SE = std.error, F_value = F.value, p_value = Pr)
  
  return(results)
}

# Store results from all models
all_results <- list()

# Experiment 1 Simple LMEs
formulas <- c(
  "average_time_through_maze ~ visual*haptic + (1|participant)",
  "average_path_length ~ visual*haptic + (1|participant)",
  "average_velocity ~ visual*haptic + (1|participant)",
  "average_proximity ~ visual*haptic + (1|participant)",
  "average_force ~ visual*haptic + (1|participant)"
)

# Fit models for Experiment 1 and store results
for (formula in formulas) {
  results <- fit_model_and_extract(as.formula(formula), tmp_exp1)
  all_results[[formula]] <- results
}

# Experiment 2 Simple LMEs
for (formula in formulas) {
  results <- fit_model_and_extract(as.formula(formula), tmp_exp2)
  all_results[[paste0("exp2_", formula)]] <- results
}

# Combine all results into a single data frame
combined_results <- bind_rows(all_results, .id = "model")

# View or save the combined results
print(combined_results)

# Optionally, save the results to a CSV file
write.csv(combined_results, "model_results.csv", row.names = FALSE)
