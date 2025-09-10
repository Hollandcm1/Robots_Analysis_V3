


library(targets)
library(here)
library(dplyr)
library(ggplot2)
library(ggbeeswarm)


workload_data <- tar_read(workload_data)
three_back_data_with_composites <- tar_read(three_back_data_with_composites)
data_long_calculated <- tar_read(data_long_calculated)

# summarize data 
data_long_calculated_summary <- data_long_calculated %>%
  group_by(participant, haptic, visual, arithmatic) %>%
  summarise(
    mean_velocity = mean(average_velocity, na.rm = TRUE),
    mean_time = mean(time_through_maze, na.rm = TRUE),
    mean_force = mean(average_force, na.rm = TRUE),
    mean_proximity = mean(average_proximity, na.rm = TRUE),
    mean_path_length = mean(path_length, na.rm = TRUE),
    .groups = "drop"
  )

# t-tests

# arithmatic

t.test(mean_velocity ~ arithmatic, data = data_long_calculated_summary)

t.test(mean_velocity ~ visual, data = data_long_calculated_summary)

t.test(mean_velocity ~ haptic, data = data_long_calculated_summary)




# LME 

library(lme4)
lme_model <- lmer(mean_velocity ~ arithmatic * visual * haptic + (1|participant), data = data_long_calculated_summary)
# lme_model <- lmer(mean_velocity ~ visual * haptic + (1|participant), data = data_long_calculated_summary)

# install.packages("emmeans")
library(emmeans)


# Get estimated marginal means
emm <- emmeans(lme_model, ~ visual * haptic * arithmatic)

# Pairwise comparisons -> forest plot
contr <- pairs(emm)                           # emmeans contrast results
contr_df <- as.data.frame(
  summary(contr, infer = c(TRUE, TRUE))       # adds 95% CIs (lower.CL / upper.CL)
)

# Clean labels and order by effect size
contr_df <- contr_df %>%
  mutate(
    contrast_label = as.character(contrast),
    contrast_label = gsub(":", " × ", contrast_label),   # nicer interaction separator
    contrast_label = gsub(" - ", " vs ", contrast_label),
    p_label = ifelse(p.value < .001, "< .001", sprintf("%.3f", p.value))
  ) %>%
  arrange(desc(abs(estimate)))

# Forest plot
forest_plot <- ggplot(contr_df, aes(x = estimate, y = reorder(contrast_label, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(xmin = lower.CL, xmax = upper.CL)) +
  labs(
    title = "Pairwise Contrasts (emmeans)",
    subtitle = "Outcome: mean_velocity; 95% confidence intervals",
    x = "Estimated difference (mean_velocity)",
    y = "Contrast",
    caption = "Dashed line at 0. P-values from emmeans pairwise contrasts."
  ) +
  theme_minimal(base_size = 12)

# Print to viewer and save a high-res copy
print(forest_plot)

ggsave(filename = here::here("contrast_forest_plot.png"), plot = forest_plot,
       width = 8, height = 10, dpi = 300)















# LME 

library(lme4)
lme_model <- lmer(mean_velocity ~ arithmatic * visual * haptic + (1|participant), data = data_long_calculated_summary)
# lme_model <- lmer(mean_velocity ~ visual * haptic + (1|participant), data = data_long_calculated_summary)

# install.packages("emmeans")
library(emmeans)


# Get estimated marginal means
emm <- emmeans(lme_model, ~ visual | haptic * arithmatic)

# Pairwise comparisons -> forest plot
contr <- pairs(emm)                           # emmeans contrast results
contr_df <- as.data.frame(
  summary(contr, infer = c(TRUE, TRUE))       # adds 95% CIs (lower.CL / upper.CL)
)

# Clean labels and order by effect size
contr_df <- contr_df %>%
  mutate(
    contrast_label = as.character(contrast),
    contrast_label = gsub(":", " × ", contrast_label),   # nicer interaction separator
    contrast_label = gsub(" - ", " vs ", contrast_label),
    p_label = ifelse(p.value < .001, "< .001", sprintf("%.3f", p.value))
  ) %>%
  arrange(desc(abs(estimate)))

# export to csv
write.csv(contr_df, here::here("contrast_results_vision.csv"), row.names = FALSE)





# Get estimated marginal means
emm <- emmeans(lme_model, ~ haptic | visual * arithmatic)

# Pairwise comparisons -> forest plot
contr <- pairs(emm)                           # emmeans contrast results
contr_df <- as.data.frame(
  summary(contr, infer = c(TRUE, TRUE))       # adds 95% CIs (lower.CL / upper.CL)
)

# Clean labels and order by effect size
contr_df <- contr_df %>%
  mutate(
    contrast_label = as.character(contrast),
    contrast_label = gsub(":", " × ", contrast_label),   # nicer interaction separator
    contrast_label = gsub(" - ", " vs ", contrast_label),
    p_label = ifelse(p.value < .001, "< .001", sprintf("%.3f", p.value))
  ) %>%
  arrange(desc(abs(estimate)))

# export to csv
write.csv(contr_df, here::here("contrast_results_haptic.csv"), row.names = FALSE)



# Get estimated marginal means
emm <- emmeans(lme_model, ~ arithmatic | visual * haptic)

# Pairwise comparisons -> forest plot
contr <- pairs(emm)                           # emmeans contrast results
contr_df <- as.data.frame(
  summary(contr, infer = c(TRUE, TRUE))       # adds 95% CIs (lower.CL / upper.CL)
)

# Clean labels and order by effect size
contr_df <- contr_df %>%
  mutate(
    contrast_label = as.character(contrast),
    contrast_label = gsub(":", " × ", contrast_label),   # nicer interaction separator
    contrast_label = gsub(" - ", " vs ", contrast_label),
    p_label = ifelse(p.value < .001, "< .001", sprintf("%.3f", p.value))
  ) %>%
  arrange(desc(abs(estimate)))

# export to csv
write.csv(contr_df, here::here("contrast_results_arithmatic.csv"), row.names = FALSE)
