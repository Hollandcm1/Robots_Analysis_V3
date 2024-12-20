# chi_square analysis 

library(tidyverse)

data <- tar_read(strategic_data_appended)

#################
### Aggregate ###
#################
average_by_trial <- data %>%
  group_by(participant, trial, condition_nums, time_through_maze, max_force, path_length, haptic, visual, map, strategic_both, strategic_either, collaborative, strategic_agreement) %>%
  summarise(average_force = mean(force_magnitude, na.rm = TRUE))

average_by_trial$condition_nums_num <- as.numeric(average_by_trial$condition_nums)

chi_sq_result <- chisq.test(average_by_trial$condition_nums, average_by_trial$collaborative)

print(chi_sq_result)

#visualize
ggplot(average_by_trial, aes(x = collaborative, fill = condition_nums, group = condition_nums)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Chi-Square Analysis of Collaborative and Condition",
       x = "Condition",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(average_by_trial, aes(x = condition_nums, fill = collaborative, group = collaborative)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Chi-Square Analysis of Collaborative and Condition",
       x = "Condition",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# remove condition 3 and 4
average_by_trial <- average_by_trial %>%
  filter(condition_nums_num != 3 & condition_nums_num != 4)

# make collaborative a factor
average_by_trial$collaborative <- as.factor(average_by_trial$collaborative)

# plot again 
ggplot(average_by_trial, aes(x = condition_nums, fill = collaborative, group = collaborative)) +
  geom_bar(position = "dodge") +
  theme_classic() +
  labs(title = "Chi-Square Analysis of Collaborative and Condition",
       x = "Conditi",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





text_size.title <- 0
text_size.x_title <- 16
text_size.y_title <- 16
text_size.x_text <- 14
text_size.y_text <- 14
text_size.legend_text <- 12
text_size.legend_title <- 14



# relabel the two values in the collaborative to say "absent" and "present"
average_by_trial$collaborative <- recode(average_by_trial$collaborative, "0" = "Absent", "1" = "Present")

# relabel the two values in condition to say "Full Vision" and "Flickering Vision", then make sure flickering is first for a figure
average_by_trial$condition_nums <- recode(average_by_trial$condition_nums, "1" = "Full Vision", "2" = "Flickering Vision")
average_by_trial$condition_nums <- factor(average_by_trial$condition_nums, levels = c("Flickering Vision", "Full Vision"))

# make a version of the figure with percentages on the y axis
g <- ggplot(average_by_trial, aes(x = condition_nums, fill = collaborative, group = collaborative)) +
  geom_bar(position = "fill") +
  theme_classic() +
  labs(title = "",
       x = "",
       y = "Percent of Trials") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # relabel legend title to "Behaviour"
  scale_fill_discrete(name = "Behaviour") + 
  theme(
        plot.title = element_text(size=text_size.title, hjust=0.5),         # Set title text size
        axis.title.x = element_text(size=text_size.x_title),                    # Set x-axis label text size
        axis.title.y = element_text(size=text_size.y_title),                     # Set y-axis label text size
        axis.text.x = element_text(size=text_size.x_text),                  # Set x-axis tick mark text size
        axis.text.y = element_text(size=text_size.y_text),
        legend.text = element_text(size=text_size.legend_text),  # Set y-axis tick mark text size
        legend.title = element_text(size=text_size.legend_text)  # Set y-axis tick mark text size
  )

print(g)

# save
ggsave(g, file = here('output', 'standard_figures', 'chi_square_analysis.png'), width=6, height=8)




# count total of strategic_agreement compared to all trials
total_agreement <- sum(average_by_trial$strategic_agreement)
total_trials <- nrow(average_by_trial)
print(paste("Total agreement:", total_agreement, "Total trials:", total_trials))
# one devided by the other
print(paste("Percent agreement:", total_agreement / total_trials))
