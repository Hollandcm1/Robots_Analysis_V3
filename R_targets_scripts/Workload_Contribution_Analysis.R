#Workload_contribution_analysis


library(ggplot2)
library(here)
library(tidyr)
library(dplyr)
library(lme4)
library(targets)

# load data
data <- read.csv(here("data", "NASA-TLX", "Workload_exp_1_and_2.csv"))
codes_participant_conditions_exp1 <- tar_read('codes_participant_conditions')
codes_participant_conditions_exp2 <- tar_read('codes_participant_conditions_exp2')

##################################
### Functions to Simplify Code ###
##################################

fix_means_df <- function(means, codes) {
  
  # rename the columns
  colnames(means) <- c("condition", "mean", "sd", "sample_size", "se")
  
  # add new colums called 'visual' and 'haptic' based on codes dataframe
  means$visual <- codes$Visual
  means$haptic <- codes$Haptic
  
  # relabel visual, where 1 = Full Vision and 0 = Flickering Vision
  means$visual <- factor(means$visual, levels = c(0, 1), labels = c("Flickering Vision", "Full Vision"))
  
  # relabel haptic, where 1 = Haptic and 0 = No Haptic
  means$haptic <- factor(means$haptic, levels = c(0, 1), labels = c("No Haptic Feedback", "Haptic Feedback"))
  
  return(means)
}


codes <- tar_read('codes_conditions')

codes <- t(codes)

# use 'Factor' row as column labels
colnames(codes) <- codes["Factor",]

# remove the 'Factor' row
codes <- codes[-1,]

# convert to dataframe
codes <- as.data.frame(codes)


#####################
### Data Cleaning ###
#####################

# combine the codes
codes_participant_conditions <- rbind(codes_participant_conditions_exp1, codes_participant_conditions_exp2)

# remove participants 1-25
# data <- data[!data$Q1 %in% 1:25,]

# add new column called experiment, where if participant == 1 through 25 the value is 1, if not 2
data$experiment <- ifelse(data$Q1 %in% 1:25, 1, 2)

# remove participants if they don't have 4 of their values in the participant column
data <- data[data$Q1 %in% data$Q1[duplicated(data$Q1)],]


# relabel column Q1 as Participant
data <- data.frame(data, Participant = data$Q1)
data$Q1 <- NULL

# relabel column Q2 as Condition
data <- data.frame(data, Condition = data$Q2)
data$Q2 <- NULL

# create new column for Corrected_Condition
data$Corrected_Condition <- NA

# Condition Corretion
for (i in 1:nrow(data)) {
  
  p_num <- data$Participant[i]
  cond_number <- data$Condition[i]
  
  # Correction based on participant counterbalancing
  #corrected_cond_number <- codes_participant_conditions[[cond_number+1]][as.numeric(p_num)]
  corrected_cond_number <- codes_participant_conditions[[cond_number + 1]][which(codes_participant_conditions$Participant == as.numeric(p_num))]
  
  # add to list
  data$Corrected_Condition[i] <- corrected_cond_number
  
}

# Remove old conditions values (just to prevent mistakes)
data$Condition <- data$Corrected_Condition
data$Corrected_Condition <- NULL

# convert Participant and Condition to factors
data$Participant <- as.factor(data$Participant)
data$Condition <- as.factor(data$Condition)

# In columns Q3 through Q8, relabel vales: if value contains 1 somewhere in it, relabel as 1. If a value contains 10 somewhere in it, relabel as 10
data$Q3 <- ifelse(grepl(0, data$Q3), '10', ifelse(grepl(1, data$Q3), '1', data$Q3))
data$Q4 <- ifelse(grepl(0, data$Q4), '10', ifelse(grepl(1, data$Q4), '1', data$Q4))
data$Q5 <- ifelse(grepl(0, data$Q5), '10', ifelse(grepl(1, data$Q5), '1', data$Q5))
data$Q6 <- ifelse(grepl(0, data$Q6), '10', ifelse(grepl(1, data$Q6), '1', data$Q6))
data$Q7 <- ifelse(grepl(0, data$Q7), '10', ifelse(grepl(1, data$Q7), '1', data$Q7))
data$Q8 <- ifelse(grepl(0, data$Q8), '10', ifelse(grepl(1, data$Q8), '1', data$Q8))

# convert columns Q3 through Q8 to numeric
data$Q3 <- as.numeric(data$Q3)
data$Q4 <- as.numeric(data$Q4)
data$Q5 <- as.numeric(data$Q5)
data$Q6 <- as.numeric(data$Q6)
data$Q7 <- as.numeric(data$Q7)
data$Q8 <- as.numeric(data$Q8)

# calculate the average workload
data$Workload <- rowMeans(data[, c("Q3", "Q4", "Q5", "Q6", "Q7", "Q8")])

# create new column that has workload out of 100
data$Workload100 <- data$Workload * 10

# create new column for sum of workload
data$WorkloadSum <- rowSums(data[, c("Q3", "Q4", "Q5", "Q6", "Q7", "Q8")])

# create new column called Haptic, where if condition is 1 or 2, the value is 1, if not 0
data$Haptic <- ifelse(data$Condition %in% c(1, 2), 1, 0)
# convert to factor
data$Haptic <- as.factor(data$Haptic)

# create new column called Visual, where if condition is 1 or 3, the value is 1, if not 0
data$Visual <- ifelse(data$Condition %in% c(1, 3), 1, 0)
# convert to factor
data$Visual <- as.factor(data$Visual)


########################
### data Corrections ###
########################

# relabel visual, where 1 = Full Vision and 0 = Flickering Vision in data
data$visual <- factor(data$Visual, levels = c(0, 1), labels = c("Flickering Vision", "Full Vision"))
# remove Visual column
data$Visual <- NULL

# relabel haptic, where 1 = Haptic and 0 = No Haptic in data
data$haptic <- factor(data$Haptic, levels = c(0, 1), labels = c("No Haptic Feedback", "Haptic Feedback"))
# remove Haptic column
data$Haptic <- NULL



##############################################
### Renaming and Selecting Subsets of Data ###
##############################################

# make new variable that contains all data
data_all <- data

# create a subset variable that only has data from experiment == 1
data_exp1 <- subset(data, experiment == 1)

# create a subset variable that only has data from experiment == 2
data_exp2 <- subset(data, experiment == 2)

# # remap experiment column values with 1=Exp1 and 2=Exp2
# data_all$experiment <- factor(data$experiment, levels = c(1, 2), labels = c("Exp1", "Exp2"))


################################
### Analysis - by Experiment ###
################################

# get the column mean for each questions Q3 through Q8
column_means_exp1 <- colMeans(data_exp1[, c("Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Workload")], na.rm = TRUE)
column_means_exp2 <- colMeans(data_exp2[, c("Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Workload")], na.rm = TRUE)

# difference
difference_means <- column_means_exp2 - column_means_exp1


# remove the workload column and store as it's own var
average_workload_difference <- difference_means["Workload"]
difference_means <- difference_means[-7]

# convert to df
difference_means_df <- data.frame(
  Question = names(difference_means),
  Difference = difference_means)

g1 <- ggplot(difference_means_df, aes(x = Question, y = Difference)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_classic() +
  labs(title = "Differences in Column Means Between Experiments",
       x = "Question",
       y = "Difference in Means") +
  # add a dotted line at y = average_workload_difference
  geom_hline(yintercept = average_workload_difference, linetype = "dashed", color = "red", linewidth = 1 ) 

print(g1)

ggsave(here("output", "NASA-TLX", "difference_means.png"), g1, width = 10, height = 6, units = "in")


##############################################
### Analysis - By Experiment and Condition ###
##############################################

# get the mean of the workload for each questions by condition in each experiment
# Calculate column means grouped by Condition
column_means_exp1 <- data_exp1 %>%
  group_by(Condition) %>%
  summarise(across(c(Q3, Q4, Q5, Q6, Q7, Q8, Workload), mean, na.rm = TRUE))

column_means_exp2 <- data_exp2 %>%
  group_by(Condition) %>%
  summarise(across(c(Q3, Q4, Q5, Q6, Q7, Q8, Workload), mean, na.rm = TRUE))

conditions <- c("1", "2", "3", "4")

# Calculate the differences between the means
difference_means <- column_means_exp2 - column_means_exp1

# remove the empty first column 
difference_means <- difference_means[-1]

# add the conditions back to the first column 
difference_means <- cbind(conditions, difference_means)

# convert to df
difference_means_df <- as.data.frame(difference_means)

# store workload column for later use
average_workload_difference_by_condition <- difference_means_df$Workload
#average_workload_difference <- mean(average_workload_difference, na.rm = TRUE)

# remove the workload column
difference_means_df <- difference_means_df[-8]

# convert dataframe to long format
difference_means_df_long <- gather(difference_means_df, Question, Difference, -conditions)

# convert average_workload_difference_by_condition to named vector, where the names are the conditions
names(average_workload_difference_by_condition) <- conditions

# multiple each value 6 times to get the same length as the difference_means_df_long
average_workload_difference_by_condition <- rep(average_workload_difference_by_condition, each = 6)

# convert to dataframe
average_workload_difference_by_condition <- as.data.frame(average_workload_difference_by_condition)

# add a question column, counting from Q3-Q8
average_workload_difference_by_condition$Question <- rep(c("Q3", "Q4", "Q5", "Q6", "Q7", "Q8"), 4)

# add a condition column 
average_workload_difference_by_condition$Condition <- rep(conditions, each = 6)

warning("Manual tweaking of averages!")
average_workload_difference_by_condition$average_workload_difference_by_condition <- average_workload_difference_by_condition$average_workload_difference_by_condition - 0.05

# Plot
g2 <- ggplot(difference_means_df_long, aes(x = conditions, y = Difference, fill = Question)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_classic() +
  labs(title = "Differences in Column Means Between Experiments by Condition",
       x = "Condition",
       y = "Difference in Means") +
  # add a dotted line at y = average_workload_difference
  geom_hline(yintercept = average_workload_difference, linetype = "dashed", color = "red", linewidth = 1 ) +
  # Add black dotted lines at y = average workload_difference_by_condition for each condition
  geom_segment(data = average_workload_difference_by_condition,
               aes(x = as.numeric(Condition)-0.5,
                   xend = as.numeric(Condition)+0.5,
                   y = average_workload_difference_by_condition,
                   yend = average_workload_difference_by_condition)) +
  # relabel the x axis as Full Vision + Haptic, Haptic, Full Vision, Nothing
  scale_x_discrete(labels = c("Full Vision + Haptic", "Haptic", "Full Vision", "Nothing"))


  
print(g2)

ggsave(here("output", "NASA-TLX", "difference_means_by_condition.png"), g2, width = 10, height = 6, units = "in")



# make a line graph version
g3 <- ggplot(difference_means_df_long, aes(x = conditions, y = Difference, group = Question, color = Question)) +
  geom_line() +
  geom_point() +
  theme_classic() +
  labs(title = "Differences in Column Means Between Experiments by Condition",
       x = "Condition",
       y = "Difference in Means") +
  # add a dotted line at y = average_workload_difference
  geom_hline(yintercept = average_workload_difference, linetype = "dashed", color = "red", linewidth = 1 ) +
  # Add black dotted lines at y = average workload_difference_by_condition for each condition
  geom_segment(data = average_workload_difference_by_condition,
               aes(x = as.numeric(Condition)-0.5,
                   xend = as.numeric(Condition)+0.5,
                   y = average_workload_difference_by_condition,
                   yend = average_workload_difference_by_condition)) +
  # relabel the x axis as Full Vision + Haptic, Haptic, Full Vision, Nothing
  scale_x_discrete(labels = c("Full Vision + Haptic", "Haptic", "Full Vision", "Nothing")) 

print(g3)

ggsave(here("output", "NASA-TLX", "difference_means_by_condition_line.png"), g3, width = 10, height = 6, units = "in")


# convert to long format
column_means_exp1_long <- gather(column_means_exp1, Question, Workload, -Condition)
column_means_exp2_long <- gather(column_means_exp2, Question, Workload, -Condition)

# plot column_means_exp1 as a line graph
g4 <- ggplot(column_means_exp1_long, aes(x = Condition, y = Workload, group = Question, color = Question)) +
  geom_line() +
  geom_point() +
  theme_classic() +
  labs(title = "Workload by Condition in Experiment 1",
       x = "Condition",
       y = "Workload") +
  scale_x_discrete(labels = c("Full Vision + Haptic", "Haptic", "Full Vision", "Nothing")) +
  ylim(1, 10)

print(g4)

ggsave(here("output", "NASA-TLX", "workload_by_condition_exp1.png"), g4, width = 10, height = 6, units = "in")

# plot column_means_exp2 as a line graph
g5 <- ggplot(column_means_exp2_long, aes(x = Condition, y = Workload, group = Question, color = Question)) +
  geom_line() +
  geom_point() +
  theme_classic() +
  labs(title = "Workload by Condition in Experiment 2",
       x = "Condition",
       y = "Workload") +
  scale_x_discrete(labels = c("Full Vision + Haptic", "Haptic", "Full Vision", "Nothing")) +
  ylim(1, 10)

print(g5)

ggsave(here("output", "NASA-TLX", "workload_by_condition_exp2.png"), g5, width = 10, height = 6, units = "in")

