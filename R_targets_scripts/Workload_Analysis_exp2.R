# workload analysis

library(here)
library(ggplot2)

# load data
data <- read.csv(here("data", "NASA-TLX", "Workload_exp_1_and_2.csv"))
codes_participant_conditions <- tar_read(codes_participant_conditions_exp2)

# remove participants 1-25
data <- data[!data$Q1 %in% 1:25,]

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

# plot the data 
ggplot(data, aes(x = Condition, y = Workload100, fill = Haptic)) +
  geom_boxplot() +
  geom_jitter(width = 0.1) +
  labs(title = "Workload by Condition", x = "Condition", y = "Workload (out of 100)") +
  theme_classic()

# make the same figure, but where to order of conditions of the x axis is
data$Condition <- factor(data$Condition, levels = c(4, 2, 3, 1))
ggplot(data, aes(x = Condition, y = WorkloadSum, fill = Haptic)) +
  geom_boxplot() +
  geom_jitter(width = 0.1) +
  labs(title = "Workload by Condition", x = "Condition", y = "Workload (out of 60)") +
  theme_classic() +
  scale_x_discrete(labels = c("Flickering Vision", "Flickering Vision + Haptic", "Full Vision", "Full Vision + Haptic")) +
  ylim(0, 60)

# strip columns "RespondentId", "StartDate", "CompletedDate", "LanguageCode"
data$RespondentId <- NULL
data$StartDate <- NULL
data$CompletedDate <- NULL
data$LanguageCode <- NULL

# save the data
write.csv(data, here("output", "workload_data_exp2.csv"))



