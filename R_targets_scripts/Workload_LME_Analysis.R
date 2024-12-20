# Workload LME Analysis

library(lme4)
library(sjPlot)
library(here)
library(dplyr)
library(flexplot)

# load data
data <- read.csv(here("data", "NASA-TLX", "Workload_exp_1_and_2.csv"))
codes_participant_conditions_exp1 <- tar_read('codes_participant_conditions')
codes_participant_conditions_exp2 <- tar_read('codes_participant_conditions_exp2')


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

# convert to factor
data$experiment <- as.factor(data$experiment)

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



####################
### LME Analysis ###
####################

# pull from unedited data
data <- data_all


# create LME model
model <- lmer(WorkloadSum ~ visual * haptic * experiment + (1 | Participant), data = data)
tab_model(model, show.se = TRUE, show.stat = TRUE)
flexplot(WorkloadSum ~ haptic + experiment, data = data)
p1 <- flexplot(WorkloadSum ~ haptic + experiment, data = data)
flexplot(WorkloadSum ~ experiment + haptic, data = data)
p2 <- flexplot(WorkloadSum ~ visual, data = data)

# save plots
ggsave(here("output", "NASA-TLX", "WorkloadSum_haptic_experiment.png"), p1, width = 10, height = 6)
ggsave(here("output", "NASA-TLX", "WorkloadSum_visual.png"), p2, width = 10, height = 6)


# pull subset of data where experiment == 1
data_exp1 <- data[data$experiment == 1,]
data_exp2 <- data[data$experiment == 2,]


model <- lmer(WorkloadSum ~ visual * haptic + (1|Participant), data = data_exp1)
tab_model(model, 
          show.est = TRUE,         # Show estimates
          show.se = TRUE,          # Show standard errors
          show.stat = TRUE,        # We need to identify what t-value corresponds to
          show.p = TRUE,           # Show p-values
          show.ci = FALSE)  
flexplot(WorkloadSum ~ visual + haptic, data = data_exp1)

model <- lmer(WorkloadSum ~ visual * haptic + (1|Participant), data = data_exp2)
tab_model(model, 
          show.est = TRUE,         # Show estimates
          show.se = TRUE,          # Show standard errors
          show.stat = TRUE,        # We need to identify what t-value corresponds to
          show.p = TRUE,           # Show p-values
          show.ci = FALSE)  
flexplot(WorkloadSum ~ visual + haptic, data = data_exp2)
