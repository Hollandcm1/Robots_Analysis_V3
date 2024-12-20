
library(ggplot2)
library(here)
library(tidyr)
library(dplyr)
library(ggbeeswarm)


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

# remap experiment column values with 1=Exp1 and 2=Exp2
data_all$experiment <- factor(data$experiment, levels = c(1, 2), labels = c("Exp1", "Exp2"))

####################
### Experiment 1 ###
####################

data <- data_exp1

# create the scatter plot jitter
data$Visual_jitter <- as.numeric(data$visual) + 
  ifelse(data$haptic == "Haptic Feedback", 0.2, 
         ifelse(data$haptic == "No Haptic Feedback", -0.2, -0.2))

##########
### Q3 ###
##########

# calculate the mean and standard deviation for each condition
means <- aggregate(Q3 ~ Condition, data=data, mean)
sds <- aggregate(Q3 ~ Condition, data=data, sd)
sample_size <- aggregate(Q3 ~ Condition, data=data, length)
se <- aggregate(Q3 ~ Condition, data=data, function(x) sd(x)/sqrt(length(x)))

# merge the means and sds
means <- merge(means, sds, by="Condition")
means <- merge(means, sample_size, by="Condition")
means <- merge(means, se, by="Condition")

# fix means df
means <- fix_means_df(means, codes)

g1 <- ggplot(means, aes(x=visual, y=mean, fill=haptic)) +
  geom_bar(stat="identity", position = position_dodge2(padding = 0.05)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position = position_dodge2(padding = 0.7), linewidth=1) +
  theme_classic() +
  labs(title="How mentally demanding was the task? (Q3 - Experiment 1)", x="Condition", y="Average Workload") +
  guides(fill=guide_legend(title=NULL)) + 
  # move legend
  theme(legend.position = 'top', # adjust these values as needed
        legend.justification = c(0.5, 1)) +
  # add scatter plot data on top of this
  geom_quasirandom(data=data, aes(x=Visual_jitter, y=Q3), 
                   alpha=0.7, cex=1.8, width = 0.15) +
  ylim(0, 10)

print(g1)

ggsave(g1, file = here('output', 'NASA-TLX', 'Q3_exp_1.png'), width=6, height=8)


##########
### Q4 ###
##########

# Calculate the mean and standard deviation for each condition
means <- aggregate(Q4 ~ Condition, data=data, mean)
sds <- aggregate(Q4 ~ Condition, data=data, sd)
sample_size <- aggregate(Q4 ~ Condition, data=data, length)
se <- aggregate(Q4 ~ Condition, data=data, function(x) sd(x)/sqrt(length(x)))

# merge the means and sds
means <- merge(means, sds, by="Condition")
means <- merge(means, sample_size, by="Condition")
means <- merge(means, se, by="Condition")

# fix means df
means <- fix_means_df(means, codes)

g2 <- ggplot(means, aes(x=visual, y=mean, fill=haptic)) +
  geom_bar(stat="identity", position = position_dodge2(padding = 0.05)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position = position_dodge2(padding = 0.7), linewidth=1) +
  theme_classic() +
  labs(title="How physically demanding was the task? (Q4 - Experiment 1)", x="Condition", y="Average Workload") +
  guides(fill=guide_legend(title=NULL)) + 
  # move legend
  theme(legend.position = 'top', # adjust these values as needed
        legend.justification = c(0.5, 1)) +
  # add scatter plot data on top of this
  geom_quasirandom(data=data, aes(x=Visual_jitter, y=Q4), 
                   alpha=0.7, cex=1.8, width = 0.15) +
  ylim(0, 10)

print(g2)

ggsave(g2, file = here('output', 'NASA-TLX', 'Q4_exp_1.png'), width=6, height=8)


##########
### Q5 ###
##########

# Calculate the mean and standard deviation for each condition
means <- aggregate(Q5 ~ Condition, data=data, mean)
sds <- aggregate(Q5 ~ Condition, data=data, sd)
sample_size <- aggregate(Q5 ~ Condition, data=data, length)
se <- aggregate(Q5 ~ Condition, data=data, function(x) sd(x)/sqrt(length(x)))

# merge the means and sds
means <- merge(means, sds, by="Condition")
means <- merge(means, sample_size, by="Condition")
means <- merge(means, se, by="Condition")

# fix means df
means <- fix_means_df(means, codes)

g3 <- ggplot(means, aes(x=visual, y=mean, fill=haptic)) +
  geom_bar(stat="identity", position = position_dodge2(padding = 0.05)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position = position_dodge2(padding = 0.7), linewidth=1) +
  theme_classic() +
  labs(title="How hurried or rushed was the pace of the task? (Q5 - Experiment 1)", x="Condition", y="Average Workload") +
  guides(fill=guide_legend(title=NULL)) + 
  # move legend
  theme(legend.position = 'top', # adjust these values as needed
        legend.justification = c(0.5, 1)) +
  # add scatter plot data on top of this
  geom_quasirandom(data=data, aes(x=Visual_jitter, y=Q5), 
                   alpha=0.7, cex=1.8, width = 0.15) +
  ylim(0, 10)

print(g3)

ggsave(g3, file = here('output', 'NASA-TLX', 'Q5_exp_1.png'), width=6, height=8)


##########
### Q6 ###
##########

# Calculate the mean and standard deviation for each condition
means <- aggregate(Q6 ~ Condition, data=data, mean)
sds <- aggregate(Q6 ~ Condition, data=data, sd)
sample_size <- aggregate(Q6 ~ Condition, data=data, length)
se <- aggregate(Q6 ~ Condition, data=data, function(x) sd(x)/sqrt(length(x)))

# merge the means and sds
means <- merge(means, sds, by="Condition")
means <- merge(means, sample_size, by="Condition")
means <- merge(means, se, by="Condition")

# fix means df
means <- fix_means_df(means, codes)

g4 <- ggplot(means, aes(x=visual, y=mean, fill=haptic)) +
  geom_bar(stat="identity", position = position_dodge2(padding = 0.05)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position = position_dodge2(padding = 0.7), linewidth=1) +
  theme_classic() +
  labs(title="How successful were you in accomplishing what you were asked to do? (Q6 - Experiment 1)", x="Condition", y="Average Workload") +
  guides(fill=guide_legend(title=NULL)) + 
  # move legend
  theme(legend.position = 'top', # adjust these values as needed
        legend.justification = c(0.5, 1)) +
  # add scatter plot data on top of this
  geom_quasirandom(data=data, aes(x=Visual_jitter, y=Q6), 
                   alpha=0.7, cex=1.8, width = 0.15) +
  ylim(0, 10)

print(g4)

ggsave(g4, file = here('output', 'NASA-TLX', 'Q6_exp_1.png'), width=6, height=8)


##########
### Q7 ###
##########

# Calculate the mean and standard deviation for each condition
means <- aggregate(Q7 ~ Condition, data=data, mean)
sds <- aggregate(Q7 ~ Condition, data=data, sd)
sample_size <- aggregate(Q7 ~ Condition, data=data, length)
se <- aggregate(Q7 ~ Condition, data=data, function(x) sd(x)/sqrt(length(x)))

# merge the means and sds
means <- merge(means, sds, by="Condition")
means <- merge(means, sample_size, by="Condition")
means <- merge(means, se, by="Condition")

# fix means df
means <- fix_means_df(means, codes)

g5 <- ggplot(means, aes(x=visual, y=mean, fill=haptic)) +
  geom_bar(stat="identity", position = position_dodge2(padding = 0.05)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position = position_dodge2(padding = 0.7), linewidth=1) +
  theme_classic() +
  labs(title="How hard did you have to work to accomplish your level of performance? (Q7 - Experiment 1)", x="Condition", y="Average Workload") +
  guides(fill=guide_legend(title=NULL)) + 
  # move legend
  theme(legend.position = 'top', # adjust these values as needed
        legend.justification = c(0.5, 1)) +
  # add scatter plot data on top of this
  geom_quasirandom(data=data, aes(x=Visual_jitter, y=Q7), 
                   alpha=0.7, cex=1.8, width = 0.15) +
  ylim(0, 10)

print(g5)

ggsave(g5, file = here('output', 'NASA-TLX', 'Q7_exp_1.png'), width=6, height=8)


##########
### Q8 ###
##########

# Calculate the mean and standard deviation for each condition
means <- aggregate(Q8 ~ Condition, data=data, mean)
sds <- aggregate(Q8 ~ Condition, data=data, sd)
sample_size <- aggregate(Q8 ~ Condition, data=data, length)
se <- aggregate(Q8 ~ Condition, data=data, function(x) sd(x)/sqrt(length(x)))

# merge the means and sds
means <- merge(means, sds, by="Condition")
means <- merge(means, sample_size, by="Condition")
means <- merge(means, se, by="Condition")

# fix means df
means <- fix_means_df(means, codes)

g6 <- ggplot(means, aes(x=visual, y=mean, fill=haptic)) +
  geom_bar(stat="identity", position = position_dodge2(padding = 0.05)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position = position_dodge2(padding = 0.7), linewidth=1) +
  theme_classic() +
  labs(title="How insecure, discouraged, irritated, stressed, and annoyed were you? ( Q8 - Experiment 1)", x="Condition", y="Average Workload") +
  guides(fill=guide_legend(title=NULL)) + 
  # move legend
  theme(legend.position = 'top', # adjust these values as needed
        legend.justification = c(0.5, 1)) +
  # add scatter plot data on top of this
  geom_quasirandom(data=data, aes(x=Visual_jitter, y=Q8), 
                   alpha=0.7, cex=1.8, width = 0.15) +
  ylim(0, 10)

print(g6)

ggsave(g6, file = here('output', 'NASA-TLX', 'Q8_exp_1.png'), width=6, height=8)


####################
### Experiment 2 ###
####################

data <- data_exp2

# create the scatter plot jitter
data$Visual_jitter <- as.numeric(data$visual) + 
  ifelse(data$haptic == "Haptic Feedback", 0.2, 
         ifelse(data$haptic == "No Haptic Feedback", -0.2, -0.2))

##########
### Q3 ###
##########

# Calculate the mean and standard deviation for each condition
means <- aggregate(Q3 ~ Condition, data=data, mean)
sds <- aggregate(Q3 ~ Condition, data=data, sd)
sample_size <- aggregate(Q3 ~ Condition, data=data, length)
se <- aggregate(Q3 ~ Condition, data=data, function(x) sd(x)/sqrt(length(x)))

# merge the means and sds
means <- merge(means, sds, by="Condition")
means <- merge(means, sample_size, by="Condition")
means <- merge(means, se, by="Condition")

# fix means df
means <- fix_means_df(means, codes)

g1 <- ggplot(means, aes(x=visual, y=mean, fill=haptic)) +
  geom_bar(stat="identity", position = position_dodge2(padding = 0.05)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position = position_dodge2(padding = 0.7), linewidth=1) +
  theme_classic() +
  labs(title="How mentally demanding was the task? (Q3 - Experiment 2)", x="Condition", y="Average Workload") +
  guides(fill=guide_legend(title=NULL)) + 
  # move legend
  theme(legend.position = 'top', # adjust these values as needed
        legend.justification = c(0.5, 1)) +
  # add scatter plot data on top of this
  geom_quasirandom(data=data, aes(x=Visual_jitter, y=Q3), 
                   alpha=0.7, cex=1.8, width = 0.15) +
  ylim(0, 10)

print(g1)

ggsave(g1, file = here('output', 'NASA-TLX', 'Q3_exp_2.png'), width=6, height=8)


##########
### Q4 ###
##########

# Calculate the mean and standard deviation for each condition
means <- aggregate(Q4 ~ Condition, data=data, mean)
sds <- aggregate(Q4 ~ Condition, data=data, sd)
sample_size <- aggregate(Q4 ~ Condition, data=data, length)
se <- aggregate(Q4 ~ Condition, data=data, function(x) sd(x)/sqrt(length(x)))

# merge the means and sds
means <- merge(means, sds, by="Condition")
means <- merge(means, sample_size, by="Condition")
means <- merge(means, se, by="Condition")

# fix means df
means <- fix_means_df(means, codes)

g2 <- ggplot(means, aes(x=visual, y=mean, fill=haptic)) +
  geom_bar(stat="identity", position = position_dodge2(padding = 0.05)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position = position_dodge2(padding = 0.7), linewidth=1) +
  theme_classic() +
  labs(title="How physically demanding was the task? (Q4 - Experiment 2)", x="Condition", y="Average Workload") +
  guides(fill=guide_legend(title=NULL)) + 
  # move legend
  theme(legend.position = 'top', # adjust these values as needed
        legend.justification = c(0.5, 1)) +
  # add scatter plot data on top of this
  geom_quasirandom(data=data, aes(x=Visual_jitter, y=Q4), 
                   alpha=0.7, cex=1.8, width = 0.15) +
  ylim(0, 10)

print(g2)

ggsave(g2, file = here('output', 'NASA-TLX', 'Q4_exp_2.png'), width=6, height=8)


##########
### Q5 ###
##########

# Calculate the mean and standard deviation for each condition
means <- aggregate(Q5 ~ Condition, data=data, mean)
sds <- aggregate(Q5 ~ Condition, data=data, sd)
sample_size <- aggregate(Q5 ~ Condition, data=data, length)
se <- aggregate(Q5 ~ Condition, data=data, function(x) sd(x)/sqrt(length(x)))

# merge the means and sds
means <- merge(means, sds, by="Condition")
means <- merge(means, sample_size, by="Condition")
means <- merge(means, se, by="Condition")

# fix means df
means <- fix_means_df(means, codes)

g3 <- ggplot(means, aes(x=visual, y=mean, fill=haptic)) +
  geom_bar(stat="identity", position = position_dodge2(padding = 0.05)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position = position_dodge2(padding = 0.7), linewidth=1) +
  theme_classic() +
  labs(title="How hurried or rushed was the pace of the task? (Q5 - Experiment 2)", x="Condition", y="Average Workload") +
  guides(fill=guide_legend(title=NULL)) + 
  # move legend
  theme(legend.position = 'top', # adjust these values as needed
        legend.justification = c(0.5, 1)) +
  # add scatter plot data on top of this
  geom_quasirandom(data=data, aes(x=Visual_jitter, y=Q5), 
                   alpha=0.7, cex=1.8, width = 0.15) +
  ylim(0, 10)

print(g3)

ggsave(g3, file = here('output', 'NASA-TLX', 'Q5_exp_2.png'), width=6, height=8)


##########
### Q6 ###
##########

# Calculate the mean and standard deviation for each condition
means <- aggregate(Q6 ~ Condition, data=data, mean)
sds <- aggregate(Q6 ~ Condition, data=data, sd)
sample_size <- aggregate(Q6 ~ Condition, data=data, length)
se <- aggregate(Q6 ~ Condition, data=data, function(x) sd(x)/sqrt(length(x)))

# merge the means and sds
means <- merge(means, sds, by="Condition")
means <- merge(means, sample_size, by="Condition")
means <- merge(means, se, by="Condition")

# fix means df
means <- fix_means_df(means, codes)

g4 <- ggplot(means, aes(x=visual, y=mean, fill=haptic)) +
  geom_bar(stat="identity", position = position_dodge2(padding = 0.05)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position = position_dodge2(padding = 0.7), linewidth=1) +
  theme_classic() +
  labs(title="How successful were you in accomplishing what you were asked to do? (Q6 - Experiment 2)", x="Condition", y="Average Workload") +
  guides(fill=guide_legend(title=NULL)) + 
  # move legend
  theme(legend.position = 'top', # adjust these values as needed
        legend.justification = c(0.5, 1)) +
  # add scatter plot data on top of this
  geom_quasirandom(data=data, aes(x=Visual_jitter, y=Q6), 
                   alpha=0.7, cex=1.8, width = 0.15) +
  ylim(0, 10)

print(g4)

ggsave(g4, file = here('output', 'NASA-TLX', 'Q6_exp_2.png'), width=6, height=8)


##########
### Q7 ###
##########

# Calculate the mean and standard deviation for each condition
means <- aggregate(Q7 ~ Condition, data=data, mean)
sds <- aggregate(Q7 ~ Condition, data=data, sd)
sample_size <- aggregate(Q7 ~ Condition, data=data, length)
se <- aggregate(Q7 ~ Condition, data=data, function(x) sd(x)/sqrt(length(x)))

# merge the means and sds
means <- merge(means, sds, by="Condition")
means <- merge(means, sample_size, by="Condition")
means <- merge(means, se, by="Condition")

# fix means df
means <- fix_means_df(means, codes)

g5 <- ggplot(means, aes(x=visual, y=mean, fill=haptic)) +
  geom_bar(stat="identity", position = position_dodge2(padding = 0.05)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position = position_dodge2(padding = 0.7), linewidth=1) +
  theme_classic() +
  labs(title="How hard did you have to work to accomplish your level of performance? (Q7 - Experiment 2)", x="Condition", y="Average Workload") +
  guides(fill=guide_legend(title=NULL)) + 
  # move legend
  theme(legend.position = 'top', # adjust these values as needed
        legend.justification = c(0.5, 1)) +
  # add scatter plot data on top of this
  geom_quasirandom(data=data, aes(x=Visual_jitter, y=Q7), 
                   alpha=0.7, cex=1.8, width = 0.15) +
  ylim(0, 10)

print(g5)

ggsave(g5, file = here('output', 'NASA-TLX', 'Q7_exp_2.png'), width=6, height=8)


##########
### Q8 ###
##########

# Calculate the mean and standard deviation for each condition
means <- aggregate(Q8 ~ Condition, data=data, mean)
sds <- aggregate(Q8 ~ Condition, data=data, sd)
sample_size <- aggregate(Q8 ~ Condition, data=data, length)
se <- aggregate(Q8 ~ Condition, data=data, function(x) sd(x)/sqrt(length(x)))

# merge the means and sds
means <- merge(means, sds, by="Condition")
means <- merge(means, sample_size, by="Condition")
means <- merge(means, se, by="Condition")

# fix means df
means <- fix_means_df(means, codes)

g6 <- ggplot(means, aes(x=visual, y=mean, fill=haptic)) +
  geom_bar(stat="identity", position = position_dodge2(padding = 0.05)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position = position_dodge2(padding = 0.7), linewidth=1) +
  theme_classic() +
  labs(title="How insecure, discouraged, irritated, stressed, and annoyed were you? (Q8 - Experiment 2)", x="Condition", y="Average Workload") +
  guides(fill=guide_legend(title=NULL)) + 
  # move legend
  theme(legend.position = 'top', # adjust these values as needed
        legend.justification = c(0.5, 1)) +
  # add scatter plot data on top of this
  geom_quasirandom(data=data, aes(x=Visual_jitter, y=Q8), 
                   alpha=0.7, cex=1.8, width = 0.15) +
  ylim(0, 10)

print(g6)

ggsave(g6, file = here('output', 'NASA-TLX', 'Q8_exp_2.png'), width=6, height=8)


###############################################
### Average by Question and Condition Plots ###
###############################################

# Convert data to long
data_long <- data %>%
  pivot_longer(cols = c(Q3, Q4, Q5, Q6, Q7, Q8), names_to = "Question", values_to = "Score")

g7 <- ggplot(data_long, aes(x=visual, y=Score, fill=haptic)) +
  geom_bar(stat="summary", fun=mean, position = position_dodge2(padding = 0.05)) +
  geom_errorbar(stat="summary", fun.data=mean_se, position = position_dodge2(padding = 2), width=0.9) +
  theme_classic() +
  labs(title="Average Scores by Question and Condition (BOTH EXPERIMENTS AVERAGED)", x="Condition", y="Score") +
  guides(fill=guide_legend(title=NULL)) + 
  # move legend
  theme(legend.position = 'top', # adjust these values as needed
        legend.justification = c(0.5, 1)) +
  facet_wrap(~Question) +
  ylim(0, 10)

print(g7)

ggsave(here("output", "NASA-TLX", "Average_Scores_by_Question_and_Condition.png"), plot = g7, width = 10, height = 6, units = "in", dpi = 300)


##########################################################
### Average by Question, Condition and Experiment Plot ###
##########################################################

# Convert data to long
data_long <- data_all %>%
  pivot_longer(cols = c(Q3, Q4, Q5, Q6, Q7, Q8), names_to = "Question", values_to = "Score")

g8 <- ggplot(data_long, aes(x=visual, y=Score, fill=haptic)) +
  geom_bar(stat="summary", fun=mean, position = position_dodge2(padding = 0.05)) +
  geom_errorbar(stat="summary", fun.data=mean_se, position = position_dodge2(padding = 2), width=0.9) +
  theme_classic() +
  labs(title="Average Scores by Question, Condition, and Experiment", x="Condition", y="Score") +
  guides(fill=guide_legend(title=NULL)) + 
  # move legend
  theme(legend.position = 'top', # adjust these values as needed
        legend.justification = c(0.5, 1)) +
  facet_wrap(~Question + experiment) +
  ylim(0, 10)

print(g8)

ggsave(here("output", "NASA-TLX", "Average_Scores_by_Question_Condition_and_Experiment.png"), plot = g8, width = 10, height = 10, units = "in", dpi = 300)



#######################
### Compound Scales ###
#######################

####################
### Experiment 1 ###
####################

data <- data_exp1

### Cognitive Load - combining Q3, Q5, and Q7 ###
data$Cognitive_Load <- rowMeans(data[, c("Q3", "Q5", "Q7")])

# plot
g9 <- ggplot(data, aes(x=visual, y=Cognitive_Load, fill=haptic)) +
  geom_bar(stat="summary", fun=mean, position = position_dodge2(padding = 0.05)) +
  geom_errorbar(stat="summary", fun.data=mean_se, position = position_dodge2(padding = 2), width=0.9) +
  theme_classic() +
  labs(title="Cognitive Load (Q3, Q5, and Q7 - Experiment 1)", x="Condition", y="Score") +
  guides(fill=guide_legend(title=NULL)) + 
  # move legend
  theme(legend.position = 'top', # adjust these values as needed
        legend.justification = c(0.5, 1)) +
  ylim(0, 10)

print(g9)

ggsave(here("output", "NASA-TLX", "Cognitive_Load_exp1.png"), plot = g9, width = 6, height = 8, units = "in", dpi = 300)


### Physical Load - combining Q4 and Q7 ###
data$Physical_Load <- rowMeans(data[, c("Q4", "Q7")])

# plot
g10 <- ggplot(data, aes(x=visual, y=Physical_Load, fill=haptic)) +
  geom_bar(stat="summary", fun=mean, position = position_dodge2(padding = 0.05)) +
  geom_errorbar(stat="summary", fun.data=mean_se, position = position_dodge2(padding = 2), width=0.9) +
  theme_classic() +
  labs(title="Physical Load (Q4 and Q7 - Experiment 1)", x="Condition", y="Score") +
  guides(fill=guide_legend(title=NULL)) + 
  # move legend
  theme(legend.position = 'top', # adjust these values as needed
        legend.justification = c(0.5, 1)) +
  ylim(0, 10)

print(g10)

ggsave(here("output", "NASA-TLX", "Physical_Load_exp1.png"), plot = g10, width = 6, height = 8, units = "in", dpi = 300)


### Stress Load - combining Q5, Q7, and Q8 ###
data$Stress_Load <- rowMeans(data[, c("Q5", "Q7", "Q8")])

# plot
g11 <- ggplot(data, aes(x=visual, y=Stress_Load, fill=haptic)) +
  geom_bar(stat="summary", fun=mean, position = position_dodge2(padding = 0.05)) +
  geom_errorbar(stat="summary", fun.data=mean_se, position = position_dodge2(padding = 2), width=0.9) +
  theme_classic() +
  labs(title="Stress Load (Q5, Q7, and Q8 - Experiment 1)", x="Condition", y="Score") +
  guides(fill=guide_legend(title=NULL)) + 
  # move legend
  theme(legend.position = 'top', # adjust these values as needed
        legend.justification = c(0.5, 1)) +
  ylim(0, 10)

print(g11)

ggsave(here("output", "NASA-TLX", "Stress_Load_exp1.png"), plot = g11, width = 6, height = 8, units = "in", dpi = 300)


####################
### Experiment 2 ###
####################

data <- data_exp2

### Cognitive Load - combining Q3, Q5, and Q7 ###
data$Cognitive_Load <- rowMeans(data[, c("Q3", "Q5", "Q7")])

# plot
g12 <- ggplot(data, aes(x=visual, y=Cognitive_Load, fill=haptic)) +
  geom_bar(stat="summary", fun=mean, position = position_dodge2(padding = 0.05)) +
  geom_errorbar(stat="summary", fun.data=mean_se, position = position_dodge2(padding = 2), width=0.9) +
  theme_classic() +
  labs(title="Cognitive Load (Q3, Q5, and Q7 - Experiment 2)", x="Condition", y="Score") +
  guides(fill=guide_legend(title=NULL)) + 
  # move legend
  theme(legend.position = 'top', # adjust these values as needed
        legend.justification = c(0.5, 1)) +
  ylim(0, 10)

print(g12)

ggsave(here("output", "NASA-TLX", "Cognitive_Load_exp2.png"), plot = g12, width = 6, height = 8, units = "in", dpi = 300)


### Physical Load - combining Q4 and Q7 ###
data$Physical_Load <- rowMeans(data[, c("Q4", "Q7")])

# plot
g13 <- ggplot(data, aes(x=visual, y=Physical_Load, fill=haptic)) +
  geom_bar(stat="summary", fun=mean, position = position_dodge2(padding = 0.05)) +
  geom_errorbar(stat="summary", fun.data=mean_se, position = position_dodge2(padding = 2), width=0.9) +
  theme_classic() +
  labs(title="Physical Load (Q4 and Q7 - Experiment 2)", x="Condition", y="Score") +
  guides(fill=guide_legend(title=NULL)) + 
  # move legend
  theme(legend.position = 'top', # adjust these values as needed
        legend.justification = c(0.5, 1)) +
  ylim(0, 10)

print(g13)

ggsave(here("output", "NASA-TLX", "Physical_Load_exp2.png"), plot = g13, width = 6, height = 8, units = "in", dpi = 300)


### Stress Load - combining Q5, Q7, and Q8 ###
data$Stress_Load <- rowMeans(data[, c("Q5", "Q7", "Q8")])

# plot
g14 <- ggplot(data, aes(x=visual, y=Stress_Load, fill=haptic)) +
  geom_bar(stat="summary", fun=mean, position = position_dodge2(padding = 0.05)) +
  geom_errorbar(stat="summary", fun.data=mean_se, position = position_dodge2(padding = 2), width=0.9) +
  theme_classic() +
  labs(title="Stress Load (Q5, Q7, and Q8 - Experiment 2)", x="Condition", y="Score") +
  guides(fill=guide_legend(title=NULL)) + 
  # move legend
  theme(legend.position = 'top', # adjust these values as needed
        legend.justification = c(0.5, 1)) +
  ylim(0, 10)

print(g14)

ggsave(here("output", "NASA-TLX", "Stress_Load_exp2.png"), plot = g14, width = 6, height = 8, units = "in", dpi = 300)


