# Create Combined Figure# # libraries


point_size = 0.5


library(ggplot2)
library(here)
library(ggbeeswarm)
library(RColorBrewer)
library(patchwork)

# load the data
data <- read.csv(here('output', 'average_by_conditions_all_data.csv'))
codes <- tar_read('codes_conditions')
codes_participant_conditions_exp1 <- tar_read('codes_participant_conditions')
codes_participant_conditions_exp2 <- tar_read('codes_participant_conditions_exp2')

# transpose the codes
codes <- t(codes)

# use 'Factor' row as column labels
colnames(codes) <- codes["Factor",]

# remove the 'Factor' row
codes <- codes[-1,]

# convert to dataframe
codes <- as.data.frame(codes)

# relabel visual, where 1 = Full Vision and 0 = Flickering Vision in data
data$visual <- factor(data$visual, levels = c(0, 1), labels = c("Flickering Vision", "Full Vision"))

# relabel haptic, where 1 = Haptic and 0 = No Haptic in data
data$haptic <- factor(data$haptic, levels = c(0, 1), labels = c("No Haptic Feedback", "Haptic Feedback"))

##########################################################
####################### GreyScale Option 1 ###############
##########################################################

# # Define a grayscale fill color palette
# grayscale_palette <- c("gray30", "gray70", "gray90")
# 
# g1 <- ggplot(means, aes(x=visual, y=mean, fill=haptic)) +
#   geom_bar(stat="identity", position=position_dodge2(padding=0.05), color="black") + 
#   scale_fill_manual(values=grayscale_palette) + 
#   geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position=position_dodge2(padding=0.7), size=1, linetype="dashed") + 
#   theme_classic() + 
#   labs(title="Average Time Through Maze by Condition (Experiment 1)", x="Condition", y="Average Time Through Maze (seconds)") + 
#   guides(fill=guide_legend(title=NULL)) + 
#   theme(legend.position='none', legend.justification=c(0.5, 1)) + 
#   geom_quasirandom(data=data, aes(x=visual_jitter, y=average_time_through_maze, shape=haptic), alpha=0.7, size=3, width=0.15) + 
#   scale_shape_manual(values=c(21, 22, 23)) + 
#   ylim(0, 400)

##########################################################
##########################################################
##########################################################


##########################################################
####################### GreyScale Option 2 ###############
##########################################################

# # Use a colorblind-friendly palette that converts well to grayscale
# color_palette <- brewer.pal(n = 3, name = "Set1")
# 
# g1 <- ggplot(means, aes(x=visual, y=mean, fill=haptic)) +
#   geom_bar(stat="identity", position=position_dodge2(padding=0.05), color="black") +
#   scale_fill_manual(values=color_palette) +
#   geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position=position_dodge2(padding=0.7), linewidth=1) +
#   theme_classic() +
#   labs(title="Average Time Through Maze by Condition (Experiment 1)", x="Condition", y="Average Time Through Maze (seconds)") +
#   guides(fill=guide_legend(title=NULL)) + 
#   theme(legend.position='none', legend.justification=c(0.5, 1)) + 
#   geom_quasirandom(data=data, aes(x=visual_jitter, y=average_time_through_maze), alpha=0.7, width=0.15) + 
#   ylim(0, 400)
# 
# print(g1)

##########################################################
##########################################################
##########################################################


################################################################################
#################################### Exp 1 #####################################
################################################################################


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


##############################################
### Renaming and Selecting Subsets of Data ###
##############################################

# make new variable that contains all data
data_all <- data

# create a subset variable that only has data from experiment == 1
data_exp1 <- subset(data, experiment == 1)

# create a subset variable that only has data from experiment == 2
data_exp2 <- subset(data, experiment == 2)





################################################################################
#################################### Exp 2 #####################################
################################################################################


########################################
### Time Through Maze (Bar + Points) ### Exp 2
########################################

data <- data_exp2

# calculate the mean and standard deviation for each condition
means <- aggregate(average_time_through_maze ~ condition_nums, data=data, mean)
sds <- aggregate(average_time_through_maze ~ condition_nums, data=data, sd)
sample_size <- aggregate(average_time_through_maze ~ condition_nums, data=data, length)
se <- aggregate(average_time_through_maze ~ condition_nums, data=data, function(x) sd(x)/sqrt(length(x)))

# merge the means and sds
means <- merge(means, sds, by="condition_nums")
means <- merge(means, sample_size, by="condition_nums")
means <- merge(means, se, by="condition_nums")

# fix means df
means <- fix_means_df(means, codes)

# create the scatter plot jitter
data$visual_jitter <- as.numeric(data$visual) + 
  ifelse(data$haptic == "Haptic Feedback", 0.2, 
         ifelse(data$haptic == "No Haptic Feedback", -0.2, -0.2))

g6 <- ggplot(means, aes(x=visual, y=mean, fill=haptic)) +
  geom_bar(stat="identity", position = position_dodge2(padding = 0.05)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position = position_dodge2(padding = 0.7), linewidth=1) +
  theme_classic() +
  labs(title="Average Time Through Maze by Condition ", x="Condition", y="Average Time Through Maze (seconds)") +
  guides(fill=guide_legend(title=NULL)) + 
  # move legend
  theme(legend.position = 'none', # adjust these values as needed
        legend.justification = c(0.5, 1)) +
  # add scatter plot data on top of this
  geom_quasirandom(data=data, aes(x=visual_jitter, y=average_time_through_maze), 
                   alpha=0.7, cex=point_size, width = 0.15) +
  ylim(0, 400)

print(g6)

ggsave(g6, file = here('output', 'standard_figures', 'average_time_through_maze_exp_2.png'), width=6, height=8)



##################################
### Path Length (Bar + Points) ### Exp 2
##################################

data <- data_exp2

# calculate the mean and standard deviation for each condition
means <- aggregate(average_path_length ~ condition_nums, data=data, mean)
sds <- aggregate(average_path_length ~ condition_nums, data=data, sd)
sample_size <- aggregate(average_path_length ~ condition_nums, data=data, length)
se <- aggregate(average_path_length ~ condition_nums, data=data, function(x) sd(x)/sqrt(length(x)))

# merge the means and sds
means <- merge(means, sds, by="condition_nums")
means <- merge(means, sample_size, by="condition_nums")
means <- merge(means, se, by="condition_nums")

# fix means df
means <- fix_means_df(means, codes)

# create the scatter plot jitter
data$visual_jitter <- as.numeric(data$visual) + 
  ifelse(data$haptic == "Haptic Feedback", 0.2, 
         ifelse(data$haptic == "No Haptic Feedback", -0.2, -0.2))

g7 <- ggplot(means, aes(x=visual, y=mean, fill=haptic)) +
  geom_bar(stat="identity", position = position_dodge2(padding = 0.05)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position = position_dodge2(padding = 0.7), linewidth=1) +
  theme_classic() +
  labs(title="Average Path Length by Condition ", x="Condition", y="Average Path Length (units)") +
  guides(fill=guide_legend(title=NULL)) + 
  # move legend
  theme(legend.position = 'none', # adjust these values as needed
        legend.justification = c(0.5, 1)) +
  # add scatter plot data on top of this
  geom_quasirandom(data=data, aes(x=visual_jitter, y=average_path_length), 
                   alpha=0.7, cex=point_size, size=point_size, width = 0.15) +
  ylim(0, 60)

print(g7)

ggsave(g7, file = here('output', 'standard_figures', 'average_path_length_exp_2.png'), width=6, height=8)


###############################
### Velocity (Bar + Points) ### Exp 2
###############################

data <- data_exp2

# calculate the mean and standard deviation for each condition
means <- aggregate(average_velocity ~ condition_nums, data=data, mean)
sds <- aggregate(average_velocity ~ condition_nums, data=data, sd)
sample_size <- aggregate(average_velocity ~ condition_nums, data=data, length)
se <- aggregate(average_velocity ~ condition_nums, data=data, function(x) sd(x)/sqrt(length(x)))

# merge the means and sds
means <- merge(means, sds, by="condition_nums")
means <- merge(means, sample_size, by="condition_nums")
means <- merge(means, se, by="condition_nums")

# fix means df
means <- fix_means_df(means, codes)

# create the scatter plot jitter
data$visual_jitter <- as.numeric(data$visual) + 
  ifelse(data$haptic == "Haptic Feedback", 0.2, 
         ifelse(data$haptic == "No Haptic Feedback", -0.2, -0.2))

g8 <- ggplot(means, aes(x=visual, y=mean, fill=haptic)) +
  geom_bar(stat="identity", position = position_dodge2(padding = 0.05)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position = position_dodge2(padding = 0.7), linewidth=1) +
  theme_classic() +
  labs(title="Average Velocity by Condition ", x="Condition", y="Average Velocity (units/second)") +
  guides(fill=guide_legend(title=NULL)) + 
  # move legend
  theme(legend.position = 'none', # adjust these values as needed
        legend.justification = c(0.5, 1)) +
  # add scatter plot data on top of this
  geom_quasirandom(data=data, aes(x=visual_jitter, y=average_velocity), 
                   alpha=0.7, cex=point_size, size=point_size,,width = 0.15) +
  ylim(0, 1.2)

print(g8)

ggsave(g8, file = here('output', 'standard_figures', 'average_velocity_exp_2.png'), width=6, height=8)



################################
### Proximity (Bar + Points) ### Exp 2
################################

data <- data_exp2

# calculate the mean and standard deviation for each condition
means <- aggregate(average_proximity ~ condition_nums, data=data, mean)
sds <- aggregate(average_proximity ~ condition_nums, data=data, sd)
sample_size <- aggregate(average_proximity ~ condition_nums, data=data, length)
se <- aggregate(average_proximity ~ condition_nums, data=data, function(x) sd(x)/sqrt(length(x)))

# merge the means and sds
means <- merge(means, sds, by="condition_nums")
means <- merge(means, sample_size, by="condition_nums")
means <- merge(means, se, by="condition_nums")

# fix means df
means <- fix_means_df(means, codes)

# create the scatter plot jitter
data$visual_jitter <- as.numeric(data$visual) + 
  ifelse(data$haptic == "Haptic Feedback", 0.2, 
         ifelse(data$haptic == "No Haptic Feedback", -0.2, -0.2))

g9 <- ggplot(means, aes(x=visual, y=mean, fill=haptic)) +
  geom_bar(stat="identity", position = position_dodge2(padding = 0.05)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position = position_dodge2(padding = 0.7), linewidth=1) +
  theme_classic() +
  labs(title="Average Proximity by Condition ", x="Condition", y="Average Proximity (units)") +
  guides(fill=guide_legend(title=NULL)) + 
  # move legend
  theme(legend.position = 'none', # adjust these values as needed
        legend.justification = c(0.5, 1)) +
  # add scatter plot data on top of this
  geom_quasirandom(data=data, aes(x=visual_jitter, y=average_proximity), 
                   alpha=0.7, cex=point_size, size=point_size,,width = 0.15) +
  ylim(0, 1)

print(g9)

ggsave(g9, file = here('output', 'standard_figures', 'average_proximity_exp_2.png'), width=6, height=8)



############################
### Force (Bar + Points) ### Exp 2
############################

data <- data_exp2

# calculate the mean and standard deviation for each condition
means <- aggregate(average_force ~ condition_nums, data=data, mean)
sds <- aggregate(average_force ~ condition_nums, data=data, sd)
sample_size <- aggregate(average_force ~ condition_nums, data=data, length)
se <- aggregate(average_force ~ condition_nums, data=data, function(x) sd(x)/sqrt(length(x)))

# merge the means and sds
means <- merge(means, sds, by="condition_nums")
means <- merge(means, sample_size, by="condition_nums")
means <- merge(means, se, by="condition_nums")

# fix means df
means <- fix_means_df(means, codes)

# create the scatter plot jitter
data$visual_jitter <- as.numeric(data$visual) + 
  ifelse(data$haptic == "Haptic Feedback", 0.2, 
         ifelse(data$haptic == "No Haptic Feedback", -0.2, -0.2))

g10 <- ggplot(means, aes(x=visual, y=mean, fill=haptic)) +
  geom_bar(stat="identity", position = position_dodge2(padding = 0.05)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position = position_dodge2(padding = 0.7), linewidth=1) +
  theme_classic() +
  labs(title="Average Force by Condition ", x="Condition", y="Average Force (N)") +
  guides(fill=guide_legend(title=NULL)) + 
  # move legend
  theme(legend.position = 'none', # adjust these values as needed
        legend.justification = c(0.5, 1)) +
  # add scatter plot data on top of this
  geom_quasirandom(data=data, aes(x=visual_jitter, y=average_force), 
                   alpha=0.7, cex=point_size, size=point_size,,width = 0.15) +
  ylim(0, 1.7)

print(g10)

ggsave(g10, file = here('output', 'standard_figures', 'average_force_exp_2.png'), width=6, height=8)



################################################################################
##################################### Workload #################################
################################################################################


# load data
data <- read.csv(here("data", "NASA-TLX", "Workload_exp_1_and_2.csv"))
#codes_participant_conditions_exp1 <- tar_read('codes_participant_conditions')
#codes_participant_conditions_exp2 <- tar_read('codes_participant_conditions_exp2')

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

# save
write.csv(data, here("output", "workload_data.csv"))

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


###############################
### Workload (Bar + Points) ### Exp 1
###############################

data <- data_exp1

# calculate the mean and standard deviation for each condition
means <- aggregate(WorkloadSum ~ Condition, data=data, mean)
sds <- aggregate(WorkloadSum ~ Condition, data=data, sd)
sample_size <- aggregate(WorkloadSum ~ Condition, data=data, length)
se <- aggregate(WorkloadSum ~ Condition, data=data, function(x) sd(x)/sqrt(length(x)))

# merge the means and sds
means <- merge(means, sds, by="Condition")
means <- merge(means, sample_size, by="Condition")
means <- merge(means, se, by="Condition")

# fix means df
means <- fix_means_df(means, codes)

# create the scatter plot jitter
data$Visual_jitter <- as.numeric(data$visual) + 
  ifelse(data$haptic == "Haptic Feedback", 0.2, 
         ifelse(data$haptic == "No Haptic Feedback", -0.2, -0.2))

g11 <- ggplot(means, aes(x=visual, y=mean, fill=haptic)) +
  geom_bar(stat="identity", position = position_dodge2(padding = 0.05)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position = position_dodge2(padding = 0.7), linewidth=1) +
  theme_classic() +
  labs(title="Average Workload by Condition (Experiment 1)", x="Condition", y="Average Workload") +
  guides(fill=guide_legend(title=NULL)) + 
  # move legend
  theme(legend.position = 'none', # adjust these values as needed
        legend.justification = c(0.5, 1)) +
  # add scatter plot data on top of this
  geom_quasirandom(data=data, aes(x=Visual_jitter, y=WorkloadSum), 
                   alpha=0.7, cex=point_size, size=point_size,,width = 0.15) +
  ylim(0, 60)

print(g11)

ggsave(g11, file = here('output', 'standard_figures', 'average_workload_exp_1.png'), width=6, height=8)



###############################
### Workload (Bar + Points) ### Exp 2
###############################

data <- data_exp2

# calculate the mean and standard deviation for each condition
means <- aggregate(WorkloadSum ~ Condition, data=data, mean)
sds <- aggregate(WorkloadSum ~ Condition, data=data, sd)
sample_size <- aggregate(WorkloadSum ~ Condition, data=data, length)
se <- aggregate(WorkloadSum ~ Condition, data=data, function(x) sd(x)/sqrt(length(x)))

# merge the means and sds
means <- merge(means, sds, by="Condition")
means <- merge(means, sample_size, by="Condition")
means <- merge(means, se, by="Condition")

# fix means df
means <- fix_means_df(means, codes)

# create the scatter plot jitter
data$Visual_jitter <- as.numeric(data$visual) + 
  ifelse(data$haptic == "Haptic Feedback", 0.2, 
         ifelse(data$haptic == "No Haptic Feedback", -0.2, -0.2))

g12 <- ggplot(means, aes(x=visual, y=mean, fill=haptic)) +
  geom_bar(stat="identity", position = position_dodge2(padding = 0.05)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position = position_dodge2(padding = 0.7), linewidth=1) +
  theme_classic() +
  labs(title="Average Workload by Condition ", x="Condition", y="Average Workload") +
  guides(fill=guide_legend(title=NULL)) + 
  # move legend
  theme(legend.position = 'none', # adjust these values as needed
        legend.justification = c(0.5, 1)) +
  # add scatter plot data on top of this
  geom_quasirandom(data=data, aes(x=Visual_jitter, y=WorkloadSum), 
                   alpha=0.7, cex=point_size, size=point_size,,width = 0.15) +
  ylim(0, 60)

print(g12)

ggsave(g12, file = here('output', 'standard_figures', 'average_workload_exp_2.png'), width=6, height=8)


combined_plot <- (g6 | g7 | g8) /
  (g9 | g10 | g12)

print(combined_plot)

# save
ggsave(combined_plot, file = here('output', 'standard_figures', 'combined_plot_exp_2.png'), width=16, height=12)




