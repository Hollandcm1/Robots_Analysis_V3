# Import Codebook

#library(openxlsx)
codebook <- read.xlsx(here("data", "codebook", "Robots - Codebook.xlsx"))

# pull subset relevant for determining what the different conditions were doing
codes.conditions <- codebook[, c("X7", "Cond.1", "Cond.2", "Cond.3", "Cond.4")]
codes.conditions <- codes.conditions[1:2,]
# relabel
names(codes.conditions) <- c("Factor", "Condition 1", "Condition 2", "Condition 3", "Condition 4")

# pull participant information
codes.participant_conditions <- codebook[, c("Participant", "first", "second", "third", "forth")]
names(codes.participant_conditions) <- c("Participant", "First Condition", "Second Condition", "Third Condition", "Forth Condition")

print(codes.conditions)
print(codes.participant_conditions)
