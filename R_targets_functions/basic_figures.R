# # Basic Figure Visuals
# 
# data <- data_long
# 
# # Create a basic figure
# p1 <- ggplot(data, aes(x=participant, y=linear_velocity)) +
#   geom_jitter(height=0, width=0.1, alpha=0.05) +
#   labs(title="Linear Velocity by Participant",
#        x="Participant",
#        y="Linear Velocity") +
#   theme_minimal()
# p1
