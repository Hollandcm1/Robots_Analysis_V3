# Parallel Coords Plot

library(shiny)
library(plotly)
library(dplyr)
library(targets)

# load the data
data <- tar_read(combined_analysis_results)

# convert haptic and visual to numeric
data$haptic <- as.numeric(data$haptic)
data$visual <- as.numeric(data$visual)

# convert condition_nums to numeric
data$condition_nums <- as.numeric(data$condition_nums)

# create a list of the columns to be used in the parallel coords plot
cols <- c("participant", "experiment", "haptic", "visual", "average_path_length", "average_force", "average_proximity", "condition_nums", "average_time_through_maze")

ui <- fluidPage(
  plotlyOutput("parallel_coords")
)

server <- function(input, output, session) {
  
  # Calculate ranges dynamically
  ranges <- list(
    participant = range(data$participant, na.rm = TRUE),
    experiment = range(data$experiment, na.rm = TRUE),
    haptic = range(data$haptic, na.rm = TRUE),
    visual = range(data$visual, na.rm = TRUE),
    average_path_length = range(data$average_path_length, na.rm = TRUE),
    average_force = range(data$average_force, na.rm = TRUE),
    average_proximity = range(data$average_proximity, na.rm = TRUE),
    condition_nums = range(data$condition_nums, na.rm = TRUE),
    average_time_through_maze = range(data$average_time_through_maze, na.rm = TRUE)
  )
  
  output$parallel_coords <- renderPlotly({
    plot_ly(data = data, type = 'parcoords',
            line = list(
              color = ~condition_nums, 
              colorscale = 'Viridis', 
              showscale = TRUE,  # Show color scale
              colorbar = list(title = "Condition Nums")  # Title for the color scale
            ),  
            dimensions = list(
              list(range = ranges$participant, label = 'Participant', values = ~participant),
              list(range = ranges$experiment, label = 'Experiment', values = ~experiment),
              list(range = ranges$haptic, label = 'Haptic', values = ~haptic),
              list(range = ranges$visual, label = 'Visual', values = ~visual),
              list(range = ranges$average_path_length, label = 'Avg Path Length', values = ~average_path_length),
              list(range = ranges$average_force, label = 'Avg Force', values = ~average_force),
              list(range = ranges$average_proximity, label = 'Avg Proximity', values = ~average_proximity),
              list(range = ranges$condition_nums, label = 'Condition Nums', values = ~condition_nums),
              list(range = ranges$average_time_through_maze, label = 'Avg Time Through Maze', values = ~average_time_through_maze)
            ))
  })
}

shinyApp(ui, server)
