# Parallel Coords Plot

library(shiny)
library(plotly)
library(dplyr)
library(targets)

# load the data
data <- tar_read(strategic_data_appended)

# convert haptic and visual to numeric
data$haptic <- as.numeric(data$haptic)
data$visual <- as.numeric(data$visual)

# pull just data for participant == 1
data_p1 <- data %>% filter(participant == 1)

# create a list of the columns to be used in the parallel coords plot
cols <- c("trial", "participant", "time", "haptic", "visual", "map", "path_length", "average_force", "average_proximity")

ui <- fluidPage(
  plotlyOutput("parallel_coords")
)

server <- function(input, output, session) {
  
  # Calculate ranges dynamically
  ranges <- data_p1 %>% summarise(
    trial = range(trial),
    participant = range(participant),
    time = range(time),
    haptic = range(haptic),
    visual = range(visual),
    map = range(map),
    path_length = range(path_length),
    average_force = range(average_force),
    average_proximity = range(average_proximity)
  )
  
  output$parallel_coords <- renderPlotly({
    plot_ly(data = data_p1, type = 'parcoords',
            line = list(color = ~time, colorscale = 'Viridis'),  # Color by time using Viridis color scale
            dimensions = list(
              list(range = ranges$trial, label = 'Trial', values = ~trial),
              list(range = ranges$participant, label = 'Participant', values = ~participant),
              list(range = ranges$time, label = 'Time', values = ~time),
              list(range = ranges$haptic, label = 'Haptic', values = ~haptic),
              list(range = ranges$visual, label = 'Visual', values = ~visual),
              list(range = ranges$map, label = 'Map', values = ~map),
              list(range = ranges$path_length, label = 'Path Length', values = ~path_length),
              list(range = ranges$average_force, label = 'Average Force', values = ~average_force),
              list(range = ranges$average_proximity, label = 'Average Proximity', values = ~average_proximity)
            ))
  })
}

shinyApp(ui, server)
