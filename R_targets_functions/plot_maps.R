#' @title map_figures.R
#' @description This script creates figures from the maps in the processed data folder
#' @return NULL
#' @examples
#' plot_maps()
#' @importFrom here here
#' @import ggplot2
#' @import ggsave
#' @export

plot_maps <- function(maps) {
  
  # this is jank to make the _targets work
  # maps <- NULL
  
  # create path strings
  maps_filepath <- here('data', 'processed', 'maps')
  filenames <- list.files(maps_filepath, pattern = "\\.csv$")
  
  for (f in filenames) {
    # pull map
    map <- read.csv(here('data', 'processed', 'maps', f))
    names(map) <- c("X", "Y", "Size")
    # plot map
    g1 <- ggplot(map, aes(x = X, y = Y)) +
      geom_point() +
      theme_classic() +
      labs(title = sub("\\.csv$", "", f)) +
      xlim(-10, 10) + 
      ylim(-10, 10) +
      theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
            axis.line = element_blank(), # Remove axis lines
            axis.ticks = element_blank()) # Remove axis ticks
    # save map
    ggsave(filename = paste0("figure_", sub("\\.csv$", "", f), ".png"), plot = g1, path = here('data', 'processed', 'maps'), width = 5, height = 5)
    # clear map
    rm(map)
  }
  
  return(Sys.time())
  
}