# map figures

print("Running map_figures.R to update figures")

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
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
          axis.line = element_blank(), # Remove axis lines
          axis.ticks = element_blank()) # Remove axis ticks
  print(g1)
  # save map
  ggsave(filename = paste0("figure_", sub("\\.csv$", "", f), ".png"), plot = g1, path = here('data', 'processed', 'maps'), width = 5, height = 5)
  # clear map
  rm(map)
}