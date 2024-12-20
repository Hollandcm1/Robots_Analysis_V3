# Flatten data

# load compiled data
load(here('data', 'processed', 'compiled', 'compiled_data.RData'))
warning('Using compiled data instead of cleaned data')

# flatten data
flattened_list <- list()

for (p_num in names(compiled_data)) {
  for (base in names(compiled_data[[p_num]])) {
    flattened_list[[paste(p_num, base, sep = "_")]] <- compiled_data[[p_num]][[base]]
  }
}

flatted_data <- flattened_list

#save flattened data
save(flatted_data, file = here("data", "processed", "flattened", "flatted_data.RData"))
