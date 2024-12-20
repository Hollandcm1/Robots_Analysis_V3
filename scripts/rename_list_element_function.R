# rename element

rename_list_element <- function(list, old_name, new_name) {
  if (old_name %in% names(list)) {
    names(list)[names(list) == old_name] <- new_name
  } else {
    warning(paste("No element named", old_name, "found in the list."))
  }
  return(list)
}


# Example usage:
# my_list <- list(oldName = 123, anotherElement = 456)
# my_list <- rename_list_element(my_list, "oldName", "newName")
# print(my_list)