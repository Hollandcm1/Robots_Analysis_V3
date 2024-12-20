#' @title Rename an element in a list
#' @description This function renames an element in a list.
#' @param list The list to be modified.
#' @param old_name The name of the element to be renamed.
#' @param new_name The new name for the element.
#' @return The modified list.
#' @examples
#' rename_list_element(list, "old_name", "new_name")
#' @export

rename_list_element <- function(list, old_name, new_name) {
  if (old_name %in% names(list)) {
    names(list)[names(list) == old_name] <- new_name
  } else {
    warning(paste("No element named", old_name, "found in the list."))
  }
  return(list)
}
