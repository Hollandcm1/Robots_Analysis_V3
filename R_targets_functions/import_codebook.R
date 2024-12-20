#' @title Import Codebook
#' @description Import the codebook for the Robots dataset
#' @return A list with two dataframes: the first contains the conditions and the second contains the participant information
#' @examples
#' codebook <- import_codebook()
#' 
#' @importFrom here here
#' @importFrom openxlsx read.xlsx

# Import Codebook
import_codebook <- function() {
  codebook <- read.xlsx(here("data", "codebook", "Robots - Codebook.xlsx"))
  
  # pull subset relevant for determining what the different conditions were doing
  codes.conditions <- codebook[, c("X7", "Cond.1", "Cond.2", "Cond.3", "Cond.4")]
  codes.conditions <- codes.conditions[1:2,]
  
  # relabel
  names(codes.conditions) <- c("Factor", "Condition 1", "Condition 2", "Condition 3", "Condition 4")
  
  # pull participant information
  codes.participant_conditions <- codebook[, c("Participant", "first", "second", "third", "forth")]
  names(codes.participant_conditions) <- c("Participant", "First Condition", "Second Condition", "Third Condition", "Forth Condition")
  
  Sys.time()
  return(list(codes.conditions, codes.participant_conditions))
}
