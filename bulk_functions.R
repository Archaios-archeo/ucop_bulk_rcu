library(tidyverse)

# opposite of %in%
`%ni%` <- Negate(`%in%`)



#' function to retrieve the number of times a separation pattern appears for any modality in a variable and repeat the information 
#' contained in another variable according to the number of separation
#' @example: if we have surveyors names in a variable "surveyors" and each name is separated by a "|" in that column,
#' we could duplicate the information contained in the "date" variable for as many surveyors as we have in each row
#' @param x: data table related to a sheet of one of the Arches spreadsheets “bulk upload”
#' @param variable_a_bon_pattern: variable that serves as a reference for the duplication of information 
#' (in the @example it is the variable “surveyors”)
#' @param variable_revue: variable that must be modified according to the number of separation pattern accounted
#' (in the @example it is the variable "date")

repetition_pattern_n_exact <- function(x, variable_a_bon_pattern, variable_revue){
  
  x$pattern <- str_count(string = x[[variable_a_bon_pattern]], pattern = "\\|") + 1
  
  for (i in 1:nrow(x)) {
    nombre_de_pattern <- x[i,"pattern", drop = TRUE]
    
    x[i, variable_revue] <- paste0(
      replicate(n = nombre_de_pattern, expr = x[i, variable_revue]), 
      collapse = "|")
    
  }
  
  x <- x %>%
    select(-pattern)
  
  return(x)
  
}


#' function for undetermined polygones > need to add documentation
st_queen <- function(a, b = a) st_relate(a, b, pattern = "****1****") # pattern to find sides, not corners