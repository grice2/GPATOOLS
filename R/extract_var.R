#' Extract Text from Variable of Interest
#'
#' This function allows for the extraction of text from a variable to make a new variable.
#' 
#' Developer note: Very useful when used with read_all()
#'
#' @param data Data containing variables of interest
#' @param from The varible from which text will be extracted
#' @param to The variable to be created containing values specified in "keys"
#' @param keys List of strings to be extracted from the variable specified in "from"
#' @param labels List of labels to be use instead of raw values in "keys"
#' @param remove.dash Whether to remove dash symbols in values of the variable specifed in "to" (Default to 'TRUE')
#' @param remove.underscore Whether to remove underscore symbols in values of the variable specifed in "to" (Default to 'TRUE')
#' @param capitalize Whether to capitalized values in the variable specifed in "to" (Default to 'TRUE')
#' @return Original dataset with an additional column containing the variable named in "to" with values in "keys" extracted from the variable specified in "from" 
#' @examples dat <- df %>% extract_var(from = "survey", to = "parent", keys = c("parents", "non-parents"), labels = c("With Children","Without Children"))
#' 
#' @export
extract_var <- function(data, from = "survey", to = "var",  keys= NA, labels = NA, remove.dash = TRUE, remove.underscore = TRUE, capitalize = TRUE) {
  data[[to]] <- NA
  if (!is.na(labels)) {
    for (i in 1:length(labels)) {
      data[[to]][str_detect(tolower(data[[from]]),tolower(keys[i]))] <- labels[i]
    }
  } else {
    for (i in 1:length(keys)) {
      data[[to]][str_detect(tolower(data[[from]]),tolower(keys[i]))] <- keys[i]
    }
  }
  re_from <- "\\b([[:lower:]])([[:lower:]]+)"
  if (capitalize) {data[[to]] <- gsub(re_from, "\\U\\1\\L\\2" ,data[[to]], perl=TRUE)}
  if (remove.underscore) {data[[to]] <- gsub("_", " ", data[[to]])}
  if (remove.dash) {data[[to]] <- gsub("-", " ", data[[to]])}
  data[[to]] <- trimws(data[[to]])
  return(data)
}