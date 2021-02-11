#' Label Text Wrapping
#'
#' This function wraps texts in the specified column by the number of character specified without 
#' breaking words.
#'
#' @param x Vector of strings or string object
#' @param len Maximum length of string per line
#' @return Wrapped vector of strings or wrapped string object
#' @examples wrap_labels(df$labels, 50)
#' @export
wrap_labels <- function(x, len) {
  if (is.list(x))  {
    lapply(x, 
           function(x, len){ 
             sapply(x, 
                    function(y) paste(strwrap(y, len), 
                                      collapse = "\n"), 
                    USE.NAMES = FALSE)
           }, 
           len)  
  } 
  else {
    sapply(x, 
           function(y) paste(strwrap(y, len), 
                             collapse = "\n"), 
           USE.NAMES = FALSE)
  }
}
