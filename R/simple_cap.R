#' Capitalize the first letter of every word
#'
#' This function will capitalize the first letter of every word in a given string
#'
#' @param x A string object or vector
#' @return A captitalized string or vector
#' @examples
#' ad.info$text <- simple_cap(ad.info$text)
#' @export
simple_cap <- function(y) {
  sapply(y,
         function(x) {
           s <- strsplit(x, " ")[[1]]
           paste(toupper(substring(s, 1,1)), substring(s, 2),
                 sep="", collapse=" ")
         },
         USE.NAMES = FALSE
  )
}

