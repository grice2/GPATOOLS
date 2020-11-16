#' Capitalize the first letter of every word
#'
#' This function will capitalize the first letter of every word in a given string
#'
#' @param x A string object
#' @return A captitalized string
#' @examples
#' ad.info$text <- sapply(ad.info$text, simpleCap)
#' @export
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
