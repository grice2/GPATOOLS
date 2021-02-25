#' Sort Variables by Attributes
#'
#' This function allows users to sort by two variables with specified attributes
#' and position the control variable at a specified location
#'
#' Developer notes: This function works exceptionally well when you need to prepare data for ggplot.As example,
#' users need to sort the ad variable by increasing percentage but only when the answer for question 5 is "Trustworthy".
#'
#' @param data The data frame that contains the variables of interest
#' @param var The variable to be sorted
#' @param by Quanitative variable that determine the order
#' @param attr Attribute variable to filter (Default to NA)
#' @param ctrl_posn Position of the control variable: "Top", "Bottom", "Default"
#' @param decreasing Decreasing order of the variable specified in "by" (Defaults to "increasing")
#' @return Data frame with "var" sorted by "by" and attributes specified in "attr" and the position of the control specified in "ctrl_posn"
#' @examples ad_analysis <- ad_analysis %>% ggsort(var = "text", by = "netp", attr = c("age", "Over 35"), ctrl_posn = "Bottom")
#' @export 
ggsort <- function(data, var, by, attr = NA,  ctrl_posn = "Default", decreasing = FALSE){
  ctrl_posn <- tolower(ctrl_posn)
  if (!is.na(attr)) {
    if (ctrl_posn == "top") {
      data[[var]] <- factor( data[[var]], levels = 
        c(unique(
          data[[var]][
            data[[var]] != "Control" & 
              data[[attr[1]]] == attr[2]
            ][
              order(data[[by]][data[[var]] != "Control" & 
                                 data[[attr[1]]] == attr[2]
                               ], 
                    decreasing = !decreasing)]), "Control"))}
    if (ctrl_posn == "bottom") {
      data[[var]] <- factor( data[[var]], levels = 
        c("Control", unique(
          data[[var]][
            data[[var]] != "Control" & 
              data[[attr[1]]] == attr[2]
            ][
              order(data[[by]][data[[var]] != "Control" & 
                                 data[[attr[1]]] == attr[2]
                               ], 
                    decreasing = !decreasing)])))}
    if (ctrl_posn == "default") {
      data[[var]] <- factor( data[[var]], levels = 
        c(unique(
          data[[var]][
            data[[attr[1]]] == attr[2]
            ][
              order(data[[by]][
                data[[attr[1]]] == attr[2]
                ], 
                decreasing = !decreasing)])))}
  } else {
    if (ctrl_posn == "top") {
      data[[var]] <- factor( data[[var]], levels = 
        c(unique(
          data[[var]][
            data[[var]] != "Control" 
            ][
              order(data[[by]][data[[var]] != "Control" 
                               ], 
                    decreasing = !decreasing)]), "Control"))}
    if (ctrl_posn == "bottom") {
      data[[var]] <- factor( data[[var]], levels = 
        c("Control", unique(
          data[[var]][
            data[[var]] != "Control" 
            ][
              order(data[[by]][data[[var]] != "Control" 
                               ], 
                    decreasing = !decreasing)])))}
    if (ctrl_posn == "default") {
      data[[var]] <- factor( data[[var]], levels = 
        c(unique(
          data[[var]][
            order(data[[by]], 
                  decreasing = !decreasing)])))}
  }
  return(data)
}