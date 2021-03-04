#' Compute GPA's Persuasion Test Power Statistics
#'
#' This function generates the power values associated with
#' several treatments against a control, for a single survey question where
#' the sample size is equal across all (treatment and control). For unequal sample size, see get_power2().
#'
#' Developer note: users can take advantages of the "control" parameter to  perform hypothesis testing in the event no control variable is present.
#'
#' @param data Data containing survey response data (pre-processed by hypos_prep or equivalent)
#' @param treatment Column specifying treatments of interest (e.g., ad_name)
#' @param survey_q The survey question of interest
#' @param counts Column containing response counts for the response of interest (default to be "responses")
#' @param size Column containing sample size for each treatment (default to be "total")
#' @param alt Alternative hypothesis; can be "one.sided" or "two.sided" (default to be "one.sided")
#' @param control Variable serving as the control (default to "Control")
#' 
#' @return A new data frame containing input data plus an additional column for power values
#' @examples get_power_1n(data = df, treatment = "ads", survey_q = "q4",  counts="responses", size = "total", alt="greater", control = "Control")
#' @examples get_power_1n(data = df, treatment = "theme", survey_q = "q4",  counts="responses", size = "total", alt="greater", control = "Biden")
#' @export
get_power_1n <- function(data, treatment, survey_q, prop="percent", size = "total", alt="one.sided", control = "Control"){
  name <- paste0("power_",survey_q)
  data[[name]] <- 0
  data[[name]][which(data[[treatment]] != control)] <-
    round(power.prop.test(n=data[[size]][which(data[[treatment]] != control)],
                          p1 = data[[prop]][which(data[[treatment]] != control)],
                          p2 = data[[prop]][which(data[[treatment]] == control)],
                          sig.level = 0.05,
                          power = NULL,
                          alternative = alt,
                          strict = FALSE,
                          tol = .Machine$double.eps^0.25)$power,4)
  return(data)
}
