#' Compute GPA's Persuasion Test Power Statistics
#'
#' This function generates the power values associated with
#' several treatments against a control, for a single survey question, where
#' the sample size is different between treatment and control. A time where this may be necessary 
#' is when one groups by a theme level or country level and the sample size is not equal.
#'
#' Developer note: users can take advantages of the "control" parameter to  perform hypothesis testing in the event no control variable is present.
#'
#' @param data Data containing survey response data (pre-processed by hypos_prep or equivalent)
#' @param treatment Column specifying treatments of interest (e.g., ad_name)
#' @param survey_q The survey question of interest
#' @param prop Column containing proportions for the response of interest (default to be "responses")
#' @param n1 Sample size for treatments
#' @param n2 Sample size for control
#' @param alt Alternative hypothesis; can be "greater", "less", or "two.sided" (default to be "greater")
#' @param control Variable serving as the control (default to "Control")
#' 
#' @return A new data frame containing input data plus an additional column for power values
#' @examples get_power_2n(data = df, treatment = "ads", survey_q = "Q4",  n1=1200, n2=600, alt="greater", control = "Control")
#' @examples get_power_2n(data = df, treatment = "theme", survey_q = "Q4",  n1=1200, n2=600, alt="greater", control = "Naturalization")
#' @export
get_power_2n <- function(data, treatment, survey_q, prop="percent", n1, n2, alt="greater", control = "Control"){
  name <- paste0("power_",survey_q)
  data[[name]] <- 0

  h2 <- pwr::ES.h(data[[prop]][which(data[[treatment]] != control)], data[[prop]][which(data[[treatment]] == control)])

  data[[name]][which(data[[treatment]] != control)] <-
    round(pwr::pwr.2p2n.test(h2, n1=n1, n2=n2,
                        sig.level = 0.05,
                        power = NULL,
                        alternative = "greater")$power,4)
  return(data)
}
