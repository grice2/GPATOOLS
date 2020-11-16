#' Compute GPA's Persuasion Test Power Stasitics
#'
#' This function generates the power values associated with
#' several treatments against a control, for a single survey question where
#' the sample size is equal across all (treatment and control). For unequal sample size, see get_power2
#'
#' @param data Data containing survey response data (pre-processed by hypos_prep or equivalent)
#' @param treatment Column specifying treatments of interest (e.g., ad_name)
#' @param survey_q The survey question of interest to summarise by
#' @param counts Column containing response counts for the question of interest (default to be "responses")
#' @param size Column containing sample size for each treatment (default to be "total")
#' @param alt Alternative hypothesis; can be "greater", "less", or "two.sided" (default to be "greater")
#' @return A new data frame containing input data plus an additional column for power values
#' @examples get_power1(data = df, treatment = "ads", suvey_q = "q4",  counts="responses", size = "total", alt="greater")
#' @export
get_power1 <- function(data, treatment, survey_q, prop="percent", size = "total", alt="one.sided"){
  name <- paste0("power_",survey_q)
  data[[name]] <- 0
  data[[name]][which(data[[treatment]] != "Control")] <-
    round(power.prop.test(n=data[[size]],
                          p1 = data[[prop]][which(data[[treatment]] != "Control")],
                          p2 = data[[prop]][which(data[[treatment]] == "Control")],
                          sig.level = 0.05,
                          power = NULL,
                          alternative = alt,
                          strict = FALSE,
                          tol = .Machine$double.eps^0.25)$power,4)
  return(data)
}
