#' Compute GPA's Persuasion Test Power Stasitics
#'
#' This function generates the power values associated with
#' several treatments against a control, for a single survey question, where
#' the sample size is different between treatment and control.
#'
#' @param data Data containing survey response data (pre-processed by hypos_prep or equivalent)
#' @param treatment Column specifying treatments of interest (e.g., ad_name)
#' @param survey_q The survey question of interest
#' @param prop Column containing proportions for the response of interest (default to be "responses")
#' @param n1 Sample size for treatments
#' @param n2 Sample size for control
#' @param alt Alternative hypothesis; can be "greater", "less", or "two.sided" (default to be "greater")
#' @return A new data frame containing input data plus an additional column for power values
#' @examples get_power2(data = df, treatment = "ads", suvey_q = "Q4",  n1=1200, n2=600, alt="greater")
#' @export

get_power2 <- function(data, treatment, survey_q, prop="percent", n1, n2, alt="greater"){
  name <- paste0("power_",survey_q)
  data[[name]] <- 0

  h2 <- pwr::ES.h(data[[prop]][which(data[[treatment]] != "Control")], data[[prop]][which(data[[treatment]] == "Control")])

  data[[name]][which(data[[treatment]] != "Control")] <-
    round(pwr::pwr.2p2n.test(h2, n1=n1, n2=n2,
                        sig.level = 0.05,
                        power = NULL,
                        alternative = "greater")$power,4)
  return(data)
}
