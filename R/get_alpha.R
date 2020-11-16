#' Compute GPA's Persuasion Test P-values
#'
#' This function generates the p-values associated with
#' several treatments of inteterest against a single control, for responses to a single survey question.
#'
#' Data must have been preprocessed by hypos_prep() or equivalent, and should contain survey response data
#' summarized at the desired treatment level, for a single survey question. Preprocessing that generates input data usually looks like this:
#' input_df <- survey_data %>% group_by(treatment,question) %>% summarise(responses=n()) %>% group_by(treatment) %>%
#' mutate(total = sum(responses)) %>% mutate(percent = responses/total) %>% filter(question == value)
#'
#' @param data Data containing survey response data (pre-processed by hypos_prep or equivalent)
#' @param treatment Column specifying treatments of interest (e.g., ad_name)
#' @param survey_q The survey question of interest to summarise by
#' @param counts Column containing response counts for the question of interest (default to be "responses")
#' @param size Column containing sample size for each treatment (default to be "total")
#' @param alt Alternative hypothesis; can be "greater", "less", or "two.sided" (default to be "greater")
#' @return A new data frame containing input data plus an additional column for p-values
#' @examples get_alpha(data = df, treatment = "ads", suvey_q = "q4", counts="responses", size = "total", alt="greater")
#' @export
get_alpha <- function(data, treatment, survey_q, counts="responses", size = "total", alt="greater"){
  name <- paste0("pval_",survey_q)
  data[[name]] <- 0
  for (i in which(data[[treatment]] != "Control")){
    data[[name]][i] <-round(
      prop.test(x = c(data[[counts]][i],
                      data[[counts]][data[[treatment]]=='Control']),
                n = c(data[[size]][i],
                      data[[size]][data[[treatment]]=='Control']),
                alternative = alt)$p.value,4)
  }
  return(data)
}

