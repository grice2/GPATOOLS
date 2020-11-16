#' Compute GPA's Persuasion Test P-values
#'
#' This function generates the p-values associated with
#' several treatments against a SINGLE control, for a single survey question's response.
#'
#' Data must have been preprocessed by hypos_prep() or equivalence. Data containing survey response data,
#' summarized at the treatment level and containing counts of responses for
#' several treatments against a SINGLE control, for a single survey question.
#' Preprocessing that generates input data usually looks like this:
#' Q4.ad.analysis <- combined.data %>% group_by(Ad,Q4) %>% summarise(n_q4=n()) %>% group_by(Ad) %>%
#' mutate(total_q4 = sum(n_q4)) %>% mutate(percent_q4 = n_q4/total_q4) %>% filter(Q4 == 1)
#'
#' @param data Data containing survey response data (pre-processed by hypos_prep)
#' @param treatment Column specifying treatments of interest (e.g., ad_name)
#' @param survey_q The survey question of interest to summarise by
#' @param counts Column containing response counts for the question of interest (default to be "responses")
#' @param size Column containing sample size for each treatment (default to be "total")
#' @param alt Alternative hypothesis; can be "greater", "less", or "two.sided" (default to be "greater")
#' @return A new data frame that contains an additional column for p-values
#' @examples get_alpha(data = df, treatment = "ads", suvey_q = "q4", counts="responses", size = "total", alt="greater")
#' @export
get_alpha <- function(data, treatment, survey_q, counts="responses", size = "total", alt="greater"){
  name <- paste0("pval_",suvey_q)
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

