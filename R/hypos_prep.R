#' Data Preparation for P-value and Power Statistics Computation
#'
#' This function summarises the data by specific question(s) and its value in the survey data.
#'
#' @param data Data containing survey response data
#' @param treatment The treatment of interest (e.g., ad_name, theme, etc.)
#' @param survey_q The survey question of interest to summarise by
#' @return A smaller data frame ready for analysis
#' @param q_value The value of interest to filter by.
#' @examples hypos_prep(data = survey, treatment = "ad", survey_q = "q4", value = "agree")
#' @export
hypos_prep <- function(data, treatment, survey_q, q_value){
  treatment_var <- rlang::quo(treatment)
  survey_q_var <- rlang::quo(survey_q)
  prep <- data %>% dplyr::group_by(!!treatment_var,!!survey_q_var) %>%
    dplyr::summarise(responses=n()) %>%
    dplyr::group_by(!!treatment_var) %>%
    mutate(total = sum(responses)) %>%
    mutate(percent = responses/total) %>%
    filter({{survey_q}} == q_value)
  return(prep)
  }
