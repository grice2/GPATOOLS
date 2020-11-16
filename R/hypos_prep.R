#' Data Preparation for P-value and Power Statistics Computation
#'
#' This function summarises survey reponse data at the desired treatment level, 
#' for a single question and a single response of interest.
#'
#' @param data Data containing survey response data
#' @param treatment Column containing the treatments of interest (e.g., ad_name, theme, etc.)
#' @param survey_q The survey question of interest to summarise by
#' @return A smaller data frame ready for analysis
#' @param q_value The response of interest to filter by
#' @examples hypos_prep(data = survey, treatment = "ad", survey_q = "q4", value = "agree")
#' @export
hypos_prep <- function(data, treatment, survey_q, q_value){
  treatment_var <- rlang::sym(treatment)
  survey_q_var <- rlang::sym(survey_q)
  prep <- data %>% dplyr::group_by(!!treatment_var,!!survey_q_var) %>%
    dplyr::summarise(responses=n()) %>%
    dplyr::group_by(!!treatment_var) %>%
    mutate(total = sum(responses)) %>%
    mutate(percent = responses/total) %>%
    filter(!!survey_q_var == q_value)
  return(prep)
  }
